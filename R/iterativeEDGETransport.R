#' EDGE-Transport iterative
#'
#' Run in the REMIND output folder.
#'
#' @md
#' @param reporting generate EDGE-T reporting data
#' @return NULL
#' @author Alois Dirnaichner
#' @importFrom data.table fread
#' @importFrom gdxdt writegdx.parameter
#' @importFrom gdxrrw rgdx
#' @export


toolIterativeEDGETransport <- function(reporting=FALSE) {
  `.` <- CountryCode <- EJ_Mpkm_final <- RegionCode <- V1 <- cfg <- check <- demNew <- demVintEachYear <-
    demand_F <- full_demand_vkm <- iternum <- maxtech <- sector <- shareVS1 <- sharetech_new <-
      subsector_L1 <- subsector_L3 <- sumvalue <- sw <- tot_price <- totdem <- value <-
        vintage_demand_vkm <- NULL

  print(paste("---", Sys.time(), "Start of the EDGE-T iterative model run."))

  data_folder <- "EDGE-T"

  datapath <- function(fname) {
    file.path(data_folder, fname)
  }

  REMINDpath <- function(fname) {
    file.path("../../", fname)
  }

  REMINDyears <- c(1990,
                   seq(2005, 2060, by = 5),
                   seq(2070, 2110, by = 10),
                   2130, 2150)

  gdx <- "input.gdx"
  if(file.exists("fulldata.gdx"))
    gdx <- "fulldata.gdx"

  load("config.Rdata")
  scenario <- cfg$gms$cm_GDPscen
  EDGE_scenario <- cfg$gms$cm_EDGEtr_scen
  demScen <- cfg$gms$cm_demScen

  ## learning is OFF by default
  learning = FALSE

  REMIND2ISO_MAPPING <- fread(REMINDpath(cfg$regionmapping))[, .(iso = CountryCode, region = RegionCode)]
  EDGE2teESmap <- fread("mapping_EDGE_REMIND_transport_categories.csv")

  ## input data loading
  input_folder = paste0("./")
  if (length(list.files(path = data_folder, pattern = "RDS")) < 8) {
    toolCreateRDS(input_folder, data_folder,
                  SSP_scenario = scenario,
                  DEM_scenario = demScen,
                  EDGE_scenario = EDGE_scenario)
  }
  inputdata <- toolLoadInputData(data_folder)


  vot_data = inputdata$vot_data
  logit_params = inputdata$logit_params
  int_dat = inputdata$int_dat
  nonfuel_costs = inputdata$nonfuel_costs
  capcost4W = inputdata$capcost4W
  loadFactor = inputdata$loadFactor
  price_nonmot = inputdata$price_nonmot
  pref_data = inputdata$pref_data
  preftab4W = inputdata$ptab4W

  setnames(preftab4W, old = "ptab4W", new = "value")

  ## mrremind produces all combinations of iso-vehicle types and attributes a 0. These ghost entries have to be cleared.
  int_dat = int_dat[EJ_Mpkm_final>0]
  prefdata_nonmot = pref_data$FV_final_pref[subsector_L3 %in% c("Walk", "Cycle")]
  pref_data$FV_final_pref = merge(pref_data$FV_final_pref, unique(int_dat[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)
  pref_data$FV_final_pref[, check := sum(value), by = c("vehicle_type", "region")]
  pref_data$FV_final_pref = pref_data$FV_final_pref[check>0]
  pref_data$FV_final_pref[, check := NULL]
  pref_data$FV_final_pref = rbind(prefdata_nonmot, pref_data$FV_final_pref)

  prefdata_nonmotV = pref_data$VS1_final_pref[subsector_L3 %in% c("Walk", "Cycle")]
  pref_data$VS1_final_pref = merge(pref_data$VS1_final_pref, unique(int_dat[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)
  pref_data$VS1_final_pref[, check := sum(sw), by = c("vehicle_type", "region")]
  pref_data$VS1_final_pref = pref_data$VS1_final_pref[check>0]
  pref_data$VS1_final_pref[, check := NULL]
  pref_data$VS1_final_pref = rbind(prefdata_nonmotV, pref_data$VS1_final_pref)

  ## optional average of prices
  average_prices = TRUE

  ## calculate the ES demand (in million km)
  ES_demand_all = toolReadREMINDdemand(gdx = gdx, REMINDmapping = REMIND2ISO_MAPPING,
                                       EDGE2teESmap = EDGE2teESmap, years = REMINDyears,
                                       scenario = scenario)

  ## select from total demand only the passenger sm
  ES_demand = ES_demand_all[sector == "trn_pass",]


  if (file.exists(datapath("demand_previousiter.RDS")) & learning) {
    ## load previous iteration number of cars
    demand_learntmp = readRDS(datapath("demand_learn.RDS"))
    ## load previous iteration demand
    ES_demandpr = readRDS(datapath("demand_previousiter.RDS"))
    ## load previus iteration number of stations
    stations = readRDS(datapath("stations.RDS"))
    ## calculate non fuel costs for technologies subjected to learning and merge the resulting values with the historical values
    nonfuel_costs = merge(nonfuel_costs, unique(int_dat[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)

    nonfuel_costs_list = toolLearning(
      non_fuel_costs = nonfuel_costs, capcost4W = capcost4W,
      gdx =  gdx, EDGE2teESmap = EDGE2teESmap, demand_learntmp = demand_learntmp,
      ES_demandpr =  ES_demandpr, ES_demand =  ES_demand)

    nonfuel_costs = nonfuel_costs_list$nonfuel_costs
    capcost4W = nonfuel_costs_list$capcost4W
    saveRDS(nonfuel_costs, "nonfuel_costs_learning.RDS")
    saveRDS(capcost4W, "capcost_learning.RDS")
  } else {
    totveh = NULL
  }

  ## load price
  REMIND_prices <- toolMergePrices(
    gdx = gdx,
    REMINDmapping = REMIND2ISO_MAPPING,
    REMINDyears = REMINDyears,
    intensity_data = int_dat,
    nonfuel_costs = nonfuel_costs)


  ## save prices
  ## read last iteration count
  keys <- c("region", "year", "technology", "vehicle_type")
  setkeyv(REMIND_prices, keys)

  pfile <- "EDGE_transport_prices.rds"
  iter <- as.vector(rgdx(gdx, list(name="o_iterationNumber"))$val)

  REMIND_prices[, iternum := iter]

  ## save REMIND prices (before dampening)
  saveRDS(REMIND_prices, datapath(paste0("REMINDprices", iter, ".RDS")))


  if(average_prices) {

    if(max(unique(REMIND_prices$iternum)) >= 20 &
       max(unique(REMIND_prices$iternum)) <= 30 &
       file.exists(datapath(pfile))) {
      old_prices <- readRDS(datapath(pfile))
      all_prices <- rbind(old_prices, REMIND_prices)
      setkeyv(all_prices, keys)
      ## apply moving avg
      REMIND_prices <- REMIND_prices[
        all_prices[iternum >= 20, mean(tot_price), by=keys], tot_price := V1]
      all_prices <- rbind(old_prices, REMIND_prices)
    } else {
      all_prices <- REMIND_prices
    }
    saveRDS(all_prices, datapath(pfile))

    ## save REMIND prices (after dampening)
    saveRDS(REMIND_prices, datapath(paste0("REMINDpricesDampened", iter, ".RDS")))

  }

  REMIND_prices[, "iternum" := NULL]

  ## calculates logit
  years=copy(REMINDyears)
  if (file.exists(datapath("demand_totalLDV.RDS"))) {
    ## load previous iteration number of cars
    totveh = readRDS(datapath("demand_totalLDV.RDS"))
  }
  logit_data_4W <- toolCalculateLogitIncost(
    prices= REMIND_prices[tot_price > 0],
    vot_data = vot_data,
    pref_data = pref_data,
    logit_params = logit_params,
    intensity_data = int_dat,
    price_nonmot = price_nonmot,
    ptab4W = preftab4W,
    tech_scen = EDGE_scenario,
    totveh = if (!is.null(totveh)) totveh)

  logit_data <- toolCalculateLogitSW(
    prices= REMIND_prices[tot_price > 0],
    vot_data = vot_data,
    pref_data = pref_data,
    logit_params = logit_params,
    intensity_data = int_dat,
    price_nonmot = price_nonmot,
    ptab4W = preftab4W,
    logit_data_4W = logit_data_4W)

  shares <- logit_data[["share_list"]] ## shares of alternatives for each level of the logit function
  ## shares$VS1_shares=shares$VS1_shares[,-c("sector","subsector_L2","subsector_L3")]

  mj_km_data <- logit_data[["mj_km_data"]] ## energy intensity at a technology level
  prices <- logit_data[["prices_list"]] ## prices at each level of the logit function, 1990USD/pkm


  ## calculate vintages (new shares, prices, intensity)
  vintages = toolCalcVint(shares = shares,
                          totdem_regr = ES_demand_all,
                          prices = prices,
                          mj_km_data = mj_km_data,
                          years = REMINDyears)

  shares$FV_shares = vintages[["shares"]]$FV_shares
  prices = vintages[["prices"]]
  mj_km_data = vintages[["mj_km_data"]]


  ## use logit to calculate shares and intensities (on tech level)
  EDGE2CESmap <- fread("mapping_CESnodes_EDGE.csv")


  shares_int_dem <- toolSharesIntensityDemand(
    logit_shares=shares,
    MJ_km_base=mj_km_data,
    REMINDyears=REMINDyears,
    scenario=scenario,
    demand_input = if (reporting) ES_demand_all)

  demByTech <- shares_int_dem[["demand"]] ##in [-]
  intensity <- shares_int_dem[["demandI"]] ##in million pkm/EJ
  norm_demand <- shares_int_dem[["demandF_plot_pkm"]] ## totla demand normalized to 1; if reporting, in million km

  if (reporting) {
    saveRDS(vintages, file = datapath("vintages.RDS"))
    saveRDS(shares, file = datapath("shares.RDS"))
    saveRDS(logit_data$EF_shares, file = datapath("EF_shares.RDS"))
    saveRDS(logit_data$mj_km_data, file = datapath("mj_km_data.RDS"))
    saveRDS(shares_int_dem$demandF_plot_EJ,
            file=datapath("demandF_plot_EJ.RDS"))
    saveRDS(shares_int_dem$demandF_plot_pkm,
            datapath("demandF_plot_pkm.RDS"))
    saveRDS(logit_data$annual_sales, file = datapath("annual_sales.RDS"))
    saveRDS(logit_data$pref_data, file = datapath("pref_output.RDS"))
    saveRDS(logit_params, file = datapath("logit_params.RDS"))
    saveRDS(logit_data, file = datapath("logit_data.RDS"))

    ## the following vintages calculation is deprecated and shall not be updated!
    ## as soon as the new reporting (edgeTransport::reportEDGETransport2) is used,
    ## this can be deleted.
    vint <- vintages[["vintcomp_startyear"]]
    newd <- vintages[["newcomp"]]
    sharesVS1 <- shares[["VS1_shares"]]
    setnames(sharesVS1, "share", "shareVS1")

    newd <- sharesVS1[newd, on=c("region", "year", "subsector_L1", "vehicle_type")]
    newd[, demNew := totdem * sharetech_new * shareVS1]

    ## newd <- loadFactor[newd, on=c("year", "region", "vehicle_type")]
    ## newd[, demNew := demNew/loadFactor]

    vint <- newd[vint, on=c("region", "subsector_L1", "vehicle_type", "technology", "year", "sector")]
    vint <- vint[!is.na(demNew)]
    vint <- vint[, c("year", "region", "vehicle_type", "technology", "variable", "demNew", "demVintEachYear")]
    vint[, demand_F := demNew + sum(demVintEachYear), by=c("region", "year", "vehicle_type", "technology")]

    vint <- loadFactor[vint, on=c("year", "region", "vehicle_type")]

    vint[, full_demand_vkm := demand_F/loadFactor]
    vint[, vintage_demand_vkm := demVintEachYear/loadFactor]
    vint[, c("demand_F", "demVintEachYear", "loadFactor", "demNew") := NULL]

    setnames(vint, "variable", "construction_year")

    vintfile <- "vintcomp.csv"
    cat("# LDV Fleet vintages.", file=vintfile, sep="\n")
    cat("# full_demand_vkm is the full demand for a given year, region, vehicle_type and technology.", file=vintfile, sep="\n", append=TRUE)
    cat("# New sales for the current year can be calculated by full_demand_vkm - sum(vintage_demand_vkm).", file=vintfile, sep="\n", append=TRUE)
    cat("# Units: million vkms.", file=vintfile, sep="\n", append=TRUE)

    fwrite(vint, vintfile, col.names=TRUE, append=TRUE)

    return()
  }

  num_veh_stations = toolVehicleStations(
    norm_dem = norm_demand[
      subsector_L1 == "trn_pass_road_LDV_4W", ## only 4wheelers
      c("region", "year", "sector", "vehicle_type", "technology", "demand_F") ],
    ES_demand_all = ES_demand_all,
    intensity = intensity,
    loadFactor = loadFactor,
    EDGE2teESmap = EDGE2teESmap,
    rep = reporting)

  ## save number of vehicles for next iteration
  saveRDS(num_veh_stations$learntechdem, datapath("demand_learn.RDS"))  ## in million veh
  saveRDS(num_veh_stations$alltechdem, datapath("demand_totalLDV.RDS")) ## in million veh
  ## save the demand for next iteration renaming the column
  setnames(ES_demand, old ="demand", new = "demandpr")                  ## in million passenger-km
  saveRDS(ES_demand, datapath("demand_previousiter.RDS"))


  ## use logit to calculate costs
  REMINDCapCost <- toolCapCosts(
    base_price=prices$base,
    Fdemand_ES = shares_int_dem[["demandF_plot_pkm"]],
    stations = num_veh_stations$stations,
    EDGE2CESmap = EDGE2CESmap,
    EDGE2teESmap = EDGE2teESmap,
    REMINDyears = REMINDyears,
    scenario = scenario)
  saveRDS(REMINDCapCost[["capCostPerTech"]], file = datapath("capCostPerTech.RDS"))

  ## full REMIND time range for inputs
  REMINDtall <- c(seq(1900,1985,5),
                  seq(1990, 2060, by = 5),
                  seq(2070, 2110, by = 10),
                  2130, 2150)

  ## prepare the entries to be saved in the gdx files: intensity, shares, non_fuel_price. Final entries: intensity in [trillionkm/Twa], capcost in [2005USD/trillionpkm], shares in [-]
  finalInputs <- toolPrepare4REMIND(
    demByTech = demByTech,
    intensity = intensity,
    capCost = REMINDCapCost[["CESlevelCosts"]],
    EDGE2teESmap = EDGE2teESmap,
    REMINDtall = REMINDtall)



  ## add the columns of SSP scenario and EDGE scenario to the output parameters
  for (i in names(finalInputs)) {
    finalInputs[[i]]$SSP_scenario <- scenario
    finalInputs[[i]]$DEM_scenario <- demScen
    finalInputs[[i]]$EDGE_scenario <- EDGE_scenario
  }


  ## calculate shares
  finalInputs$shFeCes = finalInputs$demByTech[, value := value/sum(value), by = c("tall", "all_regi", "all_in")]
  ## 7 decimals the lowest accepted value
  finalInputs$shFeCes[, value := round(value, digits = 7)]
  finalInputs$shFeCes[, value := ifelse(value == 0, 1e-7, value)]
  finalInputs$shFeCes[, sumvalue := sum(value), by = c("tall", "all_regi", "all_in")]
  finalInputs$shFeCes[, maxtech := ifelse(value == max(value), TRUE, FALSE), by =c("tall", "all_regi", "all_in")]

  ## attribute the variation to the maximum share value
  finalInputs$shFeCes[sumvalue!=1 & maxtech==TRUE, value := value + (1-sumvalue), by = c("tall", "all_regi")]
  ## remove temporary columns
  finalInputs$shFeCes[, c("sumvalue", "maxtech") := NULL]

  ## CapCosts
  writegdx.parameter("p35_esCapCost.gdx", finalInputs$capCost, "p35_esCapCost",
                     valcol="value", uelcols=c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs"))

  ## Intensities
  writegdx.parameter("p35_fe2es.gdx", finalInputs$intensity, "p35_fe2es",
                     valcol="value", uelcols = c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs"))

  ## Shares: demand can represent the shares since it is normalized
  writegdx.parameter("p35_shFeCes.gdx", finalInputs$shFeCes, "p35_shFeCes",
                     valcol="value",
                     uelcols = c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_enty", "all_in", "all_teEs"))

  print(paste("---", Sys.time(), "End of the EDGE-T iterative model run."))

}
