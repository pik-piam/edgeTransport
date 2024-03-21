#' Reporting for the coupled EDGE-T Transport Sector Model (REMIND Module edge_esm)
#'
#' Data is loaded from the EDGE-T subfolder in the output folder or from the `level_2` folder
#' in a standalone run.
#' For a coupled run the EDGE-T output files can be (re-) generated calling
#' `Rscript EDGETransport.R --reporting`
#' from the output folder.
#'
#' Region subsets are obtained from fulldata.gdx
#'
#' @param output_folder path to the output folder, default is current folder.
#' @param extendedReporting report a larger set of variables
#' @param scenario_title a scenario title string
#' @param model_name a model name string
#' @param gdx path to the GDX file used for the run.
#' @author Johanna Hoppe Alois Dirnaichner Marianna Rottoli
#'
#' @importFrom gdxdt readgdx
#' @importFrom data.table fread fwrite rbindlist copy CJ
#' @importFrom remind2 toolRegionSubsets
#' @importFrom quitte as.quitte aggregate_map
#' @importFrom magclass as.magpie getItems getNames mselect dimSums setNames
#' @importFrom rmndt approx_dt readMIF writeMIF
#' @importFrom magrittr %>%
#' @export

toolReportEDGET <- function(output_folder = ".",
                            extendedReporting = FALSE,
                            scenario_title = NULL, model_name = "EDGE-Transport",
                            gdx = NULL) {

  RegionCode <- `.` <- sector <- subsector_L3 <- region <- year <- fuel <- demNew <- totdem <-
    subsector_L2 <- subsector_L1 <- demand_F <- remind_rep <- aggr_veh <- technology <- ttot <-
      se_share <- fe_demand <- variable <- value <- loadFactor <- sharetech_new <- shareVS1 <-
        all_enty <- ef <- variable_agg <- aggr_mode <- det_veh <- model <- scenario <- period <-
          se <- type <- ven <- vehicle_type <- vehicle_type <- capture.output <- demVintEachYear <-
            unit <- tot_VOT_price <- tot_price <- logit_type <- weight <- liqsplit <-
              full_demand_vkm <- vintage_demand_vkm <- stock_demand <- sales_demand <- full_demand_vkm <-
                typ <- FE <- tot_vint_demand <- non_fuel_price <- UE_efficiency <- NULL

  #pkm or tkm is called km in the reporting. Vehicle km are called vkm
  yrs <- c(seq(2005, 2060, 5), seq(2070, 2100, 10))
  Aggrdata <- fread(system.file("extdata", "EDGETdataAggregation.csv", package = "edgeTransport"), header = TRUE)

  datapath <- function(fname){
    file.path(output_folder, fname)}

  # splits  FE Liquids variables into Biomass, Fossil and Hydrogen according to FE demand shares
  split_fe_liquids <- function(df) {

    demFeSector <- readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = F)

    # biomass share in biomass+hydrogen liquids in total transport sector
    bioShareTrans <- dimSums(mselect(demFeSector, all_enty = "seliqbio", emi_sectors = "trans"), dim = 3, na.rm = T) /
      dimSums(mselect(demFeSector, all_enty = c("seliqbio", "seliqsyn"), emi_sectors = "trans"), dim = 3, na.rm = T)

    # hydrogen share in biomass+hydrogen liquids in total transport sector
    synShareTrans <- dimSums(mselect(demFeSector, all_enty = "seliqsyn", emi_sectors = "trans"), dim = 3, na.rm = T) /
      dimSums(mselect(demFeSector, all_enty = c("seliqbio", "seliqsyn"), emi_sectors = "trans"), dim = 3, na.rm = T)

    # calculate LDV share ----

    # liquids for LDVs
    demFeSectorLdv <- mselect(demFeSector,
                              all_enty = c("seliqfos", "seliqbio", "seliqsyn"),
                              all_enty1 = "fepet", emi_sectors = "trans"
    )

    feShareLdvLiqFos <- dimSums(demFeSectorLdv[,, "seliqfos.fepet"], dim = 3, na.rm = T) / dimSums(demFeSectorLdv, dim = 3, na.rm = T)

    # for non-fossil liquids we apply the share of the transport sector to the subsector
    feShareLdvLiqBio <- dimSums(mselect(demFeSectorLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * bioShareTrans / dimSums(demFeSectorLdv, dim = 3, na.rm = T)
    feShareLdvLiqSyn <- dimSums(mselect(demFeSectorLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * synShareTrans / dimSums(demFeSectorLdv, dim = 3, na.rm = T)

    # calculate share for Non-LDV (Trucks, Domestic Aviation etc.) ----

    # liquids for Non-LDVs
    demFeSectorNonLdv <- mselect(demFeSector,
                                 all_enty = c("seliqfos", "seliqbio", "seliqsyn"),
                                 all_enty1 = "fedie", emi_sectors = "trans", all_emiMkt = "ES"
    )

    feShareNonLdvLiqFos <- demFeSectorNonLdv[,, "seliqfos.fedie"] / dimSums(demFeSectorNonLdv, dim = 3, na.rm = T)
    feShareNonLdvLiqBio <- dimSums(mselect(demFeSectorNonLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * bioShareTrans / dimSums(demFeSectorNonLdv, dim = 3, na.rm = T)
    feShareNonLdvLiqSyn <- dimSums(mselect(demFeSectorNonLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * synShareTrans / dimSums(demFeSectorNonLdv, dim = 3, na.rm = T)

    # calculate share for bunkers ----

    # liquids for bunkers
    demFeSectorBunkers <- mselect(demFeSector,
                                  all_enty = c("seliqfos", "seliqbio", "seliqsyn"),
                                  all_enty1 = "fedie", emi_sectors = "trans", all_emiMkt = "other"
    )

    feShareBunkersLiqFos <- demFeSectorBunkers[,, "seliqfos.fedie"] / dimSums(demFeSectorBunkers, dim = 3, na.rm = T)
    feShareBunkersLiqBio <- dimSums(mselect(demFeSectorBunkers, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * bioShareTrans / dimSums(demFeSectorBunkers, dim = 3, na.rm = T)
    feShareBunkersLiqSyn <- dimSums(mselect(demFeSectorBunkers, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * synShareTrans / dimSums(demFeSectorBunkers, dim = 3, na.rm = T)

    # apply splits to data frame ----
    #magclass needs the following column order
    m <- as.magpie(df[, c(4, 1, 5, 6, 2, 3, 7)])
    y <- intersect(getItems(m, dim = 2), getItems(demFeSector, dim = 2))
    prefix <- paste0(getNames(m, dim = 1), ".", getNames(m, dim = 2), ".")
    suffix <- paste0(".", getNames(m, dim = 4))

    tmp <- mbind(
      setNames(m[, y, "FE|Transport|Pass|Road|LDV|Liquids"] * feShareLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Pass|Road|LDV|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Road|LDV|Liquids"] * feShareLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Pass|Road|LDV|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Road|LDV|Liquids"] * feShareLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Pass|Road|LDV|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Road|Liquids"] * feShareNonLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Freight|Road|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Road|Liquids"] * feShareNonLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Freight|Road|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Road|Liquids"] * feShareNonLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Freight|Road|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Road|Bus|Liquids"] * feShareNonLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Pass|Road|Bus|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Road|Bus|Liquids"] * feShareNonLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Pass|Road|Bus|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Road|Bus|Liquids"] * feShareNonLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Pass|Road|Bus|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Rail|Liquids"] * feShareNonLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Freight|Rail|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Rail|Liquids"] * feShareNonLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Freight|Rail|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Rail|Liquids"] * feShareNonLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Freight|Rail|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Rail|Liquids"] * feShareNonLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Pass|Rail|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Rail|Liquids"] * feShareNonLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Pass|Rail|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Rail|Liquids"] * feShareNonLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Pass|Rail|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Aviation|Domestic|Liquids"] * feShareNonLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Pass|Aviation|Domestic|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Aviation|Domestic|Liquids"] * feShareNonLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Pass|Aviation|Domestic|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Aviation|Domestic|Liquids"] * feShareNonLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Pass|Aviation|Domestic|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Navigation|Liquids"] * feShareNonLdvLiqFos[, y, ], paste0(prefix, "FE|Transport|Freight|Navigation|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Navigation|Liquids"] * feShareNonLdvLiqBio[, y, ], paste0(prefix, "FE|Transport|Freight|Navigation|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Freight|Navigation|Liquids"] * feShareNonLdvLiqSyn[, y, ], paste0(prefix, "FE|Transport|Freight|Navigation|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Aviation|International|Liquids"] * feShareBunkersLiqFos[, y, ], paste0(prefix, "FE|Transport|Pass|Aviation|International|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Aviation|International|Liquids"] * feShareBunkersLiqBio[, y, ], paste0(prefix, "FE|Transport|Pass|Aviation|International|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Pass|Aviation|International|Liquids"] * feShareBunkersLiqSyn[, y, ], paste0(prefix, "FE|Transport|Pass|Aviation|International|Liquids|Hydrogen", suffix)),
      setNames(m[, y, "FE|Transport|Freight|International Shipping|Liquids"] * feShareBunkersLiqFos[, y, ], paste0(prefix, "FE|Transport|Freight|International Shipping|Liquids|Fossil", suffix)),
      setNames(m[, y, "FE|Transport|Freight|International Shipping|Liquids"] * feShareBunkersLiqBio[, y, ], paste0(prefix, "FE|Transport|Freight|International Shipping|Liquids|Biomass", suffix)),
      setNames(m[, y, "FE|Transport|Freight|International Shipping|Liquids"] * feShareBunkersLiqSyn[, y, ], paste0(prefix, "FE|Transport|Freight|International Shipping|Liquids|Hydrogen", suffix))
    )
    ## Convert back to data.table
    tmp <- as.data.table(as.quitte(tmp))
    #Get rid of NAS
    tmp <- tmp[!is.na(variable)]
    tmp <- approx_dt(tmp, yrs, xcol = "period", ycol = "value",
                            idxcols = c("scenario", "variable", "unit", "model", "region"),
                            extrapolate = T)


    return(tmp)
  }

  reporting <- function(dt, mode) {
    datatable <- copy(dt)
    aggr_mode_tech <- aggr_LDV <- aggr_LDV_tech <- det_veh_tech <- aggr_bunkers <- aggr_bunkers_tech <- aggr_veh_tech <- capture.output <- NULL
    report <- list()

    datatable[, sector := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "Pass", "Freight")]
    datatable <- merge(datatable, Aggrdata, by = c("sector", "subsector_L1", "subsector_L2", "subsector_L3", "vehicle_type", "technology"), all.x = TRUE, allow.cartesian = TRUE)

    #How to account for Hybrid Electric in Final Energy?
    if (mode == "FE") {
      datatable[, remind_rep := fuel]
    } else {
      datatable[, remind_rep := technology]
    }

    datatable[remind_rep == "NG", remind_rep := "Gases"]

    datatable[!is.na(aggr_mode) & !is.na(remind_rep), aggr_mode_tech := paste0(aggr_mode, "|", remind_rep)]
    datatable[!is.na(aggr_veh) & !is.na(remind_rep), aggr_veh_tech := paste0(aggr_veh, "|", remind_rep)]
    datatable[!is.na(aggr_LDV) & !is.na(remind_rep), aggr_LDV_tech := paste0(aggr_LDV, "|", remind_rep)]
    datatable[!is.na(det_veh) & !is.na(remind_rep), det_veh_tech := paste0(det_veh, "|", remind_rep)]
    datatable[!is.na(aggr_bunkers) & !is.na(remind_rep), aggr_bunkers_tech := paste0(aggr_bunkers, "|", remind_rep)]

    prefix <- switch(mode,
                     "FE" = "FE|Transport|",
                     "ES" = "ES|Transport|",
                     "VKM" = "ES|Transport|VKM|",
                     "CC" = "Capital Cost|Transport|")

    var <- c("Pass", "Freight")

    Aggr <- c("aggr_mode", "aggr_veh", "aggr_LDV", "det_veh", "nonmot", "aggr_nonmot", "aggr_bunkers", "aggr_mode_tech", "aggr_veh_tech", "aggr_LDV_tech", "det_veh_tech", "aggr_bunkers_tech")


    for (var0 in var) {

      for (Aggr0 in Aggr) {

        unit <- switch(mode,
                   "FE" = "EJ/yr",
                   "ES" = if(var0 == "Pass"){"bn pkm/yr"}else{"bn tkm/yr"},
                   "VKM" = "bn vkm/yr",
                   "CC" = "bn US$2005")

        #Aggregate data
        datatable0 <- copy(datatable)
        datatable0 <- datatable0[!is.na(get(Aggr0))]

        datatable0 <- datatable0[sector == var0, .(value = sum(value, na.rm = T)),
                                 by = c("region", "year", Aggr0)]

        if (nrow(datatable0) > 0) {

          setnames(datatable0, "year", "period")

          datatable0 <- datatable0[, model := model_name][, scenario := scenario_title][, variable := paste0(prefix, get(Aggr0))][, unit := unit][, eval(Aggr0) := NULL]

          datatable0 <- approx_dt(datatable0, yrs, xcol = "period", ycol = "value",
                                  idxcols = c("scenario", "variable", "unit", "model", "region"),
                                  extrapolate = T)

          report <- rbind(report, datatable0)}

      }
    }

    return(report)
  }

  ## Demand emissions
  reportingEmi <- function(repFE, gdx){

    ## load emission factors for fossil fuels
    p_ef_dem <- readgdx(gdx, "p_ef_dem")[all_enty %in% c("fepet", "fedie", "fegas")]  ## MtCO2/EJ
    p_ef_dem[all_enty == "fegas", all_enty := "fegat"]
    setnames(p_ef_dem, old = c("value", "all_regi"), new = c("ef", "region"))
    ## attribute explicitly fuel used to the FE values
    emidem = repFE[grepl("Liquids|Gases|Hydrogen|Electricity", variable) & region != "World"]   ## EJ
    emidem[, all_enty := ifelse(grepl("Liquids", variable), "fedie", NA)]
    emidem[, all_enty := ifelse(grepl("LDV.+Liquids", variable), "fepet", all_enty)]
    emidem[, all_enty := ifelse(grepl("Gases", variable), "fegat", all_enty)]
    emidem[, all_enty := ifelse(grepl("Electricity", variable), "feelt", all_enty)]
    emidem[, all_enty := ifelse(grepl("Hydrogen", variable), "feh2t", all_enty)]
    ## merge with emission factors
    emidem = emidem[p_ef_dem, on = c("all_enty","region")]
    ## calculate emissions and attribute variable and unit names
    emidem[, value := value * ef][, c("variable", "unit") := list(gsub("FE", "Emi\\|CO2", variable), "Mt CO2/yr")]

    emi = rbind(copy(emidem)[, c("type", "variable") := list("tailpipe", paste0(variable, "|Tailpipe"))],
                copy(emidem)[, c("type", "variable") := list("demand", paste0(variable, "|Demand"))])

    prodFe <- readgdx(gdx, "vm_prodFE")[, ttot := as.numeric(ttot)]
    setnames(prodFe,
             c("period", "region", "se", "all_enty", "te", "fe_demand"))
    prodFe[, se_share := fe_demand/sum(fe_demand), by = c("period", "region", "all_enty")]
    prodFe <- prodFe[all_enty %in% c("fedie", "fepet", "fegat") & se %in% c("segafos", "seliqfos")][, c("se", "te", "fe_demand") := NULL]

    emi <- prodFe[emi, on = c("period", "region", "all_enty")]
    ## in case no fossil fuels are used (e.g. 100% biodiesel), the value in se_share results NA. set the NA value to 0
    emi[is.na(se_share), se_share := 0]
    emi <- emi[all_enty %in% c("fedie", "fepet", "fegat") & type == "demand", value := value * se_share]

    emi[, c("se_share", "type", "ef", "all_enty") := NULL]

    ## aggregate removing the fuel dependency
    emi[, variable_agg := gsub("\\|Liquids|\\|Electricity|\\|Hydrogen|\\|Gases", "", variable)]
    emi = emi[, .(value = sum(value)), by = c("model", "scenario", "region", "unit", "period", "variable_agg")]
    setnames(emi, old = "variable_agg", new = "variable")
    emi = emi[, .(model, scenario, region, variable, unit, period, value)]

    return(emi)
  }

  reportingVehNum <- function(demand_vkm, annual_mileage){
    venum <- copy(demand_vkm)
    ## merge annual mileage
    anmil <- copy(annual_mileage)
    anmil[grepl("Subcompact", vehicle_type),
          variable := "Pass|Road|LDV|Small"]
    anmil[grepl("Mini", vehicle_type),
          variable := "Pass|Road|LDV|Mini"]
    anmil[vehicle_type == "Compact Car", variable := "Pass|Road|LDV|Medium"]
    anmil[grepl("Large Car|Midsize Car", vehicle_type), variable := "Pass|Road|LDV|Large"]
    anmil[grepl("SUV", vehicle_type),
          variable := "Pass|Road|LDV|SUV"]
    anmil[grepl("Van|Multipurpose", vehicle_type),
          variable := "Pass|Road|LDV|Van"]
    anmil[grepl("Motorcycle|Scooter|Moped", vehicle_type),
          variable := "Pass|Road|LDV|Two-Wheelers"]
    anmil[grepl("^Truck", vehicle_type),
          variable := sprintf("Freight|Road|%s", vehicle_type)]
    anmil[grepl("Bus", vehicle_type),
          variable := "Pass|Road|Bus"]

    anmil <- anmil[,.(region, period = year, variable, annual_mileage)]

    anmil <- approx_dt(anmil, unique(demand_vkm$period), xcol = "period", ycol = "annual_mileage", idxcols = c("region", "variable"), extrapolate = T)
    anmil<- unique(anmil[, c("period", "region", "variable", "annual_mileage")])
    anmil <- anmil[, variable := paste0("ES|Transport|VKM|", variable)]
    venum <- merge(demand_vkm, anmil, by = c("variable", "region", "period"))
    venum[, ven := value/annual_mileage] # billion vehicle-km -> thousand vehicles

    venum <- venum[!is.na(ven)]
    venum[, variable := gsub("|VKM", "|VNUM", variable, fixed=TRUE)][, c("value", "annual_mileage") := NULL]
    venum[, unit := "tsd veh"]
    setnames(venum, "ven", "value")
    venum = venum[,.(model, scenario, region, variable, unit, period, value)]
    return(venum)
  }

  vintageReport <- function(load_factor) {
    ## this function should return the format originally provided by
    ## the vintages-snippet in the EDGE_transport.R script in REMIND
    ## full_demand_vkm: total demand per year and vehicle/technology
    ## vintage_demand_vkm: demand of stock vehicles per year and construction year
    ## (excluding new sales of the current year)
    vintages <- readRDS(datapath("vintages.RDS"))
    shares <- readRDS(datapath("shares.RDS"))

    vint <- vintages[["vintcomp_startyear"]]
    newd <- vintages[["newcomp"]]
    sharesVS1 <- shares[["VS1_shares"]]
    setnames(sharesVS1, "share", "shareVS1")

    newd <- sharesVS1[newd, on=c("region", "year", "subsector_L1", "vehicle_type")]
    newd[, demNew := totdem * sharetech_new * shareVS1]

    vint <- newd[vint, on=c("region", "subsector_L1", "vehicle_type", "technology", "year", "sector")]
    vint <- vint[!is.na(demNew)]
    vint <- vint[, c("year", "region", "vehicle_type", "technology", "variable", "demNew", "demVintEachYear")]
    vint[, demand_F := demNew + sum(demVintEachYear), by=c("region", "year", "vehicle_type", "technology")]
    vint <- load_factor[vint, on=c("year", "region", "vehicle_type")]
    vint[, full_demand_vkm := demand_F/loadFactor]
    vint[, vintage_demand_vkm := demVintEachYear/loadFactor]

    vint[, c("demand_F", "demVintEachYear", "loadFactor", "demNew") := NULL]

    setnames(vint, "variable", "construction_year")

    return(vint)
  }


  reportStockAndSales <- function(annual_mileage, load_factor) {
    year_c <- construction_year <- Stock <- Sales <- vintage_demand_vkm <- fct <- category <- NULL

    vint <- vintageReport(load_factor)
    vint[, tot_vint_demand := sum(vintage_demand_vkm), by=c("year", "region", "vehicle_type", "technology")]
    vint[, sales_demand := full_demand_vkm - tot_vint_demand]
    ## stock demand is demand including vintages and sales
    setnames(vint, "full_demand_vkm", "stock_demand")
    vint <- unique(
      vint[, c("construction_year", "vintage_demand_vkm", "tot_vint_demand") := NULL])
    annual_mileage_trucks <- fread(
      text = "vehicle_type,annual_mileage
              Truck (0-3.5t), 21500
              Truck (7.5t), 34500
              Truck (18t), 53000
              Truck (26t), 74000
              Truck (40t), 136500")

    cjam <- CJ(region = annual_mileage$region, year = annual_mileage$year,
               vehicle_type = annual_mileage_trucks$vehicle_type,
               unique=T)
    annual_mileage_trucks <- annual_mileage_trucks[cjam, on="vehicle_type"]
    annual_mileage <- rbind(annual_mileage, annual_mileage_trucks, use.names = TRUE)

    vint <- annual_mileage[vint, on=c("year", "region", "vehicle_type")]
    vint[, `:=`(Stock = stock_demand / annual_mileage, Sales = sales_demand / annual_mileage)][, c("annual_mileage", "stock_demand", "sales_demand") := NULL]
    vint <- melt(vint, measure.vars=c("Stock", "Sales"), variable.name="category")

    vint[grepl("^Truck", vehicle_type), typ := "Truck"]
    vint[grepl("^Bus", vehicle_type), typ := "Bus"]
    vint[is.na(typ), typ := "LDV"]

    vint[, variable := ifelse(
      vehicle_type == "Bus_tmp_vehicletype",
      sprintf("%s|Transport|Bus|%s", category, technology),
      sprintf("%s|Transport|%s|%s|%s", category, typ, vehicle_type, technology))]

    ## totals
    vint <- rbindlist(list(
      vint,
      vint[, .(value = sum(value), variable = gsub("(.+)\\|.+$", "\\1", variable)),
             by = c("category", "year", "region", "vehicle_type")],
      vint[grepl("|LDV|", variable, fixed = TRUE),
             .(value = sum(value), variable=sprintf("%s|Transport|LDV", category)),
           by = c("category", "year", "region")],
      vint[grepl("|LDV|", variable, fixed = TRUE),
             .(value = sum(value), variable=sprintf("%s|Transport|LDV|%s", category, technology)),
           by = c("category", "year", "region", "technology")],
      vint[grepl("|Truck|", variable, fixed = TRUE),
             .(value = sum(value), variable=sprintf("%s|Transport|Truck|%s", category, technology)),
           by = c("category", "year", "region", "technology")],
      vint[grepl("|Truck|", variable, fixed = TRUE),
             .(value = sum(value), variable=sprintf("%s|Transport|Truck", category)),
             by = c("category", "year", "region")]), fill = TRUE)

    vint[, c("vehicle_type", "technology", "category", "typ") := NULL]
    ## remove the variable (e.g. vehicle_types) that are not present for this specific region
    vint <- unique(vint[!is.na(value)])

    setnames(vint, "year", "period")

    vint = approx_dt(vint, c(2005, 2010, unique(vint$period), 2110, 2130, 2150),
                     xcol = "period", ycol = "value", idxcols = c("region", "variable"), extrapolate = T)

    vint[, `:=`(model = model_name, scenario = scenario_title, unit = "Million vehicles")]
    return(vint)
  }

  loadCapCosts <- function(demand_pkm) {
    ## demand_pkm: billion pkm
    costs <- readRDS(datapath(fname="capCostPerTech.RDS"))
    ## costs: unit $2005/pkm or tkm

    merged <- demand_pkm[
      costs, on=c("region", "year", "vehicle_type", "technology",
                  "sector", "subsector_L1", "subsector_L2", "subsector_L3")]

    merged[, value := value * non_fuel_price] # unit billion US$2005
    merged <- merged[value > 0]

    merged[, c("non_fuel_price", "tot_price",
               "fuel_price_pkm", "tot_VOT_price", "sector_fuel") := NULL]

    return(merged)
  }


  reportTotals <- function(aggrname, datatable, varlist){

    vars <- varlist[[aggrname]]
    #access the first element in vars
    

    if (length(unique(datatable[variable %in% vars]$variable)) < length(vars)){
      browser()
      print(paste0("Missing variables to aggregate data to ", aggrname))
      }
    else if (length(unique(datatable[variable %in% vars]$variable)) > length(vars)) {
      browser()
      print(paste0('duplicates from: ', aggrname, 'not summed up'))
      }
    else {
      datatable <- datatable[variable %in% vars,
                                 .(variable = aggrname,
                                   value = sum(value)),
                                 by = c("model", "scenario", "region", "period", "unit")]
      }
    return(datatable)
  }

  ## check the regional aggregation
  regionSubsetList <- toolRegionSubsets(gdx)

  # ADD EU-27 region aggregation if possible
  if("EUR" %in% names(regionSubsetList)){
    regionSubsetList <- c(regionSubsetList, list(
      "EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "ESW")
    ))
  }

  #Create Mapping for region Aggregation out of region SubsetList
  if (!is.null(regionSubsetList)){
    namesReg <- names(regionSubsetList)
    RegAggregation <- data.table()
    for (i in 1:length(namesReg)){
      tmp <- data.table(region=regionSubsetList[[i]], aggr_reg =namesReg[i])
      RegAggregation <- rbind(RegAggregation, tmp)
    }
  } else {
    RegAggregation <- NULL
  }


  ## load input data from last EDGE run
  ## Data manipulation shouldnt be necessary
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))
  demand_km[, demand_F := demand_F * 1e-3] ## million -> billion pkm
  setnames(demand_km, "demand_F", "value")
  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
  setnames(demand_ej, "demand_EJ", "value")
  load_factor <- readRDS(datapath(fname = "loadFactor.RDS"))
  annual_mileage <- readRDS(datapath(fname = "annual_mileage.RDS"))

  if (length(annual_mileage) > 4) {
    # Same is done in lvl2_createoutput
    annual_mileage <- unique(annual_mileage[, c("region", "year", "vkm.veh", "vehicle_type")])
    setnames(annual_mileage, old = "vkm.veh", new = "annual_mileage")
  }
  if (length(load_factor) > 4) {
    load_factor <- load_factor[, c("year", "region", "vehicle_type", "loadFactor", "technology")]
    demand_vkm <- merge(demand_km, load_factor, by = c("year", "region", "vehicle_type", "technology"))
    demand_vkm[, value := value / loadFactor] ## billion vkm
    load_factor <- load_factor[year >= 1990, .(loadFactor=mean(loadFactor)),
                               by=c("region", "year", "vehicle_type")]
  } else {
    demand_vkm <- merge(demand_km, load_factor, by = c("year", "region", "vehicle_type"))
    demand_vkm[, value := value / loadFactor] ## billion vkm
  }


  repFE <- reporting(
    dt = demand_ej,
    mode = "FE"
  )

  repCapCosts <- NULL
  if(file.exists(datapath(fname="capCostPerTech.RDS"))) {
    capCosts <- loadCapCosts(demand_km)
    repCapCosts <- reporting(dt = capCosts, mode = "CC")
  }
  liqsplit <- split_fe_liquids(repFE)

  repVKM <- reporting(
    dt = demand_vkm,
    mode = "VKM"
  )
  repES <- reporting(
    dt = demand_km,
    mode = "ES"
  )

  toMIF <- rbind(
    repFE,
    liqsplit,
    repVKM,
    repES,
    reportingVehNum(
      repVKM,
      annual_mileage),
    reportingEmi(
      repFE = repFE,
      gdx = gdx),
    repCapCosts
    )

  varsl <- list(
    `ES|Transport|Pass|Road` = c("ES|Transport|Pass|Road|LDV", "ES|Transport|Pass|Road|Bus", "ES|Transport|Pass|Road|Non-Motorized"),
    `ES|Transport|Pass|Aviation` = c("ES|Transport|Pass|Aviation|International", "ES|Transport|Pass|Aviation|Domestic"),
    `ES|Transport|VKM|Pass|Road` = c("ES|Transport|VKM|Pass|Road|LDV", "ES|Transport|VKM|Pass|Road|Bus"),
    `ES|Transport|VKM|Road` = c("ES|Transport|VKM|Freight|Road", "ES|Transport|VKM|Pass|Road|LDV", "ES|Transport|VKM|Pass|Road|Bus"),
    `ES|Transport|VKM|Rail` = c("ES|Transport|VKM|Pass|Rail|HSR", "ES|Transport|VKM|Pass|Rail|non-HSR", "ES|Transport|VKM|Freight|Rail" ),

    `Emi|CO2|Transport|Pass|Road|Tailpipe` = c("Emi|CO2|Transport|Pass|Road|LDV|Tailpipe", "Emi|CO2|Transport|Pass|Road|Bus|Tailpipe"),
    `Emi|CO2|Transport|Pass|Road|Demand` = c("Emi|CO2|Transport|Pass|Road|LDV|Demand", "Emi|CO2|Transport|Pass|Road|Bus|Demand"),
    `Emi|CO2|Transport|Rail|Tailpipe` = c("Emi|CO2|Transport|Pass|Rail|non-HSR|Tailpipe", "Emi|CO2|Transport|Freight|Rail|Tailpipe"),
    `Emi|CO2|Transport|Rail|Demand` = c("Emi|CO2|Transport|Pass|Rail|non-HSR|Demand", "Emi|CO2|Transport|Freight|Rail|Demand"),
    `Emi|CO2|Transport|Demand` = c("Emi|CO2|Transport|Pass|Rail|non-HSR|Demand", "Emi|CO2|Transport|Freight|Rail|Demand", "Emi|CO2|Transport|Freight|Road|Demand", "Emi|CO2|Transport|Pass|Road|LDV|Demand", "Emi|CO2|Transport|Pass|Road|Bus|Demand",
                                   "Emi|CO2|Transport|Freight|International Shipping|Demand", "Emi|CO2|Transport|Freight|Navigation|Demand", "Emi|CO2|Transport|Pass|Aviation|Domestic|Demand", "Emi|CO2|Transport|Pass|Aviation|International|Demand"),
    `FE|Transport|Pass|Road` = c("FE|Transport|Pass|Road|LDV", "FE|Transport|Pass|Road|Bus"),
    `FE|Transport|Pass|Road|Electricity` = c("FE|Transport|Pass|Road|LDV|Electricity", "FE|Transport|Pass|Road|Bus|Electricity"),
    `FE|Transport|Pass|Road|Liquids` = c("FE|Transport|Pass|Road|LDV|Liquids", "FE|Transport|Pass|Road|Bus|Liquids"),
    `FE|Transport|Pass|Road|Hydrogen` = c("FE|Transport|Pass|Road|LDV|Hydrogen", "FE|Transport|Pass|Road|Bus|Hydrogen"),
    `FE|Transport|Pass|Road|Gases` = c("FE|Transport|Pass|Road|LDV|Gases", "FE|Transport|Pass|Road|Bus|Gases"),

    `FE|Transport|Pass|Aviation` = c("FE|Transport|Pass|Aviation|International", "FE|Transport|Pass|Aviation|Domestic"),
    `FE|Transport|Rail` = c("FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR", "FE|Transport|Freight|Rail"),
    `FE|Transport|LDV|Gases` = c("FE|Transport|Pass|Road|LDV|Gases"),
    `FE|Transport|LDV|Electricity` = c("FE|Transport|Pass|Road|LDV|Electricity"),
    `FE|Transport|LDV|Hydrogen` = c("FE|Transport|Pass|Road|LDV|Hydrogen"),
    `FE|Transport|non-LDV|Gases` = c("FE|Transport|Pass|non-LDV|Gases","FE|Transport|Freight|Road|Gases"),
    `FE|Transport|non-LDV|Electricity` = c("FE|Transport|Pass|non-LDV|Electricity","FE|Transport|Freight|Road|Electricity","FE|Transport|Freight|Rail|Electricity"),
    `FE|Transport|non-LDV|Hydrogen` = c("FE|Transport|Pass|non-LDV|Hydrogen","FE|Transport|Freight|Road|Hydrogen")
    )

  names <- names(varsl)

  totals <- sapply(names, reportTotals, datatable = toMIF, varlist = varsl, simplify = FALSE, USE.NAMES = TRUE)

  totals <- rbindlist(totals, use.names = TRUE)
  toMIF <- rbind(toMIF, totals)

  #append second aggregation level using the variables appended in the 4 lines before
  varsl <- list(`ES|Transport edge|Road` = c("ES|Transport|Pass|Road", "ES|Transport|Freight|Road"),
  `Emi|CO2|Transport|Road|Tailpipe` = c("Emi|CO2|Transport|Pass|Road|Tailpipe", "Emi|CO2|Transport|Freight|Road|Tailpipe"),
  `Emi|CO2|Transport|Road|Demand` = c("Emi|CO2|Transport|Pass|Road|Demand", "Emi|CO2|Transport|Freight|Road|Demand"),
  `FE|Transport|Road` = c("FE|Transport|Pass|Road", "FE|Transport|Freight|Road"),
  `FE|Transport|Road|Electricity` = c("FE|Transport|Pass|Road|Electricity", "FE|Transport|Freight|Road|Electricity"),
  `FE|Transport|Road|Liquids` = c("FE|Transport|Pass|Road|Liquids", "FE|Transport|Freight|Road|Liquids"),
  `FE|Transport|Road|Hydrogen` = c("FE|Transport|Pass|Road|Hydrogen", "FE|Transport|Freight|Road|Hydrogen"),
  `FE|Transport|Road|Gases` = c("FE|Transport|Pass|Road|Gases", "FE|Transport|Freight|Road|Gases")
  )
  names <- names(varsl)

  totals <- sapply(names, reportTotals, datatable = toMIF, varlist = varsl, simplify = FALSE, USE.NAMES = TRUE)

  totals <- rbindlist(totals, use.names = TRUE)
  toMIF <- rbind(toMIF, totals)

  toMIF <- rbindlist(list(toMIF, reportStockAndSales(annual_mileage, load_factor)), use.names=TRUE)

  #Aggregate variables to "World" region
  toMIF <- rbindlist(list(toMIF, toMIF[, .(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]), use.names=TRUE)

  if (!is.null(regionSubsetList)){
    toMIF <- rbind(
      toMIF,
      aggregate_map(toMIF[region %in% unique(RegAggregation$region)], RegAggregation, by = "region"))
  }

  #ES pkm are used as weights for data aggregation
  weight_pkm <- copy(demand_km)
  setnames(weight_pkm, c("value", "year"), c("weight", "period"))



  if (extendedReporting) {

    reportInt <- function(var, datatable){
     #Energy Intensity MJ/pkm or MJ/tkm
     repFE <- datatable[variable == paste0("FE|Transport|", var)]
     repFE[, variable := NULL][, unit := NULL]
     setnames(repFE, "value", "FE")
     repES <- datatable[variable == paste0("ES|Transport|", var)]
     repES[, variable := NULL][, unit := NULL]

     #check if variable is found
     if (!(length(repFE)>0 && length(repES) > 0)){
       print(paste0("Variable not found to calculate (p/t)km Energy Intensity ", var))
     }

     repInt <- merge(repFE, repES, by = c("region", "period", "scenario", "model"))
     repInt[, value := FE/value][, FE := NULL][, variable := paste0("EInt|Transport|", var)]
     #FE in EJ to MJ + ES ib bn pkm to pkm 1e12/1e9
     repInt[, value := value*1e3]
     if (sub("\\|.*", "", var) == "Pass"){
      repInt[, unit := "MJ/pkm"]
     }else{
      repInt[, unit := "MJ/tkm"]}

     #Energy Intensity MJ/vkm
     repVKM <- datatable[variable == paste0("ES|Transport|VKM|", var)]
     repVKM[, variable := NULL][, unit := NULL]

     #check if variable is found
     if (!(length(repFE)>0 && length(repVKM) > 0)){
       print(paste0("Variable not found to calculate VKM Energy Intensity ", var))
     }

     repIntVKM <- merge(repFE, repVKM, by = c("region", "period", "scenario", "model"))
     repIntVKM[, value := FE/value][, FE := NULL][, variable := paste0("EInt|Transport|VKM|", var)]
     #FE in EJ to MJ + ES ib bn vkm to vkm 1e12/1e9
     repIntVKM[, value := value*1e3][, unit := "MJ/vkm"]
     return(rbind(repInt, repIntVKM))
     }

    LogitCostplotdata <- function(priceData, prefData, logitExp, groupValue, Reg_Aggregation, weightpkm){

      tot_price <- sw <- logit.exponent <- weight <- NULL
      yrs_costs <-c(seq(2005, 2060, 5), seq(2070, 2100, 10))
      all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                          "subsector_L3", "sector")
      # change variable names for quitte format
      setnames(priceData, c("year"), c("period"))
      setnames(prefData, c("year"), c("period"))
      prefData <- prefData[period %in% yrs_costs][, unit := "-"]
      priceData<-  priceData[period %in% yrs_costs][, -c("share")][, unit := "$2005/km"]

      #Filter for logit level according to groupValue. leave out tmp placeholders
      priceData <- priceData[!grepl("tmp", get(groupValue))]
      prefData <- prefData[!grepl("tmp", get(groupValue))]

      # Calculate Inconvenience Cost from share Weight
      # Logit Exponent and total price are needed for this
      prefData_inco <- merge(prefData, logitExp, all.y = TRUE)
      #rename original prefs afterwards
      setnames(prefData, c("sw"), c("value"))

      #Reduce priceData to total price
      price_tot <- priceData[, c("period", "region", "tot_price", "unit", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]), with = FALSE]

      prefData_inco <- merge(prefData_inco[, unit := "$2005/km"], price_tot, by = c("period", "region", "unit", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))

      prefData_inco[, value := tot_price * (sw^(1 / logit.exponent) - 1)]

      #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero
      prefData_inco <- prefData_inco[is.infinite(prefData_inco$value), value := 0]
      prefData_inco <- prefData_inco[, c("region", "period", "unit", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)], "value"), with = FALSE][, variable := "Eq inconvenience cost"]

      #Prepare PriceData
      priceData <- data.table::melt(priceData[, -c("tot_price")], id.vars = c("region", "period", "unit", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))

      priceData <- rbind(prefData_inco, priceData)

      #Regional Aggregation
      #Costs are intensive variables and are aggregated with ES weights for each level of the logit
      weight_pkm_logitlevel <- weightpkm[, .(weight = sum(weight)), by = c("region", "period", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)])]

      if (!is.null(Reg_Aggregation)){
        priceData <- rbind(priceData, aggregate_map(priceData[region %in% unique(RegAggregation$region)], RegAggregation, by = "region", weights = weight_pkm_logitlevel, weight_val_col = "weight", weight_item_col = groupValue))
        prefData <- rbind(prefData, aggregate_map(prefData[region %in% unique(RegAggregation$region)], RegAggregation, by = "region", weights = weight_pkm_logitlevel, weight_val_col = "weight", variable = groupValue, weight_item_col = groupValue))
      }

      if (groupValue == "vehicle_type"){
        #Before prices are finally structured, vehicles are aggregated
        Aggrdata_veh <- as.data.table(Aggrdata[, c("vehicle_type", "det_veh")])
        Aggrdata_veh <- unique(Aggrdata_veh[!is.na(det_veh)])[, det_veh := gsub("Freight\\|Road\\||Pass\\|Road\\|", "", det_veh)]

        #Exclude those wihout aggregation
        Aggrdata_veh <- Aggrdata_veh[!vehicle_type == det_veh & !grepl("Rail", det_veh)]
        priceData <- priceData[, c("region", "variable", "vehicle_type", "period", "value", "unit")]
        weight_pkm_VS1 <- weight_pkm[,.(weight = sum(weight)), by = c("region", "vehicle_type", "period")]
        #Add weights for aggregated regions
        if (!is.null(Reg_Aggregation)){
          #Add unit so that aggregate_map find its way through
          weight_pkm_VS1 <- weight_pkm_VS1[, unit := "bn pkm/yr"]
          weight_pkm_VS1 <- rbind(weight_pkm_VS1, aggregate_map(weight_pkm_VS1[region %in% unique(RegAggregation$region)], RegAggregation, by = "region", variable = groupValue, value = "weight"))
          weight_pkm_VS1 <- weight_pkm_VS1[, unit := NULL]
        }

        Prices_veh_aggr <- as.data.table(aggregate_map(priceData[vehicle_type %in% Aggrdata_veh$vehicle_type], Aggrdata_veh, by = "vehicle_type", weights = weight_pkm_VS1[vehicle_type %in% Aggrdata_veh$vehicle_type], weight_val_col = "weight",  weight_item_col = "period"))
        Prices_veh_aggr[, variable := paste0("Logit cost|V|", vehicle_type, "|", variable)][, vehicle_type := NULL]
      }

      if (groupValue=="vehicle_type"){
        #Convert original shareweights to quitte format
        prefData[, variable := paste0("Shareweight|V|", get(groupValue))]
        prefData <- prefData[, .(region, period, scenario, variable, value, unit)]
        #Convert costs to quitte format
        priceData[, variable := paste0("Logit cost|V|", get(groupValue), "|", variable)]
        priceData <- priceData[, .(region, period, scenario, variable, value, unit)]
        priceData <- rbind(priceData, Prices_veh_aggr)
        }
      else{
        #Convert original shareweights to quitte format
        prefData[, variable := paste0("Shareweight|S",gsub("[^123]",  "", groupValue), "|", get(groupValue))]
        prefData <- prefData[, .(region, period, scenario, variable, value, unit)]
        #Convert costs to quitte format
        priceData[, variable := paste0("Logit cost|S",gsub("[^123]", "", groupValue), "|", get(groupValue), "|", variable)]
        priceData <- priceData[, .(region, period, scenario, variable, value, unit)]
      }

      data <- rbind(prefData, priceData)
      data[, scenario := scenario_title][, model := model_name]

      return(data)
    }

    LogitCostplotdata_FV <- function(priceData, prefData, logitExp, Reg_Aggregation, weightpkm){
      tot_price <- sw <- logit.exponent <- weight <- logit_type <- av_veh <- NULL
      #Calcualte equivalent inconvenience cost and
      yrs_costs <-c(seq(2005, 2060, 5), seq(2070, 2100, 10))

      # change variable names for mip
      setnames(priceData, c("year"), c("period"))
      setnames(prefData, c("year"), c("period"))

      #Exclude active modes as they have no fuel
      prefData <- prefData[period %in% yrs_costs & !technology %in% c("Cycle_tmp_technology", "Walk_tmp_technology")][, unit := "-"]
      priceData<-  priceData[period %in% yrs_costs][, unit := "$2005/km"]

      # Calculate Inconvenience Cost from share Weight
      priceData_sw <- copy(prefData)
      priceData_sw <- priceData_sw[logit_type == "sw"][, logit_type := NULL]
      setnames(priceData_sw, "value", "sw")
      priceData_sw <- merge(priceData_sw, logitExp, all.x = TRUE)

      #This should be removed in refactoring process
      priceData_sw[grepl("^Truck", vehicle_type), logit.exponent := -4]
      priceData_sw <- priceData_sw[is.na(logit.exponent), logit.exponent :=  -10]

      price_tot <- priceData[, c("period", "region","tot_price", "technology", "vehicle_type", "unit")]
      priceData_sw <- merge(priceData_sw, price_tot, by = c("period", "region", "technology", "vehicle_type", "unit"),
                            all.x=TRUE)

      priceData_sw[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
      #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero
      priceData_sw <- priceData_sw[is.infinite(priceData_sw$value), value := 0]
      #Some total prices are missing
      priceData_sw <- priceData_sw[is.na(priceData_sw$value), value := 0]
      priceData_sw <- priceData_sw[, c("period", "region", "technology", "vehicle_type", "value", "unit")][, variable := "Eq inconvenience cost"]
      priceData_inco_LDV <- prefData[!logit_type == "sw"][, c("period", "region", "technology", "vehicle_type", "value", "logit_type", "unit")]
      setnames(priceData_inco_LDV, "logit_type", "variable")
      #Exclude LDV inco from prefdata
      prefData <- prefData[logit_type == "sw"]
      prefData <- prefData[, .(region, period, scenario, vehicle_type, technology, unit, value)]

      priceData <- data.table::melt(priceData[, -c("tot_price", "share", "subsector_L1", "subsector_L2", "subsector_L3", "sector")], id.vars = c("region", "period", "technology", "vehicle_type", "unit"))
      priceData <- rbind(priceData, priceData_sw, priceData_inco_LDV)

      #Regional Aggregation
      #Costs are intensive variables and are aggregated with ES weights for each level of the logit
      weight_pkm_FV <- weightpkm[, .(weight = sum(weight)), by = c("region", "period","vehicle_type", "technology")]
      #TO FIX:
      #Hydrogen and BEV technologies for aviation and 2Wheelers are not everywhere available: -> Insert zero as weight
      weight_pkm_FV <- merge(weight_pkm_FV, priceData, on = c("region", "period", "vehicle_type", "technology"), all = TRUE)
      weight_pkm_FV[is.na(weight_pkm_FV$weight), weight := 0]
      weight_pkm_FV <- weight_pkm_FV[, c("region", "period","vehicle_type", "technology", "weight")]
      weight_pkm_FV <- weight_pkm_FV[period > 1990 & period < 2110]
      weight_pkm_FV <- unique(weight_pkm_FV)

      if (!is.null(Reg_Aggregation)){
        weight_pkm_FV[, unit := "bn pkm/yr"]
        weight_pkm_FV <-  rbind(weight_pkm_FV, aggregate_map(weight_pkm_FV[region %in% Reg_Aggregation$region], Reg_Aggregation, by = "region", variable = "technology", value = "weight"))
        weight_pkm_FV[, unit := NULL]
        priceData <- rbind(priceData, aggregate_map(priceData[region %in% Reg_Aggregation$region], Reg_Aggregation, by = "region", weights = weight_pkm_FV, weight_val_col = "weight", variable = "technology", weight_item_col = "technology"))
        prefData <- rbind(prefData, aggregate_map(prefData[region %in% Reg_Aggregation$region], Reg_Aggregation, by = "region", weights = weight_pkm_FV, weight_val_col = "weight", variable = "technology", weight_item_col = "technology"))
      }

      #Before prices are finally structured, vehicles are aggregated
      #ES pkm are used as weights for data aggregation
      Aggrdata_veh <- as.data.table(Aggrdata[, c("vehicle_type", "det_veh")])
      Aggrdata_veh <- unique(Aggrdata_veh[!is.na(det_veh)])[, det_veh := gsub("Freight\\|Road\\||Pass\\|Road\\|", "", det_veh)]
      #Exclude those wihout aggregation
      Aggrdata_veh <- Aggrdata_veh[!vehicle_type == det_veh & !grepl("Rail", det_veh)]

      priceData_aggr <- as.data.table(aggregate_map(priceData[vehicle_type %in% Aggrdata_veh$vehicle_type], Aggrdata_veh, by = "vehicle_type", weights = weight_pkm_FV[vehicle_type %in% Aggrdata_veh$vehicle_type], weight_val_col = "weight",  weight_item_col = "period"))

      #Aggregate average vehicle
      Aggrdata_avveh <- as.data.table(Aggrdata)
      Aggrdata_avveh <- Aggrdata_avveh[subsector_L1 == "trn_pass_road_LDV_4W"]
      Aggrdata_avveh <- unique(Aggrdata_avveh[, c("vehicle_type")])
      Aggrdata_avveh[, av_veh := "Average veh"]

      priceData_av <- as.data.table(aggregate_map(priceData[vehicle_type %in% Aggrdata_avveh$vehicle_type], Aggrdata_avveh, by = "vehicle_type", weights = weight_pkm_FV[vehicle_type %in% Aggrdata_veh$vehicle_type], weight_val_col = "weight",  weight_item_col = "period"))

      priceData <- rbind(priceData, priceData_aggr, priceData_av)
      priceData <- priceData[, variable := paste0("Logit cost|F|", gsub("_tmp_vehicletype", "", vehicle_type), "|", technology, "|", variable)][, c("region", "period", "variable", "value", "unit")][, unit := "$2005/km"][, model := model_name][, scenario := scenario_title]

      prefData[, variable := paste0("Shareweight|F|", gsub("_tmp_vehicletype", "", vehicle_type), "|", technology)][, model := model_name][, scenario := scenario_title]
      prefData <- prefData[, c("period", "region", "variable", "unit", "model", "scenario", "value")]
      data <- rbind(priceData, prefData)

      return(data)
    }

    varslist <- list("Pass|w/o bunkers",
                     "Pass|Aviation|International",
                     "Pass|Rail",
                     "Pass|Aviation|Domestic",
                     "Pass|Road",
                     "Pass|Road|LDV",
                     "Pass|Road|LDV|Two Wheelers",
                     "Pass|Road|LDV|Four Wheelers",
                     "Pass|Road|Bus",
                     "Freight|w/o bunkers",
                     "Freight|International Shipping",
                     "Freight|Navigation",
                     "Freight|Rail",
                     "Freight|Road")

    EInt <- sapply(varslist, reportInt, datatable = toMIF, simplify = FALSE, USE.NAMES = TRUE)
    EInt <- rbindlist(EInt, use.names = TRUE)
    toMIF <- rbind(toMIF, EInt)

    #Calculate logit Costs
    #Read in additional data if exist
    if (file.exists(datapath(fname = "logit_data.RDS"))){
      logit_data <- readRDS(datapath(fname = "logit_data.RDS"))
      prices <- logit_data$share_list
      Pref <- logit_data$pref_data
      if (file.exists(datapath(fname = "logit_exp.RDS"))){
        logit_exp <- readRDS(datapath(fname = "logit_exp.RDS"))
        #Needed due to different structure of logit_exp for coupled and SA version.. FIX IN REFACTORING
        if (length(logit_exp) == 3) {
         logit_exp <- logit_exp$logit_output}

        #Prices S3S
        Prices_S3S <- prices$S3S_shares
        setkey(Prices_S3S, NULL)
        Pref_S3S <- Pref$S3S_final_pref
        setkey(Pref_S3S, NULL)
        logit_exp_S3S <- logit_exp$logit_exponent_S3S
        setkey(logit_exp_S3S, NULL)

        #Adjust in model itself in refactoring process
        Prices_S3S[subsector_L3 %in% c("Cycle", "Walk"), tot_VOT_price := tot_price]
        PrefandPrices_S3S <- LogitCostplotdata(priceData = Prices_S3S, prefData = Pref_S3S, logitExp = logit_exp_S3S, groupValue = "subsector_L3", Reg_Aggregation = RegAggregation, weightpkm = weight_pkm)

        #Prices S2S3
        Prices_S2S3 <- prices$S2S3_shares
        setkey(Prices_S2S3, NULL)
        Pref_S2S3 <- Pref$S2S3_final_pref
        setkey(Pref_S2S3, NULL)
        logit_exp_S2S3 <- logit_exp$logit_exponent_S2S3
        setkey(logit_exp_S2S3, NULL)

        PrefandPrices_S2S3 <- LogitCostplotdata(priceData = Prices_S2S3, prefData = Pref_S2S3, logitExp = logit_exp_S2S3, groupValue = "subsector_L2", Reg_Aggregation = RegAggregation, weightpkm = weight_pkm)

        #Prices S1S2
        Prices_S1S2 <- prices$S1S2_shares
        setkey(Prices_S1S2, NULL)
        Pref_S1S2 <- Pref$S1S2_final_pref
        setkey(Pref_S1S2, NULL)
        logit_exp_S1S2 <- logit_exp$logit_exponent_S1S2
        setkey(logit_exp_S1S2, NULL)

        PrefandPrices_S1S2 <- LogitCostplotdata(priceData = Prices_S1S2, prefData = Pref_S1S2, logitExp = logit_exp_S1S2, groupValue = "subsector_L1", Reg_Aggregation = RegAggregation, weightpkm = weight_pkm)

        #Prices VS1
        Prices_VS1 <- prices$VS1_shares
        setkey(Prices_VS1, NULL)
        Pref_VS1 <- Pref$VS1_final_pref
        setkey(Pref_VS1, NULL)
        logit_exp_VS1 <- logit_exp$logit_exponent_VS1
        setkey(logit_exp_VS1, NULL)

        #Add subsector_L2, subsector L3 and sector to Prices_VS1 (for structural conformity)
        Prices_VS1 <- merge(Prices_VS1, unique(Pref_VS1[, c("subsector_L2", "subsector_L3", "sector", "vehicle_type")]), by = "vehicle_type", all.x = TRUE)
        PrefandPrices_VS1 <- LogitCostplotdata(priceData = Prices_VS1, prefData = Pref_VS1,logitExp = logit_exp_VS1, groupValue = "vehicle_type", Reg_Aggregation = RegAggregation, weightpkm = weight_pkm)

        #Prices FV
        Prices_FV <- prices$FV_shares
        setkey(Prices_FV, NULL)
        Pref_FV <- Pref$FV_final_pref
        setkey(Pref_FV, NULL)
        logit_exp_VS1 <- logit_exp$logit_exponent_FV
        setkey(logit_exp_VS1, NULL)

        Prices_FV <- LogitCostplotdata_FV(priceData = Prices_FV, prefData = Pref_FV, logitExp = logit_exp_VS1, Reg_Aggregation = RegAggregation, weightpkm = weight_pkm)

        Pref_FV <- Pref_FV[logit_type == "sw"]
        #Walking and cycling have no fuel options
        Pref_FV <- Pref_FV[!technology %in% c("Cycle_tmp_technology", "Walk_tmp_technology")]
        Pref_FV[, variable := paste0("Shareweight|F|", gsub("_tmp_vehicletype", "", vehicle_type), "|", technology)][, unit := "-"][,scenario := scenario_title][, model := model_name]
        Pref_FV <- Pref_FV[, .(region, period, scenario, variable, value, unit, model)]

        toMIF <- rbind(toMIF, PrefandPrices_S3S, PrefandPrices_S2S3, PrefandPrices_S1S2, PrefandPrices_VS1, Prices_FV, Pref_FV)}}

    #Aggregate data
    #Insert POP and GDP
    if (file.exists(datapath(fname = "POP.RDS")) & file.exists(datapath(fname = "GDP.RDS"))){
      POP <- readRDS(datapath(fname = "POP.RDS"))
      GDP <- readRDS(datapath(fname = "GDP.RDS"))
      POP <- POP[year %in% yrs]
      GDP <- GDP[year %in% yrs]
      POP[, model := model_name][, scenario := scenario_title][, variable := "Population"][, unit := "million"]
      GDP[, model := model_name][, scenario := scenario_title][, variable := "GDP|PPP"]
      GDP[, weight := weight*0.001][, unit := "billion US$2005/yr"]
      setnames(GDP, c("year", "weight"), c("period", "value"))
      setnames(POP, "year", "period")
      #Aggregate World
      POP <- rbindlist(list(POP, POP[, .(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]), use.names = TRUE)
      GDP <- rbindlist(list(GDP, GDP[, .(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]), use.names = TRUE)

      if (!is.null(regionSubsetList)){
        toMIF <- rbindlist(list(
          toMIF,
          POP[region %in% regionSubsetList[["EUR"]], .(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
          POP[region %in% regionSubsetList[["NEU"]], .(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
          POP[region %in% regionSubsetList[["EU27"]], .(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)],
          GDP[region %in% regionSubsetList[["EUR"]], .(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
          GDP[region %in% regionSubsetList[["NEU"]], .(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
          GDP[region %in% regionSubsetList[["EU27"]], .(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)]
        ), use.names = TRUE)
      }

      toMIF <- rbind(toMIF, POP, GDP)
    }

    varsl <- list(
      `ES|Transport edge|Pass` = c("ES|Transport|Pass|Road|LDV", "ES|Transport|Pass|Road|Bus", "ES|Transport|Pass|Road|Non-Motorized","ES|Transport|Pass|Rail|HSR",
                              "ES|Transport|Pass|Rail|non-HSR","ES|Transport|Pass|Aviation|International", "ES|Transport|Pass|Aviation|Domestic"),
      `ES|Transport edge|Freight` = c("ES|Transport|Freight|Road","ES|Transport|Freight|International Shipping","ES|Transport|Freight|Rail", "ES|Transport|Freight|Navigation"),
      `FE|Transport edge|Pass` = c("FE|Transport|Pass|Road|LDV", "FE|Transport|Pass|Road|Bus","FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR","FE|Transport|Pass|Aviation|International",
                              "FE|Transport|Pass|Aviation|Domestic"),
      `FE|Transport edge|Freight` = c("FE|Transport|Freight|Road","FE|Transport|Freight|International Shipping","FE|Transport|Freight|Rail", "FE|Transport|Freight|Navigation"),
      `FE|Transport edge` = c("FE|Transport|Freight|Road","FE|Transport|Freight|International Shipping","FE|Transport|Freight|Rail", "FE|Transport|Freight|Navigation","FE|Transport|Pass|Road|LDV",
                         "FE|Transport|Pass|Road|Bus","FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR","FE|Transport|Pass|Aviation|International", "FE|Transport|Pass|Aviation|Domestic"),
      `FE|Transport edge|w/o bunkers` = c("FE|Transport|Freight|w/o bunkers","FE|Transport|Pass|w/o bunkers"),
      `FE|Transport edge|Pass|Liquids` = c("FE|Transport|Pass|Road|LDV|Liquids", "FE|Transport|Pass|Road|Bus|Liquids", "FE|Transport|Pass|Rail|non-HSR|Liquids","FE|Transport|Pass|Aviation|International|Liquids",
                                      "FE|Transport|Pass|Aviation|Domestic|Liquids"),
      `FE|Transport edge|Pass|Hydrogen` = c("FE|Transport|Pass|Road|LDV|Hydrogen", "FE|Transport|Pass|Road|Bus|Hydrogen", "FE|Transport|Pass|Aviation|Domestic|Hydrogen"),
      `FE|Transport edge|Pass|Gases` = c("FE|Transport|Pass|Road|LDV|Gases", "FE|Transport|Pass|Road|Bus|Gases"),
      `FE|Transport edge|Pass|Electricity` = c("FE|Transport|Pass|Road|LDV|Electricity", "FE|Transport|Pass|Road|Bus|Electricity","FE|Transport|Pass|Rail|HSR|Electricity", "FE|Transport|Pass|Rail|non-HSR|Electricity"),
      `FE|Transport edge|Freight|Liquids` = c("FE|Transport|Freight|Road|Liquids","FE|Transport|Freight|International Shipping|Liquids","FE|Transport|Freight|Rail|Liquids", "FE|Transport|Freight|Navigation|Liquids"),
      `FE|Transport edge|Freight|Hydrogen` = c("FE|Transport|Freight|Road|Hydrogen"),
      `FE|Transport edge|Freight|Gases` = c("FE|Transport|Freight|Road|Gases"),
      `FE|Transport edge|Freight|Electricity` = c("FE|Transport|Freight|Road|Electricity","FE|Transport|Freight|Rail|Electricity"),
      `FE|Transport edge|Liquids` = c("FE|Transport|Freight|Road|Liquids","FE|Transport|Freight|International Shipping|Liquids","FE|Transport|Freight|Rail|Liquids", "FE|Transport|Freight|Navigation|Liquids",
                                 "FE|Transport|Pass|Road|LDV|Liquids", "FE|Transport|Pass|Road|Bus|Liquids", "FE|Transport|Pass|Rail|non-HSR|Liquids","FE|Transport|Pass|Aviation|International|Liquids",
                                 "FE|Transport|Pass|Aviation|Domestic|Liquids"),
      `FE|Transport edge|Hydrogen` = c("FE|Transport|Freight|Road|Hydrogen","FE|Transport|Pass|Road|LDV|Hydrogen", "FE|Transport|Pass|Road|Bus|Hydrogen", "FE|Transport|Pass|Aviation|Domestic|Hydrogen"),
      `FE|Transport edge|Gases` = c("FE|Transport|Freight|Road|Gases","FE|Transport|Pass|Road|LDV|Gases", "FE|Transport|Pass|Road|Bus|Gases"),
      `FE|Transport edge|Electricity` = c("FE|Transport|Freight|Road|Electricity","FE|Transport|Freight|Rail|Electricity","FE|Transport|Pass|Road|LDV|Electricity", "FE|Transport|Pass|Road|Bus|Electricity","FE|Transport|Pass|Rail|HSR|Electricity",
                                     "FE|Transport|Pass|Rail|non-HSR|Electricity"),
      `FE|Transport edge|w/o bunkers|Liquids` = c("FE|Transport|Freight|w/o bunkers|Liquids","FE|Transport|Pass|w/o bunkers|Liquids"),
      `FE|Transport edge|w/o bunkers|Hydrogen` = c("FE|Transport|Freight|w/o bunkers|Hydrogen","FE|Transport|Pass|w/o bunkers|Hydrogen"),
      `FE|Transport edge|w/o bunkers|Gases` = c("FE|Transport|Freight|w/o bunkers|Gases","FE|Transport|Pass|w/o bunkers|Gases"),
      `FE|Transport edge|w/o bunkers|Electricity` = c("FE|Transport|Freight|w/o bunkers|Electricity","FE|Transport|Pass|w/o bunkers|Electricity"))

    names <- names(varsl)
    totals <- sapply(names, reportTotals, datatable = toMIF, varlist = varsl, simplify = FALSE, USE.NAMES = TRUE)

    totals <- rbindlist(totals, use.names = TRUE)
    toMIF <- rbind(toMIF, totals)

  ##################
  #Note that we do only have efficiencies for Electric, Liquids and Hydrogen.Thus, useful energy e.g. from aviation cannot be estimated realistically
  # Mapping efficiencies for useful energy
  Mapp_UE <- data.table(
    technology = c("Electric", "Liquids", "Hydrogen"),
    UE_efficiency = c(0.64, 0.23, 0.25))

  #Calculate useful energy
  UE <- toMIF[grepl("FE", variable) & grepl("Electric|Liquids|Hydrogen", variable)] #select only FE for electricity, liquids and hydrogen
  
  
  #create new column named technology and assign values based on variables in UE.varialbe: if the variable contains "Electricity" then technology is "Electric", if the variable contains "Liquids" then technology is "Liquids", if the variable contains "Hydrogen" then technology is "Hydrogen"
  UE[, technology := ifelse(grepl("Electricity", variable), "Electric", ifelse(grepl("Liquids", variable), "Liquids", ifelse(grepl("Hydrogen", variable), "Hydrogen", NA)))]
  
  UE <- merge(UE, Mapp_UE, technology) #merge with efficiencies
  UE[, value:= value*UE_efficiency][, c("variable", "technology", "UE_efficiency"):= list(gsub("FE","UE", variable), NULL, NULL)] #calculate useful energy

  toMIF  <- rbind(toMIF, UE)

  varsl <- list(

    `UE|Transport|Pass|Liquids` = c("UE|Transport|Pass|Road|LDV|Liquids", "UE|Transport|Pass|Road|Bus|Liquids", "UE|Transport|Pass|Rail|non-HSR|Liquids","UE|Transport|Pass|Aviation|International|Liquids", "UE|Transport|Pass|Aviation|Domestic|Liquids"),
    `UE|Transport|Pass|Hydrogen` = c("UE|Transport|Pass|Road|LDV|Hydrogen", "UE|Transport|Pass|Road|Bus|Hydrogen", "UE|Transport|Pass|Aviation|Domestic|Hydrogen"),
    `UE|Transport|Pass|Electricity` = c("UE|Transport|Pass|Road|LDV|Electricity", "UE|Transport|Pass|Road|Bus|Electricity","UE|Transport|Pass|Rail|HSR|Electricity", "UE|Transport|Pass|Rail|non-HSR|Electricity"),

    `UE|Transport|w/o bunkers|Liquids` = c("UE|Transport|Freight|w/o bunkers|Liquids","UE|Transport|Pass|w/o bunkers|Liquids"),
    `UE|Transport|w/o bunkers|Hydrogen` = c("UE|Transport|Freight|w/o bunkers|Hydrogen","UE|Transport|Pass|w/o bunkers|Hydrogen"),
    `UE|Transport|w/o bunkers|Electricity` = c("UE|Transport|Freight|w/o bunkers|Electricity","UE|Transport|Pass|w/o bunkers|Electricity"))

  names <- names(varsl)
  totals <- sapply(names, reportTotals, datatable = toMIF, varlist = varsl, simplify = FALSE, USE.NAMES = TRUE)

  totals <- rbindlist(totals, use.names = TRUE)
  toMIF <- rbind(toMIF, totals)
  }

  #We should finally decide for which yrs the model runs and shows reasonable results
  toMIF <- toMIF[period %in% yrs]
  toMIF <- rbind(
    toMIF,
    toMIF[period == 2100][, period := 2110],
    toMIF[period == 2100][, period := 2130],
    toMIF[period == 2100][, period := 2150]
  )

  ## Make sure there are no duplicates!
  idx <- anyDuplicated(toMIF, by = c("region", "variable", "period"))
  if(idx){
    warning(paste0("Duplicates found in EDGE-T reporting output:",
                   capture.output(toMIF[idx]), collapse="\n"))
  }

  toMIF <- toMIF[!duplicated(toMIF)]
  toMIF <- toMIF[, c("model", "scenario", "region", "variable", "unit", "period", "value")]

  return(as.quitte(toMIF))

}
