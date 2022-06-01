#' Reporting for the coupled EDGE-T Transport Sector Model (REMIND Module edge_esm)
#'
#' Data is loaded from the EDGE-T subfolder in the output folder.
#' The input files can be (re-) generated calling
#' `Rscript EDGETransport.R --reporting`
#' from the output folder.
#'
#' *Warning* The function modifies the "REMIND_generic_<scenario>.mif" file by appending the
#' additional reporting variables and replaces the "_withoutPlus" version.
#'
#' Region subsets are obtained from fulldata.gdx
#'
#' @param output_folder path to the output folder, default is current folder.
#' @param sub_folder subfolder with EDGE-T output files (level_2 for standalone, EDGE-T for coupled runs)
#' @param loadmif shall we try to load a REMIND MIF file from the output folder to append the variables?
#' @param extendedReporting report a larger set of variables
#' @param scenario_title a scenario title string
#' @param model_name a model name string
#' @param gdx path to the GDX file used for the run.
#' @author Johanna Hoppe Alois Dirnaichner Marianna Rottoli
#'
#' @importFrom rmndt approx_dt readMIF writeMIF
#' @importFrom gdxdt readgdx
#' @importFrom data.table fread fwrite rbindlist copy CJ
#' @importFrom remind2 toolRegionSubsets
#' @importFrom quitte as.quitte
#' @export

reportEDGETransport2 <- function(output_folder = ".", sub_folder = "EDGE-T/",
                                loadmif = TRUE , extendedReporting = FALSE,
                                scenario_title = NULL, model_name = "EDGE-Transport",
                                gdx = NULL) {


  ## NULL Definitons for codeCheck compliance
  RegionCode <- CountryCode <- `.` <- sector <- subsector_L3 <- region <- year <- NULL
  subsector_L2 <- subsector_L1 <- aggr_mode <- vehicle_type <- det_veh <- aggr_nonmot <- NULL
  demand_F <- demand_EJ <- remind_rep <- V25 <- aggr_veh <- technology <- NULL
  ttot <- se_share <- fe_demand <- variable <- value <- demand_VKM <- loadFactor <- NULL
  all_enty <- ef <- variable_agg <- model <- scenario <- period <- NULL
  Region <- Variable <- co2 <- co2val <- elh2 <- fe  <- NULL
  int <- se <- sec  <- sharesec <- te  <- tech <-  val <- share <- NULL
  eff <- sharebio <- sharesyn <- totseliq <- type <- ven <- NULL
  unit <- tot_VOT_price <- tot_price <- logit_type <- capture.output <- weight <- NULL

  #pkm or tkm is called km in the reporting. Vehicle km are called vkm

  yrs <- c(seq(2005, 2060, 5), seq(2070, 2100, 10))

  datapath <- function(fname){
    file.path(output_folder, sub_folder, fname)}

  reporting <- function(datatable, mode){
    aggr_mode_tech <- aggr_LDV <- aggr_LDV_tech <- det_veh_tech <- aggr_bunkers <- aggr_bunkers_tech <- aggr_veh_tech <- capture.output <- NULL
    report <- list()


    datatable[, sector := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "Pass", "Freight")]
    datatable <- merge(datatable,Aggrdata,by = c("sector", "subsector_L1", "subsector_L2", "subsector_L3", "vehicle_type", "technology"), all.x = TRUE, allow.cartesian = TRUE)

    #How to account for Hybrid Electric in Final Energy?
    if (mode == "FE") {
      techmap <- data.table(
        technology = c("BEV","Electric", "Hybrid Electric", "FCEV", "Hydrogen", "Liquids", "NG"),
        remind_rep = c("Electricity", "Electricity", "Liquids", "Hydrogen", "Hydrogen", "Liquids", "Gases"))
    } else {
      techmap <- data.table(
        technology = c("BEV", "Electric", "Hybrid Electric", "FCEV", "Hydrogen", "Liquids","NG"),
        remind_rep = c("BEV", "Electric", "Hybrid Electric", "FCEV", "Hydrogen", "Liquids", "Gases"))
    }

    datatable <- merge(datatable,techmap,by = c("technology"), all.x = TRUE)

    datatable[!is.na(aggr_mode) & !is.na(remind_rep), aggr_mode_tech := paste0(aggr_mode, "|", remind_rep)]
    datatable[!is.na(aggr_veh) & !is.na(remind_rep), aggr_veh_tech := paste0(aggr_veh, "|", remind_rep)]
    datatable[!is.na(aggr_LDV) & !is.na(remind_rep), aggr_LDV_tech := paste0(aggr_LDV, "|", remind_rep)]
    datatable[!is.na(det_veh) & !is.na(remind_rep), det_veh_tech := paste0(det_veh, "|", remind_rep)]
    datatable[!is.na(aggr_bunkers) & !is.na(remind_rep), aggr_bunkers_tech := paste0(aggr_bunkers, "|", remind_rep)]

    unit <- switch(mode,
                   "FE" = "EJ/yr",
                   "ES" = "bn km/yr",
                   "VKM" = "bn vkm/yr")

    prefix <- switch(mode,
                     "FE" = "FE|Transport|",
                     "ES" = "ES|Transport|",
                     "VKM" = "ES|Transport|VKM|")

    var <- c("Pass","Freight")

    Aggr <- c("aggr_mode", "aggr_veh", "aggr_LDV", "det_veh", "nonmot", "aggr_nonmot", "aggr_bunkers", "aggr_mode_tech", "aggr_veh_tech", "aggr_LDV_tech", "det_veh_tech","aggr_bunkers_tech")


    for (var0 in var) {

      for (Aggr0 in Aggr) {


        #Aggregate data
        datatable0 <- copy(datatable)
        datatable0 <- datatable0[!is.na(get(Aggr0))]

        datatable0 <- datatable0[sector == var0, .(value = sum(value, na.rm = T)),
                                 by = c("region", "year", Aggr0)]

        if(nrow(datatable0) > 0) {

          setnames(datatable0, "year", "period")

          datatable0 <- datatable0[, model := model_name][, scenario := scenario_title][, variable := paste0(prefix, get(Aggr0))][, unit := unit][, eval(Aggr0) := NULL]

          datatable0 <- approx_dt(datatable0, yrs, xcol = "period", ycol = "value",
                                  idxcols = c("scenario","variable","unit","model","region"),
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
    emidem[, value := value*ef][, c("variable", "unit") := list(gsub("FE", "Emi\\|CO2", variable), "Mt CO2/yr")]

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
    emi <- emi[all_enty %in% c("fedie", "fepet", "fegat") & type == "demand", value := value*se_share]

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

  reportStockAndSales <- function(annual_mileage){
    if(file.exists(file.path(output_folder, "vintcomp.csv"))){
      vintages_file <- file.path(output_folder, "vintcomp.csv")
      vintgs <- fread(vintages_file)
    } else if (file.exists(datapath(fname = "vintcomp.RDS"))){
      #vintages_file <- datapath(fname = "vintcomp.RDS")
      #vintgs <- readRDS(vintages_file)
      return(NULL)
    } else {
      print("EDGE-T Reporting: No vintages file found.")
      return(NULL)
    }

    year_c <- construction_year <- Stock <- Sales <- vintage_demand_vkm <- fct <- category <- NULL


    ## backward compat. fix
    fct <- 1.
    if("variable" %in% colnames(vintgs)){
      fct <- 1e-6
      setnames(vintgs, "variable", "construction_year")
    }

    vintgs[, year_c := as.numeric(gsub("C_", "", construction_year))]

    ## stock is the full stock up to the end of the current year
    ## sales are the sales of the current year

    setnames(vintgs, "full_demand_vkm", "Stock")
    vintgs[, Stock := Stock * fct]
    vintgs[, Sales := Stock - sum(vintage_demand_vkm), by=.(year, region, vehicle_type, technology)]
    vintgs[, c("construction_year", "vintage_demand_vkm", "year_c") := NULL]
    vintgs <- unique(vintgs)

    vintgs <- data.table::melt(vintgs, measure.vars = c("Stock", "Sales"), variable.name = "category")
    ## vkm -> v-num
    vintgs = merge(vintgs, annual_mileage, by = c("year", "region", "vehicle_type"))
    vintgs[, value := value / annual_mileage]
    vintgs[, variable := ifelse(
      vehicle_type == "Bus_tmp_vehicletype",
      sprintf("%s|Transport|Bus|%s", category, technology),
      sprintf("%s|Transport|LDV|%s|%s", category, vehicle_type, technology))]

    ## totals
    vintgs <- rbindlist(list(
      vintgs,
      vintgs[, .(value=sum(value), variable=gsub("(.+)\\|.+$", "\\1", variable)),
             by=c("category", "year", "region", "vehicle_type")],
      vintgs[grepl("|LDV|", variable, fixed=TRUE),
             .(value=sum(value), variable=sprintf("%s|Transport|LDV", category)),
             by=c("category", "year", "region")]), fill=TRUE)

    vintgs[, c("vehicle_type", "technology", "annual_mileage", "category") := NULL]
    vintgs <- unique(vintgs[!is.na(value)])

    setnames(vintgs, "year", "period")

    vintgs = approx_dt(vintgs, c(2005, 2010, unique(vintgs$period), 2110, 2130, 2150),
                       xcol = "period", ycol = "value", idxcols = c("region", "variable"), extrapolate = T)
    vintgs[period <= 2010|period > 2100, value := 0]

    ## remove the variable (e.g. vehicle_types) that are not present for this specific region
    vintgs[, `:=`(model = model_name, scenario = scenario_title, unit = "Million vehicles")]

    return(vintgs)

  }

  reportTotals <- function(aggrname, datatable, varlist){

    vars <- varlist[[aggrname]]
    if (length(unique(datatable[variable %in% vars]$variable)) < length(vars)){
     print(paste0("Missing variables to aggregate data to ", aggrname))}

     datatable <- datatable[variable %in% vars,
                           .(variable = aggrname,
                             value = sum(value)),
                           by = c("model", "scenario", "region", "period","unit")]

    return(datatable)
  }

  ## check the regional aggregation
  regionSubsetList <- toolRegionSubsets(gdx)

  # ADD EU-27 region aggregation if possible
  if("EUR" %in% names(regionSubsetList)){
    regionSubsetList <- c(regionSubsetList,list(
      "EU27"=c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "ESW")
    ))
  }



  Aggrdata <- fread(system.file("extdata", "EDGETdataAggregation.csv", package = "edgeTrpLib"),header = TRUE)


  ## load input data from last EDGE run
  ## Data manipulation shouldnt be necessary
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))
  demand_km[, demand_F := demand_F * 1e-3] ## million -> billion pkm
  setnames(demand_km, "demand_F", "value")
  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
  setnames(demand_ej, "demand_EJ", "value")
  demand_ej[, demand_F := NULL]
  load_factor <- readRDS(datapath(fname = "loadFactor.RDS"))
  annual_mileage <- readRDS(datapath(fname = "annual_mileage.RDS"))

  if (length(annual_mileage)> 4){
    #Same is done in lvl2_createoutput
    annual_mileage <- unique(annual_mileage[, c("region", "year", "vkm.veh", "vehicle_type")])
    setnames(annual_mileage, old = "vkm.veh", new = "annual_mileage")
  }
  if (length(load_factor)> 4){
   load_factor <- load_factor[, c("year","region","vehicle_type","loadFactor","technology")]
  demand_vkm <- merge(demand_km, load_factor, by = c("year", "region", "vehicle_type","technology"))
  demand_vkm[, value := value/loadFactor] ## billion vkm
  } else {
  demand_vkm <- merge(demand_km, load_factor, by = c("year", "region", "vehicle_type"))
  demand_vkm[, value := value/loadFactor]} ## billion vkm


  repFE <- reporting(
    demand_ej,
    mode = "FE")
  repVKM <- reporting(
    datatable = demand_vkm,
    mode = "VKM")
  repES <- reporting(
    datatable = demand_km,
    mode = "ES")
  toMIF <- rbind(
    repFE,
    repVKM,
    repES,
    reportingVehNum(repVKM,
                    annual_mileage),
    reportingEmi(repFE = repFE,
                 gdx = gdx)
  )

  varsl <- list(
    `ES|Transport|Pass|Road` = c("ES|Transport|Pass|Road|LDV", "ES|Transport|Pass|Road|Bus", "ES|Transport|Pass|Road|Non-Motorized"),
    `ES|Transport|Pass|Aviation` = c("ES|Transport|Pass|Aviation|International", "ES|Transport|Pass|Aviation|Domestic"),
    `ES|Transport|Pass|Rail` = c("ES|Transport|Pass|Rail|HSR", "ES|Transport|Pass|Rail|non-HSR"),
    `ES|Transport|Pass` = c("ES|Transport|Pass|Road|LDV", "ES|Transport|Pass|Road|Bus", "ES|Transport|Pass|Road|Non-Motorized","ES|Transport|Pass|Rail|HSR", "ES|Transport|Pass|Rail|non-HSR","ES|Transport|Pass|Aviation|International", "ES|Transport|Pass|Aviation|Domestic"),
    `ES|Transport|Freight` = c("ES|Transport|Freight|Road","ES|Transport|Freight|International Shipping","ES|Transport|Freight|Rail", "ES|Transport|Freight|Navigation"),
    `ES|Transport` = c("ES|Transport|Freight|Road","ES|Transport|Freight|International Shipping","ES|Transport|Freight|Rail", "ES|Transport|Freight|Navigation","ES|Transport|Pass|Road|LDV", "ES|Transport|Pass|Road|Bus", "ES|Transport|Pass|Road|Non-Motorized","ES|Transport|Pass|Rail|HSR", "ES|Transport|Pass|Rail|non-HSR","ES|Transport|Pass|Aviation|International", "ES|Transport|Pass|Aviation|Domestic"),
    `ES|Transport|VKM|Pass|Road` = c("ES|Transport|VKM|Pass|Road|LDV", "ES|Transport|VKM|Pass|Road|Bus"),
    `ES|Transport|VKM||Road` = c("ES|Transport|VKM|Freight|Road", "ES|Transport|VKM|Pass|Road|LDV", "ES|Transport|VKM|Pass|Road|Bus"),
    `ES|Transport|VKM|Rail` = c("ES|Transport|VKM|Pass|Rail|HSR", "ES|Transport|VKM|Pass|Rail|non-HSR", "ES|Transport|VKM|Freight|Rail" ),
    `FE|Transport|Pass|Road` = c("FE|Transport|Pass|Road|LDV", "FE|Transport|Pass|Road|Bus"),
    `FE|Transport|Road` = c("FE|Transport|Freight|Road", "FE|Transport|Pass|Road|LDV", "FE|Transport|Pass|Road|Bus"),
    `FE|Transport|Pass|Rail` = c("FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR"),
    `FE|Transport|Rail` = c("FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR", "FE|Transport|Freight|Rail"),
    `FE|Transport|Pass` = c("FE|Transport|Pass|Road|LDV", "FE|Transport|Pass|Road|Bus","FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR","FE|Transport|Pass|Aviation|International", "FE|Transport|Pass|Aviation|Domestic"),
    `FE|Transport|Freight` = c("FE|Transport|Freight|Road","FE|Transport|Freight|International Shipping","FE|Transport|Freight|Rail", "FE|Transport|Freight|Navigation"),
    `FE|Transport` = c("FE|Transport|Freight|Road","FE|Transport|Freight|International Shipping","FE|Transport|Freight|Rail", "FE|Transport|Freight|Navigation","FE|Transport|Pass|Road|LDV", "FE|Transport|Pass|Road|Bus","FE|Transport|Pass|Rail|HSR", "FE|Transport|Pass|Rail|non-HSR","FE|Transport|Pass|Aviation|International", "FE|Transport|Pass|Aviation|Domestic"),
    `FE|Transport|w/o bunkers` = c("FE|Transport|Freight|w/o bunkers","FE|Transport|Pass|w/o bunkers"),
    `FE|Transport|Pass|Liquids` = c("FE|Transport|Pass|Road|LDV|Liquids", "FE|Transport|Pass|Road|Bus|Liquids", "FE|Transport|Pass|Rail|non-HSR|Liquids","FE|Transport|Pass|Aviation|International|Liquids", "FE|Transport|Pass|Aviation|Domestic|Liquids"),
    `FE|Transport|Pass|Hydrogen` = c("FE|Transport|Pass|Road|LDV|Hydrogen", "FE|Transport|Pass|Road|Bus|Hydrogen", "FE|Transport|Pass|Aviation|Domestic|Hydrogen"),
    `FE|Transport|Pass|Gases` = c("FE|Transport|Pass|Road|LDV|Gases", "FE|Transport|Pass|Road|Bus|Gases"),
    `FE|Transport|Pass|Electricity` = c("FE|Transport|Pass|Road|LDV|Electricity", "FE|Transport|Pass|Road|Bus|Electricity","FE|Transport|Pass|Rail|HSR|Electricity", "FE|Transport|Pass|Rail|non-HSR|Electricity"),
    `FE|Transport|Freight|Liquids` = c("FE|Transport|Freight|Road|Liquids","FE|Transport|Freight|International Shipping|Liquids","FE|Transport|Freight|Rail|Liquids", "FE|Transport|Freight|Navigation|Liquids"),
    `FE|Transport|Freight|Hydrogen` = c("FE|Transport|Freight|Road|Hydrogen"),
    `FE|Transport|Freight|Gases` = c("FE|Transport|Freight|Road|Gases"),
    `FE|Transport|Freight|Electricity` = c("FE|Transport|Freight|Road|Electricity","FE|Transport|Freight|Rail|Electricity"),
    `FE|Transport|Liquids` = c("FE|Transport|Freight|Road|Liquids","FE|Transport|Freight|International Shipping|Liquids","FE|Transport|Freight|Rail|Liquids", "FE|Transport|Freight|Navigation|Liquids","FE|Transport|Pass|Road|LDV|Liquids", "FE|Transport|Pass|Road|Bus|Liquids", "FE|Transport|Pass|Rail|non-HSR|Liquids","FE|Transport|Pass|Aviation|International|Liquids", "FE|Transport|Pass|Aviation|Domestic|Liquids"),
    `FE|Transport|Hydrogen` = c("FE|Transport|Freight|Road|Hydrogen","FE|Transport|Pass|Road|LDV|Hydrogen", "FE|Transport|Pass|Road|Bus|Hydrogen", "FE|Transport|Pass|Aviation|Domestic|Hydrogen"),
    `FE|Transport|Gases` = c("FE|Transport|Freight|Road|Gases","FE|Transport|Pass|Road|LDV|Gases", "FE|Transport|Pass|Road|Bus|Gases"),
    `FE|Transport|Electricity` = c("FE|Transport|Freight|Road|Electricity","FE|Transport|Freight|Rail|Electricity","FE|Transport|Pass|Road|LDV|Electricity", "FE|Transport|Pass|Road|Bus|Electricity","FE|Transport|Pass|Rail|HSR|Electricity", "FE|Transport|Pass|Rail|non-HSR|Electricity"),
    `FE|Transport|w/o bunkers|Liquids` = c("FE|Transport|Freight|w/o bunkers|Liquids","FE|Transport|Pass|w/o bunkers|Liquids"),
    `FE|Transport|w/o bunkers|Hydrogen` = c("FE|Transport|Freight|w/o bunkers|Hydrogen","FE|Transport|Pass|w/o bunkers|Hydrogen"),
    `FE|Transport|w/o bunkers|Gases` = c("FE|Transport|Freight|w/o bunkers|Gases","FE|Transport|Pass|w/o bunkers|Gases"),
    `FE|Transport|w/o bunkers|Electricity` = c("FE|Transport|Freight|w/o bunkers|Electricity","FE|Transport|Pass|w/o bunkers|Electricity"),
    `Emi|CO2|Transport|Pass|Road|Tailpipe` = c("Emi|CO2|Transport|Pass|Road|LDV|Tailpipe", "Emi|CO2|Transport|Pass|Road|Bus|Tailpipe"),
    `Emi|CO2|Transport|Pass|Road|Demand` = c("Emi|CO2|Transport|Pass|Road|LDV|Demand", "Emi|CO2|Transport|Pass|Road|Bus|Demand"),
    `Emi|CO2|Transport|Road|Tailpipe` = c("Emi|CO2|Transport|Freight|Road|Tailpipe", "Emi|CO2|Transport|Pass|Road|LDV|Tailpipe", "Emi|CO2|Transport|Pass|Road|Bus|Tailpipe"),
    `Emi|CO2|Transport|Rail|Tailpipe` = c("Emi|CO2|Transport|Pass|Rail|non-HSR|Tailpipe", "Emi|CO2|Transport|Freight|Rail|Tailpipe"),
    `Emi|CO2|Transport|Road|Demand` = c("Emi|CO2|Transport|Freight|Road|Demand", "Emi|CO2|Transport|Pass|Road|LDV|Demand", "Emi|CO2|Transport|Pass|Road|Bus|Demand"),
    `Emi|CO2|Transport|Rail|Demand` = c("Emi|CO2|Transport|Pass|Rail|non-HSR|Demand", "Emi|CO2|Transport|Freight|Rail|Demand"))

  names <- names(varsl)
  totals <- sapply(names, reportTotals, datatable = toMIF, varlist = varsl, simplify = FALSE, USE.NAMES = TRUE)

  totals <- rbindlist(totals, use.names = TRUE)
  toMIF <- rbind(toMIF, totals)

  toMIF <- rbindlist(list(toMIF, reportStockAndSales(annual_mileage)), use.names=TRUE)

  if (!is.null(regionSubsetList)){
    toMIF <- rbindlist(list(
      toMIF,
      toMIF[region %in% regionSubsetList[["EUR"]], .(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[["NEU"]], .(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[["EU27"]], .(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)],
      toMIF[, .(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    ), use.names=TRUE)
  }


  if (extendedReporting) {

    LogitCostplotdata <- function(priceData, prefData, logitExp, groupValue, Reg_Aggregation){
      tot_price <- sw <- logit.exponent <- weight <- NULL
      yrs_costs <-c(seq(2005, 2060, 5), seq(2070, 2100, 10))
      all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                          "subsector_L3", "sector")
      # change variable names for quitte format
      setnames(priceData, c("year"), c("period"))
      setnames(prefData, c("year"), c("period"))
      prefData <- prefData[period %in% yrs_costs]
      priceData<-  priceData[period %in% yrs_costs][, -c("share")]

      #Filter for logit level according to groupValue. leave out tmp placeholders
      priceData <- priceData[!grepl("tmp", get(groupValue))]
      prefData <- prefData[!grepl("tmp", get(groupValue))]

      # Calculate Inconvenience Cost from share Weight
      # Logit Exponent and total price are needed for this
      prefData_inco <- merge(prefData, logitExp, all.y = TRUE)
      #rename original prefs afterwards
      setnames(prefData,c("sw"),c("value"))

      #Reduce priceData to total price
      price_tot <- priceData[, c("period", "region", "tot_price", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]), with = FALSE]

      prefData_inco <- merge(prefData_inco, price_tot, by = c("period", "region", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))

      prefData_inco[, value := tot_price * (sw^(1 / logit.exponent) - 1)]

      #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero
      prefData_inco <- prefData_inco[is.infinite(prefData_inco$value), value:=0]
      prefData_inco <- prefData_inco[, c("region", "period", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)], "value"), with = FALSE][, variable := "Eq inconvenience cost"]

      #Prepare PriceData
      priceData <- data.table::melt(priceData[, -c("tot_price")], id.vars = c("region", "period", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))

      #Regional Aggregation
      #Costs are intensive variables and are aggregated with ES weights for each level of the logit
      weight_pkm_logitlevel <- weight_pkm[, .(weight = sum(weight)), by = c("region", "period", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)])]

      prefData_aggr <- aggregate_dt(prefData[region %in% Reg_Aggregation$region], Reg_Aggregation ,fewcol = "aggr_reg", manycol = "region",  yearcol = "period", weights = weight_pkm_logitlevel[region %in% Reg_Aggregation$region & period %in% prefData$period], datacols = c("period", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))
      setnames(prefData_aggr,"aggr_reg","region")

      prefData_inco_aggr <- aggregate_dt(prefData_inco[region %in% Reg_Aggregation$region], Reg_Aggregation , fewcol = "aggr_reg", manycol = "region", yearcol = "period", weights = weight_pkm_logitlevel[region %in% Reg_Aggregation$region], datacols = c("period", "variable", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))

      setnames(prefData_inco_aggr,"aggr_reg","region")
      priceData_aggr <- aggregate_dt(priceData[region %in% Reg_Aggregation$region], Reg_Aggregation , fewcol = "aggr_reg", manycol = "region", yearcol = "period", weights = weight_pkm_logitlevel[region %in% Reg_Aggregation$region], datacols = c("period", all_subsectors[
        seq(match(groupValue, all_subsectors),
            length(all_subsectors), 1)]))

      setnames(priceData_aggr,"aggr_reg","region")

      prefData <- rbind(prefData, prefData_aggr)

      priceData <- rbind(prefData_inco, prefData_inco_aggr, priceData,priceData_aggr)

      if (groupValue=="vehicle_type"){
        #Before prices are finally structured, vehicles are aggregated
        Aggrdata_veh <- as.data.table(Aggrdata[, c("vehicle_type", "det_veh")])
        Aggrdata_veh <- unique(Aggrdata_veh[!is.na(det_veh)])[, det_veh := gsub("Freight\\|Road\\||Pass\\|Road\\|", "", det_veh)]

        #Exclude those wihout aggregation
        Aggrdata_veh <- Aggrdata_veh[!vehicle_type==det_veh]
        priceData <- priceData[, c("region","variable","vehicle_type","period","value")]
        weight_pkm_VS1 <- weight_pkm[,.(weight = sum(weight)), by = c("region", "vehicle_type", "period")]
        weight_pkm_VS1_aggrreg <- aggregate_dt(weight_pkm_VS1[region %in% Reg_Aggregation$region], Reg_Aggregation ,fewcol = "aggr_reg", manycol = "region",  yearcol = "period", valuecol="weight",  datacols = c("period", "vehicle_type"))
        setnames(weight_pkm_VS1_aggrreg,"aggr_reg","region")
        weight_pkm_VS1 <- rbind(weight_pkm_VS1, weight_pkm_VS1_aggrreg)
        Prices_veh_aggr <- aggregate_dt(priceData[vehicle_type %in% Aggrdata_veh$vehicle_type], Aggrdata_veh , fewcol = "det_veh", manycol = "vehicle_type", yearcol = "period", weights = weight_pkm_VS1[vehicle_type %in% Aggrdata_veh$vehicle_type], datacols = c("region","variable"))
        setnames(Prices_veh_aggr, "det_veh", "vehicle_type")
        Prices_veh_aggr[, variable:=paste0("Logit cost|V|", vehicle_type, "|", variable)][, vehicle_type := NULL]
      }

      if (groupValue=="vehicle_type"){
        #Convert original shareweights to quitte format
        prefData[, variable := paste0("Shareweight|V|", get(groupValue))]
        prefData <- prefData[, .(region, period, scenario, variable, value)]
        #Convert costs to quitte format
        priceData[, variable := paste0("Logit cost|V|", get(groupValue), "|", variable)]
        priceData <- priceData[, .(region, period, scenario, variable, value)]
        priceData <- rbind(priceData, Prices_veh_aggr)}
      else{
        prefData[, variable := paste0("Shareweight|S",gsub("[^123]","",groupValue), "|", get(groupValue))]
        prefData <- prefData[, .(region, period, scenario, variable, value)]
        #Convert costs to quitte format
        priceData[, variable := paste0("Logit cost|S",gsub("[^123]","",groupValue), "|", get(groupValue), "|", variable)]
        priceData <- priceData[, .(region, period, scenario, variable, value)]
      }

      data <- rbind(prefData[, unit := "-"], priceData[, unit := "$2005/km"])
      data[, scenario := scenario_title][, model := model_name]


      return(data)
    }

    LogitCostplotdata_FV <- function(priceData, prefData, logitExp, Reg_Aggregation){
      tot_price <- sw <- logit.exponent <- weight <- logit_type <- av_veh <- NULL
      #Calcualte equivalent inconvenience cost and
      yrs_costs <-c(seq(2005, 2060, 5), seq(2070, 2100, 10))

      # change variable names for mip
      setnames(priceData, c("year"), c("period"))
      setnames(prefData, c("year"), c("period"))

      #Exclude active modes as they have no fuel
      prefData <- prefData[period %in% yrs_costs & !technology %in% c("Cycle_tmp_technology","Walk_tmp_technology")]
      priceData<-  priceData[period %in% yrs_costs]

      # Calculate Inconvenience Cost from share Weight
      priceData_sw <- copy(prefData)
      priceData_sw <- priceData_sw[logit_type == "sw"][, logit_type := NULL]
      setnames(priceData_sw, "value", "sw")
      priceData_sw <- merge(priceData_sw, logitExp, all.x = TRUE)

      #This should be removed in refactoring process
      priceData_sw[grepl("^Truck", vehicle_type), logit.exponent := -4]
      priceData_sw <- priceData_sw[is.na(logit.exponent), logit.exponent :=  -10]

      price_tot <- priceData[, c("period", "region","tot_price", "technology","vehicle_type")]
      priceData_sw <- merge(priceData_sw, price_tot, by = c("period", "region", "technology","vehicle_type"),
           all.x=TRUE)

      priceData_sw[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
      #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero
      priceData_sw <- priceData_sw[is.infinite(priceData_sw$value), value := 0]
      #Some total prices are missing
      priceData_sw <- priceData_sw[is.na(priceData_sw$value), value := 0]
      priceData_sw <- priceData_sw[, c("period", "region", "technology","vehicle_type","value")][, variable := "Eq inconvenience cost"]
      priceData_inco_LDV <- prefData[!logit_type == "sw"][, c("period", "region", "technology","vehicle_type","value","logit_type")]
      setnames(priceData_inco_LDV, "logit_type", "variable")
      #Exclude LDV inco from prefdata
      prefData <- prefData[logit_type == "sw"]
      prefData <- prefData[, .(region, period, scenario, vehicle_type, technology, value)]


      priceData <- data.table::melt(priceData[, -c("tot_price", "share", "subsector_L1", "subsector_L2", "subsector_L3", "sector")], id.vars = c("region", "period", "technology", "vehicle_type"))
      priceData <- rbind(priceData, priceData_sw, priceData_inco_LDV)

      #Regional Aggregation
      #Costs are intensive variables and are aggregated with ES weights for each level of the logit
      weight_pkm_FV <- weight_pkm[, .(weight = sum(weight)), by = c("region", "period","vehicle_type", "technology")]
      #TO FIX:
      #Hydrogen and BEV technologies for aviation and 2Wheelers are not everywhere available: -> Insert zero as weight
      weight_pkm_FV <- merge(weight_pkm_FV, priceData, on=c("region", "period", "vehicle_type", "technology"), all = TRUE)
      weight_pkm_FV[is.na(weight_pkm_FV$weight), weight := 0]
      weight_pkm_FV <- weight_pkm_FV[, c("region", "period","vehicle_type", "technology", "weight")]
      weight_pkm_FV <- weight_pkm_FV[period > 1990 & period < 2110]
      weight_pkm_FV <- unique(weight_pkm_FV)
      weight_pkm_FV_aggrreg <- aggregate_dt(weight_pkm_FV[region %in% Reg_Aggregation$region], Reg_Aggregation ,fewcol = "aggr_reg", manycol = "region",  yearcol = "period", valuecol="weight",  datacols = c("period", "vehicle_type","technology"))
      setnames(weight_pkm_FV_aggrreg,"aggr_reg","region")
      weight_pkm_FV <- rbind(weight_pkm_FV, weight_pkm_FV_aggrreg)

      priceData_aggrreg <- aggregate_dt(priceData[region %in% Reg_Aggregation$region], Reg_Aggregation, fewcol = "aggr_reg", manycol = "region",  yearcol = "period", weights = weight_pkm_FV[region %in% Reg_Aggregation$region], datacols = c("period", "technology", "vehicle_type"))
      setnames(priceData_aggrreg,"aggr_reg","region")
      priceData <- rbind(priceData, priceData_aggrreg)

      prefData_aggrreg <- aggregate_dt(prefData[region %in% Reg_Aggregation$region], Reg_Aggregation, fewcol = "aggr_reg", manycol = "region",  yearcol = "period", weights = weight_pkm_FV[region %in% Reg_Aggregation$region], datacols = c("period", "technology", "vehicle_type"))
      setnames(prefData_aggrreg,"aggr_reg","region")
      prefData <- rbind(prefData, prefData_aggrreg)

      #Before prices are finally structured, vehicles are aggregated
      #ES pkm are used as weights for data aggregation
      Aggrdata_veh <- as.data.table(Aggrdata[, c("vehicle_type", "det_veh")])
      #Remove entries that are not aggregated
      Aggrdata_veh <- Aggrdata_veh[!vehicle_type == det_veh]
      Aggrdata_veh <- unique(Aggrdata_veh[!is.na(det_veh)])[, det_veh := gsub("Freight\\|Road\\||Pass\\|Road\\|", "", det_veh)]
      priceData_aggr <- aggregate_dt(priceData[vehicle_type %in% Aggrdata_veh$vehicle_type], Aggrdata_veh , fewcol = "det_veh", manycol = "vehicle_type", yearcol = "period", weights = weight_pkm_FV[vehicle_type %in% Aggrdata_veh$vehicle_type], datacols = c("region", "variable", "technology"))
      setnames(priceData_aggr, "det_veh", "vehicle_type")

      #Aggregate average vehicle
      Aggrdata_avveh <- as.data.table(Aggrdata)
      Aggrdata_avveh <- Aggrdata_avveh[subsector_L1 == "trn_pass_road_LDV_4W"]
      Aggrdata_avveh <- unique(Aggrdata_avveh[, c("vehicle_type")])
      Aggrdata_avveh[, av_veh := "Average veh"]
      priceData_av <- aggregate_dt(priceData[vehicle_type %in% Aggrdata_avveh$vehicle_type], Aggrdata_avveh , fewcol = "av_veh", manycol = "vehicle_type", yearcol = "period", weights = weight_pkm_FV[vehicle_type %in% Aggrdata_avveh$vehicle_type], datacols = c("region", "variable","technology"))
      setnames(priceData_av, "av_veh", "vehicle_type")

      priceData <- rbind(priceData, priceData_aggr, priceData_av)
      priceData <- priceData[, variable := paste0("Logit cost|F|", gsub("_tmp_vehicletype", "", vehicle_type), "|", technology, "|", variable)][, c("region", "period", "variable", "value")][, unit := "$2005/km"][, model := model_name][, scenario := scenario_title]

      prefData[, variable := paste0("Shareweight|F|", gsub("_tmp_vehicletype", "", vehicle_type), "|", technology)][, unit := "-"][, model := model_name][, scenario := scenario_title]
      prefData <- prefData[, c("period", "region", "variable", "unit", "model", "scenario", "value")]
      data <- rbind(priceData, prefData)

      return(data)
    }



    # Mapping efficiencies for useful energy
    Mapp_UE <- data.table(
      technology = c("FCEV", "BEV", "Electric", "Liquids", "Hydrogen"),
      UE_efficiency = c(0.36, 0.64, 0.8, 0.23, 0.25))

    #ES pkm are used as weights for data aggregation
    weight_pkm <- copy(demand_km)
    setnames(weight_pkm, c("value","year"), c("weight","period"))

    weight_pkm[, sector := ifelse(sector %in% c("Pass"), "trn_pass", "trn_freight")]
    weight_pkm[, sector := ifelse(subsector_L3 == c("International Aviation"), "trn_aviation_intl", sector)]
    weight_pkm[, sector := ifelse(subsector_L3 == c("International Ship"), "trn_shipping_intl", sector)]

    #Mapping for region Aggregation
    RegAggregation <- data.table(
    aggr_reg = c("EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "NEU", "NEU"),
    region = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "UKI", "ESW", "NES", "NEN"))


    # #Calculate useful energy
    # UE <- toMIF[grepl("FE" & ("FCEV"|"BEV"|"Electric"|"Liquids"|"Hydrogen"), variable)]
    # UE[, technology := gsub(!("FCEV"|"BEV"|"Electric"|"Liquids"|"Hydrogen"),"", variable)]
    # UE <- merge(UE, Mapp_UE)
    # UE[, value:= value*UE_efficiency][, variable := gsub("FE","UE", variable)]

    # toMIF <- rbind(toMIF, UE)

    #Calculate logit Costs
    #Read in additional data if exist
    if (file.exists(datapath(fname = "logit_data.RDS"))){
      logit_data <- readRDS(datapath(fname = "logit_data.RDS"))
      prices <- logit_data$share_list
      Pref <- logit_data$pref_data
      if (file.exists(datapath(fname = "logit_exp.RDS"))){
      logit_exp <- readRDS(datapath(fname = "logit_exp.RDS"))
      logit_exp <- logit_exp$logit_output

    #Prices S3S
    Prices_S3S <- prices$S3S_shares
    setkey(Prices_S3S, NULL)
    Pref_S3S <- Pref$S3S_final_pref
    setkey(Pref_S3S, NULL)
    logit_exp_S3S <- logit_exp$logit_exponent_S3S
    setkey(logit_exp_S3S, NULL)

    #Adjust in model itself in refactoring process
    Prices_S3S[subsector_L3 %in% c("Cycle","Walk"), tot_VOT_price := tot_price]
    PrefandPrices_S3S <- LogitCostplotdata(priceData = Prices_S3S, prefData = Pref_S3S, logitExp =logit_exp_S3S, groupValue = "subsector_L3", Reg_Aggregation = RegAggregation)

    #Prices S2S3
    Prices_S2S3 <- prices$S2S3_shares
    setkey(Prices_S2S3, NULL)
    Pref_S2S3 <- Pref$S2S3_final_pref
    setkey(Pref_S2S3, NULL)
    logit_exp_S2S3 <- logit_exp$logit_exponent_S2S3
    setkey(logit_exp_S2S3, NULL)

    PrefandPrices_S2S3 <- LogitCostplotdata(priceData = Prices_S2S3, prefData = Pref_S2S3, logitExp = logit_exp_S2S3, groupValue = "subsector_L2", Reg_Aggregation = RegAggregation)

    #Prices S1S2
    Prices_S1S2 <- prices$S1S2_shares
    setkey(Prices_S1S2, NULL)
    Pref_S1S2 <- Pref$S1S2_final_pref
    setkey(Pref_S1S2, NULL)
    logit_exp_S1S2 <- logit_exp$logit_exponent_S1S2
    setkey(logit_exp_S1S2, NULL)

    PrefandPrices_S1S2 <- LogitCostplotdata(priceData = Prices_S1S2, prefData = Pref_S1S2, logitExp = logit_exp_S1S2, groupValue = "subsector_L1", Reg_Aggregation = RegAggregation)

    #Prices VS1
    Prices_VS1 <- prices$VS1_shares
    setkey(Prices_VS1, NULL)
    Pref_VS1 <- Pref$VS1_final_pref
    setkey(Pref_VS1, NULL)
    logit_exp_VS1 <- logit_exp$logit_exponent_VS1
    setkey(logit_exp_VS1, NULL)

    #Add subsector_L2, subsector L3 and sector to Prices_VS1 (for structural conformity)
    Prices_VS1 <- merge(Prices_VS1, unique(Pref_VS1[, c("subsector_L2", "subsector_L3", "sector", "vehicle_type")]), by = "vehicle_type", all.x = TRUE)
    PrefandPrices_VS1 <- LogitCostplotdata(priceData=Prices_VS1, prefData = Pref_VS1,logitExp = logit_exp_VS1, groupValue = "vehicle_type", Reg_Aggregation = RegAggregation)

    #Prices FV
    Prices_FV <- prices$FV_shares
    setkey(Prices_FV, NULL)
    Pref_FV <- Pref$FV_final_pref
    setkey(Pref_FV, NULL)
    logit_exp_VS1 <- logit_exp$logit_exponent_FV
    setkey(logit_exp_VS1, NULL)

    Prices_FV <- LogitCostplotdata_FV(priceData=Prices_FV, prefData=Pref_FV, logitExp=logit_exp_VS1, Reg_Aggregation = RegAggregation)

    Pref_FV <- Pref_FV[logit_type=="sw"]
    #Walking and cycling have no fuel options
    Pref_FV <- Pref_FV[!technology %in% c("Cycle_tmp_technology","Walk_tmp_technology")]
    Pref_FV[, variable:=paste0("Shareweight|F|",gsub("_tmp_vehicletype","",vehicle_type),"|",technology)][,unit:="-"][,scenario:=scenario_title][,model:=model_name]
    Pref_FV <- Pref_FV[,.(region,period,scenario,variable,value,unit,model)]

    toMIF <- rbind(toMIF,PrefandPrices_S3S, PrefandPrices_S2S3, PrefandPrices_S1S2, PrefandPrices_VS1, Prices_FV, Pref_FV)}}

    #Aggregate data
    #Insert POP and GDP
    if (file.exists(datapath(fname = "POP.RDS")) & file.exists(datapath(fname = "GDP.RDS"))){
      POP <- readRDS(datapath(fname = "POP.RDS"))
      GDP <- readRDS(datapath(fname = "GDP.RDS"))
      POP <- POP[year %in% yrs]
      GDP <- GDP[year %in% yrs]
      POP[, model:= model_name][, scenario:= scenario_title][, variable := "Population"][, unit := "million"]
      GDP[, model:= model_name][, scenario:= scenario_title][, variable := "GDP|PPP"]
      GDP[, weight := weight*0.001][, unit := "billion US$2005/yr"]
      setnames(GDP,c("year","weight"),c("period","value"))
      setnames(POP,"year","period")

      if (!is.null(regionSubsetList)){
        toMIF <- rbindlist(list(
          toMIF,
          POP[region %in% regionSubsetList[["EUR"]], .(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
          POP[region %in% regionSubsetList[["NEU"]], .(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
          POP[region %in% regionSubsetList[["EU27"]], .(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)],
          POP[, .(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)],
          GDP[region %in% regionSubsetList[["EUR"]], .(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
          GDP[region %in% regionSubsetList[["NEU"]], .(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
          GDP[region %in% regionSubsetList[["EU27"]], .(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)],
          GDP[, .(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
        ), use.names=TRUE)
      }

      toMIF <- rbind(toMIF, POP, GDP)
      }
  }
  #We should finally decide for which yrs the model runs and shows reasonable results
  toMIF <- toMIF[period %in% yrs]

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
