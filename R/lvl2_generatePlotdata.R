#' Generate EDGE-Transport Plot Data for Standalone Report
#'
#' Run this script to prepare the input data for EDGE in EDGE-friendly units and regional aggregation
#' @param listofruns list of folders hosting raw data
#' @param AggrReg Regional Aggregation
#' @return generated EDGE-transport plot data
#' @author Johanna Hoppe
#' @import data.table
#' @import magclass getSets getSets<-
#' @import madrat calcOutput
#' @export

lvl2_generate_plotdata <- function(listofruns, AggrReg="H12"){


  scenNames <- SSP_Scen <- Tech_Scen <- c()
  demand_km <- demand_ej <- vintcomp <- newcomp <- shares <- pref <- mj_km_data <- loadFactor <- annual_mileage <- annual_sale <- prices <- logit_exp <- list()
  count_scen <- 2
  yrs <- c(1990,seq(2005, 2060, 5), seq(2070, 2100, 10))


  ## ---- Load GDP and POP ----
    GDP_country = {
      x <- calcOutput("GDP", aggregate = F)
      getSets(x)[1] <- "ISO3"
      getSets(x)[2] <- "Year"
      x
    }
    POP_country = {
      x <- calcOutput("Population", aggregate = F)
      getSets(x)[1] <- "iso2c"
      x
    }

  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  GDP_country[, variable := paste0(sub("gdp_", "", variable))]
  setnames(GDP_country, c("ISO3", "variable", "value", "year"), c("CountryCode", "scenario", "weight", "period"))
  POP <- as.data.table(POP_country)
  POP[, year := as.numeric(gsub("y", "", year))]
  POP[, variable := paste0(sub("pop_", "", variable))]
  setnames(POP, c("iso2c", "variable", "year"), c("CountryCode", "scenario", "period"))

  ## ---- Create mappings ----
  # Region Mapping
  if (AggrReg == "H12") {
    RegionMappingH12 <- fread(system.file("extdata", "regionmappingH12.csv", package = "edgeTransport"))
    Regionmapping_21_EU11 <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
    setnames(Regionmapping_21_EU11, "RegionCode", "region")
    Regionmapping <- copy(Regionmapping_21_EU11)
    Regionmapping[, missingH12 := ifelse(missingH12 == "rest", region, missingH12)]
    Regionmapping <- Regionmapping[, -c("X", "CountryCode")]
    Regionmapping <- Regionmapping[!duplicated(Regionmapping)]
    setnames(Regionmapping, c("region", "missingH12"), c("manycol", "fewcol"))
    Regionmapping_Tot <- copy(Regionmapping[, -c("manycol")])
    setnames(Regionmapping_Tot, "fewcol", "manycol")
    Regionmapping_Tot[, fewcol := "GLO"]
    Regionmapping_Tot <- Regionmapping_Tot[!duplicated(Regionmapping_Tot)]
    GDP_21 <- aggregate_dt(GDP_country, Regionmapping_21_EU11[, -c("X", "missingH12")], yearcol = "period", fewcol = "region", manycol = "CountryCode", datacols = "scenario", valuecol = "weight")
    setnames(GDP_21, "region", "manycol")
    GDP_13 <- aggregate_dt(GDP_21, Regionmapping, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("scenario"), valuecol = "weight")
    setnames(GDP_13, "fewcol", "manycol")
    GDP_13_glo <- aggregate_dt(GDP_13, Regionmapping_Tot, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("scenario"), valuecol = "weight")
    setnames(GDP_13_glo, "fewcol", "manycol")
    GDP_13 <- rbind(GDP_13, GDP_13_glo)
    POP_21 <- aggregate_dt(POP, Regionmapping_21_EU11[, -c("X", "missingH12")], yearcol = "period", fewcol = "region", manycol = "CountryCode", datacols = "scenario", valuecol = "value")
    setnames(POP_21, "region", "manycol")
    POP_13 <- aggregate_dt(POP_21, Regionmapping, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("scenario"), valuecol = "value")
    setnames(POP_13, "fewcol", "manycol")
    POP_13_glo <- aggregate_dt(POP_13, Regionmapping_Tot, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("scenario"), valuecol = "value")
    setnames(POP_13_glo, "fewcol", "region")
    setnames(POP_13, "manycol", "region")
    POP_13 <- rbind(POP_13, POP_13_glo)
  }

  if (AggrReg == "EU21") {
    Regionmapping <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
  }

  # Mapping for vehicle type aggregation
  Mapp_Aggr_vehtype <- data.table(
    gran_vehtype = c("Compact Car", "Large Car", "Large Car and SUV", "Light Truck and SUV", "Midsize Car", "Mini Car", "Subcompact Car", "Van", "International Aviation_tmp_vehicletype",
                     "Domestic Ship_tmp_vehicletype", "Freight Rail_tmp_vehicletype", "Truck (0-3.5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)", "Truck (7.5t)", "Domestic Aviation_tmp_vehicletype",
                     "HSR_tmp_vehicletype", "Passenger Rail_tmp_vehicletype", "Bus_tmp_vehicletype", "Moped", "Motorcycle (50-250cc)", "Motorcycle (>250cc)", "International Ship_tmp_vehicletype", "Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),
    aggr_vehtype = c("Small Cars", "Large Cars", "Large Cars", "Large Cars", "Large Cars", "Small Cars", "Small Cars", "Large Cars", "Aircraft international",
                     "Ships domestic", "Freight Trains", "Trucks", "Trucks", "Trucks", "Trucks", "Trucks", "Aircraft domestic",
                     "Passenger HSR", "Passenger Trains", "Busses", "Motorbikes", "Motorbikes", "Motorbikes", "Ships international", "Cycling", "Walking"),
    international = c("no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers",
                      "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "no bunkers", "bunkers", "no bunkers", "no bunkers")
  )

  Mapp_Av_LDV <- Mapp_Aggr_vehtype[gran_vehtype %in% c("Compact Car", "Large Car", "Large Car and SUV", "Midsize Car", "Mini Car", "Subcompact Car", "Van", "Light Truck and SUV"), c("gran_vehtype", "aggr_vehtype")][, aggr_vehtype := "av_veh"]


  # Mapping efficiencies for useful energy

  Mapp_UE <- data.table(
    technology = c("FCEV", "BEV", "Electric", "Liquids", "Hydrogen"),
    UE_efficiency = c(0.36, 0.64, 0.8, 0.23, 0.25))

  ## ---- Load scenario data ----

  level2path <- function(folder, fname) {
    path <- file.path(folder, "level_2", fname)
  }

  level1path <- function(folder, fname) {
    path <- file.path(folder, "level_1", fname)
  }

  level0path <- function(folder, fname) {
    path <- file.path(folder, "level_0", fname)
  }

  for (i in 1:length(listofruns)) {
    # Load random csv file to get REMIND scenario and EDGE-T techScen
    tmp <- fread(level2path(listofruns[[i]], "loadFactor.csv"))
    SSP_Scen[i] <- unique(tmp$GDP_scenario)
    Tech_Scen[i] <- unique(tmp$EDGE_scenario)

    candidate <- paste0(sub("gdp_","",SSP_Scen[i]),"-",Tech_Scen[i])
    # Create unique Scenario names for plotting
    if (candidate %in% scenNames) {
      scenNames[i] <- paste0(candidate, "_", count_scen)
      count_scen <- count_scen + 1
    }else{
      scenNames[i] <- candidate
    }

    # load input data from EDGE runs for comparison
    demand_km[[i]] <- readRDS(level2path(listofruns[i], "demandF_plot_pkm.RDS")) ## detailed energy services demand, million pkm
    demand_km[[i]]$scenario <- scenNames[i]
    demand_ej[[i]] <- readRDS(level2path(listofruns[i], "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
    demand_ej[[i]]$scenario <- scenNames[i]
    vintcomp[[i]] <- readRDS(level2path(listofruns[[i]], "vintcomp.RDS"))
    vintcomp[[i]]$scenario <- scenNames[i]
    newcomp[[i]] <- readRDS(level2path(listofruns[[i]], "newcomp.RDS"))
    newcomp[[i]]$scenario <- scenNames[i]
    shares[[i]] <- readRDS(level2path(listofruns[[i]], "shares.RDS"))
    shares[[i]]$scenario <- scenNames[i]
    logit_data <- readRDS(level2path(listofruns[[i]], "logit_data.RDS"))
    prices[[i]] <- logit_data$share_list
    prices[[i]]$scenario <- scenNames[i]
    pref[[i]] <- logit_data$pref_data
    pref[[i]]$scenario <- scenNames[i]
    logit_exp_data <- readRDS(level0path(listofruns[[i]], "logit_exp.RDS"))
    logit_exp[[i]] <- logit_exp_data$logit_output
    logit_exp[[i]]$scenario <- scenNames[i]
    mj_km_data[[i]] <- readRDS(level2path(listofruns[[i]], "mj_km_data.RDS"))
    mj_km_data[[i]]$scenario <- scenNames[i]
    loadFactor[[i]] <- readRDS(level2path(listofruns[[i]], "loadFactor.RDS"))
    loadFactor[[i]]$scenario <- scenNames[i]
    annual_mileage[[i]] <- readRDS(level2path(listofruns[[i]], "annual_mileage.RDS"))
    annual_mileage[[i]]$scenario <- scenNames[i]
    annual_sale[[i]] <- readRDS(level2path(listofruns[[i]], "annual_sales.RDS"))
    annual_sale[[i]]$scenario <- scenNames[i]
  }


  # Clean SSP_Scen
  SSP_Scen <- sub("gdp_", "", SSP_Scen)
  SSP_Scen <- sub("EU", "", SSP_Scen)
  ## ---- Extend POP and GDP for all Scenarios ----

  GDP_13_scen <- list()
  POP_13_scen <- list()
  GDP_21_scen <- list()
  POP_21_scen <- list()


  for (i in 1:length(SSP_Scen)) {
    GDP_13_tmp <- GDP_13[scenario == SSP_Scen[i]]
    GDP_13_tmp[, scenario := scenNames[i]]
    GDP_13_scen <- rbind(GDP_13_scen, GDP_13_tmp)
    POP_13_tmp <- POP_13[scenario == SSP_Scen[i]]
    POP_13_tmp[, scenario := scenNames[i]]
    POP_13_scen <- rbind(POP_13_scen, POP_13_tmp)
    GDP_21_tmp <- GDP_21[scenario == SSP_Scen[i]]
    GDP_21_tmp[, scenario := scenNames[i]]
    GDP_21_scen <- rbind(GDP_21_scen, GDP_21_tmp)
    POP_21_tmp <- POP_21[scenario == SSP_Scen[i]]
    POP_21_tmp[, scenario := scenNames[i]]
    POP_21_scen <- rbind(POP_21_scen, POP_21_tmp)
  }

  ## ---- Preprocess ES weights for aggregation----

  ## ---- Preprocess ES weights for aggregation----
  # Rawdata
  dem_pkm <- do.call(rbind.data.frame, demand_km)
  setkey(dem_pkm, NULL)
  loadFactor <- do.call(rbind.data.frame, loadFactor)
  setkey(loadFactor, NULL)
  dem_ej <- do.call(rbind.data.frame, demand_ej)
  setkey(dem_ej, NULL)
  EInt_mj_km <- do.call(rbind.data.frame, mj_km_data)
  setkey(mj_km_data, NULL)

  # Prepare ES share weights
  weight_dem_pkm <- copy(dem_pkm)

  # rename columns for mip
  setnames(weight_dem_pkm, c("demand_F", "year"), c("weight", "period"))
  weight_dem_pkm[, weight := weight / 1000][sector %in% c("trn_pass", "trn_aviation_intl"), unit := "bn pkm/yr"][sector %in% c("trn_freight", "trn_shipping_intl"), unit := "bn tkm/yr"]

  plot_loadFactor <- copy(loadFactor)
  setnames(plot_loadFactor, c("year"), c("period"))
  plot_loadFactor[is.na(loadFactor), loadFactor := 1]
  # Assume same loadFactors for Hydrogen planes as for liquid planes
  tmp_new <- plot_loadFactor[vehicle_type == "Domestic Aviation_tmp_vehicletype" & technology == "Liquids"]
  tmp_new[, technology := "Hydrogen"]
  plot_loadFactor <- plot_loadFactor[!(vehicle_type == "Domestic Aviation_tmp_vehicletype" & technology == "Hydrogen")]
  plot_loadFactor <- rbind(plot_loadFactor, tmp_new)
  plot_loadFactor <- plot_loadFactor[period <= 2100]

  weight_dem_km_21 <- merge(weight_dem_pkm[, -c("unit")], plot_loadFactor, by = c("period", "region", "vehicle_type", "sector", "scenario", "subsector_L1", "subsector_L2", "subsector_L3", "technology"), all.x = TRUE)
  weight_dem_km_21[, weight := weight / loadFactor][, unit := "bn vehkm/yr"][, loadFactor := NULL][, subsector_L1 := NULL][, subsector_L2 := NULL][, subsector_L3 := NULL]
  setnames(weight_dem_km_21, c("region"), c("manycol"))

  # Aggregate region
  weight_dem_km_12 <- aggregate_dt(weight_dem_km_21, Regionmapping, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("scenario", "sector", "unit", "vehicle_type"), valuecol = "weight")
  setnames(weight_dem_km_12, c("fewcol"), c("manycol"))
  setnames(weight_dem_pkm, c("region"), c("manycol"))

  ## ---- Preprocess plot_data ----
  # Prepare Line Plot data
  plot_dem_ej <- copy(dem_ej)

  # rename columns for mip
  setnames(plot_dem_ej, c("demand_EJ", "year", "region"), c("value", "period", "manycol"))
  plot_dem_ej <- plot_dem_ej[, c("value", "period", "manycol", "scenario", "sector", "technology", "vehicle_type")]
  plot_dem_ej[, unit := "EJ/yr"]

  # Aggregate regions
  plot_dem_ej <- aggregate_dt(plot_dem_ej, Regionmapping, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_dem_ej, "fewcol", "manycol")
  plot_dem_ej_glo <- aggregate_dt(plot_dem_ej, Regionmapping_Tot, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_dem_ej, "manycol", "region")
  setnames(plot_dem_ej_glo, "fewcol", "region")
  plot_dem_ej <- rbind(plot_dem_ej, plot_dem_ej_glo)
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]

  # Group vehicle types for plotting
  plot_dem_ej <- merge(plot_dem_ej, Mapp_Aggr_vehtype, by.x = "vehicle_type", by.y = "gran_vehtype")
  plot_dem_ej <- plot_dem_ej[, -c("vehicle_type")]
  setnames(plot_dem_ej, "aggr_vehtype", "vehicle_type")

  # Aggregate data
  plot_dem_ej <- plot_dem_ej[, .(value = sum(value)), by = c("period", "region", "scenario", "sector", "technology", "vehicle_type", "unit", "international")]

  # FE|Transport
  FE_Transport <- copy(plot_dem_ej)
  FE_Transport <- FE_Transport[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport"]
  # FE|Transport w/o bunkers
  FE_Transport_wobunk <- copy(plot_dem_ej)
  FE_Transport_wobunk <- FE_Transport_wobunk[international == "no bunkers"]
  FE_Transport_wobunk <- FE_Transport_wobunk[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport w/o bunkers"]
  # FE|Transport|Pass
  FE_Transport_Pass <- copy(plot_dem_ej)
  FE_Transport_Pass <- FE_Transport_Pass[sector %in% c("trn_pass", "trn_aviation_intl")]
  FE_Transport_Pass <- FE_Transport_Pass[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass"]
  # FE|Transport|Pass|Rail
  FE_Transport_Pass_Rail <- copy(plot_dem_ej)
  FE_Transport_Pass_Rail <- FE_Transport_Pass_Rail[vehicle_type == "Passenger Trains"]
  FE_Transport_Pass_Rail <- FE_Transport_Pass_Rail[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Rail|non-HSR"]
  # FE|Transport|Pass|Rail
  FE_Transport_Pass_HSR <- copy(plot_dem_ej)
  FE_Transport_Pass_HSR <- FE_Transport_Pass_HSR[vehicle_type == "Passenger HSR"]
  FE_Transport_Pass_HSR <- FE_Transport_Pass_HSR[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Rail|HSR"]
  # FE|Transport|Pass|Rail
  FE_Transport_Pass_RailHSR <- copy(plot_dem_ej)
  FE_Transport_Pass_RailHSR <- FE_Transport_Pass_RailHSR[vehicle_type %in% c("Passenger HSR","Passenger Trains")]
  FE_Transport_Pass_RailHSR <- FE_Transport_Pass_RailHSR[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Rail"]
  # FE|Transport|Pass|Road
  FE_Transport_Pass_Road <- copy(plot_dem_ej)
  FE_Transport_Pass_Road <- FE_Transport_Pass_Road[vehicle_type %in% c("Busses", "Small Cars", "Large Cars", "Motorbikes")]
  FE_Transport_Pass_Road <- FE_Transport_Pass_Road[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Road"]
  # FE|Transport|Pass|Road|Bus
  FE_Transport_Pass_Road_Bus <- copy(plot_dem_ej)
  FE_Transport_Pass_Road_Bus <- FE_Transport_Pass_Road_Bus[vehicle_type %in% c("Busses")]
  FE_Transport_Pass_Road_Bus <- FE_Transport_Pass_Road_Bus[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Road|Bus"]
  # FE|Transport|Pass|Road|LDV
  FE_Transport_Pass_Road_LDV <- copy(plot_dem_ej)
  FE_Transport_Pass_Road_LDV <- FE_Transport_Pass_Road_LDV[vehicle_type %in% c("Motorbikes", "Small Cars", "Large Cars")]
  FE_Transport_Pass_Road_LDV <- FE_Transport_Pass_Road_LDV[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Road|LDV"]
  # FE|Transport|Freight
  FE_Transport_Freight <- copy(plot_dem_ej)
  FE_Transport_Freight <- FE_Transport_Freight[sector %in% c("trn_freight", "trn_shipping_intl")]
  FE_Transport_Freight <- FE_Transport_Freight[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Freight"]
  # FE|Transport|Freight|Navigation
  FE_Transport_Freight_Nav <- copy(plot_dem_ej)
  FE_Transport_Freight_Nav <- FE_Transport_Freight_Nav[vehicle_type %in% c("Ships domestic", "Ships international")]
  FE_Transport_Freight_Nav <- FE_Transport_Freight_Nav[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Freight|Navigation"]
  # FE|Transport|Freight|Rail
  FE_Transport_Freight_Rail <- copy(plot_dem_ej)
  FE_Transport_Freight_Rail <- FE_Transport_Freight_Rail[vehicle_type %in% c("Freight Trains")]
  FE_Transport_Freight_Rail <- FE_Transport_Freight_Rail[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Freight|Rail"]
  # FE|Transport|Freight|Road
  FE_Transport_Freight_Road <- copy(plot_dem_ej)
  FE_Transport_Freight_Road <- FE_Transport_Freight_Road[vehicle_type %in% c("Trucks")]
  FE_Transport_Freight_Road <- FE_Transport_Freight_Road[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Freight|Road"]


  # ES
  plot_dem_pkm <- copy(dem_pkm)
  setkey(plot_dem_pkm, NULL)

  # rename columns for mip
  setnames(plot_dem_pkm, c("demand_F", "year", "region"), c("value", "period", "manycol"))
  plot_dem_pkm <- plot_dem_pkm[, c("value", "period", "manycol", "scenario", "sector", "technology", "vehicle_type")]
  # convert unit drom million pkm/yr to billion pkm/yr
  plot_dem_pkm[, value := value / 1000]
  plot_dem_pkm[, unit := "bn pkm/yr"]

  # Aggregate regions
  plot_dem_pkm <- aggregate_dt(plot_dem_pkm, Regionmapping, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_dem_pkm, "fewcol", "manycol")
  plot_dem_pkm_glo <- aggregate_dt(plot_dem_pkm, Regionmapping_Tot, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_dem_pkm, "manycol", "region")
  setnames(plot_dem_pkm_glo, "fewcol", "region")
  plot_dem_pkm <- rbind(plot_dem_pkm, plot_dem_pkm_glo)
  plot_dem_pkm <- plot_dem_pkm[!duplicated(plot_dem_pkm)]

  # Group vehicle types for plotting
  plot_dem_pkm <- merge(plot_dem_pkm, Mapp_Aggr_vehtype, by.x = "vehicle_type", by.y = "gran_vehtype")
  plot_dem_pkm <- plot_dem_pkm[, -c("vehicle_type")]
  setnames(plot_dem_pkm, "aggr_vehtype", "vehicle_type")

  # Aggregate data
  plot_dem_pkm <- plot_dem_pkm[, .(value = sum(value)), by = c("period", "region", "scenario", "sector", "technology", "vehicle_type", "unit", "international")]

  # ES|Transport
  ES_Transport <- copy(plot_dem_pkm)
  ES_Transport <- ES_Transport[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport"]
  # ES|Transport w/o bunkers
  ES_Transport_wobunk <- copy(plot_dem_pkm)
  ES_Transport_wobunk <- ES_Transport_wobunk[international == "no bunkers"]
  ES_Transport_wobunk <- ES_Transport_wobunk[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport w/o bunkers"]
  # ES|Transport|Pass
  ES_Transport_Pass <- copy(plot_dem_pkm)
  ES_Transport_Pass <- ES_Transport_Pass[sector %in% c("trn_pass", "trn_aviation_intl")]
  ES_Transport_Pass <- ES_Transport_Pass[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass"]
  # ES|Transport|Pass|Aviation|International
  ES_Transport_Pass_Av_intl <- copy(plot_dem_pkm)
  ES_Transport_Pass_Av_intl <- ES_Transport_Pass_Av_intl[sector %in% c("trn_aviation_intl")]
  ES_Transport_Pass_Av_intl <- ES_Transport_Pass_Av_intl[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Aviation|International"]
  # ES|Transport|Pass|Aviation|Domestic
  ES_Transport_Pass_Av_dom <- copy(plot_dem_pkm)
  ES_Transport_Pass_Av_dom <- ES_Transport_Pass_Av_dom[vehicle_type == "Aircraft domestic"]
  ES_Transport_Pass_Av_dom <- ES_Transport_Pass_Av_dom[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Aviation|Domestic"]
  # ES|Transport|Pass|Road|Non-Motorized
  ES_Transport_Pass_nonmot <- copy(plot_dem_pkm)
  ES_Transport_Pass_nonmot <- ES_Transport_Pass_nonmot[vehicle_type %in% c("Cycling", "Walking")]
  ES_Transport_Pass_nonmot <- ES_Transport_Pass_nonmot[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable :="ES|Transport|Pass|Road|Non-Motorized"]

  # ES|Transport|Pass|Rail|non-HSR
  ES_Transport_Pass_Rail <- copy(plot_dem_pkm)
  ES_Transport_Pass_Rail <- ES_Transport_Pass_Rail[vehicle_type == "Passenger Trains"]
  ES_Transport_Pass_Rail <- ES_Transport_Pass_Rail[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Rail|non-HSR"]
  # ES|Transport|Pass|Rail|HSR
  ES_Transport_Pass_HSR <- copy(plot_dem_pkm)
  ES_Transport_Pass_HSR <- ES_Transport_Pass_HSR[vehicle_type == "Passenger HSR"]
  ES_Transport_Pass_HSR <- ES_Transport_Pass_HSR[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Rail|HSR"]
  # ES|Transport|Pass|Rail
  ES_Transport_Pass_RailHSR <- copy(plot_dem_pkm)
  ES_Transport_Pass_RailHSR <- ES_Transport_Pass_RailHSR[vehicle_type %in% c("Passenger HSR","Passenger Trains")]
  ES_Transport_Pass_RailHSR <- ES_Transport_Pass_RailHSR[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Rail"]
  # ES|Transport|Pass|Road
  ES_Transport_Pass_Road <- copy(plot_dem_pkm)
  ES_Transport_Pass_Road <- ES_Transport_Pass_Road[vehicle_type %in% c("Busses", "Small Cars", "Large Cars", "Motorbikes")]
  ES_Transport_Pass_Road <- ES_Transport_Pass_Road[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Road"]
  # ES|Transport|Pass|Road|Bus
  ES_Transport_Pass_Road_Bus <- copy(plot_dem_pkm)
  ES_Transport_Pass_Road_Bus <- ES_Transport_Pass_Road_Bus[vehicle_type %in% c("Busses")]
  ES_Transport_Pass_Road_Bus <- ES_Transport_Pass_Road_Bus[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Road|Bus"]
  # ES|Transport|Pass|Road|LDV
  ES_Transport_Pass_Road_LDV <- copy(plot_dem_pkm)
  ES_Transport_Pass_Road_LDV <- ES_Transport_Pass_Road_LDV[vehicle_type %in% c("Motorbikes", "Small Cars", "Large Cars")]
  ES_Transport_Pass_Road_LDV <- ES_Transport_Pass_Road_LDV[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Road|LDV"]
  # ES|Transport|Freight
  ES_Transport_Freight <- copy(plot_dem_pkm)
  ES_Transport_Freight <- ES_Transport_Freight[sector %in% c("trn_freight", "trn_shipping_intl")]
  ES_Transport_Freight <- ES_Transport_Freight[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Freight"]
  # ES|Transport|Freight|Navigation
  ES_Transport_Freight_Nav <- copy(plot_dem_pkm)
  ES_Transport_Freight_Nav <- ES_Transport_Freight_Nav[vehicle_type %in% c("Ships domestic")]
  ES_Transport_Freight_Nav <- ES_Transport_Freight_Nav[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Freight|Navigation"]
  # ES|Transport|Freight|Shipping International
  ES_Transport_Freight_Shipintl <- copy(plot_dem_pkm)
  ES_Transport_Freight_Shipintl <- ES_Transport_Freight_Shipintl[vehicle_type %in% c("Ships international")]
  ES_Transport_Freight_Shipintl <- ES_Transport_Freight_Shipintl[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Freight|International Shipping"]


  # ES|Transport|Freight|Rail
  ES_Transport_Freight_Rail <- copy(plot_dem_pkm)
  ES_Transport_Freight_Rail <- ES_Transport_Freight_Rail[vehicle_type %in% c("Freight Trains")]
  ES_Transport_Freight_Rail <- ES_Transport_Freight_Rail[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Freight|Rail"]
  # ES|Transport|Freight|Road
  ES_Transport_Freight_Road <- copy(plot_dem_pkm)
  ES_Transport_Freight_Road <- ES_Transport_Freight_Road[vehicle_type %in% c("Trucks")]
  ES_Transport_Freight_Road <- ES_Transport_Freight_Road[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Freight|Road"]




  # EInt
  plot_EInt <- copy(EInt_mj_km)
  setkey(plot_EInt, NULL)
  # rename columns for mip
  setnames(plot_EInt, c("MJ_km", "year", "region"), c("value", "period", "manycol"))
  plot_EInt <- plot_EInt [, c("value", "period", "manycol", "scenario", "sector", "technology", "vehicle_type")]
  plot_EInt[, unit := "MJ/km"]

  # Aggregate regions
  plot_EInt <- aggregate_dt(plot_EInt, Regionmapping, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("technology", "scenario", "sector", "vehicle_type"), valuecol = "value", weights = GDP_21_scen[period %in% unique(plot_EInt$period)])
  setnames(plot_EInt, "fewcol", "manycol")
  plot_EInt_glo <- aggregate_dt(plot_EInt, Regionmapping_Tot, fewcol = "fewcol", yearcol = "period", manycol = "manycol", datacols = c("technology", "scenario", "sector", "vehicle_type"), valuecol = "value", weights = GDP_13_scen[period %in% unique(plot_EInt$period)])
  setnames(plot_EInt, "manycol", "region")
  setnames(plot_EInt_glo, "fewcol", "region")
  plot_EInt <- rbind(plot_EInt, plot_EInt_glo)
  plot_EInt <- plot_EInt[!duplicated(plot_EInt)]

  plot_EInt_MidsizeCar_BEV <- plot_EInt[vehicle_type == "Midsize Car" & technology == "BEV", c("period", "region", "scenario", "value", "unit")][, variable := "EInt|Transport|Pass|Road|LDV|MidsizeCar|BEV"]
  plot_EInt_Bus_BEV <- plot_EInt[vehicle_type == "Bus_tmp_vehicletype" & technology == "Electric", c("period", "region", "scenario", "value", "unit")][, variable := "EInt|Transport|Pass|Road|Bus|BEV"]
  plot_EInt_MidsizeCar_ICE <- plot_EInt[vehicle_type == "Midsize Car" & technology == "Liquids", c("period", "region", "scenario", "value", "unit")][, variable := "EInt|Transport|Pass|Road|LDV|MidsizeCar|ICE"]
  plot_EInt_Bus_ICE <- plot_EInt[vehicle_type == "Bus_tmp_vehicletype" & technology == "Liquids", c("period", "region", "scenario", "value", "unit")][, variable := "EInt|Transport|Pass|Road|Bus|ICE"]


  LinePlot_data <- rbind(FE_Transport, FE_Transport_wobunk, FE_Transport_Pass, FE_Transport_Pass_Rail,FE_Transport_Pass_HSR,FE_Transport_Pass_RailHSR,  FE_Transport_Pass_Road, FE_Transport_Pass_Road_Bus, FE_Transport_Pass_Road_LDV, FE_Transport_Freight, FE_Transport_Freight_Nav, FE_Transport_Freight_Rail, FE_Transport_Freight_Road,
                         ES_Transport, ES_Transport_wobunk, ES_Transport_Pass, ES_Transport_Pass_Av_dom, ES_Transport_Pass_Av_intl, ES_Transport_Pass_Rail,ES_Transport_Pass_RailHSR, ES_Transport_Pass_HSR,ES_Transport_Pass_nonmot,ES_Transport_Freight_Shipintl, ES_Transport_Pass_Road, ES_Transport_Pass_Road_Bus, ES_Transport_Pass_Road_LDV, ES_Transport_Freight, ES_Transport_Freight_Nav, ES_Transport_Freight_Rail, ES_Transport_Freight_Road,
                         plot_EInt_MidsizeCar_BEV, plot_EInt_Bus_BEV, plot_EInt_MidsizeCar_ICE, plot_EInt_Bus_ICE
  )

  LinePlot_data[, model := "EDGE-T"]

  #Prepare Energy Service Shares with bunkers

  vars <- c(
    "ES|Transport|Pass|Aviation|Domestic",
    "ES|Transport|Pass|Aviation|International",
    "ES|Transport|Pass|Rail|non-HSR",
    "ES|Transport|Pass|Rail|HSR",
    "ES|Transport|Pass|Road|Bus",
    "ES|Transport|Pass|Road|LDV",
    "ES|Transport|Pass|Road|Non-Motorized"
  )


  ES_shares_Pass_wbunk <- LinePlot_data[variable %in% vars][,unit:=NULL]
  if (length(unique(ES_shares_Pass_wbunk$variable)) < 7) print("Error in Pass ES Shares with bunkers")
  ES_shares_Pass_wbunk[, tot:= sum(value), by= c("period","region","scenario","model")]
  ES_shares_Pass_wbunk[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
  ES_shares_Pass_wbunk <- ES_shares_Pass_wbunk[,variable:=paste0(variable,"|Share")]


  #Prepare Energy Service Shares without bunkers

  vars <- c(
    "ES|Transport|Pass|Aviation|Domestic",
    "ES|Transport|Pass|Rail|non-HSR",
    "ES|Transport|Pass|Rail|HSR",
    "ES|Transport|Pass|Road|Bus",
    "ES|Transport|Pass|Road|LDV",
    "ES|Transport|Pass|Road|Non-Motorized"
  )


  ES_shares_Pass_wobunk <- LinePlot_data[variable %in% vars][,unit:=NULL]
  if (length(unique(ES_shares_Pass_wobunk$variable)) < 6) print("Error in Freight ES Shares without bunkers")
  ES_shares_Pass_wobunk[, tot:= sum(value), by= c("period","region","scenario","model")]
  ES_shares_Pass_wobunk[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
  ES_shares_Pass_wobunk <- ES_shares_Pass_wobunk[,variable:=paste0(variable,"|Share w/o bunkers")]

  #Prepare Energy Service Shares with bunkers
  vars <- c(
    "ES|Transport|Freight|Road",
    "ES|Transport|Freight|Rail",
    "ES|Transport|Freight|International Shipping",
    "ES|Transport|Freight|Navigation"
  )

  ES_shares_Freight_wbunk <- LinePlot_data[variable %in% vars][,unit:=NULL]
  if (length(unique(ES_shares_Freight_wbunk$variable)) < 4) print("Error in Freight ES Shares with bunkers")
  ES_shares_Freight_wbunk[, tot:= sum(value), by= c("period","region","scenario","model")]
  ES_shares_Freight_wbunk[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
  ES_shares_Freight_wbunk <- ES_shares_Freight_wbunk[,variable:=paste0(variable,"|Share")]

  #Prepare Energy Service Shares without bunkers
  vars <- c(
    "ES|Transport|Freight|Road",
    "ES|Transport|Freight|Rail",
    "ES|Transport|Freight|Navigation"
  )

  ES_shares_Freight_wobunk <- LinePlot_data[variable %in% vars][,unit:=NULL]
  if (length(unique(ES_shares_Freight_wobunk$variable)) < 3) print("Error in Pass ES Shares without bunkers")
  ES_shares_Freight_wobunk[, tot:= sum(value), by= c("period","region","scenario","model")]
  ES_shares_Freight_wobunk[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
  ES_shares_Freight_wobunk <- ES_shares_Freight_wobunk[,variable:=paste0(variable,"|Share w/o bunkers")]



  LogitCostplotdata <- function(priceData,prefData,logitExp,groupValue,weight, yrs, Regionmapping){
 
    all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                      "subsector_L3", "sector")
    

    # change variable names for mip
    setnames(priceData, c("year"), c("period"))
    setnames(prefData, c("year"), c("period"))

    # Use ES as share weights
    # Aggregate data
    weight <- weight[, .(weight = sum(weight)), by = c("period", "manycol", "scenario", "unit", all_subsectors[
      seq(match(groupValue, all_subsectors) ,
          length(all_subsectors), 1)])]

    prefData <- prefData[period %in% yrs]
    priceData<-  priceData[period %in% yrs]


  # Calculate Inconvenience Cost from share Weight
    prefData <- merge(prefData, logitExp, all.y = TRUE)


    price_tot <- priceData[, c("period", "region", "scenario", "tot_price", all_subsectors[
      seq(match(groupValue, all_subsectors) ,
          length(all_subsectors), 1)]), with = FALSE]
    prefData <- merge(prefData, price_tot, by = c("period", "region", "scenario", all_subsectors[
            seq(match(groupValue, all_subsectors) ,
                length(all_subsectors), 1)]))

    prefData[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
 
    priceData <- priceData[, c("period", "region", "scenario","tot_VOT_price","fuel_price_pkm","non_fuel_price", all_subsectors[
      seq(match(groupValue, all_subsectors) ,
          length(all_subsectors), 1)]), with = FALSE]
    #Check for zero shareweights that still have ES demand
    ES_check <- weight[,!c("unit")]
    setnames(ES_check,"manycol","region")
    ES_check <- merge(prefData,ES_check, all.x = TRUE)
    ES_check[,check:=value*weight]

    if (nrow(ES_check[is.infinite(ES_check$check)]) > 0){
      print(paste0("There are zero shareweights with ES demand that are deleted: ",prefData[is.infinite(ES_check$check)]))
      prefData <- prefData[!is.infinite(ES_check$check)]
    }
    #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero
    prefData <- prefData[is.infinite(prefData$value),value:=0]
    prefData <- prefData[, c("region", "period", all_subsectors[
      seq(match(groupValue, all_subsectors) ,
          length(all_subsectors), 1)], "scenario", "value"), with = FALSE][, variable := "Inconvenience cost"]

    priceData <- melt(priceData, id.vars = c("scenario", "region", "period", all_subsectors[
      seq(match(groupValue, all_subsectors) ,
          length(all_subsectors), 1)]))

    setnames(prefData, "region", "manycol")
    setnames(priceData, "region", "manycol")

    priceData <- aggregate_dt(priceData,
                              Regionmapping,
                              manycol = "manycol",
                              fewcol = "fewcol",
                              yearcol = "period",
                              datacols = c(all_subsectors[
                                seq(match(groupValue, all_subsectors) ,
                                    length(all_subsectors), 1)], "variable", "scenario"),
                              weights = weight[period %in% unique(priceData$period)])
    setnames(priceData, c("fewcol"), c("region"))
    prefData <- aggregate_dt(prefData,
                            Regionmapping,
                            manycol = "manycol",
                            fewcol = "fewcol",
                            yearcol = "period",
                            datacols = c(all_subsectors[
                              seq(match(groupValue, all_subsectors) ,
                                  length(all_subsectors), 1)], "variable", "scenario"),
                            weights = weight[period %in% unique(prefData$period)])
    setnames(prefData, c("fewcol"), c("region"))

    data <- rbind(prefData,priceData)
    data[, unit := "$2005/pkm"]

  return(data)
  }


  Prices_S3S <- Pref_S3S <- logit_exp_S3S <- list()

  # Prepare logit price data S3S
  for (i in 1:length(listofruns)) {
    Prices_S3S[[i]] <- copy(prices[[i]]$S3S_shares)
    Prices_S3S[[i]]$scenario <- copy(prices[[i]]$scenario)
  }

  Prices_S3S <- do.call(rbind.data.frame, Prices_S3S)
  setkey(Prices_S3S, NULL)

  # Prepare logit pref data S3S
  for (i in 1:length(listofruns)) {
    Pref_S3S[[i]] <- copy(pref[[i]]$S3S_final_pref)
    Pref_S3S[[i]]$scenario <- copy(pref[[i]]$scenario)
  }

  Pref_S3S <- do.call(rbind.data.frame, Pref_S3S)
  setkey(Pref_S3S, NULL)
  # Prepare logit exponents S3S
  for (i in 1:length(listofruns)) {
    logit_exp_S3S[[i]] <- copy(logit_exp[[i]]$logit_exponent_S3S)
    logit_exp_S3S[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }

  logit_exp_S3S <- do.call(rbind.data.frame, logit_exp_S3S)
  setkey(logit_exp_S3S, NULL)
  Prices_S3S[subsector_L3 %in% c("Cycle","Walk"), tot_VOT_price:=tot_price]

  Prices_S3S <- LogitCostplotdata(priceData=Prices_S3S,prefData=Pref_S3S,logitExp=logit_exp_S3S,groupValue="subsector_L3",weight=weight_dem_pkm ,yrs,Regionmapping)

  Prices_S3S <- Prices_S3S[,c("variable", "period", "scenario", "region", "value","subsector_L3","unit")]
  setnames(Prices_S3S,"subsector_L3","groupvalue")


  Prices_S2S3 <- Pref_S2S3 <- logit_exp_S2S3 <- list()

  # Prepare logit price data S2S3
  for (i in 1:length(listofruns)) {
    Prices_S2S3[[i]] <- copy(prices[[i]]$S2S3_shares)
    Prices_S2S3[[i]]$scenario <- copy(prices[[i]]$scenario)
  }

  Prices_S2S3 <- do.call(rbind.data.frame, Prices_S2S3)
  setkey(Prices_S2S3, NULL)


  # Prepare logit pref data S2S3
  for (i in 1:length(listofruns)) {
    Pref_S2S3[[i]] <- copy(pref[[i]]$S2S3_final_pref)
    Pref_S2S3[[i]]$scenario <- copy(pref[[i]]$scenario)
  }

  Pref_S2S3 <- do.call(rbind.data.frame, Pref_S2S3)
  setkey(Pref_S2S3, NULL)
  # Prepare logit exponents S2S3
  for (i in 1:length(listofruns)) {
    logit_exp_S2S3[[i]] <- copy(logit_exp[[i]]$logit_exponent_S2S3)
    logit_exp_S2S3[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }

  logit_exp_S2S3 <- do.call(rbind.data.frame, logit_exp_S2S3)
  setkey(logit_exp_S2S3, NULL)

  Prices_S2S3 <- LogitCostplotdata(priceData=Prices_S2S3,prefData=Pref_S2S3,logitExp=logit_exp_S2S3,groupValue="subsector_L2",weight=weight_dem_pkm , yrs,Regionmapping)
  Prices_S2S3 <- Prices_S2S3[,c("variable", "period", "scenario", "region", "value","subsector_L2","unit")]
  setnames(Prices_S2S3,"subsector_L2","groupvalue")


  Prices_S1S2 <- Pref_S1S2 <- logit_exp_S1S2 <- list()

  # Prepare logit price data S1S2
  for (i in 1:length(listofruns)) {
    Prices_S1S2[[i]] <- copy(prices[[i]]$S1S2_shares)
    Prices_S1S2[[i]]$scenario <- copy(prices[[i]]$scenario)
  }

  Prices_S1S2 <- do.call(rbind.data.frame, Prices_S1S2)
  setkey(Prices_S1S2, NULL)

  # Prepare logit pref data S1S2
  for (i in 1:length(listofruns)) {
    Pref_S1S2[[i]] <- copy(pref[[i]]$S1S2_final_pref)
    Pref_S1S2[[i]]$scenario <- copy(pref[[i]]$scenario)
  }

  Pref_S1S2 <- do.call(rbind.data.frame, Pref_S1S2)
  setkey(Pref_S1S2, NULL)
  # Prepare logit exponents S1S2
  for (i in 1:length(listofruns)) {
    logit_exp_S1S2[[i]] <- copy(logit_exp[[i]]$logit_exponent_S1S2)
    logit_exp_S1S2[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }

  logit_exp_S1S2 <- do.call(rbind.data.frame, logit_exp_S1S2)
  setkey(logit_exp_S1S2, NULL)

  Prices_S1S2 <- LogitCostplotdata(priceData=Prices_S1S2,prefData=Pref_S1S2,logitExp=logit_exp_S1S2,groupValue="subsector_L1",weight=weight_dem_pkm , yrs,Regionmapping)
  Prices_S1S2 <- Prices_S1S2[, c("variable", "period", "scenario", "region", "value","subsector_L1","unit")]
  setnames(Prices_S1S2,"subsector_L1","groupvalue")


  Prices_VS1 <- Pref_VS1 <- logit_exp_VS1 <- list()

  # Prepare logit price data VS1
  for (i in 1:length(listofruns)) {
    Prices_VS1[[i]] <- copy(prices[[i]]$VS1_shares)
    Prices_VS1[[i]]$scenario <- copy(prices[[i]]$scenario)
  }

  Prices_VS1 <- do.call(rbind.data.frame, Prices_VS1)
  setkey(Prices_VS1, NULL)

  # Prepare logit pref data VS1
  for (i in 1:length(listofruns)) {
    Pref_VS1[[i]] <- copy(pref[[i]]$VS1_final_pref)
    Pref_VS1[[i]]$scenario <- copy(pref[[i]]$scenario)
  }

  Pref_VS1 <- do.call(rbind.data.frame, Pref_VS1)
  setkey(Pref_VS1, NULL)
  # Prepare logit exponents VS1
  for (i in 1:length(listofruns)) {
    logit_exp_VS1[[i]] <- copy(logit_exp[[i]]$logit_exponent_VS1)
    logit_exp_VS1[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }

  logit_exp_VS1 <- do.call(rbind.data.frame, logit_exp_VS1)
  setkey(logit_exp_VS1, NULL)

  Prices_VS1 <- LogitCostplotdata(priceData=Prices_VS1,prefData=Pref_VS1,logitExp=logit_exp_VS1,groupValue="vehicle_type",weight=weight_dem_pkm , yrs,Regionmapping)
  Prices_VS1 <- Prices_VS1[, c("variable", "period", "scenario", "region", "value","vehicle_type","unit")]
  setnames(Prices_VS1,"vehicle_type","groupvalue")






  data <- list(LinePlot_data = LinePlot_data,
               Logit_Prices_S3S= Prices_S3S,
               Logit_Prices_S2S3= Prices_S2S3,
               Logit_Prices_S1S2= Prices_S1S2,
               Logit_Prices_VS1= Prices_VS1,
               Pref_S3S= Pref_S3S,
               Pref_S2S3= Pref_S2S3,
               Pref_S1S2= Pref_S1S2,
               Pref_VS1= Pref_VS1,
               shares = rbind(ES_shares_Pass_wbunk,ES_shares_Pass_wobunk ,ES_shares_Freight_wbunk,ES_shares_Freight_wobunk))


  return(data)
}
