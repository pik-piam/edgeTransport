#' Read in all information from previous EDGE runs and create
#' the comparison.pdf
#'
#' @param listofruns folder hosting REMIND input files. If NULL, a list of magclass objects is returned (set this option in case of a REMIND preprocessing run)
#' @param hist path to a REMIND historical data file ("historical.mif")
#' @param filename name of the pdf file to be produced
#' @param y_bar years to plot on y axis
#' @param mainReg Main region to plot in single window
#' @param load_cache switch load from local cache on and off
#' @param cache_folder path to internal cache folder for local use
#' @param AggrReg Region aggregation for generating plot data
#'
#' @author Johanna Hoppe
#' @export
#' @import mip
#' @import data.table
#' @import rmndt
#' @importFrom luplot magpie2ggplot2
#' @importFrom ggplot2 facet_grid ggplot geom_col facet_wrap geom_point aes_ geom_ribbon guides guide_legend aes expand_limits alpha geom_line theme theme_minimal xlab ylab scale_color_manual
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom magclass read.report mbind getRegions new.magpie getYears add_dimension setNames getNames<- time_interpolate getNames
#' @importFrom quitte as.quitte

compScenEDGET <- function(listofruns, hist, y_bar = c(2010, 2030, 2050, 2100),
                             mainReg = "EUR", filename = "CompareScenarios_EDGET",
                             load_cache = FALSE, cache_folder = "cache", AggrReg = "H12") {

  `.` <- FE_carrier <- UE_efficiency <- Year <- aggr_vehtype <- fewcol <- gran_vehtype <- international <- logit.exponent <- logit_type <- missingH12 <- model <- newdem <- period <- region <- scenario <- sector <- share <- shareVS1 <- sharetech_new <- sharetech_vint <- subsector_L1 <- subsector_L2 <- subsector_L3 <- technology <- tot_price <- totdem <- unit <- value <- variable <- vehicle_type <- vintdem <- vkm.veh <- weight <- NULL

  #fileName <- paste0(filename, "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".pdf")
  fileName <- paste0(filename, ".pdf")
  ## ----- Line Plots per Cap----
  lineplots_perCap <- function(data, vars, percap_factor, ylabstr,
                               global = FALSE, mainReg_plot = mainReg, per_gdp = FALSE, histdata_plot = NULL) {


    ## models for historical data
    histmap <- list(
      "Population" = "WDI",
      "GDP|PPP" = "James_IMF",
      "FE" = "IEA",
      "FE|Transport" = "IEA",
      "FE|Buildings" = "IEA",
      "FE|Industry" = "IEA"
    )

    items <- c(vars,
               "Population (million)",
               "GDP|PPP (billion US$2005/yr)")
    var <- as.data.table(as.quitte(data[, , items]))[, "unit" := NULL]

    plain_items <- gsub("(.+) \\(.+\\)", "\\1", items)

    if (!is.null(histdata_plot)) {
      if (!all(items %in% getNames(histdata_plot, dim = 3))) {
        missing <- items[!items %in% getNames(histdata_plot, dim = 3)]
        stop(paste("Items missing in historical dataset:",
                   paste(missing, collapse = ", ")))
      } else if (!all(plain_items %in% names(histmap))) {
        missing <- items[!plain_items %in% names(histmap)]
        stop(paste("No model defined for item in historical dataset:",
                   paste(missing, collapse = ", ")))
      } else {
        hist_dt <- as.data.table(as.quitte(histdata_plot[, , items]))
        models <- unlist(histmap[plain_items])
        varhist <- hist_dt[
          model %in% models][ # IEA: energy, IMF: GDP, WDI: Population
            , c("unit", "model") := list(NULL, "REMIND")]
        var <- rbind(var, varhist)
      }
    }

    plain_vars <- gsub("(.+) \\(.+\\)", "\\1", vars)

    variable <- Population <- NULL
    region <- `GDP|PPP` <- model <- value <- scenario <- NULL

    hvar <- data.table::dcast(var, ... ~ variable)

    for (fe in plain_vars) {
      hvar[, (fe) := get(fe) / Population * percap_factor]
    }

    if (per_gdp) {
      hvar[, `GDP|PPP` := `GDP|PPP` / Population]
      var <- data.table::melt(hvar, id.vars = c("model", "scenario", "region", "period", "GDP|PPP"))
    } else {
      hvar[, `GDP|PPP` := NULL]
      var <- data.table::melt(hvar, id.vars = c("model", "scenario", "region", "period"))
    }

    var <- var[variable != "Population"][
      , variable := factor(variable, levels = plain_vars)]

    highlight_yrs <- c(2030, 2050, 2070)
    highlights <- var[scenario != "historical" & period %in% highlight_yrs]

    reg_cols <- plotstyle(as.character(unique(var$region)))
    reg_labels <- plotstyle(as.character(unique(var$region)), out = "legend")

    var <- var[value > 0]
    if (per_gdp) {
      if (global) {
        p <- ggplot() +
          geom_line(data = var[scenario != "historical" & region == mainReg_plot],
                    aes(x = `GDP|PPP`, y = value, linetype = scenario)) +
          geom_point(data = var[scenario == "historical" & region == mainReg_plot],
                     aes(x = `GDP|PPP`, y = value), shape = 4) +
          geom_point(data = highlights[region == mainReg_plot], aes(x = `GDP|PPP`, y = value), shape = 1)
      } else {
        p <- ggplot() +
          geom_line(data = var[scenario != "historical" & region != mainReg_plot],
                    aes(x = `GDP|PPP`, y = value, linetype = scenario, color = region)) +
          geom_point(data = var[scenario == "historical" & region != mainReg_plot],
                     aes(x = `GDP|PPP`, y = value, color = region), shape = 4) +
          geom_point(data = highlights[region != mainReg_plot], aes(x = `GDP|PPP`, y = value, color = region), shape = 1) +
          scale_color_manual(values = reg_cols,  labels = reg_labels)
      }

      p <- p +
        facet_wrap(~variable, scales = "free_y") +
        ylab(ylabstr) +
        xlab("GDP PPP per Cap. (kUS$2005)") +
        expand_limits(y = 0) +
        theme_minimal()

    } else {
      if (global) {
        p <- ggplot() +
          geom_line(data = var[scenario != "historical" & region == mainReg_plot],
                    aes(x = period, y = value, linetype = scenario)) +
          geom_point(data = var[scenario == "historical" & region == mainReg_plot],
                     aes(x = period, y = value), shape = 4)
      } else {
        p <- ggplot() +
          geom_line(data = var[scenario != "historical" & region != mainReg_plot],
                    aes(x = period, y = value, linetype = scenario, color = region)) +
          geom_point(data = var[scenario == "historical" & region != mainReg_plot],
                     aes(x = period, y = value, color = region), shape = 4) +
          scale_color_manual(values = reg_cols,  labels = reg_labels)
      }
      p <- p +
        facet_wrap(~variable, scales = "free_y") +
        xlab("year") +
        ylab(ylabstr) +
        expand_limits(y = 0) +
        theme_minimal()

    }
    return(p)
  }

  scenNames <- SSP_Scen <- Tech_Scen <- c()
  demand_km <- demand_ej <- vintcomp <- newcomp <- shares <- pref <- mj_km_data <- loadFactor <- annual_mileage <- annual_sale <- prices <- logit_exp <- list()
  count_scen <- 2


  ## ---- Read historical data ----
  items <- c("IEA ETP RTS", "IEA ETP 2DS", "IEA ETP B2DS", "IEA", "JRC")
  historical <- read.report(hist, as.list = FALSE)
  historical <- historical[, , items, pmatch = FALSE]
  # Clean zeros and global data for JRC
  historical@.Data[historical@.Data == 0] <- NA
  historical["GLO", , "JRC", pmatch = FALSE] <- NA

  ## ---- Load GDP and POP ----
  if(load_cache & file.exists(cache_folder)){
    GDP_country = readRDS(file.path(cache_folder, "GDP_country.RDS"))
    POP_country = readRDS(file.path(cache_folder, "POP_country.RDS"))
  }else{
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
                    "Passenger Trains", "Passenger Trains", "Busses", "Motorbikes", "Motorbikes", "Motorbikes", "Ships international", "Cycling", "Walking"),
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

    # Create unique Scenario names for plotting
    if (any(grepl(listofruns[[i]], scenNames))) {
      scenNames[i] <- paste0(SSP_Scen[i], "-", Tech_Scen[i])
      scenNames[i] <- paste0(sub(".*/", "", scenNames[i]), "_", count_scen)
      count_scen <- count_scen + 1
    } else {
      scenNames[i] <- sub("_20.*", "", listofruns[[i]])
      scenNames[i] <- sub(".*/", "", scenNames[i])
}

    # load input data from EDGE runs for comparison
    demand_km[[i]] <- readRDS(level2path(listofruns[[i]], "demandF_plot_pkm.RDS")) ## detailed energy services demand, million pkm
    demand_km[[i]]$scenario <- scenNames[i]
    demand_ej[[i]] <- readRDS(level2path(listofruns[[i]], "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
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
  FE_Transport_Pass_Rail <- FE_Transport_Pass_Rail[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "FE|Transport|Pass|Rail"]
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
  # ES|Transport|Pass|Rail
  ES_Transport_Pass_Rail <- copy(plot_dem_pkm)
  ES_Transport_Pass_Rail <- ES_Transport_Pass_Rail[vehicle_type == "Passenger Trains"]
  ES_Transport_Pass_Rail <- ES_Transport_Pass_Rail[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Pass|Rail"]
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
  ES_Transport_Freight_Nav <- ES_Transport_Freight_Nav[vehicle_type %in% c("Ships domestic", "Ships international")]
  ES_Transport_Freight_Nav <- ES_Transport_Freight_Nav[, .(value = sum(value)), by = c("period", "region", "scenario", "unit")][, variable := "ES|Transport|Freight|Navigation"]
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


  LinePlot_data <- rbind(FE_Transport, FE_Transport_wobunk, FE_Transport_Pass, FE_Transport_Pass_Rail,  FE_Transport_Pass_Road, FE_Transport_Pass_Road_Bus, FE_Transport_Pass_Road_LDV, FE_Transport_Freight, FE_Transport_Freight_Nav, FE_Transport_Freight_Rail, FE_Transport_Freight_Road,
                        ES_Transport, ES_Transport_wobunk, ES_Transport_Pass, ES_Transport_Pass_Rail,  ES_Transport_Pass_Road, ES_Transport_Pass_Road_Bus, ES_Transport_Pass_Road_LDV, ES_Transport_Freight, ES_Transport_Freight_Nav, ES_Transport_Freight_Rail, ES_Transport_Freight_Road,
                        plot_EInt_MidsizeCar_BEV, plot_EInt_Bus_BEV, plot_EInt_MidsizeCar_ICE, plot_EInt_Bus_ICE
  )

  LinePlot_data[, model := "EDGE-T"]

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
    logit_exp_S2S3[[i]] <- copy(logit_exp[[i]]$logit_exponent_S1S2)
    logit_exp_S2S3[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }

  logit_exp_S2S3 <- do.call(rbind.data.frame, logit_exp_S2S3)
  setkey(logit_exp_S2S3, NULL)

  # Use ES as share weights
  # Aggregate data
  weight_dem_pkm_S2S3 <- weight_dem_pkm[, .(weight = sum(weight)), by = c("period", "manycol", "scenario", "sector", "subsector_L2", "subsector_L3", "unit")]

  # change variable names for mip
  setnames(Prices_S2S3, c("year"), c("period"))

  # change variable names for mip
  setnames(Pref_S2S3, c("year"), c("period"))
  Pref_S2S3 <- Pref_S2S3[period %in% unique(Prices_S2S3$period)]

  # Calculate Inconvenience Cost from share Weight
  Pref_S2S3 <- merge(Pref_S2S3, logit_exp_S2S3, all.x = TRUE)
  Pref_S2S3 <- Pref_S2S3[is.na(logit.exponent), logit.exponent := -10]
  Pref_S2S3 <- merge(Pref_S2S3, Prices_S2S3[, c("region", "period", "subsector_L2", "subsector_L3", "sector", "tot_price", "scenario")], by = c("period", "region", "sector", "subsector_L2", "subsector_L3", "scenario"))

  Pref_S2S3[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
  Pref_S2S3 <- Pref_S2S3[, c("region", "period", "subsector_L2", "subsector_L3", "sector", "scenario", "value")][, variable := "Inconvenience cost"]
  setnames(Pref_S2S3, "region", "manycol")


  Prices_S2S3 <- melt(Prices_S2S3[, -c("tot_price", "share")], id.vars = c("scenario", "region", "period", "sector", "subsector_L2", "subsector_L3"))
  setnames(Prices_S2S3, "region", "manycol")
  Prices_S2S3 <- aggregate_dt(Prices_S2S3,
                              Regionmapping,
                              manycol = "manycol",
                              fewcol = "fewcol",
                              yearcol = "period",
                              datacols = c("sector", "subsector_L2", "subsector_L3", "variable", "scenario"),
                              weights = weight_dem_pkm_S2S3[period %in% unique(Prices_S2S3$period)])
  setnames(Prices_S2S3, c("fewcol"), c("region"))

  Pref_S2S3 <- aggregate_dt(Pref_S2S3,
                            Regionmapping,
                            manycol = "manycol",
                            fewcol = "fewcol",
                            yearcol = "period",
                            datacols = c("sector", "subsector_L2", "subsector_L3", "variable", "scenario"),
                            weights = weight_dem_pkm_S2S3[period %in% unique(Pref_S2S3$period)])
  setnames(Pref_S2S3, c("fewcol"), c("region"))

  CONV_2005USD_1990USD <- 0.67
  Prices_S2S3_Plot_data_Bus <- rbind(Prices_S2S3[subsector_L2 == "Bus", c("variable", "period", "scenario", "region", "value")], Pref_S2S3[subsector_L2 == "Bus", c("variable", "period", "scenario", "region", "value")])
  Prices_S2S3_Plot_data_Bus <- Prices_S2S3_Plot_data_Bus[, value := value * CONV_2005USD_1990USD]
  Prices_S2S3_Plot_data_Bus <- Prices_S2S3_Plot_data_Bus[, unit := "$2005/pkm"]

  Prices_S2S3_Plot_data_LDV <- rbind(Prices_S2S3[subsector_L2 == "trn_pass_road_LDV", c("variable", "period", "scenario", "region", "value")], Pref_S2S3[subsector_L2 == "trn_pass_road_LDV", c("variable", "period", "scenario", "region", "value")])
  Prices_S2S3_Plot_data_LDV <- Prices_S2S3_Plot_data_LDV[, value := value * CONV_2005USD_1990USD]
  Prices_S2S3_Plot_data_LDV <- Prices_S2S3_Plot_data_LDV[, unit := "$2005/pkm"]

  data <- list(LinePlot_data = LinePlot_data,
              Prices_S2S3_Plot_data_Bus = Prices_S2S3_Plot_data_Bus,
              Prices_S2S3_Plot_data_LDV = Prices_S2S3_Plot_data_LDV)

  ## Temporary solution: Adjust new regionmapping to old regionmapping
  Regionmapping_21_H12 <- Regionmapping
  setnames(Regionmapping_21_H12, c("manycol", "fewcol"), c("region", "missingH12"))
  Regionmapping_H12_world <- Regionmapping_Tot
  setnames(Regionmapping_H12_world, c("manycol", "fewcol"), c("missingH12", "world"))
  ## ---- Preprocess plot_data ----

  ### Final energy

  plot_dem_ej <- copy(dem_ej)

  # rename columns for mip
  setnames(plot_dem_ej, c("demand_EJ", "year"), c("value", "period"))
  plot_dem_ej <- plot_dem_ej[, c("value", "period", "region", "scenario", "sector", "technology", "vehicle_type")]
  plot_dem_ej[, unit := "EJ/yr"]
  # Aggregate regions
  plot_dem_ej <- aggregate_dt(plot_dem_ej, Regionmapping_21_H12, fewcol = "missingH12", yearcol = "period", manycol = "region", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  plot_dem_ej_glo <- aggregate_dt(plot_dem_ej, Regionmapping_H12_world, fewcol = "world", yearcol = "period", manycol = "missingH12", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_dem_ej, "missingH12", "region")
  setnames(plot_dem_ej_glo, "world", "region")
  plot_dem_ej <- rbind(plot_dem_ej, plot_dem_ej_glo)
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]

  # Rename technologies by energy carrier
  plot_dem_ej[technology %in% c("FCEV", "Hydrogen"), technology := "FE|Hydrogen"]
  plot_dem_ej[technology %in% c("BEV", "Electric"), technology := "FE|Electricity"]
  plot_dem_ej[technology == "Liquids", technology := "FE|Liquids"]
  plot_dem_ej[technology == "NG", technology := "FE|Gases"]
  setnames(plot_dem_ej, "technology", "FE_carrier")
  # Group vehicle types for plotting
  plot_dem_ej <- merge(plot_dem_ej, Mapp_Aggr_vehtype, by.x = "vehicle_type", by.y = "gran_vehtype")
  plot_dem_ej <- plot_dem_ej[, -c("vehicle_type")]
  setnames(plot_dem_ej, "aggr_vehtype", "vehicle_type")

  # Filter for bunkers/no bunkers
  plot_dem_ej_wobunk <- plot_dem_ej[international == "no bunkers"]
  plot_dem_ej_bunk <- plot_dem_ej[international == "bunkers"]
  plot_dem_ej_wwobunk <- copy(plot_dem_ej)
  plot_dem_ej_wwobunk <- plot_dem_ej_wwobunk[, c("value", "period", "region", "scenario", "international", "unit")]
  plot_dem_ej <- plot_dem_ej[, -c("international")]
  plot_dem_ej_wobunk <- plot_dem_ej_wobunk[, -c("international")]
  plot_dem_ej_bunk <- plot_dem_ej_bunk[, -c("international")]

  # Aggregate by technology for each sector and vehicle type
  plot_dem_ej[, value := sum(value), by = c("period", "region", "scenario", "sector", "FE_carrier", "vehicle_type", "unit")]
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]
  plot_dem_ej_wobunk[, value := sum(value), by = c("period", "region", "scenario", "sector", "FE_carrier", "vehicle_type", "unit")]
  plot_dem_ej_wobunk <- plot_dem_ej_wobunk[!duplicated(plot_dem_ej_wobunk)]
  plot_dem_ej_bunk[, value := sum(value), by = c("period", "region", "scenario", "sector", "FE_carrier", "vehicle_type", "unit")]
  plot_dem_ej_bunk <- plot_dem_ej_bunk[!duplicated(plot_dem_ej_bunk)]
  plot_dem_ej_wwobunk[, value := sum(value), by = c("period", "region", "scenario", "international", "unit")]
  plot_dem_ej_wwobunk <- plot_dem_ej_wwobunk[!duplicated(plot_dem_ej_wwobunk)]


  ## ---- Open output-pdf ----

  template <-  c("\\documentclass[a4paper,landscape,twocolumn]{article}",
                 "\\setlength{\\oddsidemargin}{-0.8in}",
                 "\\setlength{\\evensidemargin}{-0.5in}",
                 "\\setlength{\\topmargin}{-0.8in}",
                 "\\setlength{\\parindent}{0in}",
                 "\\setlength{\\headheight}{0in}",
                 "\\setlength{\\topskip}{0in}",
                 "\\setlength{\\headsep}{0in}",
                 "\\setlength{\\footskip}{0.2in}",
                 "\\setlength\\textheight{0.95\\paperheight}",
                 "\\setlength\\textwidth{0.95\\paperwidth}",
                 "\\setlength{\\parindent}{0in}",
                 "\\setcounter{tocdepth}{4}",
                 "\\setcounter{secnumdepth}{4}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section,colorlinks=true,linkbordercolor={0.9882353 0.8352941 0.7098039}]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={GES group, PIK}}",
                 "\\usepackage{graphicx}",
                 "\\catcode`_=12",
                 "\\usepackage{Sweave}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=110)",
                 "@")

  sw <- swopen(fileName, template = template)
  swlatex(sw, "\\tableofcontents\\newpage")

  ## empty page
  swlatex(sw, "\\newpage")
  swlatex(sw, "\\thispagestyle{empty}")
  swlatex(sw, "\\mbox{}")
  swlatex(sw, "\\newpage")


  swlatex(sw, "\\section{Energy}")

  ## ---- FE energy by carrier ----
  swlatex(sw, "\\subsection{Final energy by carrier}")


  ### ---- Total ----
  swlatex(sw, "\\subsubsection{Total}")
  # Aggregate sectors
  plot_dem_ej_Tot <- copy(plot_dem_ej)
  plot_dem_ej_Tot <- plot_dem_ej_Tot[, sector := NULL][, vehicle_type := NULL]
  plot_dem_ej_Tot <- plot_dem_ej_Tot[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_Tot <- plot_dem_ej_Tot[!duplicated(plot_dem_ej_Tot)]
  # Set technology as variable
  setnames(plot_dem_ej_Tot, "FE_carrier", "variable")

  p <- mipArea(plot_dem_ej_Tot[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Tot[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  ### ---- Passenger ----
  swlatex(sw, "\\subsubsection{Passenger}")
  # Choose and aggregate passenger sectors
  plot_dem_ej_Pass <- copy(plot_dem_ej)
  plot_dem_ej_Pass <- plot_dem_ej_Pass[sector %in% c("trn_pass", "trn_aviation_intl")]
  plot_dem_ej_Pass_tot <- plot_dem_ej_Pass[, -c("sector", "vehicle_type")]
  plot_dem_ej_Pass_tot <- plot_dem_ej_Pass_tot[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_Pass_tot <- plot_dem_ej_Pass_tot[!duplicated(plot_dem_ej_Pass_tot)]
  # Set FE_carrier as variable
  setnames(plot_dem_ej_Pass_tot, "FE_carrier", "variable")

  p <- mipArea(plot_dem_ej_Pass_tot[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_tot[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  #### ---- Passenger LDV ----
  swlatex(sw, "\\subsubsection{Passenger LDV}")

  # Choose only LDVs
  plot_dem_ej_Pass_LDV <- plot_dem_ej_Pass[vehicle_type %in% c("Small Cars", "Large Cars")][, -c("sector", "vehicle_type")]
  # Aggregate by FE_carrier
  plot_dem_ej_Pass_LDV <- plot_dem_ej_Pass_LDV[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_Pass_LDV <- plot_dem_ej_Pass_LDV[!duplicated(plot_dem_ej_Pass_LDV)]
  # Set FE_carrier as variable
  setnames(plot_dem_ej_Pass_LDV, "FE_carrier", "variable")

  p <- mipArea(plot_dem_ej_Pass_LDV[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_LDV[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  #### ---- Passenger non-LDV ----
  swlatex(sw, "\\subsubsection{Passenger non-LDV}")
  # Choose only LDVs
  plot_dem_ej_Pass_nonLDV <- plot_dem_ej_Pass[!vehicle_type %in% c("Small Cars", "Large Cars")][, -c("sector", "vehicle_type")]
  # Aggregate by FE_carrier
  plot_dem_ej_Pass_nonLDV <- plot_dem_ej_Pass_nonLDV[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_Pass_nonLDV <- plot_dem_ej_Pass_nonLDV[!duplicated(plot_dem_ej_Pass_nonLDV)]
  # Set FE_carrier as variable
  setnames(plot_dem_ej_Pass_nonLDV, "FE_carrier", "variable")

  p <- mipArea(plot_dem_ej_Pass_nonLDV[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_nonLDV[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  ### ---- Freight ----
  swlatex(sw, "\\subsubsection{Freight}")

  # Choose and aggregate freight sectors
  plot_dem_ej_Frght <- copy(plot_dem_ej)
  plot_dem_ej_Frght <- plot_dem_ej_Frght[sector %in% c("trn_freight", "trn_shipping_intl")]
  plot_dem_ej_Frght_tot <- plot_dem_ej_Frght[, -c("sector", "vehicle_type")]
  plot_dem_ej_Frght_tot <- plot_dem_ej_Frght_tot[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_Frght_tot <- plot_dem_ej_Frght_tot[!duplicated(plot_dem_ej_Frght_tot)]
  # Set FE_carrier as variable
  setnames(plot_dem_ej_Frght_tot, "FE_carrier", "variable")

  p <- mipArea(plot_dem_ej_Frght_tot[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght_tot[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsubsection{Freight Trucks}")

  # Choose and aggregate freight sectors
  plot_dem_ej_Frght_Trucks <- copy(plot_dem_ej)
  plot_dem_ej_Frght_Trucks <- plot_dem_ej_Frght_Trucks[sector %in% c("trn_freight", "trn_shipping_intl") & vehicle_type == "Trucks"]
  plot_dem_ej_Frght_Trucks_tot <- plot_dem_ej_Frght_Trucks[, -c("sector", "vehicle_type")]
  plot_dem_ej_Frght_Trucks_tot <- plot_dem_ej_Frght_Trucks_tot[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_Frght_Trucks_tot <- plot_dem_ej_Frght_Trucks_tot[!duplicated(plot_dem_ej_Frght_Trucks_tot)]
  # Set FE_carrier as variable
  setnames(plot_dem_ej_Frght_Trucks_tot, "FE_carrier", "variable")

  p <- mipArea(plot_dem_ej_Frght_Trucks_tot[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_Trucks_tot[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_Trucks_tot[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght_Trucks_tot[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  # ### ---- Bunkers ----
  # swlatex(sw,"\\subsubsection{Bunkers}")
  #
  # #Aggregate sectors
  plot_dem_ej_bunk_Tot <- copy(plot_dem_ej_bunk)
  plot_dem_ej_bunk_Tot <- plot_dem_ej_bunk_Tot[, sector := NULL][, vehicle_type := NULL]
  plot_dem_ej_bunk_Tot <- plot_dem_ej_bunk_Tot[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_bunk_Tot <- plot_dem_ej_bunk_Tot[!duplicated(plot_dem_ej_bunk_Tot)]


  # ### ---- No Bunkers ----
  # swlatex(sw,"\\subsubsection{W/O Bunkers}")
  #
  # #Aggregate sectors
  plot_dem_ej_wobunk_Tot <- copy(plot_dem_ej_wobunk)
  plot_dem_ej_wobunk_Tot <- plot_dem_ej_wobunk_Tot[, sector := NULL][, vehicle_type := NULL]
  plot_dem_ej_wobunk_Tot <- plot_dem_ej_wobunk_Tot[, value := sum(value), by = c("period", "region", "scenario", "FE_carrier")]
  plot_dem_ej_wobunk_Tot <- plot_dem_ej_wobunk_Tot[!duplicated(plot_dem_ej_wobunk_Tot)]


  ## ---- FE Transport modes----
  swlatex(sw, "\\subsection{Final energy by transport modes}")
  swlatex(sw, "\\subsubsection{Passenger without bunkers by transport modes}")


  plot_dem_ej_modes <- copy(plot_dem_ej)
  # Choose Passenger without bunkers
  plot_dem_ej_modes <- plot_dem_ej_modes[sector == "trn_pass"]
  # Aggregate by transport modes
  plot_dem_ej_modes <- plot_dem_ej_modes[, sector := NULL][, FE_carrier := NULL]
  plot_dem_ej_modes <- plot_dem_ej_modes[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type")]
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  # Set vehicle_types as variable
  setnames(plot_dem_ej_modes, "vehicle_type", "variable")
  plot_dem_ej_modes[, variable := paste0("FE|", variable)]

  p <- mipArea(plot_dem_ej_modes[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_modes[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  # swlatex(sw, "\\subsubsection{Passenger bunkers by transport modes}")
  # 
  # 
  # plot_dem_ej_modes <- copy(plot_dem_ej)
  # # Choose Passenger bunkers
  # plot_dem_ej_modes <- plot_dem_ej_modes[sector == "trn_aviation_intl"]
  # # Aggregate by transport modes
  # plot_dem_ej_modes <- plot_dem_ej_modes[, sector := NULL][, FE_carrier := NULL]
  # plot_dem_ej_modes <- plot_dem_ej_modes[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type")]
  # plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  # # Set vehicle_types as variable
  # setnames(plot_dem_ej_modes, "vehicle_type", "variable")
  # plot_dem_ej_modes <- plot_dem_ej_modes[, variable := sub("international", "", variable)]
  # plot_dem_ej_modes[, variable := paste0("FE|", variable)]
  # 
  # p <- mipArea(plot_dem_ej_modes[region == mainReg], scales = "free_y")
  # p <- p + theme(legend.position = "none")
  # swfigure(sw, print, p, sw_option = "height=3.5,width=7")
  # 
  # p <- mipBarYearData(plot_dem_ej_modes[region == mainReg & period %in% y_bar])
  # p <- p + theme(legend.position = "none")
  # swfigure(sw, print, p, sw_option = "height=4.5,width=7")
  # 
  # p <- mipBarYearData(plot_dem_ej_modes[!region == mainReg & period %in% y_bar])
  # swfigure(sw, print, p, sw_option = "height=9,width=8")
  # 
  # swlatex(sw, "\\onecolumn")
  # p <- mipArea(plot_dem_ej_modes[!region == mainReg], stack_priority = c("variable"), scales = "free_y")
  # swfigure(sw, print, p, sw_option = "height=8,width=16")
  # swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsubsection{Freight without bunkers by transport modes}")


  plot_dem_ej_modes <- copy(plot_dem_ej)
  # Choose Freight without bunkers
  plot_dem_ej_modes <- plot_dem_ej_modes[sector == "trn_freight"]
  # Aggregate by transport modes
  plot_dem_ej_modes <- plot_dem_ej_modes[, sector := NULL][, FE_carrier := NULL]
  plot_dem_ej_modes <- plot_dem_ej_modes[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type")]
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  # Set vehicle_types as variable
  setnames(plot_dem_ej_modes, "vehicle_type", "variable")
  plot_dem_ej_modes[, variable := paste0("FE|", variable)]

  p <- mipArea(plot_dem_ej_modes[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_modes[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  # swlatex(sw, "\\subsubsection{Freight bunkers by transport modes}")
  # 
  # plot_dem_ej_modes <- copy(plot_dem_ej)
  # # Choose Freight bunkers
  # plot_dem_ej_modes <- plot_dem_ej_modes[sector == "trn_shipping_intl"]
  # # Aggregate by transport modes
  # plot_dem_ej_modes <- plot_dem_ej_modes[, sector := NULL][, FE_carrier := NULL]
  # plot_dem_ej_modes <- plot_dem_ej_modes[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type")]
  # plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  # # Set vehicle_types as variable
  # setnames(plot_dem_ej_modes, "vehicle_type", "variable")
  # plot_dem_ej_modes <- plot_dem_ej_modes[, variable := sub("international", "", variable)]
  # plot_dem_ej_modes[, variable := paste0("FE|", variable)]
  # 
  # p <- mipArea(plot_dem_ej_modes[region == mainReg], scales = "free_y")
  # p <- p + theme(legend.position = "none")
  # swfigure(sw, print, p, sw_option = "height=3.5,width=7")
  # 
  # p <- mipBarYearData(plot_dem_ej_modes[region == mainReg & period %in% y_bar])
  # p <- p + theme(legend.position = "none")
  # swfigure(sw, print, p, sw_option = "height=4.5,width=7")
  # 
  # p <- mipBarYearData(plot_dem_ej_modes[!region == mainReg & period %in% y_bar])
  # swfigure(sw, print, p, sw_option = "height=9,width=8")
  # 
  # swlatex(sw, "\\onecolumn")
  # p <- mipArea(plot_dem_ej_modes[!region == mainReg], stack_priority = c("variable"), scales = "free_y")
  # swfigure(sw, print, p, sw_option = "height=8,width=16")
  # swlatex(sw, "\\twocolumn")

  ## ---- FE Transport modes bunkers vs no bunkers----
  swlatex(sw, "\\subsection{Final energy bunkers vs. no bunkers}")


  # Set bunkers vs no bunkers as variable
  setnames(plot_dem_ej_wwobunk, "international", "variable")
  plot_dem_ej_wwobunk[, variable := paste0("FE|", variable)]

  p <- mipArea(plot_dem_ej_wwobunk[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_wwobunk[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_wwobunk[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_wwobunk[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  ## ---- FE Line Transport ----

  swlatex(sw, "\\subsection{Final Energy line plots}")
  swlatex(sw, "\\subsubsection{FE|Transport [EJ/yr]}")

  vars <- c(
    "FE|Transport (EJ/yr)",
    "FE|Transport|Pass (EJ/yr)",
    "FE|Transport|Pass|Rail (EJ/yr)",
    "FE|Transport|Pass|Road|Bus (EJ/yr)",
    "FE|Transport|Pass|Road|LDV (EJ/yr)",
    "FE|Transport|Freight (EJ/yr)",
    "FE|Transport|Freight|Navigation (EJ/yr)",
    "FE|Transport|Freight|Rail (EJ/yr)",
    "FE|Transport|Freight|Road (EJ/yr)")

  for (var0 in vars) {
    if (sub(" (.*)", "", var0) %in% unique(data$LinePlot_data$variable)) {
      p <- mipLineHistorical(
        data$LinePlot_data[region == mainReg & variable == sub(" (.*)", "", var0)],
        x_hist = historical[mainReg, , var0],
        ylab = var0,
        scales = "free_y",
        xlim = c(1990, 2050)
      )
      swfigure(sw, print, p, sw_option = "height=8,width=8")
      p <- mipLineHistorical(data$LinePlot_data[!region == mainReg & variable == sub(" (.*)", "", var0)],
                              x_hist = historical[, , var0][mainReg, , , invert = TRUE],
                              ylab = var0, scales = "free_y", plot.priority = c("x_hist", "x", "x_proj"), facet.ncol = 3,
                              xlim = c(1990, 2050)
      )
      swfigure(sw, print, p, sw_option = "height=9,width=8")
    } else {
      print(paste0(var0, " not in runs"))
    }
  }

  ## ---- UE energy by carrier ----
  swlatex(sw, "\\subsection{Useful energy by carrier}")


  plot_dem_ej_UE <- copy(dem_ej)

  # rename columns for mip
  setnames(plot_dem_ej_UE, c("demand_EJ", "year"), c("value", "period"))
  plot_dem_ej_UE <- plot_dem_ej_UE[, c("value", "period", "region", "scenario", "sector", "technology", "vehicle_type")]
  plot_dem_ej_UE[, unit := "EJ/yr"]
  # Aggregate regions
  plot_dem_ej_UE <- aggregate_dt(plot_dem_ej_UE, Regionmapping_21_H12, fewcol = "missingH12", yearcol = "period", manycol = "region", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  plot_dem_ej_glo_UE <- aggregate_dt(plot_dem_ej_UE, Regionmapping_H12_world, fewcol = "world", yearcol = "period", manycol = "missingH12", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_dem_ej_UE, "missingH12", "region")
  setnames(plot_dem_ej_glo_UE, "world", "region")
  plot_dem_ej_UE <- rbind(plot_dem_ej_UE, plot_dem_ej_glo_UE)
  plot_dem_ej_UE <- plot_dem_ej_UE[!duplicated(plot_dem_ej_UE)]

  # Calculate UE
  plot_dem_ej_UE <- merge(plot_dem_ej_UE, Mapp_UE)
  plot_dem_ej_UE[, value := value * UE_efficiency][, UE_efficiency := NULL]

  # Rename and aggregate technologies by energy carrier
  plot_dem_ej_UE[technology %in% c("FCEV"), technology := "UE|Hydrogen"]
  plot_dem_ej_UE[technology %in% c("Hydrogen"), technology := "UE|Hydrogen"]
  plot_dem_ej_UE[technology %in% c("BEV", "Electric"), technology := "UE|Electricity"]
  plot_dem_ej_UE[technology == "Liquids", technology := "UE|Liquids"]
  plot_dem_ej_UE[technology == "NG", technology := "UE|Gas"]
  # Group vehicle types for plotting
  plot_dem_ej_UE <- merge(plot_dem_ej_UE, Mapp_Aggr_vehtype, by.x = "vehicle_type", by.y = "gran_vehtype")
  plot_dem_ej_UE <- plot_dem_ej_UE[, -c("vehicle_type")]
  setnames(plot_dem_ej_UE, "aggr_vehtype", "vehicle_type")

  # Filter for bunkers/no bunkers
  plot_dem_ej_wobunk_UE <- plot_dem_ej_UE[international == "no bunkers"]
  plot_dem_ej_bunk_UE <- plot_dem_ej_UE[international == "bunkers"]
  plot_dem_ej_wwobunk_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_wwobunk_UE <- plot_dem_ej_wwobunk_UE[, c("value", "period", "region", "scenario", "international", "unit")]
  plot_dem_ej_UE <- plot_dem_ej_UE[, -c("international")]
  plot_dem_ej_wobunk_UE <- plot_dem_ej_wobunk_UE[, -c("international")]
  plot_dem_ej_bunk_UE <- plot_dem_ej_bunk_UE[, -c("international")]

  # Aggregate by technology for each sector and vehicle type
  plot_dem_ej_UE[, value := sum(value), by = c("period", "region", "scenario", "sector", "technology", "vehicle_type", "unit")]
  plot_dem_ej_UE <- plot_dem_ej_UE[!duplicated(plot_dem_ej_UE)]
  plot_dem_ej_wobunk_UE[, value := sum(value), by = c("period", "region", "scenario", "sector", "technology", "vehicle_type", "unit")]
  plot_dem_ej_wobunk_UE <- plot_dem_ej_wobunk_UE[!duplicated(plot_dem_ej_wobunk_UE)]
  plot_dem_ej_bunk_UE[, value := sum(value), by = c("period", "region", "scenario", "sector", "technology", "vehicle_type", "unit")]
  plot_dem_ej_bunk_UE <- plot_dem_ej_bunk_UE[!duplicated(plot_dem_ej_bunk_UE)]
  plot_dem_ej_wwobunk_UE[, value := sum(value), by = c("period", "region", "scenario", "international", "unit")]
  plot_dem_ej_wwobunk_UE <- plot_dem_ej_wwobunk_UE[!duplicated(plot_dem_ej_wwobunk_UE)]


  ### ---- Total ----
  swlatex(sw, "\\subsubsection{Total}")
  # Aggregate sectors
  plot_dem_ej_Tot_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_Tot_UE <- plot_dem_ej_Tot_UE[, -c("sector", "vehicle_type")]
  plot_dem_ej_Tot_UE <- plot_dem_ej_Tot_UE[, value := sum(value), by = c("period", "region", "scenario", "technology")]
  plot_dem_ej_Tot_UE <- plot_dem_ej_Tot_UE[!duplicated(plot_dem_ej_Tot_UE)]
  # Set technology as variable
  setnames(plot_dem_ej_Tot_UE, "technology", "variable")

  p <- mipArea(plot_dem_ej_Tot_UE[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot_UE[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot_UE[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Tot_UE[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  ### ---- Passenger ----
  swlatex(sw, "\\subsubsection{Passenger}")
  # Choose and aggregate passenger sectors
  plot_dem_ej_Pass_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_Pass_UE <- plot_dem_ej_Pass_UE[sector %in% c("trn_pass", "trn_aviation_intl")]
  plot_dem_ej_Pass_tot_UE <- plot_dem_ej_Pass_UE[, -c("sector", "vehicle_type")]
  plot_dem_ej_Pass_tot_UE <- plot_dem_ej_Pass_tot_UE[, value := sum(value), by = c("period", "region", "scenario", "technology")]
  plot_dem_ej_Pass_tot_UE <- plot_dem_ej_Pass_tot_UE[!duplicated(plot_dem_ej_Pass_tot_UE)]
  # Set technology as variable
  setnames(plot_dem_ej_Pass_tot_UE, "technology", "variable")

  p <- mipArea(plot_dem_ej_Pass_tot_UE[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot_UE[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot_UE[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_tot_UE[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  #### ---- Passenger LDV ----
  swlatex(sw, "\\subsubsection{Passenger LDV}")

  # Choose only LDVs
  plot_dem_ej_Pass_LDV_UE <- plot_dem_ej_Pass_UE[vehicle_type %in% c("Small Cars", "Large Cars")][, -c("sector", "vehicle_type")]
  # Aggregate by technology
  plot_dem_ej_Pass_LDV_UE <- plot_dem_ej_Pass_LDV_UE[, value := sum(value), by = c("period", "region", "scenario", "technology")]
  plot_dem_ej_Pass_LDV_UE <- plot_dem_ej_Pass_LDV_UE[!duplicated(plot_dem_ej_Pass_LDV_UE)]
  # Set technology as variable
  setnames(plot_dem_ej_Pass_LDV_UE, "technology", "variable")

  p <- mipArea(plot_dem_ej_Pass_LDV_UE[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV_UE[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV_UE[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_LDV_UE[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  #### ---- Passenger non-LDV ----
  swlatex(sw, "\\subsubsection{Passenger non-LDV}")
  # Choose only LDVs
  plot_dem_ej_Pass_nonLDV_UE <- plot_dem_ej_Pass_UE[!vehicle_type %in% c("Small Cars", "Large Cars")][, -c("sector", "vehicle_type")]
  # Aggregate by technology
  plot_dem_ej_Pass_nonLDV_UE <- plot_dem_ej_Pass_nonLDV_UE[, value := sum(value), by = c("period", "region", "scenario", "technology")]
  plot_dem_ej_Pass_nonLDV_UE <- plot_dem_ej_Pass_nonLDV_UE[!duplicated(plot_dem_ej_Pass_nonLDV_UE)]
  # Set technology as variable
  setnames(plot_dem_ej_Pass_nonLDV_UE, "technology", "variable")

  p <- mipArea(plot_dem_ej_Pass_nonLDV_UE[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV_UE[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV_UE[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_nonLDV_UE[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  ### ---- Freight ----
  swlatex(sw, "\\subsubsection{Freight}")

  # Choose and aggregate freight sectors
  plot_dem_ej_Frght_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_Frght_UE <- plot_dem_ej_Frght_UE[sector %in% c("trn_freight", "trn_shipping_intl")]
  plot_dem_ej_Frght_tot_UE <- plot_dem_ej_Frght_UE[, -c("sector", "vehicle_type")]
  plot_dem_ej_Frght_tot_UE <- plot_dem_ej_Frght_tot_UE[, value := sum(value), by = c("period", "region", "scenario", "technology")]
  plot_dem_ej_Frght_tot_UE <- plot_dem_ej_Frght_tot_UE[!duplicated(plot_dem_ej_Frght_tot_UE)]
  # Set technology as variable
  setnames(plot_dem_ej_Frght_tot_UE, "technology", "variable")

  p <- mipArea(plot_dem_ej_Frght_tot_UE[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot_UE[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot_UE[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght_tot_UE[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")


  #### ---- Trucks----

  swlatex(sw, "\\subsubsection{Freight Trucks}")
  # Choose only Trucks
  plot_dem_ej_Frght_Trucks_UE <- plot_dem_ej_Frght_UE[vehicle_type == "Trucks"]
  plot_dem_ej_Frght_Trucks_UE <- plot_dem_ej_Frght_Trucks_UE[, sector := NULL][, vehicle_type := NULL]
  plot_dem_ej_Frght_Trucks_UE <- plot_dem_ej_Frght_Trucks_UE[, value := sum(value), by = c("period", "region", "scenario", "technology")]
  plot_dem_ej_Frght_Trucks_UE <-  plot_dem_ej_Frght_Trucks_UE[!duplicated(plot_dem_ej_Frght_Trucks_UE)]
  # Set technology as variable
  setnames(plot_dem_ej_Frght_Trucks_UE, "technology", "variable")

  p <- mipArea(plot_dem_ej_Frght_Trucks_UE[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_Trucks_UE[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_Trucks_UE[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=8")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght_Trucks_UE[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")



  ### Energy Services

  swlatex(sw, "\\section{Energy services}")

  plot_Energy_services <- copy(dem_pkm)



  # rename columns for mip
  setnames(plot_Energy_services, c("demand_F", "year"), c("value", "period"))
  plot_Energy_services <- plot_Energy_services[, c("value", "period", "region", "scenario", "sector", "technology", "vehicle_type")]
  plot_Energy_services[, unit := "million pkm/yr"]
  # Aggregate regions
  plot_Energy_services <- aggregate_dt(plot_Energy_services, Regionmapping_21_H12, fewcol = "missingH12", yearcol = "period", manycol = "region", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  plot_Energy_services_glo <- aggregate_dt(plot_Energy_services, Regionmapping_H12_world, fewcol = "world", yearcol = "period", manycol = "missingH12", datacols = c("technology", "scenario", "sector", "vehicle_type", "unit"), valuecol = "value")
  setnames(plot_Energy_services, "missingH12", "region")
  setnames(plot_Energy_services_glo, "world", "region")
  plot_Energy_services <- rbind(plot_Energy_services, plot_Energy_services_glo)
  plot_Energy_services <- plot_Energy_services[!duplicated(plot_Energy_services)]

  # Rename technologies by energy carrier
  plot_Energy_services[technology %in% c("FCEV", "Hydrogen"), technology := "Hydrogen"]
  plot_Energy_services[technology %in% c("BEV", "Electric"), technology := "Electricity"]
  plot_Energy_services[technology == "Liquids", technology := "Liquids"]
  plot_Energy_services[technology == "NG", technology := "Gases"]


  # Group vehicle types for plotting
  plot_Energy_services <- merge(plot_Energy_services, Mapp_Aggr_vehtype, by.x = "vehicle_type", by.y = "gran_vehtype")
  plot_Energy_services <- plot_Energy_services[, -c("vehicle_type")]
  setnames(plot_Energy_services, "aggr_vehtype", "vehicle_type")

  # Filter for bunkers/no bunkers
  plot_Energy_services_Pass_wobunk <- plot_Energy_services[sector %in% c("trn_pass", "trn_aviation_intl") & international == "no bunkers"][, international := NULL][, technology := NULL][, sector := NULL]
  plot_Energy_services_Pass_bunk <- plot_Energy_services[sector %in% c("trn_pass", "trn_aviation_intl") & international == "bunkers"][, international := NULL][, technology := NULL][, sector := NULL]
  plot_Energy_services_Frght_wobunk <- plot_Energy_services[sector %in% c("trn_freight", "trn_shipping_intl") & international == "no bunkers"][, international := NULL][, technology := NULL][, sector := NULL]
  plot_Energy_services_Frght_bunk <- plot_Energy_services[sector %in% c("trn_freight", "trn_shipping_intl") & international == "bunkers"][, international := NULL][, technology := NULL][, sector := NULL]

  # Aggregate copy by vehicle type and technology for each sector
  plot_Energy_services[, value := sum(value), by = c("period", "region", "scenario", "sector", "vehicle_type", "unit", "international", "technology")]
  plot_Energy_services <- plot_Energy_services[!duplicated(plot_Energy_services)]
  # Aggregate by vehicle_type
  plot_Energy_services_Pass_wobunk[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type", "unit")]
  plot_Energy_services_Pass_wobunk  <- plot_Energy_services_Pass_wobunk [!duplicated(plot_Energy_services_Pass_wobunk)]
  plot_Energy_services_Pass_bunk[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type", "unit")]
  plot_Energy_services_Pass_bunk  <- plot_Energy_services_Pass_bunk[!duplicated(plot_Energy_services_Pass_bunk)]
  plot_Energy_services_Frght_wobunk[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type", "unit")]
  plot_Energy_services_Frght_wobunk  <- plot_Energy_services_Frght_wobunk [!duplicated(plot_Energy_services_Frght_wobunk)]
  plot_Energy_services_Frght_bunk[, value := sum(value), by = c("period", "region", "scenario", "vehicle_type", "unit")]
  plot_Energy_services_Frght_bunk  <- plot_Energy_services_Frght_bunk[!duplicated(plot_Energy_services_Frght_bunk)]

  swlatex(sw, "\\subsection{Passenger without bunkers by vehicle type}")
  # Set vehicle type as variable
  setnames(plot_Energy_services_Pass_wobunk, "vehicle_type", "variable")
  plot_Energy_services_Pass_wobunk[, variable := paste0("ES|", variable)]

  p <- mipArea(plot_Energy_services_Pass_wobunk[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_wobunk[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_wobunk[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_Pass_wobunk[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Passenger bunkers}")
  # Set vehicle type as variable
  setnames(plot_Energy_services_Pass_bunk, "vehicle_type", "variable")
  # plot_Energy_services_Pass_bunk <- plot_Energy_services_Pass_bunk[, variable:= sub("international","",variable)]
  # plot_Energy_services_Pass_bunk[, variable:= paste0("ES|",variable)]

  p <- mipArea(plot_Energy_services_Pass_bunk[region == mainReg], stack_priority = c("variable"), scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_bunk[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_bunk[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_Pass_bunk[!region == mainReg], stack_priority = c("variable"), scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Freight without bunkers by vehicle type}")
  # Set vehicle type as variable
  setnames(plot_Energy_services_Frght_wobunk, "vehicle_type", "variable")
  plot_Energy_services_Frght_wobunk[, variable := paste0("ES|", variable)]

  p <- mipArea(plot_Energy_services_Frght_wobunk[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_wobunk[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_wobunk[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_Frght_wobunk[!region == mainReg], stack_priority = c("variable"), scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Freight bunkers by vehicle type}")
  # Set technology as variable
  setnames(plot_Energy_services_Frght_bunk, "vehicle_type", "variable")
  # plot_dem_ej_modes <- plot_dem_ej_modes[, variable:= sub("international","",variable)]
  # plot_Energy_services_Frght_bunk[, variable:= paste0("ES|",variable)]


  p <- mipArea(plot_Energy_services_Frght_bunk[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_bunk[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_bunk[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_Frght_bunk[!region == mainReg], stack_priority = c("variable"), scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{LDV's by technology}")

  plot_Energy_services_LDV <- plot_Energy_services[vehicle_type %in% c("Small Cars", "Large Cars")]
  plot_Energy_services_LDV <- plot_Energy_services_LDV[, -c("international", "vehicle_type", "sector")]
  plot_Energy_services_LDV[, value := sum(value), by = c("period", "region", "scenario", "unit", "technology")]
  plot_Energy_services_LDV <- plot_Energy_services_LDV[!duplicated(plot_Energy_services_LDV)]
  setnames(plot_Energy_services_LDV, "technology", "variable")
  plot_Energy_services_LDV[, variable := paste0("ES|", variable)]

  p <- mipArea(plot_Energy_services_LDV[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_LDV[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_LDV[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_LDV[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Busses by technology}")

  plot_Energy_services_Bus <- plot_Energy_services[vehicle_type == "Busses"]
  plot_Energy_services_Bus <- plot_Energy_services_Bus[, -c("international", "vehicle_type", "sector")]
  setnames(plot_Energy_services_Bus, "technology", "variable")
  plot_Energy_services_Bus[, variable := paste0("ES|", variable)]
  plot_Energy_services_Bus <- plot_Energy_services_Bus[, value := sum(value), by = c("period", "region", "scenario", "variable", "unit")]
  plot_Energy_services_Bus <- plot_Energy_services_Bus[!duplicated(plot_Energy_services_Bus)]

  p <- mipArea(plot_Energy_services_Bus[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Bus[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Bus[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_Bus[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Trucks by technology}")

  plot_Energy_services_Truck <- plot_Energy_services[vehicle_type == "Trucks"]
  plot_Energy_services_Truck <- plot_Energy_services_Truck[, -c("international", "vehicle_type", "sector")]
  setnames(plot_Energy_services_Truck, "technology", "variable")
  plot_Energy_services_Truck[, variable := paste0("ES|", variable)]
  plot_Energy_services_Truck <- plot_Energy_services_Truck[, value := sum(value), by = c("period", "region", "scenario", "variable", "unit")]
  plot_Energy_services_Truck <- plot_Energy_services_Truck[!duplicated(plot_Energy_services_Truck)]

  p <- mipArea(plot_Energy_services_Truck[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Truck[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Truck[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Energy_services_Truck[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")



  swlatex(sw, "\\subsubsection{Energy Services line plots}")

  vars <- c(

    "ES|Transport|Pass (bn pkm/yr)",
    "ES|Transport|Pass|Rail (bn pkm/yr)",
    "ES|Transport|Pass|Road|Bus (bn pkm/yr)",
    "ES|Transport|Pass|Road|LDV (bn pkm/yr)",
    "ES|Transport|Freight|Rail (bn tkm/yr)",
    "ES|Transport|Freight|Road (bn tkm/yr)"
  )

  for (var0 in vars) {
    if (sub(" (.*)", "", var0) %in% unique(data$LinePlot_data$variable)) {
      p <- mipLineHistorical(
        data$LinePlot_data[region == mainReg & variable == sub(" (.*)", "", var0)],
        x_hist = historical[mainReg, , var0],
        ylab = var0,
        scales = "free_y",
        xlim = c(1990, 2050)
      )
      swfigure(sw, print, p, sw_option = "height=9,width=16")
      p <- mipLineHistorical(data$LinePlot_data[!region == mainReg & variable == sub(" (.*)", "", var0)],
                              x_hist = historical[, , var0][mainReg, , , invert = TRUE],
                              ylab = var0, scales = "free_y", plot.priority = c("x_hist", "x", "x_proj"), facet.ncol = 3,
                              xlim = c(1990, 2050)
      )
      swfigure(sw, print, p, sw_option = "height=9,width=16")
    } else {
      print(paste0(var0, " not in runs"))
    }
  }






  swlatex(sw, "\\subsubsection{Energy Services for Passenger Transport (per Capita, year)}")

  ES_pass <- copy(plot_Energy_services)
  ES_pass <- ES_pass[sector %in% c("trn_pass", "trn_aviation_intl")][, variable := "ES|Transport|Pass"]
  ES_pass_Road_LDV <- copy(plot_Energy_services)
  ES_pass_Road_LDV <- ES_pass_Road_LDV[sector %in% c("trn_pass") & vehicle_type %in% c("Small Cars", "Large Cars")][, variable := "ES|Transport|Pass|Road|LDV"]
  ES_pass_Road_nonLDV <- copy(plot_Energy_services)
  ES_pass_Road_nonLDV <- ES_pass_Road_nonLDV[sector %in% c("trn_pass") & !vehicle_type %in% c("Small Cars", "Large Cars")][, variable := "ES|Transport|Pass|non-LDV"]
  ES_pass <- ES_pass[, .(value = sum(value)), by = c("region", "scenario", "period", "unit", "variable")]
  ES_pass_Road_LDV <- ES_pass_Road_LDV[, .(value = sum(value)), by = c("region", "scenario", "period", "unit", "variable")]
  ES_pass_Road_nonLDV <- ES_pass_Road_nonLDV[, .(value = sum(value)), by = c("region", "scenario", "period", "unit", "variable")]
  GDP_13_scen <- GDP_13_scen[, variable := "GDP|PPP"][, unit := "billion US$2005/yr"]
  setnames(GDP_13_scen, c("weight", "manycol"), c("value", "region"))
  POP_13_scen <- POP_13_scen[, variable := "Population"][, unit := "million"]





  data2 <- rbind (ES_pass, ES_pass_Road_LDV,  ES_pass_Road_nonLDV, GDP_13_scen, POP_13_scen)
  data2[, model := "EDGE-T"]

  items <- c(
    "ES|Transport|Pass (million pkm/yr)",
    "ES|Transport|Pass|Road|LDV (million pkm/yr)",
    "ES|Transport|Pass|non-LDV (million pkm/yr)"
  )


  p <- lineplots_perCap(data2, items, 1, "Mobility Demand per Cap. [km/yr]",
                        global = TRUE, per_gdp = FALSE)
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  p <- lineplots_perCap(data2, items, 1, "Mobility Demand per Cap. [km/yr]",
                        global = FALSE, per_gdp = FALSE)
  swfigure(sw, print, p, sw_option = "height=9,width=16")


  ## ---- ES passenger transport per capita (GDP domain, line graph)----

  swlatex(sw, "\\subsubsection{Energy Services for Passanger Transport (per Capita, GDP)}")

  p <- lineplots_perCap(data2, items, 1, "Mobility Demand per Cap. [km/yr]",
                        global = TRUE, per_gdp = TRUE)
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  p <- lineplots_perCap(data2, items, 1, "Mobility Demand per Cap. [km/yr]",
                        global = FALSE, per_gdp = TRUE)
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{Energy Intensity line plots}")

  vars <- c(

    "EInt|Transport|Pass|Road|Bus|BEV (MJ/km)",
    "EInt|Transport|Pass|Road|Bus|ICE (MJ/km)",
    "EInt|Transport|Pass|Road|LDV|Midsize Car|BEV (MJ/km)",
    "EInt|Transport|Pass|Road|LDV|Midsize Car|ICE (MJ/km)"
  )

  for (var0 in vars) {
    if (sub(" (.*)", "", var0) %in% unique(data$LinePlot_data$variable)) {
      p <- mipLineHistorical(
        data$LinePlot_data[region == mainReg & variable == sub(" (.*)", "", var0)],
        # x_hist = historical[mainReg, , var0],
        ylab = var0,
        scales = "free_y",
        xlim = c(1990, 2050)
      )
      swfigure(sw, print, p, sw_option = "height=9,width=16")
      p <- mipLineHistorical(data$LinePlot_data[!region == mainReg & variable == sub(" (.*)", "", var0)],
                              # x_hist = historical[, , var0][mainReg, , , invert = TRUE],
                              ylab = var0, scales = "free_y", plot.priority = c("x_hist", "x", "x_proj"), facet.ncol = 3,
                              xlim = c(1990, 2050)
      )
      swfigure(sw, print, p, sw_option = "height=9,width=16")
    } else {
      print(paste0(var0, " not in runs"))
    }
  }



  ## ---- LDVs vintages and sales ----

  annual_sale <- do.call(rbind.data.frame, annual_sale)
  vintcomp <- do.call(rbind.data.frame, vintcomp)
  newcomp <- do.call(rbind.data.frame, newcomp)
  annual_mileage <- do.call(rbind.data.frame, annual_mileage)

  VS1_shares <- list()

  for (i in 1:length(shares)) {
    VS1_shares[[i]] <- copy(shares[[i]]$VS1_shares)
    VS1_shares[[i]]$scenario <- copy(shares[[i]]$scenario)
  }
  VS1_shares <- do.call(rbind.data.frame, VS1_shares)
  setkey(VS1_shares, NULL)

  plot_vintcomp_LDV <- copy(vintcomp[subsector_L1 == "trn_pass_road_LDV_4W"])
  plot_vintcomp_LDV <- plot_vintcomp_LDV[, .(totdem, region, subsector_L1, year, technology, vehicle_type, sector, sharetech_vint, scenario)]
  plot_newcomp_LDV <- copy(newcomp[subsector_L1 == "trn_pass_road_LDV_4W"])
  plot_newcomp_LDV <- plot_newcomp_LDV[, .(region, subsector_L1, year, technology, vehicle_type, sector, sharetech_new, scenario)]

  allfleet <- merge(plot_newcomp_LDV, plot_vintcomp_LDV, all = TRUE, by = c("region", "sector", "subsector_L1", "vehicle_type", "technology",  "year", "scenario"))
  allfleet <- merge(allfleet, VS1_shares[subsector_L1 == "trn_pass_road_LDV_4W", .(shareVS1 = share, region, year, vehicle_type, subsector_L1, scenario)], all.x = TRUE, by = c("region", "year", "vehicle_type", "subsector_L1", "scenario"))
  allfleet[, vintdem := totdem * sharetech_vint * shareVS1]
  allfleet[, newdem := totdem * sharetech_new * shareVS1]
  allfleet <- melt(allfleet, id.vars = c("region", "sector", "subsector_L1", "vehicle_type", "technology",
                                         "year", "scenario"), measure.vars = c("vintdem", "newdem"))
  allfleet <- as.data.table(allfleet)
  allfleet <- merge(allfleet, loadFactor[, -c("subsector_L3", "subsector_L2")], by = c("region", "year", "sector", "subsector_L1", "vehicle_type", "technology", "scenario"))
  allfleet <- merge(allfleet, annual_mileage[, -c("subsector_L3", "subsector_L2")], by = c("region", "year", "sector", "subsector_L1", "vehicle_type", "technology", "scenario"))

  # fleetcomp <- allfleet[,.(value = sum(value/loadFactor/vkm.veh)), by = c("region", "year", "vehicle_type","scenario")]
  allfleet <- allfleet[, .(value = sum(value / loadFactor / vkm.veh)), by = c("region", "technology", "variable", "year", "scenario", "vehicle_type")]
  # allfleet <- allfleet[,.(value = sum(value)), by = c("region", "technology", "variable", "year","scenario")]

  # Aggregate regions
  allfleet <- aggregate_dt(allfleet, Regionmapping_21_H12, fewcol = "missingH12", manycol = "region", datacols = c("technology", "scenario", "variable", "vehicle_type"), valuecol = "value")
  allfleet_glo <- aggregate_dt(allfleet, Regionmapping_H12_world, fewcol = "world", manycol = "missingH12", datacols = c("technology", "scenario", "variable", "vehicle_type"), valuecol = "value")
  setnames(allfleet, "missingH12", "region")
  setnames(allfleet_glo, "world", "region")
  allfleet <- rbind(allfleet, allfleet_glo)
  allfleet <- allfleet[!duplicated(allfleet)]

  # allfleet[,alphaval := ifelse(variable =="vintdem", 1,0)]

  swlatex(sw, "\\section{LDV stock and sales}")

  swlatex(sw, "\\subsection{LDV stock by technology}")
  plot_LDV_stock_tech <- allfleet[variable == "vintdem", .(value = sum(value)), by = c("region", "technology", "year", "scenario")]
  setnames(plot_LDV_stock_tech, c("technology", "year"), c("variable", "period"))
  plot_LDV_stock_tech[, variable := paste0("Stock|", variable)]
  plot_LDV_stock_tech[, unit := "million Veh"]

  p <- mipArea(plot_LDV_stock_tech[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_LDV_stock_tech[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_LDV_stock_tech[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_LDV_stock_tech[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsubsection{LDV stock by vehicle type}")

  plot_LDV_stock_veht <- allfleet[variable == "vintdem", .(value = sum(value)), by = c("region", "vehicle_type", "year", "scenario")]
  setnames(plot_LDV_stock_veht, c("vehicle_type", "year"), c("variable", "period"))
  plot_LDV_stock_veht[, variable := paste0("Stock|", variable)]
  plot_LDV_stock_veht[, unit := "million Veh"]

  p <- mipArea(plot_LDV_stock_veht[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_LDV_stock_veht[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_LDV_stock_veht[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_LDV_stock_veht[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsubsection{LDV sales by technology}")

  plot_LDV_sales_tech <- allfleet[variable == "newdem", .(value = sum(value)), by = c("region", "technology", "year", "scenario")]
  setnames(plot_LDV_sales_tech, c("technology", "year"), c("variable", "period"))
  plot_LDV_sales_tech[, variable := paste0("Sales|", variable)]
  plot_LDV_sales_tech[, unit := "million Veh"]

  p <- mipArea(plot_LDV_sales_tech[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_LDV_sales_tech[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_LDV_sales_tech[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_LDV_sales_tech[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{LDV sales by vehicle type}")

  plot_LDV_sales_veht <- allfleet[variable == "newdem", .(value = sum(value)), by = c("region", "vehicle_type", "year", "scenario")]
  setnames(plot_LDV_sales_veht, c("vehicle_type", "year"), c("variable", "period"))
  plot_LDV_sales_veht[, variable := paste0("Sales|", variable)]
  plot_LDV_stock_veht[, unit := "million Veh"]

  p <- mipArea(plot_LDV_sales_veht[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_LDV_sales_veht[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_LDV_sales_veht[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_LDV_sales_veht[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  ## ---- Trucks vintages and sales ----


  plot_vintcomp_Trucks <- copy(vintcomp[subsector_L1 == "trn_freight_road_tmp_subsector_L1"])
  plot_vintcomp_Trucks <- plot_vintcomp_Trucks[, .(totdem, region, subsector_L1, year, technology, vehicle_type, sector, sharetech_vint, scenario)]
  plot_newcomp_Trucks <- copy(newcomp[subsector_L1 == "trn_freight_road_tmp_subsector_L1"])
  plot_newcomp_Trucks <- plot_newcomp_Trucks[, .(region, subsector_L1, year, technology, vehicle_type, sector, sharetech_new, scenario)]
  allfleet_Trucks <- merge(plot_newcomp_Trucks, plot_vintcomp_Trucks, all = TRUE, by = c("region", "sector", "subsector_L1", "vehicle_type", "technology",  "year", "scenario"))
  allfleet_Trucks <- allfleet_Trucks[sector == "trn_freight"]
  allfleet_Trucks <- merge(allfleet_Trucks, VS1_shares[, .(shareVS1 = share, region, year, vehicle_type, subsector_L1, scenario)], all.x = TRUE, by = c("region", "year", "vehicle_type", "subsector_L1", "scenario"))
  allfleet_Trucks[, vintdem := totdem * sharetech_vint * shareVS1]
  allfleet_Trucks[, newdem := totdem * sharetech_new * shareVS1]
  allfleet_Trucks <- melt(allfleet_Trucks, id.vars = c("region", "sector", "subsector_L1", "vehicle_type", "technology",
                                         "year", "scenario"), measure.vars = c("vintdem", "newdem"))
  allfleet_Trucks <- as.data.table(allfleet_Trucks)
  allfleet_Trucks[, alpha := ifelse(variable == "vintdem", 0, 1)]
  allfleet_Trucks <- merge(allfleet_Trucks, loadFactor, by = c("region", "year", "sector", "subsector_L1", "vehicle_type", "technology", "scenario"))

  # fleetcomp <- allfleet_Trucks[,.(value = sum(value/loadFactor/vkm.veh)), by = c("region", "year", "vehicle_type","scenario")]
  allfleet_Trucks <- allfleet_Trucks[, .(value = sum(value / loadFactor / 50000)), by = c("region", "technology", "variable", "year", "scenario", "vehicle_type")]
  # allfleet_Trucks <- allfleet_Trucks[,.(value = sum(value)), by = c("region", "technology", "variable", "year","scenario")]

  # Aggregate regions
  allfleet_Trucks <- aggregate_dt(allfleet_Trucks, Regionmapping_21_H12, fewcol = "missingH12", manycol = "region", datacols = c("technology", "scenario", "variable", "vehicle_type"), valuecol = "value")
  allfleet_Trucks_glo <- aggregate_dt(allfleet_Trucks, Regionmapping_H12_world, fewcol = "world", manycol = "missingH12", datacols = c("technology", "scenario", "variable", "vehicle_type"), valuecol = "value")
  setnames(allfleet_Trucks, "missingH12", "region")
  setnames(allfleet_Trucks_glo, "world", "region")
  allfleet_Trucks <- rbind(allfleet_Trucks, allfleet_Trucks_glo)
  allfleet_Trucks <- allfleet_Trucks[!duplicated(allfleet_Trucks)]

  swlatex(sw, "\\section{Truck stock and sales}")

  swlatex(sw, "\\subsection{Truck stock by technology}")
  plot_Trucks_stock_tech <- copy(allfleet_Trucks)
  plot_Trucks_stock_tech <- plot_Trucks_stock_tech[variable == "vintdem", .(value = sum(value)), by = c("region", "technology", "year", "scenario")]
  setnames(plot_Trucks_stock_tech, c("technology", "year"), c("variable", "period"))
  plot_Trucks_stock_tech[, variable := paste0("Stock|", variable)]
  plot_Trucks_stock_tech[, unit := "million Veh"]

  p <- mipArea(plot_Trucks_stock_tech[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Trucks_stock_tech[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Trucks_stock_tech[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Trucks_stock_tech[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Truck stock by vehicle type}")
  plot_Trucks_stock_veht <- copy(allfleet_Trucks)
  plot_Trucks_stock_veht <- plot_Trucks_stock_veht[variable == "vintdem", .(value = sum(value)), by = c("region", "vehicle_type", "year", "scenario")]
  setnames(plot_Trucks_stock_veht, c("vehicle_type", "year"), c("variable", "period"))
  plot_Trucks_stock_veht[, variable := paste0("Stock|", variable)]
  plot_Trucks_stock_veht[, unit := "million Veh"]

  p <- mipArea(plot_Trucks_stock_veht[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Trucks_stock_veht[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Trucks_stock_veht[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Trucks_stock_veht[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Truck sales by technology}")
  plot_Trucks_sales_tech <- copy(allfleet_Trucks)
  plot_Trucks_sales_tech <- plot_Trucks_sales_tech[variable == "newdem", .(value = sum(value)), by = c("region", "technology", "year", "scenario")]
  setnames(plot_Trucks_sales_tech, c("technology", "year"), c("variable", "period"))
  plot_Trucks_sales_tech[, variable := paste0("Sales|", variable)]
  plot_Trucks_sales_tech[, unit := "million Veh"]

  p <- mipArea(plot_Trucks_sales_tech[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Trucks_sales_tech[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Trucks_sales_tech[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Trucks_sales_tech[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")

  swlatex(sw, "\\subsection{Truck sales by vehicle type}")
  plot_Trucks_sales_veht <- copy(allfleet_Trucks)
  plot_Trucks_sales_veht <- plot_Trucks_sales_veht[variable == "newdem", .(value = sum(value)), by = c("region", "vehicle_type", "year", "scenario")]
  setnames(plot_Trucks_sales_veht, c("vehicle_type", "year"), c("variable", "period"))
  plot_Trucks_sales_veht[, variable := paste0("Sales|", variable)]
  plot_Trucks_sales_veht[, unit := "million Veh"]

  p <- mipArea(plot_Trucks_sales_veht[region == mainReg], scales = "free_y")
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4,width=7")

  p <- mipBarYearData(plot_Trucks_sales_veht[region == mainReg & period %in% y_bar])
  p <- p + theme(legend.position = "none")
  swfigure(sw, print, p, sw_option = "height=4.5,width=7")

  p <- mipBarYearData(plot_Trucks_sales_veht[!region == mainReg & period %in% y_bar])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\onecolumn")
  p <- mipArea(plot_Trucks_sales_veht[!region == mainReg], scales = "free_y")
  swfigure(sw, print, p, sw_option = "height=8,width=16")
  swlatex(sw, "\\twocolumn")


  ## ---- Costs----
  swlatex(sw, "\\section{Costs}")

  # Inconvenience costs
  weight_dem_pkm_FV <- weight_dem_pkm[, .(weight = sum(weight)), by = c("period", "manycol", "scenario", "vehicle_type", "unit", "technology", "subsector_L1")]
  setnames(weight_dem_pkm_FV, "manycol", "region")
  # Aggregate weights
  weight_dem_pkm_FV_12 <- aggregate_dt(weight_dem_pkm_FV,
                                      Regionmapping_21_H12,
                                      manycol = "region",
                                      fewcol = "missingH12",
                                      yearcol = "period",
                                      datacols = c("scenario", "unit", "vehicle_type"), valuecol = "weight")
  setnames(weight_dem_pkm_FV_12, c("vehicle_type", "missingH12"), c("gran_vehtype", "region"))

  FV_final_pref <- list()
  for (i in 1:length(pref)) {
    FV_final_pref[[i]] <- copy(pref[[i]]$FV_final_pref)
    FV_final_pref[[i]]$scenario <- copy(pref[[i]]$scenario)
  }

  FV_final_pref <- do.call(rbind.data.frame, FV_final_pref)
  setkey(FV_final_pref, NULL)

  # change variable names for mip
  FV_final_pref[logit_type == "pinco_tot", logit_type := "Inconvenience cost total"]
  FV_final_pref[logit_type == "pchar", logit_type := "Charger"]
  FV_final_pref[logit_type == "pmod_av", logit_type := "Model availability"]
  FV_final_pref[logit_type == "prange", logit_type := "Range anxiety"]
  FV_final_pref[logit_type == "pref", logit_type := "Ref. stations availability"]
  FV_final_pref[logit_type == "prisk", logit_type := "Risk"]
  setnames(FV_final_pref, c("year"), c("period"))

  logit_exp_FV <- list()
  # Calculate inconvenience cost for average Truck
  # Prepare logit price data S2S3
  Prices_FV <- list()
  for (i in 1:length(listofruns)) {
    Prices_FV[[i]] <- copy(prices[[i]]$FV_shares)
    Prices_FV[[i]]$scenario <- copy(prices[[i]]$scenario)
  }

  Prices_FV <- do.call(rbind.data.frame, Prices_FV)
  setkey(Prices_FV, NULL)
  setnames(Prices_FV, "year", "period")
  # Prepare logit exponents
  for (i in 1:length(listofruns)) {
    logit_exp_FV[[i]] <- copy(logit_exp[[i]]$logit_exponent_FV)
    logit_exp_FV[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }

  logit_exp_FV <- do.call(rbind.data.frame, logit_exp_FV)
  setkey(logit_exp_FV, NULL)


  FV_final_pref_Truck <- FV_final_pref[subsector_L1 == "trn_freight_road_tmp_subsector_L1" & period %in% unique(Prices_FV$period)]
  FV_final_pref_Truck <- merge(FV_final_pref_Truck, logit_exp_FV, all.x = TRUE)
  FV_final_pref_Truck <- FV_final_pref_Truck[is.na(logit.exponent), logit.exponent := -10][, logit_type := NULL]
  FV_final_pref_Truck <- merge(FV_final_pref_Truck, Prices_FV[, c("region", "period", "subsector_L2", "subsector_L3", "sector", "tot_price", "scenario", "technology", "vehicle_type", "fuel_price_pkm", "non_fuel_price")], by = c("period", "vehicle_type", "region", "sector", "subsector_L2", "subsector_L3", "scenario", "technology"))
  setnames(FV_final_pref_Truck, "value", "sw")
  FV_final_pref_Truck <- FV_final_pref_Truck[sw == 0, sw := 0.01]
  FV_final_pref_Truck[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
  FV_final_pref_Truck <- FV_final_pref_Truck[, c("region", "period", "vehicle_type", "technology", "value", "scenario", "sector", "fuel_price_pkm", "non_fuel_price")]
  setnames(FV_final_pref_Truck, "value", "inco_cost")
  FV_final_pref_Truck <- melt(FV_final_pref_Truck, id.vars = c("scenario", "region", "period", "sector", "vehicle_type", "technology"))


  tmp1 <- FV_final_pref[period %in% unique(weight_dem_pkm_FV$period), -c("logit_type")]
  tmp1 <- tmp1[!duplicated(tmp1)]
  tmp <- merge(tmp1, weight_dem_pkm_FV, all.x = TRUE, by = c("vehicle_type", "technology", "period", "scenario", "region"))
  tmp <- tmp[is.na(weight), weight := 0]
  weight_dem_pkm_FV <- tmp[, c("vehicle_type", "region", "scenario", "technology", "weight", "period")]
  weight_dem_pkm_FV <- weight_dem_pkm_FV[!duplicated(weight_dem_pkm_FV)]
  FV_final_pref <- aggregate_dt(FV_final_pref[period %in% unique(weight_dem_pkm_FV$period)],
                                Regionmapping_21_H12,
                                manycol = "region",
                                fewcol = "missingH12",
                                yearcol = "period",
                                datacols = c("vehicle_type", "technology", "sector", "logit_type", "scenario"),
                                weights = weight_dem_pkm_FV)
  setnames(FV_final_pref, c("missingH12"), c("region"))
  FV_final_pref_Truck <- aggregate_dt(FV_final_pref_Truck,
                                Regionmapping_21_H12,
                                manycol = "region",
                                fewcol = "missingH12",
                                yearcol = "period",
                                datacols = c("vehicle_type", "technology", "sector", "scenario"),
                                weights = weight_dem_pkm_FV)

  # Calculate inconvenience cost for average LDV(4W)
  # Total vehicles per region
  Inco_cost_LDV_4W <-  FV_final_pref[subsector_L1 == "trn_pass_road_LDV_4W", c("vehicle_type", "technology", "scenario", "period", "region", "value", "logit_type")]
  setnames(Inco_cost_LDV_4W, c("vehicle_type", "region"), c("gran_vehtype", "missingH12"))
  setnames(Inco_cost_LDV_4W, "missingH12", "region")
  setnames(weight_dem_pkm_FV, "vehicle_type", "gran_vehtype")
  Inco_cost_LDV_4W <- aggregate_dt(Inco_cost_LDV_4W[period %in% unique(weight_dem_pkm_FV$period)],
                                Mapp_Aggr_vehtype[gran_vehtype %in% c("Compact Car", "Large Car", "Large Car and SUV", "Midsize Car", "Mini Car", "Subcompact Car", "Van", "Light Truck and SUV"), c("gran_vehtype", "aggr_vehtype")][, aggr_vehtype := "av_veh"],
                                manycol = "gran_vehtype",
                                fewcol = "aggr_vehtype",
                                yearcol = "period",
                                datacols = c("region", "technology", "logit_type", "scenario"),
                                weights = weight_dem_pkm_FV_12)
  Inco_cost_LDV_4W <- Inco_cost_LDV_4W[, aggr_vehtype := NULL]
  setnames(Inco_cost_LDV_4W, "logit_type", "variable")


  setnames(FV_final_pref_Truck, c("vehicle_type"), c("gran_vehtype"))
  setnames(FV_final_pref_Truck, "missingH12", "region")
  FV_final_pref_Truck <- aggregate_dt(FV_final_pref_Truck[period %in% unique(weight_dem_pkm_FV$period)],
                                   Mapp_Aggr_vehtype[aggr_vehtype %in% c("Trucks")][, -c("international")],
                                   manycol = "gran_vehtype",
                                   fewcol = "aggr_vehtype",
                                   yearcol = "period",
                                   datacols = c("region", "technology", "variable", "scenario"),
                                   weights = weight_dem_pkm_FV_12)
  setnames(weight_dem_pkm_FV, "gran_vehtype", "vehicle_type")
  FV_final_pref_Truck <- FV_final_pref_Truck[, aggr_vehtype := NULL]


  REMIND_prices <- list()

  for (i in 1:length(listofruns)) {
    REMIND_prices[[i]] <- copy(prices[[i]]$FV_shares)
    REMIND_prices[[i]]$scenario <- copy(prices[[i]]$scenario)
  }

  REMIND_prices <- do.call(rbind.data.frame, REMIND_prices)
  setkey(REMIND_prices, NULL)
  REMIND_prices_LDV_4W <- REMIND_prices[subsector_L1 == "trn_pass_road_LDV_4W", c("technology", "scenario", "year", "region", "vehicle_type", "fuel_price_pkm", "non_fuel_price")]


  REMIND_prices_LDV_4W <- melt(REMIND_prices_LDV_4W, id.vars = c("scenario", "year", "region", "vehicle_type", "technology"))
  setnames(REMIND_prices_LDV_4W, "year", "period")

  REMIND_prices_LDV_4W <- aggregate_dt(REMIND_prices_LDV_4W,
                                Regionmapping_21_H12,
                                manycol = "region",
                                fewcol = "missingH12",
                                yearcol = "period",
                                datacols = c("vehicle_type", "technology", "scenario"),
                                weights = weight_dem_pkm_FV[period %in% unique(REMIND_prices_LDV_4W$period)])



  setnames(REMIND_prices_LDV_4W, c("vehicle_type", "missingH12"), c("gran_vehtype", "region"))



  REMIND_prices_LDV_4W <- aggregate_dt(REMIND_prices_LDV_4W,
                                   Mapp_Av_LDV,
                                   manycol = "gran_vehtype",
                                   fewcol = "aggr_vehtype",
                                   yearcol = "period",
                                   datacols = c("region", "technology", "scenario", "variable"),
                                   weights = weight_dem_pkm_FV_12[period %in% unique(REMIND_prices_LDV_4W$period)])
  # Average vehicle is only vehicle type and can be removed
  REMIND_prices_LDV_4W <- REMIND_prices_LDV_4W[, aggr_vehtype := NULL]
  # Convert 1990USD to 2005USD
  REMIND_prices_LDV_4W <- REMIND_prices_LDV_4W[, value := value * CONV_2005USD_1990USD]
  REMIND_prices_LDV_4W <- REMIND_prices_LDV_4W[, unit := "$2005/pkm"]

  Inco_cost_LDV_4W <- Inco_cost_LDV_4W[, value := value * CONV_2005USD_1990USD]
  Inco_cost_LDV_4W <- Inco_cost_LDV_4W[, unit := "$2005/pkm"]

  # Sum up different types of incovenience cost for total cost plots
  Inco_cost_LDV_4W_tot <- Inco_cost_LDV_4W[, .(value = sum(value)), by = c("region", "technology", "scenario", "period", "unit")][, variable := "Inconvenience cost"]
  All_prices_LDV_4W <- rbind(REMIND_prices_LDV_4W, Inco_cost_LDV_4W_tot)
  Inco_cost_LDV_4W[, variable := paste0("Inconvenience cost|", variable)]
  All_prices_LDV_4W[, variable := paste0("Total cost|", variable)]

  swlatex(sw, "\\subsection{Inconvenience cost average LDV(4W) for different technologies}")

  swlatex(sw, "\\subsubsection{BEV}")
  Plot_FV_final_pref <- Inco_cost_LDV_4W[technology == "BEV"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{FCEV}")
  Plot_FV_final_pref <- Inco_cost_LDV_4W[technology == "FCEV"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{Liquids}")
  Plot_FV_final_pref <- Inco_cost_LDV_4W[technology == "Liquids"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{NG}")
  Plot_FV_final_pref <- Inco_cost_LDV_4W[technology == "NG"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsection{Total cost average LDV(4W) for different technologies}")

  swlatex(sw, "\\subsubsection{BEV}")
  Plot_FV_final_pref <- All_prices_LDV_4W[technology == "BEV"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{FCEV}")
  Plot_FV_final_pref <- All_prices_LDV_4W[technology == "FCEV"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{Liquids}")
  Plot_FV_final_pref <- All_prices_LDV_4W[technology == "Liquids"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{NG}")
  Plot_FV_final_pref <- All_prices_LDV_4W[technology == "NG"]
  Plot_FV_final_pref <- Plot_FV_final_pref[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref <- Plot_FV_final_pref[!duplicated(Plot_FV_final_pref)]

  p <- mipBarYearData(Plot_FV_final_pref[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")



  swlatex(sw, "\\subsection{Total cost average Truck for different technologies}")

  swlatex(sw, "\\subsubsection{BEV}")
  Plot_FV_final_pref_Trucks <- All_prices_LDV_4W[technology == "BEV"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[!duplicated(Plot_FV_final_pref_Trucks)]

  p <- mipBarYearData(Plot_FV_final_pref_Trucks[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{FCEV}")
  Plot_FV_final_pref_Trucks <- All_prices_LDV_4W[technology == "FCEV"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[!duplicated(Plot_FV_final_pref_Trucks)]

  p <- mipBarYearData(Plot_FV_final_pref_Trucks[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{Liquids}")
  Plot_FV_final_pref_Trucks <- All_prices_LDV_4W[technology == "Liquids"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[!duplicated(Plot_FV_final_pref_Trucks)]

  p <- mipBarYearData(Plot_FV_final_pref_Trucks[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsubsection{NG}")
  Plot_FV_final_pref_Trucks <- All_prices_LDV_4W[technology == "NG"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[, c("period", "region", "scenario", "variable", "value")][, model := "EDGE-Transport"][, unit := "USD2005/pkm"]
  Plot_FV_final_pref_Trucks <- Plot_FV_final_pref_Trucks[!duplicated(Plot_FV_final_pref_Trucks)]

  p <- mipBarYearData(Plot_FV_final_pref_Trucks[period %in% c(2010, 2020, 2030, 2050)])
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  swlatex(sw, "\\subsection{Total cost transport passenger road}")

  p <- mipBarYearData(data$Prices_S2S3_Plot_data_LDV[period %in% c(2010, 2020, 2030, 2050)], ylab = "Prices logit_S2S3 LDV [USD2005/pkm]")
  swfigure(sw, print, p, sw_option = "height=9,width=16")
  p <- mipBarYearData(data$Prices_S2S3_Plot_data_Bus[period %in% c(2010, 2020, 2030, 2050)], ylab = "Prices logit_S2S3 Bus [USD2005/pkm]")
  swfigure(sw, print, p, sw_option = "height=9,width=16")

  ## Close output-pdf
  swclose(sw)
  }
