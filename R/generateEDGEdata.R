#' Generate EDGE-Transport Input Data for the REMIND model.
#'
#' Run this script to prepare the input data for EDGE in EDGE-friendly units and regional aggregation
#'
#' `calcgenerateEDGEdata()` is a wrapper for `generateEDGEdata()` to make use of
#' madrat caching.
#'
#' @md
#' @param input_folder folder hosting raw data
#' @param output_folder folder hosting REMIND input files. If NULL, a list of magclass objects is returned (set this option in case of a REMIND preprocessing run)
#' @param cache_folder folder hosting a "local" cache (this is not the mrremid cache, it is specific to EDGE-T).
#' @param SSP_scen SSP or SDP scenario
#' @param tech_scen EDGE-T technology scenario. Options are: ConvCase, ElecEra, HydrHype (working with SSP2 only!)
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression.
#' @param storeRDS optional saving of intermediate RDS files, only possible if output folder is not NULL
#' @param gdxPath optional path to a GDX file to load price signals from a REMIND run.
#' @param preftab path to file with trends for share weights
#' @param mitab4W.path path to file with key factors for 4W technologies for different mitigation ambition and SSP scenarios.
#' @param mitab.path path to file with key factors for share weight trends for different mitigation ambition and SSP scenarios.
#' @param ssp_demreg.path path to file with key factors for the demand regression depending on SSP/SDP scenarios.
#' @param regional_demreg.path path to file with key factors for the demand regression depending on regions and SSP scenarios.
#' @param demscen.path path to file with demScen specific mulitiplikative factors for the demand, region and sector specific.
#' @param plot.report write a report which is place in the level2 folder. Defaults to FALSE.
#' @param FEPricetab ship an external csv that includes FE prices. The prices from the gdx file will be overwritten for affected regions.
#' @param int_improvetab table with key factors for intensity improvements for different mitigation ambition and SSP scenarios.
#' @return generated EDGE-transport input data
#' @author Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @importFrom rmarkdown render
#' @importFrom quitte write.mif
#' @export


toolGenerateEDGEdata <- function(input_folder, output_folder, cache_folder = NULL,
                             SSP_scen = "SSP2", tech_scen = "Mix1", demScen = "default",
                             storeRDS = FALSE,
                             gdxPath = NULL,
                             preftab = NULL, plot.report = FALSE,
                             mitab4W.path = NULL, mitab.path = NULL,
                             ssp_demreg.path = NULL, regional_demreg.path = NULL,
                             demscen.path = NULL, FEPricetab = NULL,
                             int_improvetab = NULL) {
  scenario <- scenario_name <- vehicle_type <- type <- `.` <- CountryCode <- RegionCode <-
    technology <- non_fuel_price <- tot_price <- fuel_price_pkm <- subsector_L1 <- loadFactor <-
      ratio <- Year <- value <- DP_cap <- region <- weight <- MJ <- variable.unit <-
        EJ <- grouping_value <- sector <- variable <- region <- logit.exponent <- EDGETscen <-
          SSPscen <- default <- techscen <- share <- demand_F <- tech_scenario <- SSP_scenario <-
            demandScen <- NULL

  if(is.null(output_folder) & storeRDS == TRUE) {
    print("Warning: If storeRDS is set, output_folder has to be non-NULL. Setting storeRDS=FALSE")
    storeRDS <- FALSE
  }

  folder <- paste0(SSP_scen, "-", tech_scen, "-", demScen, "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"))

  if(!is.null(cache_folder) && !dir.exists(cache_folder)){
    dir.create(cache_folder)
  }

  levelNpath <- function(fname, N) {
    path <- file.path(output_folder, folder, paste0("level_", N))
    if(!dir.exists(path)) {
      dir.create(path, recursive = T)
    }
    return(file.path(path, fname))
  }

  level0path <- function(fname) {
    levelNpath(fname, 0)
  }

  level1path <- function(fname) {
    levelNpath(fname, 1)
  }

  level2path <- function(fname) {
    levelNpath(fname, 2)
  }

  ## store configs to this folder
  cfgpath <- file.path(output_folder, folder, "config")
  if (!is.null(output_folder) && !dir.exists(cfgpath)) {
    dir.create(cfgpath, recursive = T)
  }

  years <- c(1990,
             seq(2005, 2060, by = 5),
             seq(2070, 2110, by = 10),
             2130, 2150)
  ## load mappings
  REMIND2ISO_MAPPING = fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))[, .(iso = CountryCode,region = RegionCode)]

  GCAM2ISO_MAPPING = fread(system.file("extdata", "iso_GCAM.csv", package = "edgeTransport"))
  EDGE2teESmap = fread(system.file("extdata", "mapping_EDGE_REMIND_transport_categories.csv", package = "edgeTransport"))
  EDGE2CESmap = fread(system.file("extdata", "mapping_CESnodes_EDGE.csv", package = "edgeTransport"))

  print(paste0("You selected the ", tech_scen, " transport scenario."))
  print(paste0("You selected the ", SSP_scen, " socio-economic scenario."))

  #################################################
  ## LVL 0 scripts
  #################################################
  print("-- Start of level 0 scripts")

  mrr <- toolMrremind(SSP_scen, REMIND2ISO_MAPPING, cache_folder)

  ## function that loads raw data from the GCAM input files and
  ## modifies them, to make them compatible with EDGE setup
  ## disaggregated to ISO level
  ## demand in million pkm and tmk, EI in MJ/km
  print("-- load GCAM raw data")
  GCAM_data <- toolGCAMraw(input_folder, GCAM2ISO_MAPPING, mrr$GDP_country)
  ## add Hybrid Electric LF
  GCAM_data$load_factor = rbind(GCAM_data$load_factor,
                                GCAM_data$load_factor[technology == "BEV"][, technology := "Hybrid Electric"])
  ## function that loads the TRACCS/Eurostat data for Europe. Final units for demand: millionkm (tkm and pkm)
  ## needed at this point to be used in the intensity calculation below
  print("-- load EU data")
  if(!is.null(cache_folder) && file.exists(file.path(cache_folder, "load_EU_data.RDS"))) {
    EU_data <- readRDS(file.path(cache_folder, "load_EU_data.RDS"))
  } else {
    EU_data <- toolLoadEU(input_folder)
  }
  if(!is.null(cache_folder))
    saveRDS(EU_data, file = file.path(cache_folder, "load_EU_data.RDS"))

  ## define depreciation rate
  discount_rate_veh = 0.05   #Consumer discount rate for vehicle purchases (PSI based values)
  nper_amort_veh = 15    #Number of periods (years) over which vehicle capital payments are amortized
  fcr_veh = discount_rate_veh + discount_rate_veh/(((1+discount_rate_veh)^nper_amort_veh)-1)

  ## function that loads UCD costs and annual mileage, results are on ISO level: costs in 2005USD/vkm (2005USD/vkm), annual mileage in vkt/veh/yr (vehicle km traveled per year)
  print("-- load UCD database")
  UCD_output <- toolLoadUCD(input_folder = input_folder, fcr_veh = fcr_veh, years = years)
  ## function that loads PSI purchase costs, results are on an unspecified regional aggregation: costs in annualized 2005USD. Only years 2015 and 2040 are included.
  print("-- load PSI costs")
  PSI_costs <- toolPSICosts(input_folder = input_folder, years = years, fcr_veh = fcr_veh)
  ## function that loads CHN conventional trucks CAPEX and non/fuel OPEX, results on ISO level: costs in 2005USD/vkm
  print("-- load China truck costs")
  CHN_trucks <- toolCHNTrucksCosts(input_folder= input_folder, years = years)
  ## function that loads PSI intensities
  PSI_int <- toolPSIint(GCAM_data = GCAM_data, input_folder, PSI_dir="PSI", years)
  ## function that loads alternative trucks/buses costs and h2 planes
  altCosts <- toolAltHDV(UCD_output = UCD_output)
  ## function that merges costs, LF, energy intensity, annual mileage from the various sources.
  ## output units: costs in 2005$/pkm (or 2005$/tkm)
  ## LF in p/v (or t/v)
  ## energy intensity pkm/MJ
  ## Annual mileage in km/year. Every entry in the output is on ISO level
  print("-- merge costs, LF, annual mileage from the various sources")
  merged_data <- toolMergeDat(
    UCD_output= UCD_output, PSI_costs = PSI_costs, altCosts = altCosts,
    PSI_int=PSI_int, CHN_trucks = CHN_trucks, EU_data = EU_data,
    trsp_incent = mrr$trsp_incent, GDP_MER = mrr$GDP_POP_MER_country, fcr_veh = fcr_veh, nper_amort_veh=nper_amort_veh,
    GCAM_data = GCAM_data, SSP_scen = SSP_scen, Dem_Scen = demScen, years = years,
    REMIND2ISO_MAPPING = REMIND2ISO_MAPPING)

  if(storeRDS)
    saveRDS(merged_data, file = level0path("merged_data.RDS"))

  ## function that calculates VOT for each level and logit exponents for each level.Final values: VOT in [2005$/pkm]
  print("-- load value-of-time and logit exponents")
  VOT_lambdas <- toolVOTandExponents(GCAM_data, GDP_MER_country = mrr$GDP_MER_country,
                                     POP_country = mrr$POP_country, input_folder = input_folder)
  ## substitute lambda
  VOT_lambdas$logit_output$logit_exponent_FV[, logit.exponent := ifelse(logit.exponent==-8,-4,logit.exponent)]
  ## make freight less price sensitive
  VOT_lambdas$logit_output$logit_exponent_S3S[sector == "trn_freight", logit.exponent := -1]


  if(storeRDS){
    saveRDS(VOT_lambdas, file = level0path("logit_exp.RDS"))
  }


  ## produce regionalized versions, and ISO version of the tech_output and LF, as they are loaded on ISO level for EU. No conversion of units happening.
  print("-- generate REMIND level data")
  REMINDdat <- toolREMINDdat(merged_data = merged_data, VOT_lambdas = VOT_lambdas,
                             GDP_country = mrr$GDP_country,
                             REMIND2ISO_MAPPING = REMIND2ISO_MAPPING)


  if(storeRDS) {
    saveRDS(REMINDdat,
            file = level0path("REMINDdat.RDS"))
  }

  #################################################
  ## LVL 1 scripts
  #################################################
  print("-- Start of level 1 scripts")
  print("-- Harmonizing energy intensities to match IEA final energy balances")
  IEAbal_comparison <- toolIEAharmonization(int = REMINDdat$int, demKm = REMINDdat$dem, IEA = mrr$IEAbal)
  IEAbal_comparison$merged_intensity <- IEAbal_comparison$merged_intensity[year %in% years]
  if(storeRDS)
    saveRDS(IEAbal_comparison$merged_intensity, file = level1path("harmonized_intensities.RDS"))

  #Optional Energy Intensity improvements after 2020 depending on the tech Scen
  if(is.null(int_improvetab)){
    print("No path to a file with scenario-specific energy intensity improvements provided. Using default file.")
    ## select the right combination of techscen and SSP scen
    int_improvetab <- fread(system.file("extdata", "Intensity_improvements.csv", package = "edgeTransport"))[tech_scenario == tech_scen & SSP_scenario == SSP_scen]
  }

  if(nrow(int_improvetab) > 0){
    if (!is.null(output_folder)) {
      fwrite(int_improvetab, file.path(cfgpath, "int_improvetab.csv"))
    }
    IEAbal_comparison$merged_intensity <- toolAdjustIntensity(IEAbal_comparison$merged_intensity, int_improvetab, years)
  }

  print("-- Merge non-fuel prices with REMIND fuel prices")
  if(is.null(gdxPath)) {
    gdxPath <- file.path(input_folder, "REMIND/fulldata_EU.gdx")
  }

  if (!is.null(FEPricetab) && !is.null(output_folder)) {
    fwrite(FEPricetab, file.path(cfgpath, "FEPricetab.csv"))
  }
  REMIND_prices <- toolMergePrices(
    gdx = gdxPath,
    REMINDmapping = REMIND2ISO_MAPPING,
    REMINDyears = years,
    intensity_data = IEAbal_comparison$merged_intensity,
    nonfuel_costs = REMINDdat$NFcost,
    module = "edge_esm",
    FE_Pricetab = FEPricetab)

  if(storeRDS)
    saveRDS(REMIND_prices, file = level1path("full_prices.RDS"))



  ## function that calculates the inconvenience cost starting point between 1990 and 2020
  incocost <- toolIncocost(annual_mileage = REMINDdat$AM,
                           load_factor = REMINDdat$LF,
                           fcr_veh = fcr_veh,
                           REMINDp = REMIND_prices)

  if(storeRDS) {
    saveRDS(incocost,
            file = level0path("incocost.RDS"))
  }

  print("-- EDGE calibration")
  calibration_output <- toolCalibrateEDGEinconv(
    prices = REMIND_prices,
    tech_output = REMINDdat$dem,
    logit_exp_data = VOT_lambdas$logit_output,
    vot_data = REMINDdat$vt,
    price_nonmot = REMINDdat$pnm)


  if(storeRDS)
    saveRDS(calibration_output, file = level1path("calibration_output.RDS"))

  ## load baseline sw trend table (non-LDV)
  if(is.null(preftab)) {
    preftab <- system.file("extdata", "sw_trends.csv", package = "edgeTransport")
  }
  ptab <- fread(preftab, header=T)[SSP_scenario == SSP_scen][, SSP_scenario := NULL]
  if (!is.null(output_folder)) {
    fwrite(ptab, file.path(cfgpath, "sw_trends.csv"))
  }


  ## load mitigatin trends sw table
  if(is.null(mitab.path)) {
      mitab.path <- system.file("extdata", "edget-mitigation.csv", package="edgeTransport")
  }
  mitab <- fread(mitab.path, header = TRUE, check.names = TRUE)[
    SSP_scenario == SSP_scen & tech_scenario == tech_scen]
  if (!is.null(output_folder)) {
    fwrite(mitab, file.path(cfgpath, "edget-mitigation.csv"))
  }

  print("-- generating trends for inconvenience costs")
  prefs <- toolPreftrend(
    SWS = calibration_output$list_SW,
    ptab = ptab,
    incocost = incocost,
    calibdem = REMINDdat$dem,
    years = years,
    GDP_POP_MER = mrr$GDP_POP_MER,
    tech_scen = tech_scen,
    SSP_scen = SSP_scen,
    mitab = mitab
  )

  if(storeRDS)
    saveRDS(prefs, file = level1path("prefs.RDS"))

  #################################################
  ## LVL 2 scripts
  #################################################
  print("-- Start of level 2 scripts")
  ## LOGIT calculation
  print("-- LOGIT calculation: three iterations to provide endogenous update of inconvenience costs")
  ## filter out prices and intensities that are related to not used vehicles-technologies in a certain region
  REMIND_prices = merge(REMIND_prices, unique(prefs$FV_final_pref[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)

  #What was the purpose of this line? In the new preftab table, all vehicle classes are included to unify the structure. Vehicle classes that are not available
  #in the country, receive a zero sw. The missing input data for that vehcile classes causing NAs in the line underneath
  #IEAbal_comparison$merged_intensity = merge(IEAbal_comparison$merged_intensity, unique(prefs$FV_final_pref[!(vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype")) , c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)

  ## load inconvenience factor table for LDVs
  if(is.null(mitab4W.path)) {
    mitab4W.path <- system.file("extdata", "inconv_factor.csv", package = "edgeTransport")
  }

  preftab4W <- fread(mitab4W.path, header=T)[techscen == tech_scen & SSPscen == SSP_scen]
  if (!is.null(output_folder)) {
    fwrite(preftab4W, file.path(cfgpath, "inconv_factor.csv"))
  }

  totveh=NULL
  ## multiple iterations of the logit calculation - set to 3
  for (i in seq(1,3,1)) {

    ## 4W logit based on inconvenience cost formulation
    logit_data_4W <- toolCalculateLogitIncost(
      prices = REMIND_prices,
      vot_data = REMINDdat$vt,
      pref_data = prefs,
      logit_params = VOT_lambdas$logit_output,
      intensity_data = IEAbal_comparison$merged_intensity,
      price_nonmot = REMINDdat$pnm,
      ptab4W = preftab4W,
      totveh = totveh)

    logit_data <- toolCalculateLogitSW(
      prices = REMIND_prices,
      vot_data = REMINDdat$vt,
      pref_data = prefs,
      logit_params = VOT_lambdas$logit_output,
      intensity_data = IEAbal_comparison$merged_intensity,
      price_nonmot = REMINDdat$pnm,
      ptab4W = preftab4W,
      logit_data_4W = logit_data_4W)

    if(storeRDS) {
      saveRDS(logit_data_4W, level1path("logit_data_4W.RDS"))
      saveRDS(logit_data[["share_list"]], file = level1path("share_newvehicles.RDS"))
      saveRDS(logit_data[["pref_data"]], file = level1path("pref_data.RDS"))
      saveRDS(logit_data[["prices_list"]], file = level1path("prices_list.RDS"))
    }

    if(storeRDS)
      saveRDS(logit_data, file = level2path("logit_data.RDS"))

    shares <- logit_data[["share_list"]] ## shares of alternatives for each level of the logit function
    mj_km_data <- logit_data[["mj_km_data"]] ## energy intensity at a technology level

    prices <- logit_data[["prices_list"]] ## prices at each level of the logit function, 1990USD/pkm

    ## regression demand calculation
    print("-- performing demand regression")
    ## if NAVIGATE international task: use Business-Leisure version
    NAVIGATEintl <- FALSE
    if (NAVIGATEintl) {
      ## function that loads historical country-specific international aviation demand [billions RPK]
      print("-- prepare international aviation specific data")
      IntAv_Prep <- toolIntAvPreparation(input_folder= input_folder,
                                          GDP_country = mrr$GDP_country)

      ## Baseline demand regression run, for international aviation
      print("-- performing demand regression for Intl Av")
      NAVIGATE_intl_dem_base <- toolDemandRegNAVIGATEIntl(tech_output = REMINDdat$dem,
                                                           price_baseline = prices$S3S,
                                                           GDP_POP = mrr$GDP_POP,
                                                           REMIND_scenario = SSP_scen,
                                                           ICCT_data = IntAv_Prep,
                                                           input_folder = input_folder,
                                                           Baseline_Run = TRUE)

      ## regression demand calculation
      print("-- performing demand regression")
      ## demand in million km
      NAVIGATE_intl_dem <- toolDemandRegNAVIGATEIntl(tech_output = REMINDdat$dem,
                                                      price_baseline = prices$S3S,
                                                      GDP_POP = mrr$GDP_POP,
                                                      REMIND_scenario = SSP_scen,
                                                      ICCT_data = IntAv_Prep,
                                                      RPK_cap_baseline = NAVIGATE_intl_dem_base,
                                                      input_folder = input_folder,
                                                      Baseline_Run = FALSE)

      dem_regr = NAVIGATE_intl_dem[["D_star"]]

      if(storeRDS) {
        saveRDS(NAVIGATE_intl_dem[["D_star"]], file = level2path("demand_regression.RDS"))
        saveRDS(NAVIGATE_intl_dem[["D_star_av"]], file = level2path("demand_regression_aviation.RDS"))
      }


    } else {
      if(is.null(ssp_demreg.path)) {
        print("No path to a file with scenario-specific tuning parameters for the regression provided. Using default file.")
        ssp_demreg.path <- system.file("extdata", "ssp_regression_factors.csv", package="edgeTransport")
      }
      ssp_demreg_tab <- fread(ssp_demreg.path, header = TRUE)

      if(is.null(regional_demreg.path)) {
        print("No path to a file with region-specific tuning parameters for the regression provided. Using default file.")
        regional_demreg.path <- system.file("extdata", "regional_regression_factors.csv", package="edgeTransport")
      }
      reg_demreg_tab <- fread(regional_demreg.path, header = TRUE)

      if(is.null(demscen.path)) {
        print("No path to a file with demand scenario factors for the regression provided. Using default file.")
        demscen.path <- system.file("extdata", "demscen_factors.csv", package="edgeTransport")
      }
      demscen_factors <- fread(demscen.path, header = TRUE)[demandScen == demScen]
      if (!is.null(output_folder)) {
        fwrite(ssp_demreg_tab, file.path(cfgpath, "ssp_regression_factors.csv"))
        fwrite(reg_demreg_tab, file.path(cfgpath, "regional_regression_factors.csv"))
        fwrite(demscen_factors, file.path(cfgpath, "demscen_factors.csv"))
      }

      ## demand in million km
      dem_regr = toolDemandReg(tech_output = REMINDdat$dem,
                               price_baseline = prices$S3S,
                               GDP_POP = mrr$GDP_POP,
                               SSP_scen = SSP_scen,
                               ssp_factors = ssp_demreg_tab,
                               regional_factors = reg_demreg_tab,
                               demscen_factors = demscen_factors)
      if(storeRDS)
        saveRDS(dem_regr, file = level2path("demand_regression.RDS"))

    }

    ## calculate vintages (new shares, prices, intensity)
    prices$base=prices$base[,c("region", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "non_fuel_price", "tot_price", "fuel_price_pkm",  "tot_VOT_price", "sector_fuel")]
    vintages = toolCalcVint(shares = shares,
                        totdem_regr = dem_regr,
                        prices = prices,
                        mj_km_data = mj_km_data,
                        years = years)

    nas <- vintages[["shares"]]$FV_shares[is.na(share)]
    if(nrow(nas) > 0) {
      print("NAs found in FV vintage shares.")
      nas
      browser()
    }
    shares$FV_shares = vintages[["shares"]]$FV_shares
    prices = vintages[["prices"]]
    mj_km_data = vintages[["mj_km_data"]]


    if(storeRDS)
      saveRDS(vintages, file = level2path("vintages.RDS"))

    print("-- aggregating shares, intensity and demand along REMIND tech dimensions")
    shares_intensity_demand <- toolSharesIntensityDemand(
      logit_shares=shares,
      MJ_km_base=mj_km_data,
      REMINDyears=years,
      demand_input = dem_regr)

    demByTech <- shares_intensity_demand[["demand"]] ##in [-]
    intensity_remind <- shares_intensity_demand[["demandI"]] ##in million pkm/EJ
    norm_demand <- shares_intensity_demand[["demandF_plot_pkm"]] ## in million km

    nas <- norm_demand[is.na(demand_F)]
    if(nrow(nas) > 0) {
      print("NAs found in final demand output.")
      nas
      browser()
    }

    num_veh_stations = toolVehicleStations(
      norm_dem = norm_demand[
        subsector_L1 == "trn_pass_road_LDV_4W", ## only 4wheelers
        c("region", "year", "sector", "vehicle_type", "technology", "demand_F") ],
      ES_demand_all = dem_regr,
      intensity = intensity_remind,
      loadFactor = unique(REMINDdat$LF[,c("region", "year", "vehicle_type", "loadFactor")]),
      EDGE2teESmap = EDGE2teESmap,
      rep = TRUE)

    totveh = num_veh_stations$alltechdem

  }



  print("-- Calculating budget coefficients")
  REMINDCapCost <- toolCapCosts(
    base_price=prices$base,
    Fdemand_ES = shares_intensity_demand$demandF_plot_EJ,
    stations = num_veh_stations$stations,
    EDGE2CESmap = EDGE2CESmap,
    EDGE2teESmap = EDGE2teESmap,
    REMINDyears = years,
    scenario = scenario)


  ## full REMIND time range for inputs
  REMINDtall <- c(seq(1900,1985,5),
                  seq(1990, 2060, by = 5),
                  seq(2070, 2110, by = 10),
                  2130, 2150)

  if (storeRDS) {
    #Copy gdx file to output folder
    file.copy(gdxPath, output_folder)
    saveRDS(REMINDCapCost[["capCostPerTech"]], file = level2path("capCostPerTech.RDS"))
    saveRDS(shares, file = level2path("shares.RDS"))
    saveRDS(logit_data$annual_sales, file = level2path("annual_sales.RDS"))
    saveRDS(logit_data[["share_list"]], file = level2path("shares.RDS"))
    saveRDS(shares_intensity_demand$demandF_plot_mjkm,
            file = level2path("demandF_plot_mjkm.RDS"))
    saveRDS(shares_intensity_demand$demandF_plot_EJ,
            file=level2path("demandF_plot_EJ.RDS"))
    saveRDS(shares_intensity_demand$demandF_plot_pkm,
            level2path("demandF_plot_pkm.RDS"))
    saveRDS(logit_data$pref_data, file = level2path("pref_output.RDS"))
    saveRDS(REMINDdat$LF, file = level2path("loadFactor.RDS"))
    saveRDS(REMINDdat$AM, file = level2path("annual_mileage.RDS"))
    dem_bunk = merge(EU_data$dem_eurostat[vehicle_type %in% c("International Ship_tmp_vehicletype", "International Aviation_tmp_vehicletype")], REMIND2ISO_MAPPING, by = "iso")
    dem_bunk = dem_bunk[,.(MJ = sum(MJ)), by = c("region", "year", "vehicle_type")]
    saveRDS(dem_bunk, file = level2path("EurostatBunkers.RDS"))
    EU_data$roadFE_eu=EU=merge(EU_data$roadFE_eu[year %in% c(1990, 2005, 2010)], REMIND2ISO_MAPPING,by="iso")
    EU_data$roadFE_eu=EU_data$roadFE_eu[,.(EJ=sum(MJ)*1e-12), by = c("year","region","vehicle_type")]
    saveRDS(EU_data$roadFE_eu, file = level2path("TRACCS_FE.RDS"))

    ## do these two files *really* have to be provided by edgeTransport?
    saveRDS(mrr$POP, file = level2path("POP.RDS"))
    saveRDS(mrr$GDP, file = level2path("GDP.RDS"))

    saveRDS(IEAbal_comparison$IEA_dt2plot, file = level2path("IEAcomp.RDS"))
    md_template = level2path("report.Rmd")

    saveRDS(VOT_lambdas, file = level2path("logit_exp.RDS"))

    report <- toolReportEDGET(
      output_folder = file.path(output_folder, folder, "level_2"),
      extendedReporting = TRUE, scenario_title = paste0(tech_scen," ",SSP_scen),
      model_name = "EDGE-Transport",
      gdx = gdxPath)

    write.mif(report, file.path(output_folder, folder, "EDGE-T_SA.mif"))

    if(plot.report) {
      ## ship and run the file in the output folder
      file.copy(system.file("Rmd", "report.Rmd", package = "edgeTransport"),
                md_template, overwrite = T)
      render(md_template, output_format = "pdf_document")
    }
  }


  ## prepare the entries to be saved in the gdx files: intensity, shares, non_fuel_price. Final entries: intensity in [trillionkm/Twa], capcost in [trillion2005USD/trillionpkm], shares in [-]
  print("-- final preparation of input files")
  finalInputs <- toolPrepare4REMIND(
    demByTech = demByTech,
    intensity = intensity_remind,
    capCost = REMINDCapCost[["CESlevelCosts"]],
    EDGE2teESmap = EDGE2teESmap,
    REMINDtall = REMINDtall)


  ## calculate absolute values of demand. Final entry: demand in [trillionpkm]
  demand_traj <- toolREMINDdemand(regrdemand = dem_regr,
                                  EDGE2teESmap = EDGE2teESmap,
                                  REMINDtall = REMINDtall,
                                  SSP_scen = SSP_scen)

  print("-- preparing complex module-friendly output files")
  ## final value: in billionspkm or billions tkm and EJ; shares are in [-]
  complexValues <- toolComplexCompat(ESdem = shares_intensity_demand$demandF_plot_pkm,
                                     FEdem = shares_intensity_demand$demandF_plot_EJ,
                                     gdp_country = mrr$GDP_country,
                                     REMIND2ISO_MAPPING)

  print("-- generating CSV files to be transferred to mmremind")
  ## only the combinations (region, vehicle) present in the mix have to be included in costs
  NEC_data = merge(REMINDdat$NFcost,
                   unique(calibration_output$list_SW$VS1_final_SW[,c("region", "vehicle_type")]),
                   by =c("region", "vehicle_type"))
  capcost4W = merge(REMINDdat$costs[variable == "Capital costs (purchase)"],
                    unique(calibration_output$list_SW$VS1_final_SW[,c("region", "vehicle_type")]),
                    by =c("region", "vehicle_type"))

  if (demScen == "default") {
    demScen <- SSP_scen
  }

  ## save the output csv files or create a list of objects
  EDGETrData = toolCreateOutput(
    logit_params = VOT_lambdas$logit_output,
    pref_data = logit_data$pref_data,
    ptab4W = preftab4W,
    vot_data = REMINDdat$vt,
    int_dat = IEAbal_comparison$merged_intensity,
    NEC_data = NEC_data,
    capcost4W = capcost4W,
    demByTech = finalInputs$demByTech,
    intensity = finalInputs$intensity,
    capCost = finalInputs$capCost,
    price_nonmot = REMINDdat$pnm,
    complexValues = complexValues,
    load_Factor = REMINDdat$LF,
    annual_mileage = REMINDdat$AM,
    demISO = merged_data$dem,
    SSP_scen = SSP_scen,
    DEM_scen = demScen,
    EDGE_scenario = tech_scen,
    level2path = level2path,
    output_folder = output_folder)

  if (!is.null(EDGETrData)) {
    return(EDGETrData)
  } else {
    return(file.path(output_folder, folder))
  }

}

#' Generate EDGE-Transport Input Data for the REMIND model, madrat interface.
#'
#' `calcgenerateEDGEdata()` is a wrapper for `generateEDGEdata()` to make use of
#' madrat caching.
#'
#' @md
#' @param input_folder folder hosting raw data
#' @param output_folder folder hosting REMIND input files. If NULL, a list of magclass objects is returned (set this option in case of a REMIND preprocessing run)
#' @param cache_folder folder hosting a "local" cache (this is not the mrremid cache, it is specific to EDGE-T).
#' @param SSP_scen SSP or SDP scenario
#' @param tech_scen EDGE-T technology scenario. Options are: ConvCase, ElecEra, HydrHype (working with SSP2 only!)
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression.
#' @param storeRDS optional saving of intermediate RDS files, only possible if output folder is not NULL
#' @param gdxPath optional path to a GDX file to load price signals from a REMIND run.
#' @param preftab path to file with trends for share weights
#' @param mitab4W.path path to file with key factors for 4W technologies for different mitigation ambition and SSP scenarios.
#' @param mitab.path path to file with key factors for share weight trends for different mitigation ambition and SSP scenarios.
#' @param ssp_demreg.path path to file with key factors for the demand regression depending on SSP/SDP scenarios.
#' @param regional_demreg.path path to file with key factors for the demand regression depending on regions and SSP scenarios.
#' @param plot.report write a report which is place in the level2 folder. Defaults to FALSE.
#' @return generated EDGE-transport input data
#' @author Alois Dirnaichner, Marianna Rottoli
#' @export
#' @rdname generateEDGEdata

calcgenerateEDGEdata <- function(input_folder, output_folder,
                                 cache_folder = NULL, SSP_scen = "SSP2",
                                 tech_scen = "Mix", demScen = "default",
                                 storeRDS = FALSE,
                                 gdxPath = NULL,
                                 preftab = NULL, plot.report = FALSE,
                                 mitab4W.path = NULL, mitab.path = NULL,
                                 ssp_demreg.path = NULL,
                                 regional_demreg.path = NULL) {

  return(list(
    x = toolGenerateEDGEdata(input_folder, output_folder, cache_folder,  SSP_scen,
                         tech_scen, demScen, storeRDS,
                         gdxPath, preftab, plot.report,
                         mitab4W.path, mitab.path, ssp_demreg.path,
                         regional_demreg.path),
    class = 'list',
    unit = NA,
    description = NA))

}
