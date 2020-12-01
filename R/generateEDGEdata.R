#' Generate EDGE-Transport Input Data for the REMIND model.
#'
#' Run this script to prepare the input data for EDGE in EDGE-friendly units and regional aggregation
#' @param input_folder folder hosting raw data
#' @param output_folder folder hosting REMIND input files
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param REMIND_scenario SSP scenario
#' @param saveRDS optional saving of intermediate RDS files
#'
#' @return generated EDGE-transport input data
#' @author Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @import mrremind
#' @import edgeTrpLib
#' @importFrom madrat setConfig
#' @importFrom magclass getSets
#' @export


generateEDGEdata <- function(input_folder, output_folder,
                             EDGE_scenario, REMIND_scenario="SSP2",
                             saveRDS=FALSE){

  scenario <- scenario_name <- vehicle_type <- type <- `.` <- CountryCode <- RegionCode <- NULL
  
  setConfig(forcecache = TRUE)

  levelNpath <- function(fname, N){
    path <- file.path(output_folder, REMIND_scenario, EDGE_scenario, paste0("level_", N))
    if(!dir.exists(path)){
      dir.create(path, recursive = T)
    }
    return(file.path(path, fname))
  }

  level0path <- function(fname){
    levelNpath(fname, 0)
  }

  level1path <- function(fname){
    levelNpath(fname, 1)
  }

  level2path <- function(fname){
    levelNpath(fname, 2)
  }


  years <- c(1990,
             seq(2005, 2060, by = 5),
             seq(2070, 2110, by = 10),
             2130, 2150)
  ## load mappings
  REMIND2ISO_MAPPING = fread(system.file("extdata", "regionmappingH12.csv", package = "edgeTransport"))[, .(iso = CountryCode,region = RegionCode)]
  EDGEscenarios = fread(system.file("extdata", "EDGEscenario_description.csv", package = "edgeTransport"))
  GCAM2ISO_MAPPING = fread(system.file("extdata", "iso_GCAM.csv", package = "edgeTransport"))
  EDGE2teESmap = fread(system.file("extdata", "mapping_EDGE_REMIND_transport_categories.csv", package = "edgeTransport"))
  EDGE2CESmap = fread(system.file("extdata", "mapping_CESnodes_EDGE.csv", package = "edgeTransport"))
  
  ## load specific transport switches
  EDGEscenarios <- EDGEscenarios[scenario_name == EDGE_scenario]

  selfmarket_taxes <- EDGEscenarios[options == "selfmarket_taxes", switch]
  print(paste0("You selected self-sustaining market, option Taxes to: ", selfmarket_taxes))
  enhancedtech <- EDGEscenarios[options== "enhancedtech", switch]
  print(paste0("You selected the option to select an optimistic trend of costs/performances of alternative technologies to: ", enhancedtech))
  rebates_febates <- EDGEscenarios[options== "rebates_febates", switch]
  print(paste0("You selected the option to include rebates and ICE costs markup to: ", rebates_febates))
  smartlifestyle <- EDGEscenarios[options== "smartlifestyle", switch]
  print(paste0("You selected the option to include lifestyle changes to: ", smartlifestyle))

  if (EDGE_scenario %in% c("ConvCase", "ConvCaseWise")) {
    techswitch <- "Liquids"
  } else if (EDGE_scenario %in% c("ElecEra", "ElecEraWise")) {
    techswitch <- "BEV"
  } else if (EDGE_scenario %in% c("HydrHype", "HydrHypeWise")) {
    techswitch <- "FCEV"
  } else {
    print("You selected a not allowed scenario. Scenarios allowed are: ConvCase, ConvCaseWise, ElecEra, ElecEraWise, HydrHype, HydrHypeWise")
    quit()
  }

  print(paste0("You selected the ", EDGE_scenario, " transport scenario."))
  print(paste0("You selected the ", REMIND_scenario, " socio-economic scenario."))

  #################################################
  ## LVL 0 scripts
  #################################################
  print("-- Start of level 0 scripts")
  
  ## function that loads raw data from the GCAM input files and modifies them, to make them compatible with EDGE setup
  ## demand in million pkm and tmk, EI in MJ/km
  print("-- load GCAM raw data")
  GCAM_data <- lvl0_GCAMraw(input_folder)

  ##function that loads PSI energy intensity for Europe and merges them with GCAM intensities. Final values: MJ/km (pkm and tkm)
  print("-- merge PSI energy intensity data")
  intensity_PSI_GCAM_data <- lvl0_mergePSIintensity(GCAM_data, input_folder, enhancedtech = enhancedtech, techswitch = techswitch)
  GCAM_data$conv_pkm_mj = intensity_PSI_GCAM_data

  if(saveRDS)
    saveRDS(intensity_PSI_GCAM_data, file = level0path("intensity_PSI_GCAM.RDS"))

  ## function that calculates VOT for each level and logit exponents for each level.Final values: VOT in [1990$/pkm]
  print("-- load value-of-time and logit exponents")
  VOT_lambdas=lvl0_VOTandExponents(GCAM_data, REMIND_scenario, input_folder, GCAM2ISO_MAPPING)

  ## function that loads and prepares the non_fuel prices. It also load PSI-based purchase prices for EU. Final values: non fuel price in 1990USD/pkm (1990USD/tkm), annual mileage in vkt/veh/yr (vehicle km traveled per year),non_fuel_split in 1990USD/pkt (1990USD/tkm)
  print("-- load UCD database")
  UCD_output <- lvl0_loadUCD(GCAM_data = GCAM_data, EDGE_scenario = EDGE_scenario, REMIND_scenario = REMIND_scenario, GCAM2ISO_MAPPING = GCAM2ISO_MAPPING,
                            input_folder = input_folder, years = years, enhancedtech = enhancedtech, selfmarket_taxes = selfmarket_taxes, rebates_febates = rebates_febates, techswitch = techswitch)



  ## function that applies corrections to GCAM outdated data. No conversion of units happening.
  print("-- correct tech output")
  correctedOutput <- lvl0_correctTechOutput(GCAM_data,
                                            UCD_output$non_energy_cost,
                                            VOT_lambdas$logit_output)

  GCAM_data[["tech_output"]] = correctedOutput$GCAM_output$tech_output
  GCAM_data[["conv_pkm_mj"]] = correctedOutput$GCAM_output$conv_pkm_mj
  UCD_output$non_energy_cost$non_energy_cost = correctedOutput$NEcost$non_energy_cost
  UCD_output$non_energy_cost$non_energy_cost_split = correctedOutput$NEcost$non_energy_cost_split
  VOT_lambdas$logit_output = correctedOutput$logitexp

  if(saveRDS){
    saveRDS(GCAM_data, file = level0path("GCAM_data.RDS"))
    saveRDS(UCD_output, file = level0path("UCD_output.RDS"))
    saveRDS(VOT_lambdas, file = level0path("logit_exp.RDS"))
  }


  ## produce regionalized versions, and ISO version of the tech_output. No conversion of units happening.
  print("-- generate ISO level data")
  iso_data <- lvl0_toISO(
    input_data = GCAM_data,
    VOT_data = VOT_lambdas$VOT_output,
    price_nonmot = VOT_lambdas$price_nonmot,
    UCD_data = UCD_output,
    GCAM2ISO_MAPPING = GCAM2ISO_MAPPING,
    REMIND2ISO_MAPPING = REMIND2ISO_MAPPING,
    EDGE_scenario = EDGE_scenario,
    REMIND_scenario = REMIND_scenario)

    ## function that loads the TRACCS data for Europe. Final units for demand: millionkm (tkm and pkm)
    print("-- load EU TRACCS data")
    TRACCS_data <- lvl0_loadTRACCS(input_folder)
    if(saveRDS)
      saveRDS(TRACCS_data, file = level0path("load_TRACCS_data.RDS"))

    ## function that makes the TRACCS database compatible with the GCAM framework. Final values: EI in MJ/km (pkm and tkm), demand in million km (pkm and tkm), LF in p/v
    print("-- prepare the EU TRACCS database")
    TRACCS_EI_dem_LF <- lvl0_prepareTRACCS(TRACCS_data = TRACCS_data,
                                           GCAM_data = GCAM_data,
                                           intensity = intensity_PSI_GCAM_data,
                                           input_folder = input_folder,
                                           GCAM2ISO_MAPPING = GCAM2ISO_MAPPING)

    print("-- merge the EU TRACCS database (energy intensity, load factors and energy services)")
    iso_data$tech_output <- lvl0_mergeTRACCS(
      TRACCS_data = TRACCS_EI_dem_LF$dem_TRACCS,  ## demand in million pkm or million tkm
      output = iso_data$TO_iso,
      REMIND2ISO_MAPPING = REMIND2ISO_MAPPING)

  ## function that calculates the inconvenience cost starting point between 1990 and 2020
  incocost <- lvl0_incocost(annual_mileage = iso_data$UCD_results$annual_mileage,
                            load_factor = iso_data$UCD_results$load_factor,
                            fcr_veh = UCD_output$fcr_veh)


  if(saveRDS){
    saveRDS(iso_data$iso_VOT_results,
            file = level0path("VOT_iso.RDS"))
    saveRDS(iso_data$iso_pricenonmot_results,
            file = level0path("price_nonmot_iso.RDS"))
    saveRDS(iso_data$iso_UCD_results$nec_cost_split_iso,
            file = level0path("UCD_NEC_split_iso.RDS"))
    saveRDS(iso_data$iso_UCD_results$annual_mileage_iso,
            file = level0path("UCD_mileage_iso.RDS"))
    saveRDS(iso_data$iso_UCD_results$nec_iso,
            file = level0path("UCD_NEC_iso.RDS"))
    saveRDS(iso_data$iso_GCAMdata_results,
            file = level0path("GCAM_data_iso.RDS"))
  }

  #################################################
  ## LVL 1 scripts
  #################################################
  print("-- Start of level 1 scripts")
  
  print("-- Harmonizing energy intensities to match IEA final energy balances")
  intensity_gcam <- lvl1_IEAharmonization(tech_data = iso_data)
  if(saveRDS)
    saveRDS(intensity_gcam, file = level1path("harmonized_intensities.RDS"))

  print("-- Merge non-fuel prices with REMIND fuel prices")
  REMIND_prices <- merge_prices(
    gdx = file.path(input_folder, "REMIND/fulldata.gdx"),
    REMINDmapping = REMIND2ISO_MAPPING,
    REMINDyears = years,
    intensity_data = intensity_gcam,
    nonfuel_costs = iso_data$UCD_results$nec_cost[type == "normal"][, type := NULL],
    module = "edge_esm")

  ## hotfix: fill in advanced electric trains
  REMIND_prices[, non_fuel_price := ifelse(is.na(non_fuel_price), mean(non_fuel_price, na.rm = TRUE), non_fuel_price), by = c("technology", "vehicle_type", "year")]
  REMIND_prices[, tot_price := non_fuel_price+fuel_price_pkm]
  ##################### fin qua
  if(saveRDS)
    saveRDS(REMIND_prices, file = level1path("full_prices.RDS"))


  print("-- EDGE calibration")
  calibration_output <- lvl1_calibrateEDGEinconv(
    prices = REMIND_prices,
    tech_output = iso_data$tech_output,
    logit_exp_data = VOT_lambdas$logit_output,
    vot_data = iso_data$vot,
    price_nonmot = iso_data$price_nonmot)

  if(saveRDS)
    saveRDS(calibration_output, file = level1path("calibration_output.RDS"))

  print("-- cluster regions for share weight trends")
  clusters_overview <- lvl1_SWclustering(
    input_folder = input_folder,
    REMIND_scenario = REMIND_scenario,
    REMIND2ISO_MAPPING)

  density=clusters_overview[[1]]
  clusters=clusters_overview[[2]]
  
  if(saveRDS){
    saveRDS(clusters, file = level1path("clusters.RDS"))
    saveRDS(density, file = level1path("density.RDS"))
  }

  print("-- generating trends for inconvenience costs")
  prefs <- lvl1_preftrend(SWS = calibration_output$list_SW,
                          clusters = clusters,
                          incocost = incocost,
                          calibdem = iso_data$tech_output,
                          years = years,
                          REMIND2ISO_MAPPING = REMIND2ISO_MAPPING,
                          REMIND_scenario = REMIND_scenario,
                          EDGE_scenario = EDGE_scenario,
                          smartlifestyle = smartlifestyle,
                          techswitch = techswitch)
    
  if(saveRDS)
    saveRDS(prefs, file = level1path("prefs.RDS"))



  #################################################
  ## LVL 2 scripts
  #################################################
  print("-- Start of level 2 scripts")
  ## LOGIT calculation
  print("-- LOGIT calculation")
  ## two options of logit calculation: one is on partially on inconvenience costs and partially on preferences, the other is exclusively on preferences
  ## filter out prices and intensities that are related to not used vehicles-technologies in a certain region
  REMIND_prices = merge(REMIND_prices, unique(prefs$FV_final_pref[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)
  intensity_gcam = merge(intensity_gcam, unique(prefs$FV_final_pref[!(vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype")) , c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)
  logit_data <- calculate_logit_inconv_endog(
    prices = REMIND_prices,
    vot_data = iso_data$vot,
    pref_data = prefs,
    logit_params = VOT_lambdas$logit_output,
    intensity_data = intensity_gcam,
    price_nonmot = iso_data$price_nonmot,
    techswitch = techswitch)

  if(saveRDS){
    saveRDS(logit_data[["share_list"]], file = level1path("share_newvehicles.RDS"))
    saveRDS(logit_data[["pref_data"]], file = level1path("pref_data.RDS"))
  }

  if(saveRDS)
    saveRDS(logit_data, file = level2path("logit_data.RDS"))

  shares <- logit_data[["share_list"]] ## shares of alternatives for each level of the logit function
  mj_km_data <- logit_data[["mj_km_data"]] ## energy intensity at a technology level
  prices <- logit_data[["prices_list"]] ## prices at each level of the logit function, 1990USD/pkm

  ## regression demand calculation
  print("-- performing demand regression")
  dem_regr = lvl2_demandReg(tech_output = iso_data$tech_output, 
                          price_baseline = prices$S3S, 
                          REMIND_scenario = REMIND_scenario, 
                          smartlifestyle = smartlifestyle)

  if(saveRDS)
    saveRDS(dem_regr, file = level2path("demand_regression.RDS"))


  ## calculate vintages (new shares, prices, intensity)
  vintages = calcVint(shares = shares,
                      totdem_regr = dem_regr,
                      prices = prices,
                      mj_km_data = mj_km_data,
                      years = years)


  shares$FV_shares = vintages[["shares"]]$FV_shares
  prices = vintages[["prices"]]
  mj_km_data = vintages[["mj_km_data"]]


 if(saveRDS)
    saveRDS(vintages, file = level2path("vintages.RDS"))
  
  print("-- aggregating shares, intensity and demand along REMIND tech dimensions")
  shares_intensity_demand <- shares_intensity_and_demand(
    logit_shares=shares,
    MJ_km_base=mj_km_data,
    EDGE2CESmap=EDGE2CESmap,
    REMINDyears=years,
    demand_input = dem_regr)
  
  demByTech <- shares_intensity_demand[["demand"]] ##in [-]
  intensity_remind <- shares_intensity_demand[["demandI"]] ##in million pkm/EJ


  print("-- Calculating budget coefficients")
  budget <- calculate_capCosts(
    base_price=prices$base,
    Fdemand_ES = shares_intensity_demand$demandF_plot_EJ,
    EDGE2CESmap = EDGE2CESmap,
    EDGE2teESmap = EDGE2teESmap,
    REMINDyears = years,
    scenario = scenario)

  ## full REMIND time range for inputs
  REMINDtall <- c(seq(1900,1985,5),
                  seq(1990, 2060, by = 5),
                  seq(2070, 2110, by = 10),
                  2130, 2150)

  ## prepare the entries to be saved in the gdx files: intensity, shares, non_fuel_price. Final entries: intensity in [trillionkm/Twa], capcost in [trillion2005USD/trillionpkm], shares in [-]
  print("-- final preparation of input files")
  finalInputs <- prepare4REMIND(
    demByTech = demByTech,
    intensity = intensity_remind,
    capCost = budget,
    EDGE2teESmap = EDGE2teESmap,
    REMINDtall = REMINDtall)


  ## calculate absolute values of demand. Final entry: demand in [trillionpkm]
  demand_traj <- lvl2_REMINDdemand(regrdemand = dem_regr,
                                   EDGE2teESmap = EDGE2teESmap,
                                   REMINDtall = REMINDtall,
                                   REMIND_scenario = REMIND_scenario)

  print("-- preparing complex module-friendly output files")
  ## final value: in billionspkm or billions tkm and EJ; shares are in [-]
  complexValues <- lvl2_reportingEntries(ESdem = shares_intensity_demand$demandF_plot_pkm,
                                         FEdem = shares_intensity_demand$demandF_plot_EJ)

  print("-- generating CSV files to be transferred to mmremind")
  ## only the combinations (region, vehicle) present in the mix have to be included in costs
  NEC_data = merge(iso_data$UCD_results$nec_cost,
                   unique(calibration_output$list_SW$VS1_final_SW[,c("region", "vehicle_type")]),
                   by =c("region", "vehicle_type"))
  capcost4W = merge(iso_data$UCD_results$capcost4W,
                    unique(calibration_output$list_SW$VS1_final_SW[,c("region", "vehicle_type")]),
                    by =c("region", "vehicle_type"))
  
  lvl2_createCSV_inconv(
    logit_params = VOT_lambdas$logit_output,
    pref_data = logit_data$pref_data,
    vot_data = iso_data$vot,
    int_dat = intensity_gcam,
    NEC_data = NEC_data,
    capcost4W = capcost4W,
    demByTech = finalInputs$demByTech,
    intensity = finalInputs$intensity,
    capCost = finalInputs$capCost,
    demand_traj = demand_traj,
    price_nonmot = iso_data$price_nonmot,
    complexValues = complexValues,
    loadFactor = iso_data$UCD_results$load_factor,
    REMIND_scenario = REMIND_scenario,
    EDGE_scenario = EDGE_scenario,
    level2path = level2path)

}
