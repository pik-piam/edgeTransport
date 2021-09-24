#' Generate EDGE-Transport Input Data for the REMIND model.
#'
#' Run this script to prepare the input data for EDGE in EDGE-friendly units and regional aggregation
#' @param input_folder folder hosting raw data
#' @param output_folder folder hosting REMIND input files. If NULL, a list of magclass objects is returned (set this option in case of a REMIND preprocessing run)
#' @param SSP_scen SSP or SDP scenario
#' @param techscen EDGE-T technology scenario. Options are: ConvCase, ElecEra, HydrHype (working with SSP2 only!)
#' @param IEAbal use mrremind generated data: in case of a REMIND preprocessing run, load population.  Product of: calcOutput("IO", subtype = "IEA_output", aggregate = TRUE)
#' @param GDP_country use mrremind generated data: in case of a REMIND preprocessing run, load GDP.  Product of: calcOutput("GDPppp", aggregate =F)
#' @param RatioPPP2MER_country use mrremind generated data: in case of a REMIND preprocessing run, load ratio between PPP and MER GDP.  Product of: calcOutput("RatioPPP2MER", aggregate =F)
#' @param POP_country use mrremind generated data: in case of a REMIND preprocessing run, load IEA balances. Product of: calcOutput("Population", aggregate =F)
#' @param JRC_IDEES_Trsp use mrremind generated data: in case of a REMIND preprocessing run, load JRC_IDEE data. Product of: calcOutput("JRC_IDEES", subtype="Transport")
#' @param JRC_IDEES_MarBunk use mmrremind generated data: in case of a REMIND preprocessing run, load JRC_IDEE data. Product of: calcOutput("JRC_IDEES", subtype="maritimeBunkers")
#' @param trsp_incent use mmrremind generated data: in case of a REMIND preprocessing run, load transportSubsidies data. Product of: readSource("TransportSubsidies", convert=T)
#' @param storeRDS optional saving of intermediate RDS files
#' @param loadRDS optional load intermediate RDS files for input data to save time
#'
#' @return generated EDGE-transport input data
#' @author Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @importFrom edgeTrpLib merge_prices calculate_logit_inconv_endog calcVint shares_intensity_and_demand calculate_capCosts prepare4REMIND calc_num_vehicles_stations
#' @importFrom rmarkdown render
#' @export


generateEDGEdata <- function(input_folder, output_folder, SSP_scen="SSP2", techscen = NULL,
                             IEAbal=NULL, GDP_country=NULL, RatioPPP2MER_country = NULL, POP_country=NULL, JRC_IDEES_Trsp=NULL, JRC_IDEES_MarBunk=NULL, trsp_incent=NULL,
                             storeRDS=FALSE, loadRDS=FALSE){
  scenario <- scenario_name <- vehicle_type <- type <- `.` <- CountryCode <- RegionCode <- technology <- NULL
  non_fuel_price <- tot_price <- fuel_price_pkm <- subsector_L1 <- loadFactor <- ratio <- NULL
  Year <- value <- DP_cap <- POP_val <- GDP_cap <- region <- weight <- MJ <- variable.unit <- EJ <- grouping_value <- NULL
  sector <- variable <- region <- logit.exponent <- EDGETscen <- SSPscen <- default <- NULL
  levelNpath <- function(fname, N){
    path <- file.path(output_folder, SSP_scen, EDGE_scenario, paste0("level_", N))
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
  REMIND2ISO_MAPPING = fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))[, .(iso = CountryCode,region = RegionCode)]
  EDGEscenarios = fread(system.file("extdata", "EDGEscenario_description.csv", package = "edgeTransport"))
  GCAM2ISO_MAPPING = fread(system.file("extdata", "iso_GCAM.csv", package = "edgeTransport"))
  EDGE2teESmap = fread(system.file("extdata", "mapping_EDGE_REMIND_transport_categories.csv", package = "edgeTransport"))
  EDGE2CESmap = fread(system.file("extdata", "mapping_CESnodes_EDGE.csv", package = "edgeTransport"))

  ## load specific transport switches
  EDGEscenarios <- EDGEscenarios[SSPscen == SSP_scen]
  if (is.null(techscen)){
    EDGEscenarios <- EDGEscenarios[default == TRUE,]
  } else if (SSP_scen=="SSP2" & techscen %in% c("ConvCase", "ElecEra", "HydrHype")){
    ## the non-default scenario allow for selecting e.g. SSP2+ElecEra or SSP2+HydrHype
    EDGEscenarios <- EDGEscenarios[EDGETscen == techscen,]
  } else {
    print("Either set techscen to NULL, or use the non-default allowed combinations: SSP2->ElecEra, SSP2->ConvCase, SSP2->HydrHype")
    quit()
  }

  EDGE_scenario <- EDGEscenarios[, EDGETscen]

  smartlifestyle <- EDGEscenarios[, smartlifestyle]

  if (EDGE_scenario %in% c("ConvCase")) {
    techswitch <- "Liquids"
  } else if (EDGE_scenario %in% c("ElecEra", "ElecEraWise")) {
    techswitch <- "BEV"
  } else if (EDGE_scenario %in% c("Mix")) {
    techswitch <- "Liq_El"
  } else if (EDGE_scenario %in% c("HydrHype")) {
    techswitch <- "FCEV"
  }

  print(paste0("You selected the ", EDGE_scenario, " transport scenario."))
  print(paste0("You selected the ", SSP_scen, " socio-economic scenario."))
  print(paste0("You selected the option to include lifestyle changes to: ", smartlifestyle))

  if (!is.null(techscen)) {
    print(paste0("This is a non-default SSP-transport scenario match. Is it intended?"))
  }
  #################################################
  ## LVL 0 scripts
  #################################################
  print("-- Start of level 0 scripts")

  ## load the GDP, POP, IEA from default input data if not provided
  mrremind_folder = file.path(input_folder, "mrremind")
  if (is.null(IEAbal)) IEAbal = readRDS(file.path(mrremind_folder, "IEAbal.RDS"))
  if (is.null(GDP_country)) GDP_country = readRDS(file.path(mrremind_folder, "GDP_country.RDS"))
  if (is.null(RatioPPP2MER_country)) RatioPPP2MER_country = readRDS(file.path(mrremind_folder, "RatioPPP2MER_country.RDS"))
  if (is.null(POP_country)) POP_country = readRDS(file.path(mrremind_folder, "POP_country.RDS"))
  if (is.null(JRC_IDEES_Trsp)) JRC_IDEES_Trsp = readRDS(file.path(mrremind_folder, "JRC_IDEES_Trsp.RDS"))
  if (is.null(JRC_IDEES_MarBunk)) JRC_IDEES_MarBunk = readRDS(file.path(mrremind_folder, "JRC_IDEES_MarBunk.RDS"))
  if (is.null(trsp_incent)) trsp_incent = readRDS(file.path(mrremind_folder, "trasp_incent.RDS"))


  ## rearrange the columns and create regional values
  GDP_country = GDP_country[,,paste0("gdp_", SSP_scen)]
  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(GDP_country, old = "value", new = "weight")
  GDP = merge(GDP_country, REMIND2ISO_MAPPING, by.x = "ISO3", by.y = "iso")
  GDP = GDP[,.(weight = sum(weight)), by = c("region", "year")]
  setnames(GDP_country, c("ISO3"), c("iso"))

  RatioPPP2MER_country = as.data.table(RatioPPP2MER_country)
  RatioPPP2MER_country = RatioPPP2MER_country[, c("year", "d3") := NULL]
  setnames(RatioPPP2MER_country, old = "value", new = "ratio")

  GDP_MER_country = merge(GDP_country, RatioPPP2MER_country, by.x = "iso", by.y = "Region")
  GDP_MER_country[, weight := weight*ratio]
  GDP_MER_country[, ratio := NULL]
  GDP_MER = merge(GDP_MER_country, REMIND2ISO_MAPPING, by = "iso")
  GDP_MER = GDP_MER[,.(weight = sum(weight)), by = c("region", "year")]

  POP_country = POP_country[,,paste0("pop_", SSP_scen)]
  POP_country <- as.data.table(POP_country)
  POP_country[, year := as.numeric(gsub("y", "", year))]
  POP = merge(POP_country, REMIND2ISO_MAPPING, by.x = "iso2c", by.y = "iso")
  POP = POP[,.(value = sum(value)), by = c("region", "year")]
  setnames(POP_country, old = c("iso2c", "variable"), new = c("iso", "POP"),skip_absent=TRUE)

  GDP_POP=merge(GDP,POP[,.(region,year,POP_val=value)],all = TRUE,by=c("region","year"))
  GDP_POP[,GDP_cap:=weight/POP_val]


  GDP_POP_MER=merge(GDP_MER,POP[,.(region,year,POP_val=value)],all = TRUE,by=c("region","year"))
  GDP_POP_MER[,GDP_cap:=weight/POP_val]

  GDP_POP_MER_country = merge(GDP_MER_country, POP_country,by = c("iso", "year"))

  ## filter only non-NA countries from IDEES and transform in data table
  JRC_IDEES=rbind(
    as.data.table(JRC_IDEES_Trsp), as.data.table(JRC_IDEES_MarBunk))
  JRC_IDEES=JRC_IDEES[grep("nergy consumption",variable.unit)]
  JRC_IDEES=JRC_IDEES[!grep("Shares",variable.unit)]
  JRC_IDEES=merge(JRC_IDEES,REMIND2ISO_MAPPING,by.x="iso3c",by.y="iso")
  JRC_IDEES=JRC_IDEES[,.(value=sum(value)),by = c("variable.unit","region","year")]
  JRC_IDEES[,year :=as.numeric(gsub("y","",year))]
  JRC_IDEES=JRC_IDEES[year %in%c(1990,2000,2005,2010,2015)]
  JRC_IDEES[, EJ:= value*  ##in ktoe
                   1e3*    ## in toe
                   4.1868E-8]  ## in EJ
  useful_var = c(
    ## international shipping
    "Maritime Bunkers|Energy Consumption|Total energy consumption.ktoe",
    ## international aviation
    "Transport|Aviation|Energy Consumption|Total energy consumption|Passenger transport|International - Extra-EU.ktoe",
    "Transport|Aviation|Energy Consumption|Total energy consumption|Passenger transport|International - Intra-EU.ktoe",
    ## domestic shipping
    "Transport|Navigation|Energy Consumption|Total energy consumption.ktoe",
    ## domestic aviation
    "Transport|Aviation|Energy Consumption|Total energy consumption|Passenger transport|Domestic.ktoe",
    ## road freight
    "Transport|Energy consumption|Freight transport|Road transport|Heavy duty vehicles.ktoe",
    ## road passenger LDVs
    "Transport|Energy consumption|Passenger transport|Road transport|Passenger cars.ktoe",
    ## road passenger HDVs
    "Transport|Road|Energy Consumption|Total energy consumption|Passenger transport|Motor coaches, buses and trolley buses.ktoe",
    ## rail passenger
    "Transport|Rail|Energy Consumption|Total energy consumption|Passenger transport|Conventional passenger trains.ktoe",
    ## rail freight
    "Transport|Rail|Energy Consumption|Total energy consumption|Freight transport.ktoe"
  )

  JRC_IDEES = JRC_IDEES[variable.unit %in% useful_var]


  ## function that loads raw data from the GCAM input files and
  ## modifies them, to make them compatible with EDGE setup
  ## disaggregated to ISO level
  ## demand in million pkm and tmk, EI in MJ/km
  print("-- load GCAM raw data")
  GCAM_data <- lvl0_GCAMraw(input_folder, GCAM2ISO_MAPPING, GDP_country)
  ## add Hybrid Electric LF
  GCAM_data$load_factor = rbind(GCAM_data$load_factor,
                                GCAM_data$load_factor[technology == "BEV"][, technology := "Hybrid Electric"])

  ## function that loads the TRACCS/Eurostat data for Europe. Final units for demand: millionkm (tkm and pkm)
  ## needed at this point to be used in the intensity calculation below
  print("-- load EU data")
  if(loadRDS){
    EU_data <- readRDS(level0path("load_EU_data.RDS"))
  }else{
    EU_data <- lvl0_loadEU(input_folder)
  }
  if(storeRDS)
     saveRDS(EU_data, file = level0path("load_EU_data.RDS"))

  ## define depreciation rate
  discount_rate_veh = 0.05   #Consumer discount rate for vehicle purchases (PSI based values)
  nper_amort_veh = 15    #Number of periods (years) over which vehicle capital payments are amortized
  fcr_veh = discount_rate_veh + discount_rate_veh/(((1+discount_rate_veh)^nper_amort_veh)-1)

  ## function that loads UCD costs and annual mileage, results are on ISO level: costs in 2005USD/vkm (2005USD/vkm), annual mileage in vkt/veh/yr (vehicle km traveled per year)
  print("-- load UCD database")
  UCD_output <- lvl0_loadUCD(input_folder = input_folder, fcr_veh = fcr_veh, years = years)
  ## function that loads PSI purchase costs, results are on an unspecified regional aggregation: costs in annualized 2005USD
  print("-- load PSI costs")
  PSI_costs <- lvl0_PSI_costs(input_folder = input_folder, years = years, fcr_veh = fcr_veh)
  ## function that loads CHN conventional trucks CAPEX and non/fuel OPEX, results on ISO level: costs in 2005USD/vkm
  print("-- load China truck costs")
  CHN_trucks <- lvl0_CHNTrucksCosts(input_folder= input_folder, years = years)
  ## function that loads PSI intensities
  PSI_int = lvl0_PSIint(GCAM_data = GCAM_data, input_folder, PSI_dir="PSI", years)
  ## function that loads alternative trucks/buses costs
  altCosts <- lvl0_AltHDV(UCD_output = UCD_output)
  ## function that merges costs, LF, energy intensity, annual mileage from the various sources.
  ## output units: costs in 2005$/pkm (or 2005$/tkm)
  ## LF in p/v (or t/v)
  ## energy intensity pkm/MJ
  ## Annual mileage in km/year. Every entry in the output is on ISO level
  print("-- merge costs, LF, annual mileage from the various sources")
  merged_data <- lvl0_mergeDat(UCD_output= UCD_output, PSI_costs = PSI_costs, altCosts = altCosts, PSI_int=PSI_int, CHN_trucks = CHN_trucks, EU_data = EU_data, trsp_incent = trsp_incent, fcr_veh = fcr_veh, nper_amort_veh=nper_amort_veh, GCAM_data = GCAM_data, smartlifestyle = smartlifestyle, years = years, REMIND2ISO_MAPPING = REMIND2ISO_MAPPING)

  if(storeRDS)
    saveRDS(merged_data, file = level0path("merged_data.RDS"))

  ## function that calculates VOT for each level and logit exponents for each level.Final values: VOT in [2005$/pkm]
  print("-- load value-of-time and logit exponents")
  VOT_lambdas=lvl0_VOTandExponents(GCAM_data, GDP_MER_country = GDP_MER_country, POP_country = POP_country, input_folder = input_folder)
  ## substitute lambda
  VOT_lambdas$logit_output$logit_exponent_FV[, logit.exponent := ifelse(logit.exponent==-8,-4,logit.exponent)]
  ## make freight less price sensitive
  VOT_lambdas$logit_output$logit_exponent_S3S[sector == "trn_freight", logit.exponent := -1]


  if(storeRDS){
    saveRDS(VOT_lambdas, file = level0path("logit_exp.RDS"))
  }


  ## produce regionalized versions, and ISO version of the tech_output and LF, as they are loaded on ISO level for EU. No conversion of units happening.
  print("-- generate REMIND level data")
  REMINDdat <- lvl0_REMINDdat(merged_data = merged_data, VOT_lambdas = VOT_lambdas, GDP_country = GDP_country, REMIND2ISO_MAPPING = REMIND2ISO_MAPPING)


  ## function that calculates the inconvenience cost starting point between 1990 and 2020
  incocost <- lvl0_incocost(annual_mileage = REMINDdat$AM,
                            load_factor = REMINDdat$LF,
                            fcr_veh = fcr_veh)


  if(storeRDS){
    saveRDS(REMINDdat,
            file = level0path("REMINDdat.RDS"))
    saveRDS(incocost,
            file = level0path("incocost.RDS"))
  }

  #################################################
  ## LVL 1 scripts
  #################################################
  print("-- Start of level 1 scripts")
  print("-- Harmonizing energy intensities to match IEA final energy balances")
  IEAbal_comparison <- lvl1_IEAharmonization(int = REMINDdat$int, demKm = REMINDdat$dem, IEA = IEAbal)
  if(storeRDS)
    saveRDS(IEAbal_comparison$merged_intensity, file = level1path("harmonized_intensities.RDS"))

  print("-- Merge non-fuel prices with REMIND fuel prices")
  REMIND_prices <- merge_prices(
    gdx = file.path(input_folder, "REMIND/fulldata_EU.gdx"),
    REMINDmapping = REMIND2ISO_MAPPING,
    REMINDyears = years,
    intensity_data = IEAbal_comparison$merged_intensity,
    nonfuel_costs = REMINDdat$NFcost,
    module = "edge_esm")

  if(storeRDS)
    saveRDS(REMIND_prices, file = level1path("full_prices.RDS"))


  print("-- EDGE calibration")
  calibration_output <- lvl1_calibrateEDGEinconv(
    prices = REMIND_prices,
    tech_output = REMINDdat$dem,
    logit_exp_data = VOT_lambdas$logit_output,
    vot_data = REMINDdat$vt,
    price_nonmot = REMINDdat$pnm)


  if(storeRDS)
    saveRDS(calibration_output, file = level1path("calibration_output.RDS"))

  print("-- generating trends for inconvenience costs")
  prefs <- lvl1_preftrend(SWS = calibration_output$list_SW,
                          incocost = incocost,
                          calibdem = REMINDdat$dem,
                          GDP = GDP,
                          GDP_POP_MER = GDP_POP_MER,
                          years = years,
                          EDGE_scenario = EDGE_scenario,
                          smartlifestyle = smartlifestyle,
                          techswitch = techswitch)

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
  IEAbal_comparison$merged_intensity = merge(IEAbal_comparison$merged_intensity, unique(prefs$FV_final_pref[!(vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype")) , c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)


  totveh=NULL
  ## multiple iterations of the logit calculation - set to 3
  for (i in seq(1,3,1)) {
    logit_data <- calculate_logit_inconv_endog(
      prices = REMIND_prices,
      vot_data = REMINDdat$vt,
      pref_data = prefs,
      logit_params = VOT_lambdas$logit_output,
      intensity_data = IEAbal_comparison$merged_intensity,
      price_nonmot = REMINDdat$pnm,
      techswitch = techswitch,
      totveh = totveh)

    if(storeRDS){
      saveRDS(logit_data[["share_list"]], file = level1path("share_newvehicles.RDS"))
      saveRDS(logit_data[["pref_data"]], file = level1path("pref_data.RDS"))
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
      IntAv_Prep <- lvl0_IntAvPreparation(input_folder= input_folder,
                                          GDP_country = GDP_country)

      ## Baseline demand regression run, for international aviation
      print("-- performing demand regression for Intl Av")
      NAVIGATE_intl_dem_base <- lvl2_demandRegNAVIGATEIntl(tech_output = REMINDdat$dem,
                                                           price_baseline = prices$S3S,
                                                           GDP_POP = GDP_POP,
                                                           REMIND_scenario = SSP_scen,
                                                           smartlifestyle = smartlifestyle,
                                                           ICCT_data = IntAv_Prep,
                                                           input_folder = input_folder,
                                                           Baseline_Run = TRUE)

      ## regression demand calculation
      print("-- performing demand regression")
      ## demand in million km
      NAVIGATE_intl_dem <- lvl2_demandRegNAVIGATEIntl(tech_output = REMINDdat$dem,
                                                      price_baseline = prices$S3S,
                                                      GDP_POP = GDP_POP,
                                                      REMIND_scenario = SSP_scen,
                                                      smartlifestyle = smartlifestyle,
                                                      ICCT_data = IntAv_Prep,
                                                      RPK_cap_baseline = NAVIGATE_intl_dem_base,
                                                      input_folder = input_folder,
                                                      Baseline_Run = FALSE)

      dem_regr = NAVIGATE_intl_dem[["D_star"]]

      if(storeRDS){
        saveRDS(NAVIGATE_intl_dem[["D_star"]], file = level2path("demand_regression.RDS"))
        saveRDS(NAVIGATE_intl_dem[["D_star_av"]], file = level2path("demand_regression_aviation.RDS"))
      }


    } else {
      ## demand in million km
      dem_regr = lvl2_demandReg(tech_output = REMINDdat$dem,
                                price_baseline = prices$S3S,
                                GDP_POP = GDP_POP,
                                smartlifestyle = smartlifestyle)
      if(storeRDS)
        saveRDS(dem_regr, file = level2path("demand_regression.RDS"))

    }

    ## calculate vintages (new shares, prices, intensity)
    prices$base=prices$base[,c("region", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "non_fuel_price", "tot_price", "fuel_price_pkm",  "tot_VOT_price", "sector_fuel")]
    vintages = calcVint(shares = shares,
                        totdem_regr = dem_regr,
                        prices = prices,
                        mj_km_data = mj_km_data,
                        years = years)


    shares$FV_shares = vintages[["shares"]]$FV_shares
    prices = vintages[["prices"]]
    mj_km_data = vintages[["mj_km_data"]]


    if(storeRDS)
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
    norm_demand <- shares_intensity_demand[["demandF_plot_pkm"]] ## in million km

    num_veh_stations = calc_num_vehicles_stations(
      norm_dem = norm_demand[
        subsector_L1 == "trn_pass_road_LDV_4W", ## only 4wheelers
        c("region", "year", "sector", "vehicle_type", "technology", "demand_F") ],
      ES_demand_all = dem_regr,
      intensity = intensity_remind,
      techswitch = techswitch,
      loadFactor = unique(REMINDdat$LF[,c("region", "year", "vehicle_type", "loadFactor")]),
      EDGE2teESmap = EDGE2teESmap,
      rep = TRUE)

    totveh = num_veh_stations$alltechdem

  }



  print("-- Calculating budget coefficients")
  budget <- calculate_capCosts(
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
    saveRDS(vintages[["vintcomp"]], file = level2path("vintcomp.RDS"))
    saveRDS(vintages[["newcomp"]], file = level2path("newcomp.RDS"))
    saveRDS(shares, file = level2path("shares.RDS"))
    saveRDS(logit_data$mj_km_data, file = level2path("mj_km_data.RDS"))
    saveRDS(logit_data$annual_sales, file = level2path("annual_sales.RDS"))
    saveRDS(logit_data[["share_list"]], file = level2path("shares.RDS"))
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

    saveRDS(JRC_IDEES, file = level2path("JRC_IDEES.RDS"))

    saveRDS(POP, file = level2path("POP.RDS"))
    saveRDS(IEAbal_comparison$IEA_dt2plot, file = level2path("IEAcomp.RDS"))
    md_template = level2path("report.Rmd")
    ## ship and run the file in the output folder
    file.copy(system.file("Rmd", "report.Rmd", package = "edgeTransport"),
              md_template, overwrite = T)
    render(md_template, output_format="pdf_document")
  }


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
                                   SSP_scen = SSP_scen)

  print("-- preparing complex module-friendly output files")
  ## final value: in billionspkm or billions tkm and EJ; shares are in [-]
  complexValues <- lvl2_reportingEntries(ESdem = shares_intensity_demand$demandF_plot_pkm,
                                         FEdem = shares_intensity_demand$demandF_plot_EJ,
                                         gdp_country = GDP_country,
                                         REMIND2ISO_MAPPING)

  print("-- generating CSV files to be transferred to mmremind")
  ## only the combinations (region, vehicle) present in the mix have to be included in costs
  NEC_data = merge(REMINDdat$NFcost,
                   unique(calibration_output$list_SW$VS1_final_SW[,c("region", "vehicle_type")]),
                   by =c("region", "vehicle_type"))
  capcost4W = merge(REMINDdat$costs[variable == "Capital costs (purchase)"],
                    unique(calibration_output$list_SW$VS1_final_SW[,c("region", "vehicle_type")]),
                    by =c("region", "vehicle_type"))

  ## save the output csv files or create a list of objects
  EDGETrData = lvl2_createoutput(
    logit_params = VOT_lambdas$logit_output,
    pref_data = logit_data$pref_data,
    vot_data = REMINDdat$vt,
    int_dat = IEAbal_comparison$merged_intensity,
    NEC_data = NEC_data,
    capcost4W = capcost4W,
    demByTech = finalInputs$demByTech,
    intensity = finalInputs$intensity,
    capCost = finalInputs$capCost,
    price_nonmot = REMINDdat$pnm,
    complexValues = complexValues,
    loadFactor = REMINDdat$LF,
    annual_mileage = REMINDdat$AM,
    demISO = merged_data$dem,
    SSP_scen = SSP_scen,
    EDGE_scenario = EDGE_scenario,
    level2path = level2path,
    output_folder = output_folder)

  if (!is.null(EDGETrData)) {
    return(EDGETrData)
  }

}
