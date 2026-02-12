#' EDGE-Transport iterative
#'
#' Run in the REMIND output folder in between iterations
#'
#' @author Johanna Hoppe, Alex K. Hagen
#' @importFrom reporttransport storeData reportEdgeTransport reportToREMINDcapitalCosts
#' reportToREMINDenergyEfficiency reportToREMINDfinalEnergyShares
#' @import data.table
#' @export
#'
iterativeEdgeTransport <- function() {
  print(paste("---", Sys.time(), "Start of the EDGE-T iterative model run."))

  #############################################################
  ## Settings
  #############################################################
  cfg <- sumWeight <- weight <- region <- ..cols <- vehicleType <- technology <- NULL

  # Set paths to folders
  edgeTransportFolder <- "EDGE-T"
  edgeTransportPath <- function(fname) {
    file.path(edgeTransportFolder, fname)
  }
  REMINDpath <- function(fname) {
    file.path("../../", fname)
  }

  # Set gdx in the current REMIND folder
  gdxPath <- "input.gdx"
  if (file.exists("fulldata.gdx"))
    gdxPath <- "fulldata.gdx"

  # initialize scenario Parameter
  SSPscen <- c("", "")
  transportPolScen <- c("", "")
  demScen <- c("default", "")

  # config from current run contains information about possible reference run
  load("config.Rdata")
  cfgCurrentRun <- copy(cfg)
  cfg <- NULL

  #  scenario after startyear (if given) but no earlier than 2020 from current REMIND config
  SSPscen[2] <- cfgCurrentRun$gms$cm_GDPpopScen
  # translate scenario labels between REMIND and EDGET
  edgeTransportScenario <- toolTranslateTransportScenario(
                                                          cfgCurrentRun$gms$cm_demScen,
                                                          cfgCurrentRun$gms$cm_EDGEtr_scen,
                                                          direction = "REMINDtoEDGE")
  demScen[2] <- edgeTransportScenario$demScen
  transportPolScen[2] <- edgeTransportScenario$transportPolScen


  startyear <- as.numeric(cfgCurrentRun$gms$cm_startyear)

  # If there is a reference run, load config of REMIND reference run for fixing before startyear
  # if not: duplicate scenario from current config in analogy of solution in standalone
  reference <- cfgCurrentRun$files2export$start["input_ref.gdx"]

  if (!is.na(reference)) {

    isAbsolutePath <- grepl("^(?:/|~)", reference)
    if (isAbsolutePath) {
      referenceCfgPath <- file.path(dirname(reference), "config.Rdata")
    } else {
      # go back to remind folder and take relative path from there
      referenceCfgPath <- file.path("../..", dirname(reference), "config.Rdata")
    }

    load(referenceCfgPath)
    cfgReferenceRun <- copy(cfg)
    cfg <- NULL
    # load scenarios from cfgReferenceRun
    SSPscen[1] <- cfgReferenceRun$gms$cm_GDPpopScen
    edgeTransportScenarioRef <- toolTranslateTransportScenario(
                                                               cfgReferenceRun$gms$cm_demScen,
                                                               cfgReferenceRun$gms$cm_EDGEtr_scen,
                                                               direction = "REMINDtoEDGE")
    demScen[1] <- edgeTransportScenarioRef$demScen
    transportPolScen[1] <- edgeTransportScenarioRef$transportPolScen

  } else {
    SSPscen[1] <- SSPscen[2]
    transportPolScen[1] <- transportPolScen[2]
  }

  # get ICEban information
  isICEban <- c(FALSE, FALSE)
  for (i in 1:length(transportPolScen)) {
    if (grepl(".*ban$", transportPolScen[i])) {
      isICEban[i] <- TRUE
      transportPolScen[i] <- gsub("ICEban", "", transportPolScen[i])
    }
  }

  commonParams <- toolGetCommonParameters(startyear, isICEban[1], isICEban[2])

  # cm_startyear in REMIND is first timepoint where differentiation is observed
  # allEqYear in EDGET is last timepoint in which all scenarios are equal, earliest 2020
  allEqYear <- commonParams$allEqYear
  # Years in which ICEban is in effect
  ICEbanYears <- commonParams$ICEbanYears

  # set GDP cutoff to differentiate between regions
  GDPcutoff <- commonParams$GDPcutoff
  # last time step of historical data
  baseYear <- commonParams$baseYear
  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- commonParams$hybridElecShare

  #############################################################
  ## Load input data via mrtransport
  #############################################################
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- unit <- univocalName <- iteration <- type <- variable <- . <- NULL


  numberOfRegions <- length(gdx::readGDX(gdxPath, "all_regi"))
  iterationNumber <- as.vector(gdxrrw::rgdx(gdxPath, list(name = "o_iterationNumber"))$val)

  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, hybridElecShare, allEqYear)

  # Can we sort that a little better? Both is needed for the standalone and the iterative version
  ## from REMIND
  REMINDfuelCosts <- toolLoadREMINDfuelCosts(gdxPath = gdxPath,
                                             hybridElecShare = hybridElecShare,
                                             helpers = inputs$helpers,
                                             transportFolder = file.path(".", edgeTransportFolder),
                                             iterationNumber = iterationNumber)

  ## Check if REMINDfuelCosts needs region deaggregation
  if (numberOfRegions == 12) {
    # store data of IND as an example of a non-aggregated region for testing
    # reorder columns for comparison
    TestIND <- copy(REMINDfuelCosts)[region == "IND"]

    setnames(REMINDfuelCosts, "region", "regionCode12")
    REMINDfuelCosts <- merge(REMINDfuelCosts,
                             unique(inputs$helpers$regionmappingISOto21to12[
                               , c("regionCode12", "regionCode21")]),
                             by = "regionCode12", allow.cartesian = TRUE)
    REMINDfuelCosts[, "regionCode12" := NULL]
    setnames(REMINDfuelCosts, "regionCode21", "region")

    # test: values for IND should stay unchanged
    # bring in same order, use data.frame for comparison to ignore keys
    cols <- names(REMINDfuelCosts)
    TestIND <- TestIND[, ..cols]
    TestIND <- as.data.frame(TestIND[do.call(order, TestIND)])
    TestINDafter <- REMINDfuelCosts[region == "IND"]
    TestINDafter <- as.data.frame(TestINDafter[do.call(order, TestINDafter)])
    if (!isTRUE(all.equal(TestIND, TestINDafter))) {
      stop("Error in deaggregation of fuel costs in iterativeEDGETransport()")
    }
  }

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  inputDataRaw <- append(inputs$inputDataRaw, list(REMINDfuelCosts = REMINDfuelCosts))

  # If no demand scenario specific factors are applied, the demScen equals the SSPscen
  if (is.null(scenModelPar$scenParDemFactors)) demScen <- SSPscen

  ########################################################
  ## Prepare input data and apply scenario specific changes
  ########################################################

  scenSpecInputData <- toolPrepareScenInputData(genModelPar,
                                                scenModelPar,
                                                inputDataRaw,
                                                allEqYear,
                                                GDPcutoff,
                                                helpers)

  ########################################################
  ## Calibrate historical preferences
  ########################################################
  sharesToBeCalibrated <- toolCalculateSharesDecisionTree(inputDataRaw$histESdemand, helpers)
  histPrefs <- toolCalibratePreferences(sharesToBeCalibrated,
                                        scenSpecInputData$combinedCAPEXandOPEX,
                                        inputDataRaw$timeValueCosts,
                                        genModelPar$lambdasDiscreteChoice,
                                        helpers)
  # Don't use calibrated shareweights for LDV 4w, as they receive inconvenience costs
  histPrefs$calibratedPreferences <- histPrefs$calibratedPreferences[!(subsectorL3 == "trn_pass_road_LDV_4W" & level == "FV")]

  scenSpecPrefTrends <- rbind(histPrefs$calibratedPreferences,
                              scenSpecInputData$scenSpecPrefTrends)
  scenSpecPrefTrends <- toolApplyMixedTimeRes(scenSpecPrefTrends,
                                              helpers)

  if (isICEban[1] | isICEban[2]) {
    scenSpecPrefTrends <- toolApplyICEbanOnPreferences(scenSpecPrefTrends, helpers, ICEbanYears)
  }
  scenSpecPrefTrends <- toolNormalizePreferences(scenSpecPrefTrends)

  #######################################################
  # Iterative specific Input data
  #######################################################

  # Data from previous EDGE-T iteration
  fleetVehiclesPerTech <- NULL
  pathTofleetVehiclesPerTech <- list.files(file.path(".", edgeTransportFolder),
                                           "fleetVehiclesPerTech.RDS",
                                           recursive = TRUE,
                                           full.names = TRUE)
  if (length(pathTofleetVehiclesPerTech) > 0) {
    fleetVehiclesPerTech <- readRDS(pathTofleetVehiclesPerTech)
  }

  ## Load REMIND energy service demand
  REMINDsectorESdemand <- toolLoadREMINDesDemand(gdxPath, helpers)

  ## Check if REMINDsectorESdemand needs region deaggregation
  if (numberOfRegions == 12) {
    # Demand from the standalone regression module
    # This is only used as deaggregation weight in the iterative version
    # The deaggregation weights used are static across all iterations
    filePath <- list.files(edgeTransportFolder, recursive = TRUE, full.names = TRUE)
    filePath <- filePath[grepl(".*sectorESdemand.RDS", filePath)]

    if (length(filePath == 1)) {
      # Load from folder after first iterative edge-t run
      sectorESdemand <- readRDS(filePath)
    } else {
      # Create for first iterative edge-t run
      sectorESdemand <- toolDemandRegression(inputDataRaw$histESdemand,
                                             inputDataRaw$GDPpcPPP,
                                             inputDataRaw$population,
                                             genModelPar$genParDemRegression,
                                             scenModelPar$scenParDemRegression,
                                             scenModelPar$scenParRegionalDemRegression,
                                             scenModelPar$scenParDemFactors,
                                             baseYear,
                                             allEqYear,
                                             helpers)
    }

    ## store for testing
    totalESdemand <- sum(REMINDsectorESdemand$value)
    TestIND <- copy(REMINDsectorESdemand)[region == "IND"]

    weightEs <- copy(sectorESdemand)[, "unit" := NULL]
    setnames(weightEs, c("region", "value"), c("regionCode21", "weight"))
    dataColumns <- names(REMINDsectorESdemand)[!names(REMINDsectorESdemand) %in% c("region", "period", "value")]
    setnames(REMINDsectorESdemand, "region", "regionCode12")
    REMINDsectorESdemand <- merge(REMINDsectorESdemand, unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")]),
                                  by = "regionCode12", allow.cartesian = TRUE)
    REMINDsectorESdemand <- merge(REMINDsectorESdemand, weightEs, intersect(names(REMINDsectorESdemand), names(weightEs)))
    REMINDsectorESdemand[, sumWeight := sum(weight), by = c("period", "sector", "regionCode12")]
    REMINDsectorESdemand <- REMINDsectorESdemand[, .(value = value * (weight / sumWeight)), by = c("regionCode21", "period", "sector", "variable", "unit")]
    setnames(REMINDsectorESdemand, "regionCode21", "region")
    REMINDsectorESdemand <-  REMINDsectorESdemand[do.call(order, REMINDsectorESdemand)]
    # test if total ES demand stayed the same and if demand in IND is unchanged
    totalESdemand21 <- sum(REMINDsectorESdemand$value)
    if (!isTRUE(all.equal(totalESdemand, totalESdemand21))) {
      stop("Error in regional deaggregation of REMIND ES demand. Total ES demand changed.")
    }
    # bring in same order
    cols <- names(REMINDsectorESdemand)
    TestIND <- TestIND[, ..cols]
    TestIND <- as.data.frame(TestIND[do.call(order, TestIND)])
    TestINDafter <- REMINDsectorESdemand[region == "IND"]
    TestINDafter <- as.data.frame(TestINDafter[do.call(order, TestINDafter)])
    if (!isTRUE(all.equal(TestIND, TestINDafter))) {
      stop("Error in regional deaggregation of REMIND ES demand. ES demand for non-aggregated region IND changed.")
    }
  }

  inputData <- list(
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecInputData$scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecInputData$scenSpecEnIntensity,
    combinedCAPEXandOPEX = scenSpecInputData$combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = scenSpecInputData$upfrontCAPEXtrackedFleet,
    initialIncoCosts = scenSpecInputData$initialIncoCosts,
    annualMileage = inputDataRaw$annualMileage,
    timeValueCosts = inputDataRaw$timeValueCosts,
    REMINDsectorESdemand = REMINDsectorESdemand,
    histESdemand = inputDataRaw$histESdemand
  )

  ########################################################
  ## Prepare data for
  ## endogenous costs update
  ########################################################

  vehicleDepreciationFactors <- toolCalculateVehicleDepreciationFactors(genModelPar$annuityCalc, helpers)

  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(inputData,
                                                        genModelPar$lambdasDiscreteChoice,
                                                        helpers)
  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  #################################################
  ## Cost module
  #################################################
  # provide endogenous updates to cost components -----------
  # number of vehicles changes in the vehicle stock module and serves
  # as new input for endogenous cost update
  endogenousCosts <- toolUpdateEndogenousCosts(dataEndogenousCosts,
                                               vehicleDepreciationFactors,
                                               scenModelPar$scenParIncoCost,
                                               allEqYear,
                                               inputData$timeValueCosts,
                                               inputData$scenSpecPrefTrends,
                                               genModelPar$lambdasDiscreteChoice,
                                               helpers,
                                               (isICEban[1] | isICEban[2]),
                                               ICEbanYears,
                                               fleetVehiclesPerTech)

  print("Endogenous updates to cost components finished")

  #################################################
  ## Discrete choice module
  #################################################
  # calculate vehicle sales shares and mode shares for all levels of the decisionTree
  vehSalesAndModeShares <- toolDiscreteChoice(inputData,
                                              genModelPar,
                                              endogenousCosts$updatedEndogenousCosts,
                                              helpers)

  ESdemandFVsalesLevel <- toolCalculateFVdemand(inputData$REMINDsectorESdemand,
                                                vehSalesAndModeShares$shares,
                                                helpers,
                                                inputData$histESdemand,
                                                baseYear)
  print("Calculation of vehicle sales and mode shares finished")

  #################################################
  ## Vehicle stock module
  #################################################
  # Calculate vehicle stock for cars, trucks and busses -------
  fleetSizeAndComposition <- toolCalculateFleetComposition(ESdemandFVsalesLevel,
                                                           vehicleDepreciationFactors,
                                                           vehSalesAndModeShares$shares,
                                                           inputData$annualMileage,
                                                           inputData$scenSpecLoadFactor,
                                                           helpers)
  fleetVehiclesPerTech <- fleetSizeAndComposition$fleetVehiclesPerTech
  storeData(file.path(".", edgeTransportFolder), fleetVehiclesPerTech = fleetVehiclesPerTech)
  print("Calculation of vehicle stock finished")

  #------------------------------------------------------
  # End of iterative section
  #------------------------------------------------------

  #################################################
  ## Reporting
  #################################################
  # For reporting add ICEban info to transportPolScen again
  if (isICEban[2]) {
    transportPolScen <- paste0(transportPolScen, "ICEban")
  }

  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an
  # interpolation step in toolCalculateOutputVariables()
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  # in the iterative version sectorESdemand comes from REMIND
  sectorESdemand <- REMINDsectorESdemand

  outputRaw <- list(
    SSPscen = SSPscen,
    transportPolScen = transportPolScen,
    demScen = demScen,
    startyear = startyear,
    gdxPath = gdxPath,
    hybridElecShare = hybridElecShare,
    histPrefs = histPrefs,
    fleetSizeAndComposition = fleetSizeAndComposition,
    endogenousCosts = endogenousCosts,
    vehSalesAndModeShares = vehSalesAndModeShares$shares,
    sectorESdemand = sectorESdemand,
    ESdemandFVsalesLevel = ESdemandFVsalesLevel,
    helpers = helpers
  )

  # not all data from inputdataRaw and inputdata is needed for the reporting,
  # esp. histESdemand, GDP and population are only present when REMIND runs in 12regi
  # REMINDsectorESdemand is already covered by sectorESdemand
  add <- append(inputDataRaw[!names(inputDataRaw) %in% c("histESdemand", "GDPMER", "GDPpcMER", "GDPpcPPP", "population")],
                inputData[!names(inputData) %in% c("REMINDsectorESdemand", "histESdemand", "GDPMER", "GDPpcMER", "GDPpcPPP", "population")])
  outputRaw <- append(outputRaw, add)

  storeData(edgeTransportFolder, outputRaw)

  baseOutput <- reportEdgeTransport(edgeTransportFolder,
                                    outputRaw,
                                    isTransportReported = FALSE)

  esCapCost <- baseOutput$int$fleetCost[variable == "Capital costs"]
  fleetESdemand <- baseOutput$ext$fleetESdemand
  fleetFEdemand <- baseOutput$ext$fleetFEdemand

  # Variables that are reported back to REMIND must be offered in the correct regional resolution
  # Currently EDGE-T always runs on 21 regions, but REMIND potentially runs only on 12 regions
  if (numberOfRegions == 12) {
    ESweight <- copy(fleetESdemand)[, c("unit", "variable") := NULL]
    setnames(ESweight, c("region", "value"), c("regionCode21", "weight"))
    dataColumns <- names(esCapCost)[!names(esCapCost) %in% c("region", "period", "value")]
    setnames(esCapCost, "region", "regionCode21")
    regionMap <- unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")])
    esCapCost <- rmndt::aggregate_dt(esCapCost, regionMap,
                                     fewcol = "regionCode12", manycol = "regionCode21",
                                     datacols = dataColumns, weights = ESweight, yearcol = "period")
    setnames(esCapCost, "regionCode12", "region")
    setnames(fleetESdemand, "region", "regionCode21")
    fleetESdemand <- rmndt::aggregate_dt(fleetESdemand, regionMap,
                                         fewcol = "regionCode12", manycol = "regionCode21",
                                         datacols = dataColumns, yearcol = "period")

    setnames(fleetESdemand, "regionCode12", "region")
    setnames(fleetFEdemand, "region", "regionCode21")
    fleetFEdemand <- rmndt::aggregate_dt(fleetFEdemand, regionMap,
                                         fewcol = "regionCode12", manycol = "regionCode21",
                                         datacols = dataColumns, yearcol = "period")
    setnames(fleetFEdemand, "regionCode12", "region")
  }

  # Keep only final SSPscen, demScen, transportPolScen
  SSPscen <- SSPscen[length(SSPscen)]
  transportPolScen <- transportPolScen[length(transportPolScen)]
  demScen <- demScen[length(demScen)]

  # translate scenario labels from EDGET to REMIND labels
  remindScenario <- toolTranslateTransportScenario(demScen, transportPolScen, direction = "EDGEtoREMIND")
  transportPolScen <- remindScenario$transportPolScen
  demScen <- remindScenario$demScen

  f35_esCapCost <- reportToREMINDcapitalCosts(esCapCost, fleetESdemand, hybridElecShare, timeResReporting,
                                              demScen, SSPscen, transportPolScen, helpers)

  f35_fe2es <- reportToREMINDenergyEfficiency(fleetFEdemand,
                                              fleetESdemand,
                                              hybridElecShare,
                                              timeResReporting,
                                              demScen,
                                              SSPscen,
                                              transportPolScen,
                                              helpers)

  f35_shFeCes <- reportToREMINDfinalEnergyShares(fleetFEdemand,
                                                 timeResReporting,
                                                 demScen,
                                                 SSPscen,
                                                 transportPolScen,
                                                 helpers)

  ## CapCosts
  gdxdt::writegdx.parameter(
    "p35_esCapCost.gdx",
    f35_esCapCost,
    "p35_esCapCost",
    valcol = "value",
    uelcols = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs")
  )

  ## Intensities
  gdxdt::writegdx.parameter(
    "p35_fe2es.gdx",
    f35_fe2es,
    "p35_fe2es",
    valcol = "value",
    uelcols = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs")
  )

  ## Shares: demand can represent the shares since it is normalized
  gdxdt::writegdx.parameter(
    "p35_shFeCes.gdx",
    f35_shFeCes,
    "p35_shFeCes",
    valcol = "value",
    uelcols = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_enty", "all_in", "all_teEs")
  )

  print(paste("---", Sys.time(), "End of the EDGE-T iterative model run."))
}
