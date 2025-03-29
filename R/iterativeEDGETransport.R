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
  cfg <- sumWeight <- weight <- NULL

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

  #initialize scenario Parameter
  SSPscen <- c("","")
  transportPolScen <- c("","")
  demScen <- c("default","")

  # config from current run contains information about possible reference run
  load("config.Rdata")
  cfgCurrentRun <- copy(cfg)
  cfg <- NULL

  #  scenario after startyear (if given) but no earlier than 2020 from current REMIND config
  SSPscen[2] <- cfgCurrentRun$gms$cm_GDPpopScen
  transportPolScen[2] <- cfgCurrentRun$gms$cm_EDGEtr_scen
  demScen[2] <- cfgCurrentRun$gms$cm_demScen

  startyear <- cfgCurrentRun$gms$cm_startyear
  # REMIND startyear is the year in which differences are observed
  # allEqYear in EDGE-T is the last year of the previous scenario and differentiation sets in directly after that, earliest: 2020
  allEqYear <- startyear - 5
  if (allEqYear < 2020){
    allEqYear <- 2020
  }

  # If there is a reference run, load config of REMIND reference run for fixing before startyear
  # if not: duplicate scenario from current config in analogy of solution in standalone
  if (!is.na(cfgCurrentRun$files2export$start["input_ref.gdx"])) {
    load(file.path(dirname(cfgCurrentRun$files2export$start["input_ref.gdx"]), "config.Rdata"))
    cfgReferenceRun <- copy(cfg)
    cfg <- NULL
    SSPscen[1] <- cfgReferenceRun$gms$cm_GDPpopScen
    transportPolScen[1] <- cfgReferenceRun$gms$cm_EDGEtr_scen
    demScen[1] <- cfgReferenceRun$gms$cm_demScen
  } else {
    SSPscen[1] <- SSPscen[2]
    transportPolScen[1] <- transportPolScen[2]
  }

  isICEban <- c(FALSE, FALSE)

  for (i in 1:length(transportPolScen)) {
    if (grepl(".*ban$", transportPolScen[i])) {
      isICEban[i] <- TRUE
      transportPolScen[i] <- gsub('ICEban','', transportPolScen[i])
    }
  }

  # find years in which ICEban is used
  if (isICEban[1] & isICEban[2]) {
    ICEbanYears <- c(seq(2021, 2100, 1), 2110, 2130, 2150)
  } else if (isICEban[1] & allEqYear > 2020) {
    ICEbanYears  <- seq(2021, allEqYear, 1)
  } else if (isICEban[2]){
    ICEbanYears <-  c(seq(allEqYear, 2100, 1), 2110, 2130, 2150)
  } else {
    ICEbanYears <- NULL
  }


  #############################################################
  ## Load input data via mrtransport
  #############################################################
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- unit <- univocalName <- iteration <- type <- variable <- . <- NULL

  # To do: How to get rid of these manually set parameters in the standalone and iterative script?
  # set GDP cutoff to differentiate between regions
  GDPcutoff <- 30800 # [constant 2017 US$MER]
  # last time step of historical data
  baseYear <- 2010
  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- 0.4

  numberOfRegions <- length(gdx::readGDX(gdxPath, "all_regi"))
  iterationNumber <- as.vector(gdxrrw::rgdx(gdxPath, list(name = "o_iterationNumber"))$val)

  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, hybridElecShare)

  # Can we sort that a little better? Both is needed for the standalone and the iterative version
  ## from mrdrivers
  mrdriversData <- toolLoadmrdriversData(SSPscen, inputs$helpers, allEqYear)
  ## from REMIND
  REMINDfuelCosts <- toolLoadREMINDfuelCosts(gdxPath = gdxPath,
                                             hybridElecShare = hybridElecShare,
                                             helpers = inputs$helpers,
                                             transportFolder = file.path(".", edgeTransportFolder),
                                             iterationNumber = iterationNumber)

  inputDataIterative <- list(
    REMINDfuelCosts = REMINDfuelCosts,
    #GDPMER = mrdriversData$GDPMER,
    GDPpcMER = mrdriversData$GDPpcMER,
    #GDPppp = mrdriversData$GDPppp,
    GDPpcPPP = mrdriversData$GDPpcPPP,
    population = mrdriversData$population
  )

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  inputDataRaw <- append(inputs$inputDataRaw, inputDataIterative)

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
  histPrefs <- toolCalibrateHistPrefs(scenSpecInputData$combinedCAPEXandOPEX,
                                      inputDataRaw$histESdemand,
                                      inputDataRaw$timeValueCosts,
                                      genModelPar$lambdasDiscreteChoice,
                                      helpers)

  scenSpecPrefTrends <- rbind(histPrefs$historicalPreferences,
                              scenSpecInputData$scenSpecPrefTrends)
  scenSpecPrefTrends <- toolApplyMixedTimeRes(scenSpecPrefTrends,
                                              helpers)
  if (isICEban[1] | isICEban[2]) {
    scenSpecPrefTrends <- toolApplyICEbanOnPreferences(scenSpecPrefTrends, helpers, ICEbanYears)
  }
  scenSpecPrefTrends <- toolNormalizePreferences(scenSpecPrefTrends)


  #######################################################
  # Iterative specific Input data                       #
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

  if (numberOfRegions == 12) {

    # Demand from the standalone regression module as weight
    filePath <- list.files(edgeTransportFolder, recursive = TRUE, full.names = TRUE)
    filePath <- filePath[grepl(".*sectorESdemand.RDS", filePath)]

    if (length(filePath == 1)) {
      # Load from folder after first iterative edge-t run
      sectorESdemand <- readRDS(filePath)
    } else {
      # Create and store for first iterative edge-t run
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

      storeData(outputFolder = edgeTransportFolder, list(sectorESdemand = sectorESdemand))
    }

    weightEs <- copy(sectorESdemand)[, "unit" := NULL]
    setnames(weightEs, c("region", "value"), c("regionCode21", "weight"))
    dataColumns <- names(REMINDsectorESdemand)[!names(REMINDsectorESdemand) %in% c("region", "period", "value")]
    setnames(REMINDsectorESdemand, "region", "regionCode12")
    # disaggregate_dt produces duplicates right now - Todo: Check fucntion
    #REMINDsectorESdemand <- rmndt::disaggregate_dt(REMINDsectorESdemand, helpers$regionmappingISOto21to12, fewcol = "regionCode12", manycol = "regionCode21",
                                                   #datacol = dataColumns, weights = weightEs)
    REMINDsectorESdemand <- merge(REMINDsectorESdemand, unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")]),
                                  by = "regionCode12", allow.cartesian = TRUE)
    REMINDsectorESdemand <- merge(REMINDsectorESdemand, weightEs, intersect(names(REMINDsectorESdemand), names(weightEs)))
    REMINDsectorESdemand[, sumWeight := sum(weight), by = c("period", "sector", "regionCode12")]
    REMINDsectorESdemand <- REMINDsectorESdemand[, .(value = value * (weight/sumWeight)), by = c("regionCode21", "period", "sector", "variable", "unit")]
    setnames(REMINDsectorESdemand, "regionCode21", "region")
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
                                               isICEban[1] | isICEban[2],
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
  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an
  # interpolation step in toolCalculateOutputVariables()
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  outputRaw <- list(
    combinedCAPEXandOPEX = inputData$combinedCAPEXandOPEX,
    scenSpecEnIntensity = inputData$scenSpecEnIntensity,
    scenSpecLoadFactor = inputData$scenSpecLoadFactor,
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
    sectorESdemand = REMINDsectorESdemand,
    ESdemandFVsalesLevel = ESdemandFVsalesLevel,
    helpers = helpers
  )

  storeData(edgeTransportFolder, outputRaw)

  baseOutput <- reportEdgeTransport(edgeTransportFolder,
                                    outputRaw,
                                    isTransportReported = FALSE)

  esCapCost <- baseOutput$int$fleetCost[variable == "Capital costs"]
  fleetESdemand <- baseOutput$ext$fleetESdemand
  fleetFEdemand <- baseOutput$ext$fleetFEdemand

  #-- Variables that are reported back to REMIND must be offered in the correct regional resolution
  # Currently EDGE-T always runs on 21 regions, but REMIND potentially runs only on 12 regions
  if (numberOfRegions == 12) {
    ESweight <- copy(fleetESdemand)[, c("unit", "variable") := NULL]
    setnames(ESweight, c("region", "value"), c("regionCode21", "weight"))
    dataColumns <- names(esCapCost)[!names(esCapCost) %in% c("region", "period", "value")]
    setnames(esCapCost, "region", "regionCode21")
    regionMap <- unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")])
    esCapCost <- rmndt::aggregate_dt(esCapCost, regionMap,
                                     fewcol = "regionCode12", manycol = "regionCode21",
                                     datacol = dataColumns, weights = ESweight, yearcol = "period")
    setnames(esCapCost, "regionCode12", "region")
    setnames(fleetESdemand, "region", "regionCode21")
    fleetESdemand <- rmndt::aggregate_dt(fleetESdemand, regionMap,
                                         fewcol = "regionCode12", manycol = "regionCode21",
                                         datacol = dataColumns, yearcol = "period")

    setnames(fleetESdemand, "regionCode12", "region")
    setnames(fleetFEdemand, "region", "regionCode21")
    fleetFEdemand <- rmndt::aggregate_dt(fleetFEdemand, regionMap,
                                         fewcol = "regionCode12", manycol = "regionCode21",
                                         datacol = dataColumns, yearcol = "period")
    setnames(fleetFEdemand, "regionCode12", "region")
  }

  # Keep only final SSPscen, demScen, transportPolScen
  demScen <- demScen[length(demScen)]
  SSPscen <- SSPscen[length(SSPscen)]
  transportPolScen <- transportPolScen[length(transportPolScen)]

  # For reporting to REMIND add ICEban info to transportPolScen again
  if (isICEban[2]) {
    transportPolScen <- paste0(transportPolScen, "ICEban")
  }

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
