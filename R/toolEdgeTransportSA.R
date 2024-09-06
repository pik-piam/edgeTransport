#' Energy Demand Generator (EDGE)- Transport Model
#'
#' The Edge Transport Model includes the transport specific input data preparation,
#' a choice model to determine transport mode and technology shares, a demand regression
#' and a fleet tracking for cars, busses and trucks
#'
#' @param SSPscen SSP or SDP scenario
#' @param transportPolScen EDGE-T transport policy scenario
#' @param isICEban optional enabling of ICE ban
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param gdxPath Path to a GDX file to load price signals from a REMIND run
#' @param outputFolder Path to folder for storing output data
#' @param isStored Optional saving of intermediate RDS files
#' @param isTransportReported Optional transport reporting in MIF format
#' @param isTransportExtendedReported Optional extension of transport reporting providing more detailed variables
#' @param isREMINDinputReported Optional reporting of REMIND input data
#' @param isAnalyticsReported Optional reporting of analytics data (e.g. variables over iterations)
#' @returns Transport input data for REMIND
#' @author Johanna Hoppe, Jarusch Müßel, Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @importFrom reporttransport reportEdgeTransport storeData
#' @export

toolEdgeTransportSA <- function(SSPscen,
                                transportPolScen,
                                isICEban = FALSE,
                                demScen = "default",
                                gdxPath = NULL,
                                outputFolder = NULL,
                                isStored = TRUE,
                                isTransportReported = TRUE,
                                isTransportExtendedReported = FALSE,
                                isREMINDinputReported = FALSE,
                                isAnalyticsReported = FALSE){

  # set GDP cutoff to differentiate between regions
  GDPcutoff <- 30800 # [constant 2017 US$MER]
  # Year when scenario differentiation sets in
  policyStartYear <- 2021
  # last time step of historical data
  baseYear <- 2010
  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- 0.4

  ########################################################
  ## Load input data
  ########################################################
  if (is.null(outputFolder) & isStored) stop("Please provide an outputfolder to store your results")
  if (is.null(gdxPath)) {gdxPath <- file.path(getConfig("sourcefolder"),
                                              "REMINDinputForTransportStandalone", "fulldata.gdx")}
  if (!file.exists(gdxPath)) stop("Please provide valid path to REMIND fulldata.gdx as input for fuel costs")

  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, gdxPath, hybridElecShare)

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  inputDataRaw <- inputs$inputDataRaw

  # If no demand scenario specific factors are applied, the demScen equals the SSPscen
  if (is.null(scenModelPar$scenParDemFactors)) demScen <- SSPscen

  ########################################################
  ## Prepare input data and apply scenario specific changes
  ########################################################

  scenSpecInputData <- toolPrepareScenInputData(genModelPar,
                                                scenModelPar,
                                                inputDataRaw,
                                                policyStartYear,
                                                GDPcutoff,
                                                helpers,
                                                isICEban)

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
  if (isICEban) scenSpecPrefTrends <- toolApplyICEbanOnPreferences(scenSpecPrefTrends, helpers)
  scenSpecPrefTrends <- toolNormalizePreferences(scenSpecPrefTrends)

  #-------------------------------------------------------
  inputData <- list(
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecInputData$scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecInputData$scenSpecEnIntensity,
    combinedCAPEXandOPEX = scenSpecInputData$combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = scenSpecInputData$upfrontCAPEXtrackedFleet,
    initialIncoCosts = scenSpecInputData$initialIncoCosts,
    annualMileage = inputDataRaw$annualMileage,
    timeValueCosts = inputDataRaw$timeValueCosts,
    histESdemand = inputDataRaw$histESdemand,
    GDPMER = inputDataRaw$GDPMER,
    GDPpcMER = inputDataRaw$GDPpcMER,
    GDPpcPPP = inputDataRaw$GDPpcPPP,
    population = inputDataRaw$population
  )

  print("Input data preparation finished")
  ########################################################
  ## Prepare data for
  ## endogenous costs update
  ########################################################

  vehicleDepreciationFactors <- toolCalculateVehicleDepreciationFactors(genModelPar$annuityCalc,
                                                                        helpers)
  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(inputData,
                                                        genModelPar$lambdasDiscreteChoice,
                                                        helpers)

  #################################################
  ## Demand regression module
  #################################################
  ## demand in million km
  sectorESdemand <- toolDemandRegression(inputData$histESdemand,
                                         inputData$GDPpcPPP,
                                         inputData$population,
                                         genModelPar$genParDemRegression,
                                         scenModelPar$scenParDemRegression,
                                         scenModelPar$scenParRegionalDemRegression,
                                         scenModelPar$scenParDemFactors,
                                         baseYear,
                                         policyStartYear,
                                         helpers)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  fleetVehiclesPerTech <- NULL
  iterations <- 3

  if (isAnalyticsReported) {
    endogenousCostsIterations <- list()
    fleetVehNumbersIterations <- list()
  }

  for (i in seq(1, iterations, 1)) {

    #################################################
    ## Cost module
    #################################################
    # provide endogenous updates to cost components -----------
    # number of vehicles changes in the vehicle stock module and serves
    # as new input for endogenous cost update
    endogenousCosts <- toolUpdateEndogenousCosts(dataEndogenousCosts,
                                                 vehicleDepreciationFactors,
                                                 scenModelPar$scenParIncoCost,
                                                 policyStartYear,
                                                 inputData$timeValueCosts,
                                                 inputData$scenSpecPrefTrends,
                                                 genModelPar$lambdasDiscreteChoice,
                                                 helpers,
                                                 isICEban,
                                                 fleetVehiclesPerTech)

    if (isAnalyticsReported) {
      endogenousCostsIterations[[i]] <- lapply(copy(endogenousCosts),
                                               function(x){ x[, variable := paste0(variable, "|Iteration ", i)]})
    }

    print("Endogenous updates to cost components finished")
    #################################################
    ## Discrete choice module
    #################################################
    # calculate vehicle sales shares and mode shares for all levels of the decisionTree
    vehSalesAndModeShares <- toolDiscreteChoice(inputData,
                                                genModelPar,
                                                endogenousCosts$updatedEndogenousCosts,
                                                helpers)
    ESdemandFVsalesLevel <- toolCalculateFVdemand(sectorESdemand,
                                                  vehSalesAndModeShares,
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
                                                             vehSalesAndModeShares,
                                                             inputData$annualMileage,
                                                             inputData$scenSpecLoadFactor,
                                                             helpers)

    if (isAnalyticsReported) {
      fleetVehNumbersIterations[[i]] <- copy(fleetSizeAndComposition$fleetVehNumbers)
      fleetVehNumbersIterations[[i]][, variable := paste0(variable, "|Iteration ", i)]
    }
    fleetVehiclesPerTech <- fleetSizeAndComposition$fleetVehiclesPerTech

    print("Calculation of vehicle stock finished")
  }
  #------------------------------------------------------
  # End of iterative section
  #------------------------------------------------------

  #################################################
  ## Reporting
  #################################################
  # Rename transportPolScen if ICE ban is activated
  if (isICEban & (transportPolScen %in% c("Mix1", "Mix2", "Mix3", "Mix4"))) transportPolScen <- paste0(transportPolScen, "ICEban")

  print(paste("Run", SSPscen, transportPolScen, "demand scenario", demScen, "finished"))

  # Save data
  outputFolder <- file.path(outputFolder, paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"),
                                                 "-", SSPscen, "-", transportPolScen, "-", demScen))
  demScen <- paste0("gdp_", demScen)
  SSPscen <- paste0("gdp_", SSPscen)

  outputRaw <- list(
    SSPscen = SSPscen,
    transportPolScen = transportPolScen,
    demScen = demScen,
    gdxPath = gdxPath,
    hybridElecShare = hybridElecShare,
    histPrefs = histPrefs,
    fleetSizeAndComposition = fleetSizeAndComposition,
    endogenousCosts = endogenousCosts,
    vehSalesAndModeShares = vehSalesAndModeShares,
    sectorESdemand = sectorESdemand,
    ESdemandFVsalesLevel = ESdemandFVsalesLevel,
    helpers = helpers
  )
  # not all data from inputdataRaw and inputdata is needed for the reporting
  add <- append(inputDataRaw[!names(inputDataRaw) %in% c("GDPMER", "GDPpcMER", "GDPpcPPP")],
                      inputData[!names(inputData) %in% c("histESdemand", "GDPMER","GDPpcMER", "GDPpcPPP", "population")])
  outputRaw <- append(outputRaw, add)

  if (isAnalyticsReported) outputRaw <- append(outputRaw, list(endogenousCostsIterations = endogenousCostsIterations,
                                    fleetVehNumbersIterations = fleetVehNumbersIterations))

  if (isStored) storeData(outputFolder = outputFolder, varsList = outputRaw)

  output <- reportEdgeTransport(outputFolder,
                                outputRaw,
                                isTransportReported,
                                isTransportExtendedReported,
                                isAnalyticsReported,
                                isREMINDinputReported,
                                isStored)

return(output)
}
