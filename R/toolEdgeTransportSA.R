#' Energy Demand Generator (EDGE)- Transport Model
#'
#' The Edge Transport Model includes the transport specific input data preparation,
#' a choice model to determine transport mode and technology shares, a demand regression
#' and a fleet tracking for cars, busses and trucks
#'
#' @param SSPscen SSP or SDP scenarios
#' @param transportPolScen EDGE-T transport policy scenarios
#' @param isICEban optional enabling of ICE ban
#' @param demScen Demand scenarios, used to apply reduction factors on total demands from the regression
#' @param allEqYear Year after which policy differentiation sets in
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
#' @importFrom madrat getConfig
#' @export

toolEdgeTransportSA <- function(SSPscen,
                                transportPolScen,
                                isICEban = c(FALSE, FALSE),
                                demScen = c("default", "default"),
                                startyear = 2030,
                                gdxPath = NULL,
                                outputFolder = NULL,
                                isStored = TRUE,
                                isTransportReported = TRUE,
                                isTransportExtendedReported = FALSE,
                                isREMINDinputReported = FALSE,
                                isAnalyticsReported = FALSE){

  # bind variables locally to prevent NSE notes in R CMD CHECK
  variable <- NULL

  # set GDP cutoff to differentiate between regions
  GDPcutoff <- 30800 # [constant 2017 US$MER]
  # last time step of historical data
  baseYear <- 2010
  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- 0.4

  # cm_startyear in REMIND is first timepoint where differentiation is observed
  # allEqYear in EDGET is last timepoint in which all scenarios are equal, earliest 2020
  allEqYear <- startyear - 5
  if (allEqYear < 2020){
    allEqYear <- 2020
  }

  # find years in which ICEban is used
  if (isICEban[1] & isICEban[2]) {
    ICEbanYears <- c(seq(2021, 2100, 1), 2110, 2130, 2150)
    isICEban <- TRUE
  } else if (isICEban[1] & allEqYear > 2020) {
    ICEbanYears  <- seq(2021, allEqYear, 1)
    isICEban <- TRUE
  } else if (isICEban[2]){
    ICEbanYears <-  c(seq(allEqYear, 2100, 1), 2110, 2130, 2150)
    isICEban <- TRUE
  } else {
    ICEbanYears <- NULL
    isICEban <- FALSE
  }


  ########################################################
  ## Load input data
  ########################################################

  if (is.null(outputFolder) & isStored) stop("Please provide an outputfolder to store your results")

  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, hybridElecShare)

  if (is.null(gdxPath)) {gdxPath <- file.path(getConfig("sourcefolder"),
                                              "REMINDinputForTransportStandalone", "v1.2", "fulldata.gdx")}
  if (!file.exists(gdxPath)) stop("Please provide valid path to REMIND fulldata.gdx as input for fuel costs")

  # Load standalone specific inputs
  ## from mrdrivers
  mrdriversData <- toolLoadmrdriversData(SSPscen, helpers, allEqYear)
  ## from REMIND
  REMINDfuelCosts <- toolLoadREMINDfuelCosts(gdxPath, hybridElecShare, helpers)

  inputDataStandalone <- list(
    REMINDfuelCosts = REMINDfuelCosts,
    GDPMER = mrdriversData$GDPMER,
    GDPpcMER = mrdriversData$GDPpcMER,
    GDPppp = mrdriversData$GDPppp,
    GDPpcPPP = mrdriversData$GDPpcPPP,
    population = mrdriversData$population
  )

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  inputDataRaw <- append(inputs$inputDataRaw, inputDataStandalone)

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
  if (isICEban) {
   scenSpecPrefTrends <- toolApplyICEbanOnPreferences(scenSpecPrefTrends, helpers, ICEbanYears)
  }
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
                                         allEqYear,
                                         helpers)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  fleetVehiclesPerTech <- NULL
  iterations <- 3

  if (isAnalyticsReported) {
    endogenousCostsIterations <- list()
    fleetVehNumbersIterations <- list()
    costsDiscreteChoiceIterations <- list()
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
                                                 allEqYear,
                                                 inputData$timeValueCosts,
                                                 inputData$scenSpecPrefTrends,
                                                 genModelPar$lambdasDiscreteChoice,
                                                 helpers,
                                                 isICEban,
                                                 ICEbanYears,
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
    if (isAnalyticsReported) {
      costsDiscreteChoiceIterations[[i]] <- lapply(copy(vehSalesAndModeShares$costsDiscreteChoice),
                                               function(x){ x[, variable := paste0(variable, "|Iteration ", i)]})
    }
    ESdemandFVsalesLevel <- toolCalculateFVdemand(sectorESdemand,
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
  SSPscen <- SSPscen[2]
  transportPolScen <- transportPolScen[2]
  demScen <- demScen[2]

  # Rename transportPolScen if ICE ban is activated
  if (isICEban & (transportPolScen %in% c("Mix1", "Mix2", "Mix3", "Mix4"))) transportPolScen <- paste0(transportPolScen, "ICEban")

  print(paste("Run", SSPscen, transportPolScen, "demand scenario", demScen, "with startyear", startyear, "finished"))

  # Save data
  outputFolder <- file.path(outputFolder, paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"),
                                                 "-", SSPscen, "-", transportPolScen, "-", demScen, "-", startyear))


  outputRaw <- list(
    SSPscen = SSPscen,
    transportPolScen = transportPolScen,
    demScen = demScen,
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
  # not all data from inputdataRaw and inputdata is needed for the reporting
  add <- append(inputDataRaw,
                inputData[!names(inputData) %in% c("histESdemand", "GDPMER","GDPpcMER", "GDPpcPPP", "population")])
  outputRaw <- append(outputRaw, add)

  if (isAnalyticsReported) outputRaw <- append(outputRaw, list(endogenousCostsIterations = endogenousCostsIterations,
                                                               costsDiscreteChoiceIterations = costsDiscreteChoiceIterations,
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
