#' Energy Demand Generator (EDGE)- Transport Model
#'
#' The Edge Transport Model includes the transport specific input data preparation,
#' a choice model to determine transport mode and technology shares, a demand regression and a fleet tracking for cars, busses and trucks
#'
#' @param SSPscen SSP or SDP scenario
#' @param transportPolScen EDGE-T transport policy scenario
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param gdxPath Path to a GDX file to load price signals from a REMIND run
#' @param outputFolder Path to folder for storing output data
#' @param storeRDS Optional saving of intermediate RDS files
#' @param reportMif Optional transport reporting in MIF format
#' @param generateREMINDinputData generate the REMIND input data cs4 files
#' @return generated REMIND transport input data
#' @author Johanna Hoppe, Jarusch Müßel, Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @importFrom rmarkdown render
#' @importFrom quitte write.mif
#' @export

toolEdgeTransportSA <- function(SSPscen, transportPolScen, demScen = "default", gdxPath = NULL, outputFolder = NULL, generateTransportData = TRUE, generateREMINDinputData = FALSE){

  # set GDP cutoff to differentiate between regions
  GDPcutoff <- 25000 # [constant 2005 US$MER]
  # Year when scenario differentiation sets in
  policyStartYear <- 2021
  # last time step of historical data
  baseYear <- 2010
  # share of electricity in hybrid electric vehicles
  hybridElecShare <- 0.4

  ########################################################
  ## Load input data
  ########################################################

  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, gdxPath, hybridElecShare)

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  inputDataRaw <- inputs$inputDataRaw

  ########################################################
  ## Prepare input data and apply policy specific changes
  ########################################################

  scenSpecInputData <- toolPrepareScenInputData(genModelPar, scenModelPar, inputDataRaw, policyStartYear, helpers)

  ########################################################
  ## Calibrate historical preferences
  ########################################################
  histPrefs <- toolCalibrateHistPrefs(scenSpecInputData$combinedCAPEXandOPEX, inputDataRaw$histESdemand, inputDataRaw$timeValueCosts,
                                      genModelPar$lambdasDiscreteChoice, helpers)
  scenSpecPrefTrends <- rbind(histPrefs, scenSpecInputData$scenSpecPrefTrends)
  scenSpecPrefTrends <- toolApplyMixedTimeRes(scenSpecPrefTrends, helpers)

  #-------------------------------------------------------
  inputData <- list(
    prefTrends = scenSpecPrefTrends,
    loadFactor = scenSpecInputData$scenSpecLoadFactor,
    enIntensity = scenSpecInputData$scenSpecEnIntensity,
    combinedCAPEXandOPEX = scenSpecInputData$combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = scenSpecInputData$upfrontCAPEXtrackedFleet,
    initialIncoCosts = scenSpecInputData$initialIncoCosts,
    annualMileage = inputDataRaw$annualMileage,
    timeValueCosts = inputDataRaw$timeValueCosts,
    histESdemand = inputDataRaw$histESdemand,
    GDPMER = inputDataRaw$GDPMER,
    GDPpcMER = inputDataRaw$GDPpcMER,
    population = inputDataRaw$population
  )

  print("Input data preparation finished")

  ########################################################
  ## Prepare data for
  ## endogenous costs update
  ########################################################

  vehicleDepreciationFactors <- toolCalculateVehicleDepreciationFactors(genModelPar$annuityCalc, helpers)
  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(inputData, genModelPar$lambdasDiscreteChoice, policyStartYear, helpers)

  #################################################
  ## Demand regression module
  #################################################
  ## demand in million km
  sectorESdemand <- toolDemandRegression(inputData$histESdemand,
                                         inputData$combinedCAPEXandOPEX,
                                         inputData$GDPpcMER,
                                         inputData$population,
                                         scenModelPar$scenParDemRegression,
                                         scenModelPar$scenParRegionalDemRegression,
                                         scenModelPar$scenParDemFactors, baseYear, policyStartYear)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  endogenousCostsIterations <- list()
  fleetSizeAndCompositionIterations <- list()
  fleetVehiclesPerTech <- NULL
  iterations <- 2

  for (i in seq(1, iterations, 1)) {

    #################################################
    ## Cost module
    #################################################
    # provide endogenous updates to cost components -----------
    # number of vehicles changes in the vehicle stock module and serves as new input for endogenous cost update
    endogenousCostsIterations[[i]] <- toolUpdateEndogenousCosts(dataEndogenousCosts, vehicleDepreciationFactors, scenModelPar$scenParIncoCost,
                                                 policyStartYear, inputData$timeValueCosts, inputData$prefTrends, genModelPar$lambdasDiscreteChoice,
                                                 helpers, years, fleetVehiclesPerTech)
    lapply(endogenousCostsIterations[[i]], function(dt) dt[, variable := paste0(variable, "|Iteration ", i)])
    print("Endogenous updates to cost components finished")
    #################################################
    ## Discrete choice module
    #################################################
    # calculate vehicle sales shares and mode shares for all levels of the decisionTree
    vehSalesAndModeShares <- toolDiscreteChoice(inputData, genModelPar, endogenousCostsIterations[[i]]$updatedEndogenousCosts, years, helpers)

    ESdemandFuelVehicle <- toolCalculateFVdemand(sectorESdemand, vehSalesAndModeShares, helpers, inputData$histESdemand, baseYear)
    print("Calculation of vehicle sales and mode shares finished")
    #################################################
    ## Vehicle stock module
    #################################################
    # Calculate vehicle stock for cars, trucks and busses -------
    fleetSizeAndCompositionIterations[[i]] <- toolCalculateFleetComposition(ESdemandFuelVehicle, vehicleDepreciationFactors, vehSalesAndModeShares,
                                                             inputData$annualMileage, inputData$loadFactor, helpers)
    fleetVehiclesPerTech <- fleetSizeAndCompositionIterations[[i]]$fleetVehiclesPerTech

    print("Calculation of vehicle stock finished")
  }
  #------------------------------------------------------
  # End of iterative section
  #------------------------------------------------------

  #################################################
  ## Reporting
  #################################################
  # Save data
  outputFolder <- file.path(outputFolder, paste0(format(Sys.time(), "%Y-%m-%d"),
                                                 SSPscen, "-", transportPolScen, "-", demScen))

  if (generateTransportData == TRUE) {
    result <- system.time({
    toolStoreData(outputFolder = outputFolder,
                  SSPscen = SSPscen,
                  transportPolScen = transportPolScen,
                  demScen = demScen,
                  gdxPath = gdxPath,
                  inputDataRaw = inputDataRaw,
                  hybridElecShare = hybridElecShare,
                  inputData = inputData,
                  histPrefs = histPrefs,
                  fleetSizeAndCompositionIterations = fleetSizeAndCompositionIterations,
                  endogenousCostsIterations = endogenousCostsIterations,
                  ESdemandFuelVehicle = ESdemandFuelVehicle,
                  vehSalesAndModeShares = vehSalesAndModeShares)
      Sys.sleep(2)  # Example code that takes some time
    })
    elapsed_time <- result[3]
    cat("Elapsed time:", elapsed_time, "seconds\n")
    toolReportEdgeTransport(folderPath = outputFolder,
                            gdxPath = gdxPath,
                            ESdemandFuelVehicle = ESdemandFuelVehicle,
                            inputData = inputData,
                            inputDataRaw = inputDataRaw,
                            fleetSizeAndCompositionIterations = fleetSizeAndCompositionIterations,
                            endogenousCostsIterations = endogenousCostsIterations,
                            hybridElecShare = hybridElecShare,
                            extendedReporting = TRUE,
                            analytics = TRUE)
  }

  if (generateREMINDinputData == TRUE) {
    REMINDinputData <- reportREMINDinputData()
    iterativeEDGETinputData <- reportIterativeEDGETinputData()
    if (storeREMINDinputData == TRUE) {
      lapply(REMINDinputData,  function(x) write.csv(x, file.path(outputFolder, folderName, "5_REMINDinputData", paste0(data.table.name(x), ".csv"))))
    }
  }



}
