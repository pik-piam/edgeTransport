#' Energy Demand Generator (EDGE)- Transport Model
#'
#' The Edge Transport Model includes the transport specific input data preparation,
#' a choice model to determine transport mode and technology shares, a demand regression
#' and a fleet tracking for cars, busses and trucks
#'
#' @param SSPscen SSP or SDP scenario
#' @param transportPolScen EDGE-T transport policy scenario
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param gdxPath Path to a GDX file to load price signals from a REMIND run
#' @param outputFolder Path to folder for storing output data
#' @param storeData Optional saving of intermediate RDS files
#' @param reportTransportData Optional transport reporting in MIF format
#' @param reportExtendedTransportData Optional extension of transport reporting providing more detailed variables
#' @param reportREMINDinputData Optional reporting of REMIND input data
#' @param reportAnalytics Optional reporting of analytics data (e.g. variables over iterations)
#' @return generated REMIND transport input data
#' @author Johanna Hoppe, Jarusch Müßel, Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @export

toolEdgeTransportSA <- function(SSPscen, transportPolScen, ICEban = FALSE, demScen = "default", gdxPath = NULL, outputFolder = ".",
                                storeData = TRUE, reportTransportData = TRUE, reportExtendedTransportData = FALSE,
                                reportREMINDinputData = FALSE, reportAnalytics = FALSE){

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

  # If no demand scenario specific factors are applied, the demScen equals the SSPscen
  if (is.null(scenModelPar$scenParDemFactors)) demScen <- SSPscen

  ########################################################
  ## Prepare input data and apply policy specific changes
  ########################################################

  scenSpecInputData <- toolPrepareScenInputData(genModelPar, scenModelPar, inputDataRaw,
                                                policyStartYear, GDPcutoff, helpers, ICEban)

  ########################################################
  ## Calibrate historical preferences
  ########################################################
  histPrefs <- toolCalibrateHistPrefs(scenSpecInputData$combinedCAPEXandOPEX,
                                      inputDataRaw$histESdemand,
                                      inputDataRaw$timeValueCosts,
                                      genModelPar$lambdasDiscreteChoice,
                                      helpers)
  scenSpecPrefTrends <- rbind(histPrefs$historicalPreferences, scenSpecInputData$scenSpecPrefTrends)
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
  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(inputData, genModelPar$lambdasDiscreteChoice,
                                                        policyStartYear, helpers)

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
                                         scenModelPar$scenParDemFactors,
                                         baseYear,
                                         policyStartYear,
                                         helpers)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  fleetVehiclesPerTech <- NULL
  iterations <- 3

  if (reportAnalytics) {
    endogenousCostsIterations <- list()
    fleetVehNumbersIterations <- list()
  }

  for (i in seq(1, iterations, 1)) {

    #################################################
    ## Cost module
    #################################################
    # provide endogenous updates to cost components -----------
    # number of vehicles changes in the vehicle stock module and serves as new input for endogenous cost update
    endogenousCosts <- toolUpdateEndogenousCosts(dataEndogenousCosts,
                                                 vehicleDepreciationFactors,
                                                 scenModelPar$scenParIncoCost,
                                                 policyStartYear,
                                                 inputData$timeValueCosts,
                                                 inputData$prefTrends,
                                                 genModelPar$lambdasDiscreteChoice,
                                                 helpers,
                                                 ICEban,
                                                 fleetVehiclesPerTech)

    if (reportAnalytics) endogenousCostsIterations[[i]] <- copy(endogenousCosts)[, iteration := i]

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
                                                             inputData$loadFactor,
                                                             helpers)
    if (reportAnalytics) fleetVehNumbersIterations[[i]] <- fleetSizeAndComposition$fleetVehNumbers
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
  if (ICEban & transportPolScen %in% c("Mix1", "Mix2", "Mix3", "Mix4")) transportPolScen <- paste0(transportPolScen, "ICEban")
  # Save data
  outputFolder <- file.path(outputFolder, paste0(format(Sys.time(), "%Y-%m-%d"),
                                                 SSPscen, "-", transportPolScen, "-", demScen))

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
    ESdemandFVsalesLevel = ESdemandFVsalesLevel,
    helpers = helpers
  )
  # not all data from inputdataRaw and inputdata is needed for the reporting
  add <- append(inputDataRaw[!names(inputDataRaw) %in% c("GDPpcMER", "GDPpcPPP", "population")],
                      inputData[!names(inputData) %in% c("histESdemand", "GDPMER", "GDPpcMER", "population")])
  outputRaw <- append(outputRaw, add)

  if (reportAnalytics) outputRaw <- append(outputRaw, list(endogenousCostsIterations = endogenousCostsIterations,
                                 fleetVehNumbersIterations = fleetVehNumbersIterations))

  if (storeData) toolStoreData(outputFolder = outputFolder, outputRaw = outputRaw)

  output <- toolReportEdgeTransport(folderPath = outputFolder,
                                    data = outputRaw,
                                    reportTransportData = reportTransportData,
                                    reportREMINDinputData = reportREMINDinputData,
                                    reportExtendedTransportData = reportExtendedTransportData,
                                    reportAnalytics = reportAnalytics)


return(output)
}
