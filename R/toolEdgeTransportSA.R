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

toolEdgeTransport <- function(SSPscen, transportPolScen, demScen = "default", gdxPath, outputFolder = NULL, generateTransportData = TRUE, generateREMINDinputData = FALSE){

  ## manually set input data
  years <- c(
    1990,
    seq(2005, 2060, by = 5),
    seq(2070, 2110, by = 10),
    2130, 2150
  )
  # set GDP cutoff to differentiate between regions
  GDPcutoff <- 25000 # [constant 2005 US$MER]
  # Year when scenario differentiation sets in
  policyStartYear <- 2021
  # last time step of historical data
  baseYear <- 2010

  ########################################################
  ## Load input data
  ########################################################

  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, gdxPath, years)

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  inputDataRaw <- inputs$inputDataRaw

  ########################################################
  ## Prepare input data and apply policy specific changes
  ########################################################

  scenSpecInputData <- toolPrepareScenInputData(copy(genModelPar), copy(scenModelPar), copy(inputDataRaw), years, policyStartYear, helpers)

  ########################################################
  ## Calibrate historical preferences
  ########################################################
  #histPrefs <- toolCalibrateHistPrefs(copy(combinedCostperES), copy(mrtransportData$histESdemand), copy(mrtransportData$timeValueCosts), copy(packageData$lambdasDiscreteChoice))
  histPrefs <- readRDS("C:/Users/johannah/Documents/Git_repos/edgeTransport/R/TestPrefTrends.RDS")
  histPrefs <- histPrefs[!subsectorL3 == "trn_pass_road_LDV_4W"][, variable := paste0("Preference|", level)][, unit := "-"]

  scenSpecPrefTrends <- rbind(histPrefs, scenSpecInputData$scenSpecPrefTrends)
  scenSpecPrefTrends <- approx_dt(scenSpecPrefTrends, years, "period", "value",
                                               c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "level"), extrapolate = TRUE)

  #-------------------------------------------------------
  inputData <- list(
    prefTrends = scenSpecPrefTrends,
    loadFactor = scenSpecInputData$scenSpecLoadFactor,
    enIntensity = scenSpecInputData$scenSpecEnIntensity,
    combinedCAPEXandOPEX = scenSpecInputData$combinedCAPEXandOPEX,
    initialIncoCosts = scenSpecInputData$initialIncoCosts,
    annualMileage = inputDataRaw$annualMileage,
    timeValueCosts = inputDataRaw$timeValueCosts,
    histESdemand = inputDataRaw$histESdemand,
    GDPpcMER = inputDataRaw$GDPpcMER,
    population = inputDataRaw$population
  )

  print("Input data preparation finished")

  ########################################################
  ## Prepare data for
  ## endogenous costs update
  ########################################################

  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(copy(inputData), copy(genModelPar$lambdasDiscreteChoice), policyStartYear, helpers)
  vehicleDepreciationFactors <- toolCalculateVehicleDepreciationFactors(copy(genModelPar$annuityCalc), helpers)

  #################################################
  ## Demand regression module
  #################################################
  ## demand in million km
  sectorESdemand <- toolDemandRegression(copy(inputData$histESdemand),
                                         copy(inputData$combinedCAPEXandOPEX),
                                         copy(inputData$GDPpcMER),
                                         copy(inputData$population),
                                         scenModelPar$scenParDemRegression,
                                         scenModelPar$scenParRegionalDemRegression,
                                         scenModelPar$scenParDemFactors, baseYear, policyStartYear)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  storeEndogenousCostsIterations <- list()
  storeFleetSizeAndCompositionIterations <- list()
  fleetVehiclesPerTech <- NULL
  iterations <- 1

  for (i in seq(1, iterations, 1)) {

    #################################################
    ## Cost module
    #################################################
    # provide endogenous updates to cost components -----------
    # number of vehicles changes in the vehicle stock module and serves as new input for endogenous cost update
    endogenousCosts <- toolUpdateEndogenousCosts(copy(dataEndogenousCosts), copy(vehicleDepreciationFactors), copy(scenModelPar$scenParIncoCost),
                                                 policyStartYear, copy(inputData$timeValueCosts),  copy(genModelPar$lambdasDiscreteChoice), helpers, years, fleetVehiclesPerTech)
    storeEndogenousCostsIterations[[i]] <- copy(endogenousCosts)
    lapply(storeEndogenousCostsIterations[[i]], function(dt) dt[, variable := paste0(variable, "|Iteration ", i)])
    print("Endogenous updates to cost components finished")
    #################################################
    ## Discrete choice module
    #################################################
    # calculate vehicle sales shares and mode shares for all levels of the decisionTree
    vehSalesAndModeShares <- toolDiscreteChoice(copy(inputData), copy(genModelPar), copy(endogenousCosts$updatedEndogenousCosts), years, helpers)

    fuelVehicleESdemand <- toolCalculateFVdemand(copy(sectorESdemand), copy(vehSalesAndModeShares), copy(inputData$histESdemand), baseYear, helpers)
    print("Calculation of vehicle sales and mode shares finished")
    #################################################
    ## Vehicle stock module
    #################################################
    # Calculate vehicle stock for cars, trucks and busses -------
    fleetSizeAndComposition <- toolCalculateFleetComposition(copy(fuelVehicleESdemand), copy(vehicleDepreciationFactors), copy(vehSalesAndModeShares),
                                                             copy(inputData$annualMileage), copy(inputData$loadFactor), baseYear, helpers)
    fleetVehiclesPerTech <- fleetSizeAndComposition$fleetVehiclesPerTech
    storeFleetSizeAndCompositionIterations[[i]] <- copy(fleetSizeAndComposition$fleetVehNumbers)
    storeFleetSizeAndCompositionIterations[[i]][, variable := paste0(variable, "|Iteration ", i)]
    print("Calculation of vehicle stock finished")
  }
  #------------------------------------------------------
  # End of iterative section
  #------------------------------------------------------

  #################################################
  ## Reporting
  #################################################
  vars <- toolCalculateOutputVariables(copy(fuelVehicleESdemand), copy(inputData$enIntensity), copy(inputData$loadFactor), copy(fleetSizeAndComposition),
            copy(inputDataRaw$CAPEXtrackedFleet), copy(inputDataRaw$subsidies),
            copy(inputData$combinedCAPEXandOPEX, gdx))

  if (generateTransportData == TRUE) {
    toolSaveRDS(SSPscen, transportPolScen, demScen, inputDataRaw, inputData, histPrefs,
                storeFleetSizeAndCompositionIterations, storeEndogenousCostsIterations, fleetVariables)
    report <- reportTransportData()
    write.mif(report, file.path(outputFolder, folderName, "edgeTransportSA.mif"))
  }

  if (generateREMINDinputData == TRUE) {
    REMINDinputData <- reportREMINDinputData()
    if (storeREMINDinputData == TRUE) {
      lapply(REMINDinputData,  function(x) write.csv(x, file.path(outputFolder, folderName, "5_REMINDinputData", paste0(data.table.name(x), ".csv"))))
    }
  }



}
