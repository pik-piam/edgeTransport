#' Load input data from the edgeTransport package and choose data according to SSP and transport policy scenario
#'
#' @importFrom data.table fread

toolLoadPackageData <- function(SSPscenario, transportPolScenario, demScenario = NULL) {

  ## model input parameters from the package

  # Decision tree discrete choice model
  decisionTree <- fread(system.file("extdata/decisionTree.csv", package = "edgeTransport", mustWork = TRUE))
  # Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/inputLambdasDiscreteChoiceModel.csv", package = "edgeTransport", mustWork = TRUE))
  # Baseline preference trends
  baselinePreftrends <- fread(system.file("extdata/inputBaselinePrefTrends.csv", package = "edgeTransport", mustWork = TRUE))
  baselinePreftrends <- baselinePreftrends[SSPscen = SSPscenario][, SSPscen := NULL]
  #Startparameter inconvenience costs
  incoCostStartVal <- fread(system.file("extdata/inputIncoCostStartVal.csv", package = "edgeTransport", mustWork = TRUE))
  # Interest Rate and vehicle service life for annuity calculation
  annuityCalc <- fread(system.file("extdata/inputAnnuityCalc.csv", package = "edgeTransport", mustWork = TRUE))
  # SSP/SDP specific regression factors
  DemRegressionFactorsSSP <- fread(system.file("extdata/inputDemRegressionFactorsSSP.csv", package = "edgeTransport", mustWork = TRUE))
  DemRegressionFactorsSSP <- DemRegressionFactorsSSP[SSPscen = SSPscenario][, SSPscen := NULL]
  # Regional regression factors
  DemRegressionFactorsRegional <- fread(system.file("extdata/inputDemRegressionFactorsRegional.csv", package = "edgeTransport", mustWork = TRUE))

  ## scenario specific levers

  # Transport policy scenario preference factors
  policyParPrefTrends <- fread(system.file("extdata/policyParPrefTrends.csv", package = "edgeTransport", mustWork = TRUE))
  policyParPrefTrends <- policyParPrefTrends[SSPscen = SSPscenario & transportPolScen = transportPolScenario][, c("SSPscen", "transportPolScen") := NULL]
  # Transport policy scenario inconvenience cost factors
  policyParIncoCost <- fread(system.file("extdata/policyParIncoCost.csv", package = "edgeTransport", mustWork = TRUE))
  policyParIncoCost <- policyParIncoCost[SSPscen = SSPscenario & transportPolScen = transportPolScenario][, c("SSPscen", "transportPolScen") := NULL]
  # Transport policy scenario demand reduction factors
  policyParDemScen <- fread(system.file("extdata/policyParDemScen.csv", package = "edgeTransport", mustWork = TRUE))
  if  (demScenario %in% policyParDemScen$demScen){
   policyParDemScen <- policyParDemScen[demScen = demScenario]} else {
   policyParDemScen <- NULL
   }
  # Transport policy scenario energy intensity reduction factors
  policyParEnergyIntensity <- fread(system.file("extdata/policyParEnergyIntensity.csv", package = "edgeTransport", mustWork = TRUE))
  if  (transportPolScenario %in% policyParEnergyIntensity$transportPolScen){
    policyParEnergyIntensity <- policyParEnergyIntensity[transportPolScen = transportPolScenario]} else {
    policyParEnergyIntensity <- NULL
  }

  return(
    list(
      decisionTree = decisionTree,
      lambdasDiscreteChoice = lambdasDiscreteChoice,
      baselinePreftrends = baselinePreftrends,
      incoCostStartVal = incoCostStartVal,
      annuityCalc = annuityCalc,
      DemRegressionFactorsSSP = DemRegressionFactorsSSP,
      DemRegressionFactorsRegional = DemRegressionFactorsRegional,
      policyParPrefTrends = policyParPrefTrends,
      policyParIncoCost = policyParIncoCost,
      policyParDemScen = policyParDemScen,
      policyParEnergyIntensity = policyParEnergyIntensity
    )
  )

  }
