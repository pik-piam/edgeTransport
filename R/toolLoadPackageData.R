#' Load input data from the edgeTransport package and choose data according to SSP and transport policy scenario
#'
#' @importFrom data.table fread

toolLoadPackageData <- function(SSPscenario, transportPolScenario, demScenario = NULL) {

  ## model input parameters from the package

  # Decision tree discrete choice model
  decisionTree <- toolLoadDecisionTree("regionCode21")
  # Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Baseline preference trends
  baselinePrefTrends <- fread(system.file("extdata/genParBaselinePrefTrends.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  baselinePrefTrends <- baselinePrefTrends[SSPscen == SSPscenario][, SSPscen := NULL]
  #Startparameter inconvenience costs
  incoCostStartVal <- fread(system.file("extdata/genParIncoCostStartVal.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Regional regression factors
  regionalDemRegression <- fread(system.file("extdata/genParRegionalDemRegression.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)

  annuityCalc <- fread(system.file("extdata/genParAnnuityCalc.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Interest Rate and vehicle service life for annuity calculation # maybe we want another scenario column here rather than the transport policy scenario
  if  (transportPolScenario %in% annuityCalc$transportPolScen){
    annuityCalc <- annuityCalc[transportPolScen == transportPolScenario][, transportPolScen := NULL]} else {
    annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
  }

  ## scenario specific levers
  # Transport policy scenario preference factors
  scenParPrefTrends <- fread(system.file("extdata/scenParPrefTrends.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParPrefTrends <- scenParPrefTrends[SSPscen == SSPscenario & transportPolScen == transportPolScenario][, c("SSPscen", "transportPolScen") := NULL]
  if (nrow(scenParPrefTrends) == 0) scenParPrefTrends <- NULL

  # Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost <- scenParIncoCost[SSPscen == SSPscenario & transportPolScen == transportPolScenario][, c("SSPscen", "transportPolScen") := NULL]
  # Transport policy scenario demand reduction factors
  scenParDemFactors <- fread(system.file("extdata/scenParDemFactors.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  if  (demScenario %in% scenParDemFactors$demScen){
   scenParDemFactors <- scenParDemFactors[demScen == demScenario]} else {
   scenParDemFactors <- NULL
  }
  # Transport policy scenario energy intensity reduction factors
  scenParEnergyIntensity <- fread(system.file("extdata/scenParEnergyIntensity.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  if  (transportPolScenario %in% scenParEnergyIntensity$transportPolScen){
    scenParEnergyIntensity <- scenParEnergyIntensity[transportPolScen == transportPolScenario]} else {
    scenParEnergyIntensity <- NULL
    }
  # Transport scenario load factor changes
  scenParLoadFactor <- fread(system.file("extdata/scenParLoadFactor.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # SSP/SDP specific regression factors
  scenParDemRegression <- fread(system.file("extdata/scenParDemRegression.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParDemRegression <- scenParDemRegression[SSPscen == SSPscenario][, SSPscen := NULL]

  # Transport scenario exogenous demand changes
  if  (SSPscen %in% unique(scenParLoadFactor[demScen == "default"]$SSPscen)) {
    scenParLoadFactor <- scenParLoadFactor[SSPscen == SSPscenario]
  } else {
    scenParLoadFactor <- scenParLoadFactor[SSPscen == SSPscenario & demScen == demScenario]
  }
  if (nrow(scenParLoadFactor) == 0) scenParLoadFactor <- NULL

  ##helpers
  mitigationTechMap <- fread(system.file("extdata", "helpersMitigationTechmap.csv",
                                         package = "edgeTransport"))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                    package = "edgeTransport"))

  return(
    list(
      decisionTree = decisionTree,
      lambdasDiscreteChoice = lambdasDiscreteChoice,
      baselinePrefTrends = baselinePrefTrends,
      incoCostStartVal = incoCostStartVal,
      annuityCalc = annuityCalc,
      scenParDemRegression = scenParDemRegression,
      regionalDemRegression = regionalDemRegression,
      scenParPrefTrends = scenParPrefTrends,
      scenParIncoCost = scenParIncoCost,
      scenParDemFactors = scenParDemFactors,
      scenParEnergyIntensity = scenParEnergyIntensity,
      scenParLoadFactor = scenParLoadFactor,
      mitigationTechMap = mitigationTechMap,
      regionmappingISOto21to12 = regionmappingISOto21to12
    )
  )

  }
