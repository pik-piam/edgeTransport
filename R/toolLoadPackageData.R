#' Load input data from the edgeTransport package and choose data
#' according to SSP and transport policy scenario
#' @param SSPscenario SSP scenario for which the package data should be loaded
#' @param transportPolScenario transport policy scenario for which the package data should be loaded
#' @param demScenario demand scenario for which the package input data should be loaded
#' @returns list of data.tables with the package input data
#' @importFrom data.table fread

toolLoadPackageData <- function(SSPscenario, transportPolScenario, demScenario = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  SSPscen <- transportPolScen <- demScen <- NULL

  #As a starting point, we only use GDP and Population data from the IND-scenarios. Changes in transport policy scenarios to the SSP2 scenario are not considered.
  if (SSPscenario %in% c("SSP2IndiaHigh", "SSP2IndiaDEAs", "SSP2IndiaMedium")){SSPscenario <- "SSP2"}
  ## General model parameters from the package

  # Decision tree discrete choice model
  decisionTree <- toolLoadDecisionTree("regionCode21")
  # Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv",
                                             package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Baseline preference trends
  baselinePrefTrends <- fread(system.file("extdata/genParBaselinePrefTrends.csv",
                                          package = "edgeTransport", mustWork = TRUE), header = TRUE)
  baselinePrefTrends <- baselinePrefTrends[SSPscen == SSPscenario][, SSPscen := NULL]
  # Startparameter inconvenience costs
  incoCostStartVal <- fread(system.file("extdata/genParIncoCostStartVal.csv",
                                        package = "edgeTransport", mustWork = TRUE), header = TRUE)

  genParDemRegression <- fread(system.file("extdata/genParDemRegression.csv",
                                                  package = "edgeTransport", mustWork = TRUE), skip = 1, header = TRUE)

  annuityCalc <- fread(system.file("extdata/genParAnnuityCalc.csv",
                                   package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Interest Rate and vehicle service life for annuity calculation
  # NOTE: right now there is only "default". If we add scenario specific annuity parameters,
  # we can shift annuityCalc to the scenPar's
  if  (transportPolScenario %in% annuityCalc$transportPolScen) {
    annuityCalc <- annuityCalc[transportPolScen == transportPolScenario][, transportPolScen := NULL]
} else {
    annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
  }

  ## scenario specific levers
  # Transport policy scenario preference factors
  scenParPrefTrends <- fread(system.file("extdata/scenParPrefTrends.csv", package = "edgeTransport", mustWork = TRUE),
                             header = TRUE)
  scenParPrefTrends <- scenParPrefTrends[SSPscenario == SSPscen & transportPolScen == transportPolScenario]
  scenParPrefTrends[, c("SSPscen", "transportPolScen") := NULL]
  if (nrow(scenParPrefTrends) == 0) scenParPrefTrends <- NULL

  # Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost <- scenParIncoCost[SSPscen == SSPscenario & transportPolScen == transportPolScenario]
  scenParIncoCost[, c("SSPscen", "transportPolScen") := NULL]
  # Transport policy scenario demand reduction factors
  scenParDemFactors <- fread(system.file("extdata/scenParDemFactors.csv",
                                         package = "edgeTransport", mustWork = TRUE), header = TRUE)
  if  (demScenario %in% scenParDemFactors$demScen) {
   scenParDemFactors <- scenParDemFactors[demScen == demScenario][, demScen := NULL]
} else {
   scenParDemFactors <- NULL
  }
  # Transport policy scenario energy intensity reduction factors
  scenParEnergyIntensity <- fread(system.file("extdata/scenParEnergyIntensity.csv",
                                              package = "edgeTransport", mustWork = TRUE), header = TRUE)
  if  (transportPolScenario %in% scenParEnergyIntensity$transportPolScen) {
    scenParEnergyIntensity <- scenParEnergyIntensity[transportPolScen == transportPolScenario & SSPscen == SSPscenario]
    scenParEnergyIntensity[, c("transportPolScen", "SSPscen") := NULL]
} else {
    scenParEnergyIntensity <- NULL
    }

  # SSP/SDP specific general regression factors
  scenParDemRegression <- fread(system.file("extdata/scenParDemRegression.csv",
                                            package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParDemRegression <- scenParDemRegression[SSPscen == SSPscenario][, SSPscen := NULL]
  # SSP/SDP specific regional regression factors
  scenParRegionalDemRegression <- fread(system.file("extdata/scenParRegionalDemRegression.csv",
                                                    package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParRegionalDemRegression <- scenParRegionalDemRegression[SSPscen == SSPscenario][, SSPscen := NULL]

  # Transport scenario (demand scenario or SSP scenario) load factor changes
  scenParLoadFactor <- fread(system.file("extdata/scenParLoadFactor.csv",
                                         package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Demand scenario exogenous demand changes
  if  (SSPscenario %in% unique(scenParLoadFactor[demScen == "default"]$SSPscen)) {
    scenParLoadFactor <- scenParLoadFactor[SSPscen == SSPscenario][, c("SSPscen", "demScen") := NULL]
  } else {
    scenParLoadFactor <- scenParLoadFactor[SSPscen == SSPscenario & demScen == demScenario]
    scenParLoadFactor[, c("SSPscen", "demScen") := NULL]
  }
  if (nrow(scenParLoadFactor) == 0) scenParLoadFactor <- NULL

  ## helpers
  mitigationTechMap <- fread(system.file("extdata", "helpersMitigationTechmap.csv",
                                         package = "edgeTransport"))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                    package = "edgeTransport"))
  reportingNames <- fread(system.file("extdata", "helpersReportingNames.csv",
                                      package = "edgeTransport"), skip = 1)
  reportingAggregation <- fread(system.file("extdata", "helpersReportingAggregation.csv",
                                      package = "edgeTransport"), skip = 1)
  mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv",
                                       package = "edgeTransport", mustWork = TRUE))

  return(
    list(
      decisionTree = decisionTree,
      lambdasDiscreteChoice = lambdasDiscreteChoice,
      baselinePrefTrends = baselinePrefTrends,
      incoCostStartVal = incoCostStartVal,
      genParDemRegression = genParDemRegression,
      annuityCalc = annuityCalc,
      scenParDemRegression = scenParDemRegression,
      scenParRegionalDemRegression = scenParRegionalDemRegression,
      scenParPrefTrends = scenParPrefTrends,
      scenParIncoCost = scenParIncoCost,
      scenParDemFactors = scenParDemFactors,
      scenParEnergyIntensity = scenParEnergyIntensity,
      scenParLoadFactor = scenParLoadFactor,
      mitigationTechMap = mitigationTechMap,
      regionmappingISOto21to12 = regionmappingISOto21to12,
      mapEdgeToREMIND = mapEdgeToREMIND,
      reportingNames = reportingNames,
      reportingAggregation = reportingAggregation
    )
  )

}
