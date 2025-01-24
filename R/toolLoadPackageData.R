#' Load input data from the edgeTransport package and choose data
#' according to SSP and transport policy scenario
#' @param SSPs SSP scenarios for which the package data should be loaded
#' @param transportPolS transport policy scenarios for which the package data should be loaded
#' @param demScenario demand scenario for which the package input data should be loaded
#' @returns list of data.tables with the package input data
#' @importFrom data.table fread

toolLoadPackageData <- function(SSPs, transportPolS, demScenario = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  SSPscen <- transportPolScen <- demScen <- NULL

  ## General model parameters from the package

  # Decision tree discrete choice model
  decisionTree <- toolLoadDecisionTree("regionCode21")
  # Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv",
                                             package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Baseline preference trends
  baselinePrefTrends <- fread(system.file("extdata/genParBaselinePrefTrends.csv",
                                          package = "edgeTransport", mustWork = TRUE), header = TRUE)
  baselinePrefTrends <- baselinePrefTrends[SSPscen == SSPs[1]][, SSPscen := NULL]
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
  if  (transportPolS[1] %in% annuityCalc$transportPolScen) {
    # once this is used, scenario switchin with cm_startYear needs to be checked
    annuityCalc[, "startYearCat" := fcase( transportPolScen == transportPolS[1], "origin", transportPolScen == transportPolS[2], "final")]
    annuityCalc <- annuityCalc[!is.na(startYearCat)][, transportPolScen := NULL]
} else {
    annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
  }

  ## scenario specific levers
  # Transport policy scenario preference factors
  scenParPrefTrends <- fread(system.file("extdata/scenParPrefTrends.csv", package = "edgeTransport", mustWork = TRUE),
                             header = TRUE)
  # Option A) adds 1200 lines in upstream toolApply, but includes case handling for SSPs[1] == SSPs[2]
   scenParPrefTrends[, "startYearCat" := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
   scenParPrefTrends <- scenParPrefTrends[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]
  # Option B) cannot be done inplace, renaming is ugly, different option? no case handling for SSPs[1] == SSPs[2] so far
  # scenParPrefTrendsO <- scenParPrefTrends[(SSPscen == SSPs[1] & transportPolScen == transportPolS[1])][, c("transportPolScen", "SSPscen") := NULL]
  # scenParPrefTrendsF <- scenParPrefTrends[(SSPscen == SSPs[2] & transportPolScen == transportPolS[2])][, c("transportPolScen", "SSPscen") := NULL]
  # setnames(scenParPrefTrendsF, (ncol(scenParPrefTrendsF)-2):ncol(scenParPrefTrendsF), c("targetF", "symmyrF", "speedF"))
  # scenParPrefTrends <- merge(scenParPrefTrendsO, scenParPrefTrendsF, by = intersect(names(scenParPrefTrendsO), names(scenParPrefTrendsF)))

   if (nrow(scenParPrefTrends) == 0) scenParPrefTrends <- NULL

  # Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost[, "startYearCat" := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
  scenParIncoCost <- scenParIncoCost[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]

  # Transport policy scenario demand reduction factors
  scenParDemFactors <- fread(system.file("extdata/scenParDemFactors.csv",
                                         package = "edgeTransport", mustWork = TRUE), header = TRUE)
  if  (demScenario %in% scenParDemFactors$demScen) {
   scenParDemFactors <- scenParDemFactors[demScen == demScenario][, demScen := NULL]
} else {
   scenParDemFactors <- NULL
}

  # Transport policy scenario energy intensity reduction factors, only for very specific scenarios
  scenParEnergyIntensity <- fread(system.file("extdata/scenParEnergyIntensity.csv",
                                              package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # How can a switch from no to some scenario be achieved? -> modify startYear and endYear directly in the mask?!
  if  ((transportPolS[1] %in% scenParEnergyIntensity$transportPolScen)|(transportPolS[2] %in% scenParEnergyIntensity$transportPolScen)) {
    scenParEnergyIntensity[, "startYearCat" := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
    scenParEnergyIntensity <- scenParEnergyIntensity[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]
  } else {
    scenParEnergyIntensity <- NULL
    }

  # ToDo SSPscen flex
  # SSP/SDP specific general regression factors
  scenParDemRegression <- fread(system.file("extdata/scenParDemRegression.csv",
                                            package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParDemRegression <- scenParDemRegression[SSPscen == SSPs[1]][, SSPscen := NULL]

  # SSP/SDP specific regional regression factors
  scenParRegionalDemRegression <- fread(system.file("extdata/scenParRegionalDemRegression.csv",
                                                    package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParRegionalDemRegression <- scenParRegionalDemRegression[SSPscen == SSPs[1]][, SSPscen := NULL]

  # Transport scenario (demand scenario or SSP scenario) load factor changes
  scenParLoadFactor <- fread(system.file("extdata/scenParLoadFactor.csv",
                                         package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # ToDo demScen flex
  # Demand scenario exogenous demand changes
  if  (SSPs[1] %in% unique(scenParLoadFactor[demScen == "default"]$SSPscen)) {
    scenParLoadFactor <- scenParLoadFactor[SSPscen == SSPs[1]][, c("SSPscen", "demScen") := NULL]
  } else {
    scenParLoadFactor <- scenParLoadFactor[SSPscen == SSPs[1] & demScen == demScenario]
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
