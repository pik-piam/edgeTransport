#' Load input data from the edgeTransport package and choose data
#' according to SSP and transport policy scenario
#' @param SSPs SSP scenarios for which the package data should be loaded
#' @param transportPolS transport policy scenarios for which the package data should be loaded
#' @param demScenario demand scenario for which the package input data should be loaded
#' @returns list of data.tables with the package input data
#' @importFrom data.table fread

toolLoadPackageData <- function(SSPs, transportPolS, demScenario = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  SSPscen <- transportPolScen <- demScen <- startYearCat <- NULL

  # set demand scenario to default when not supplied
  if (is.null(demScenario)) {
    demScenario <- "default"
    for (i in 1:length(SSPs)) {
        paste(i)
        demScenario[i] <- SSPs[i]
      }
  }

  # Replace IND SSP scenarios, as they get the same package data as SSP2 for now
  for (i in 1:length(SSPs)) {
    if (SSPs[i] %in% c("SSP2IndiaHigh", "SSP2IndiaDEAs", "SSP2IndiaMedium")) {
      SSPs[i] <- "SSP2"
    }
  }

  ## General model parameters from the package

  # Decision tree discrete choice model
  decisionTree <- toolLoadDecisionTree("regionCode21")
  # Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv",
                                             package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Baseline preference trends, SSP dependent, time res: 2020, 2030, 2050, 2100, 2150
  # Keep data for all used SSPs
  baselinePrefTrends <- fread(system.file("extdata/genParBaselinePrefTrends.csv",
                                          package = "edgeTransport", mustWork = TRUE), header = TRUE)
  baselinePrefTrends[, "startYearCat" := fcase(SSPscen == SSPs[1], "origin", SSPscen == SSPs[2], "final")]
  baselinePrefTrends <- baselinePrefTrends[!is.na(startYearCat)][, SSPscen := NULL]
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
    # once this is used, scenario switching with allEqYear needs to be checked
    annuityCalc[, "startYearCat" := fcase( transportPolScen == transportPolS[1], "origin", transportPolScen == transportPolS[2], "final")]
    annuityCalc <- annuityCalc[!is.na(startYearCat)][, transportPolScen := NULL]
  } else {
    annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
  }

  ## scenario specific levers
  # Transport policy scenario preference factors
  scenParPrefTrends <- fread(system.file("extdata/scenParPrefTrends.csv", package = "edgeTransport", mustWork = TRUE),
                             header = TRUE)
  scenParPrefTrends[, startYearCat := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
  scenParPrefTrends <- scenParPrefTrends[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]
  # Check if before and after startyear scenario stays the same
  if (transportPolS[1] == transportPolS[2] & SSPs[1] == SSPs[2]) {
    scenParPrefTrends[, startYearCat := "full"]
  }

  if (nrow(scenParPrefTrends) == 0) scenParPrefTrends <- NULL

  # Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost[, "startYearCat" := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
  scenParIncoCost <- scenParIncoCost[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]
  # Transport policy scenario demand reduction factors
  scenParDemFactors <- fread(system.file("extdata/scenParDemFactors.csv",
                                         package = "edgeTransport", mustWork = TRUE), header = TRUE)
  if  (demScenario[1] %in% scenParDemFactors$demScen | demScenario[2] %in% scenParDemFactors$demScen) {
    if (demScenario[1] == demScenario[2]) {
      scenParDemFactors <- scenParDemFactors[, "startYearCat" := fcase(demScen == demScenario[2], "full")]
    } else {
      scenParDemFactors <- scenParDemFactors[, "startYearCat" := fcase(demScen == demScenario[1], "origin", demScen == demScenario[2], "final")]
    }
    scenParDemFactors <- scenParDemFactors[!is.na(startYearCat)][, demScen := NULL]
  } else {
    scenParDemFactors <- NULL
  }

  # Transport policy scenario energy intensity reduction factors, only for very specific scenarios
  scenParEnergyIntensity <- fread(system.file("extdata/scenParEnergyIntensity.csv",
                                              package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # How can a switch from no to some scenario be achieved? -> modify startYear and endYear directly in the mask?!
  if  ((transportPolS[1] %in% scenParEnergyIntensity$transportPolScen)|(transportPolS[2] %in% scenParEnergyIntensity$transportPolScen)) {
    scenParEnergyIntensity[, "startYearCat" := fcase(SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final", SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin")]
    scenParEnergyIntensity <- scenParEnergyIntensity[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]
  } else {
    scenParEnergyIntensity <- NULL
  }

  # SSP/SDP specific general regression factors
  scenParDemRegression <- fread(system.file("extdata/scenParDemRegression.csv",
                                            package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParDemRegression[, "startYearCat" := fcase(SSPscen == SSPs[1], "origin", SSPscen == SSPs[2], "final")]
  scenParDemRegression <- scenParDemRegression[!is.na(startYearCat)][, SSPscen := NULL]

  # SSP/SDP specific regional regression factors, time res: 2015, 2030, 2050, 2100
  scenParRegionalDemRegression <- fread(system.file("extdata/scenParRegionalDemRegression.csv",
                                                    package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParRegionalDemRegression[, "startYearCat" := fcase(SSPscen == SSPs[1], "origin", SSPscen == SSPs[2], "final")]
  scenParRegionalDemRegression <- scenParRegionalDemRegression[!is.na(startYearCat)][, SSPscen := NULL]
  # Transport scenario (demand scenario or SSP scenario) load factor changes
  scenParLoadFactor <- fread(system.file("extdata/scenParLoadFactor.csv",
                                         package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Demand scenario exogenous demand changes
  scenParLoadFactor[, "startYearCat" := fcase(SSPscen == SSPs[2] & demScen == demScenario[2], "final", SSPscen == SSPs[1] & demScen == demScenario[1], "origin")]
  scenParLoadFactor <- scenParLoadFactor[!is.na(startYearCat)][, c("SSPscen", "demScen") := NULL]
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
