#' Load all inputs that are required to run the model
#' @author Johanna Hoppe, Alex K. Hagen
#' @param SSPscen SSP or SDP scenarios
#' @param transportPolScen EDGE-T transport policy scenarios
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param hybridElecShare Share of electricity in Hybrid electric vehicles
#' @param allEqYear Year after which scenario differentiation sets in, last year in which scenarios are the same
#' @returns list with different input data sets
#' @import data.table
#' @importFrom mrtransport toolPrepareTransportSubsidies
#' @export

toolLoadInputs <- function(SSPscen, transportPolScen, demScen, hybridElecShare, allEqYear) {

  ### load inputs  ------------------------------------------------------------

  ## from mrtransport
  # ToDo temporary default to SSP2, to be removed again
  mrtransportData <- toolLoadmrtransportData()

  ### edgeTransport package data
  packageData <- toolLoadPackageData(SSPscen, transportPolScen, demScen)

  # mappings and other helpers incl. filterEntries and temporal resolution
  # energyIntensityRaw provides the reference for the mixed temporal resolution
  helpers <- toolBuildHelpers(
    decisionTree = packageData$decisionTree,
    timeResDataBase = mrtransportData$energyIntensityRaw
  )

  ## from mrdrivers
  mrdriversData <- toolLoadmrdriversData(SSPscen, helpers, allEqYear)

  # load and prepare transport subsidies from mrtransport
  subsidies <- toolPrepareTransportSubsidies(helpers)

  ### structure inputs  ------------------------------------------------------------

  # general model parameters
  genModelPar <- list(
    lambdasDiscreteChoice = packageData$lambdasDiscreteChoice,
    baselinePrefTrends = packageData$baselinePrefTrends,
    incoCostStartVal = packageData$incoCostStartVal,
    genParDemRegression = packageData$genParDemRegression,
    annuityCalc = packageData$annuityCalc
  )

  # transport scenario (SSPscen + demScen + polScen) specific model parameters
  scenModelPar <- list(
    scenParEnergyIntensity = packageData$scenParEnergyIntensity,
    scenParLoadFactor = packageData$scenParLoadFactor,
    scenParPrefTrends = packageData$scenParPrefTrends,
    scenParIncoCost = packageData$scenParIncoCost,
    scenParDemRegression = packageData$scenParDemRegression,
    scenParRegionalDemRegression = packageData$scenParRegionalDemRegression,
    scenParDemFactors = packageData$scenParDemFactors
  )

  # raw input data
  inputDataRaw <- list(
    histESdemand = mrtransportData$histESdemand,
    energyIntensityRaw = mrtransportData$energyIntensityRaw,
    loadFactorRaw = mrtransportData$loadFactorRaw,
    annualMileage = mrtransportData$annualMileage,
    CAPEXtrackedFleet = mrtransportData$CAPEXtrackedFleet,
    nonFuelOPEXtrackedFleet = mrtransportData$nonFuelOPEXtrackedFleet,
    CAPEXother = mrtransportData$CAPEXother,
    nonFuelOPEXother = mrtransportData$nonFuelOPEXother,
    timeValueCosts = mrtransportData$timeValueCosts,
    subsidies = subsidies,
    GDPMER = mrdriversData$GDPMER,
    GDPpcMER = mrdriversData$GDPpcMER,
    GDPppp = mrdriversData$GDPppp,
    GDPpcPPP = mrdriversData$GDPpcPPP,
    population = mrdriversData$population
  )

  input <- list(
    helpers = helpers,
    genModelPar = genModelPar,
    scenModelPar = scenModelPar,
    inputDataRaw = inputDataRaw
  )

  return(input)
}
