#' Load all inputs that are required to run the model
#' @author Johanna Hoppe
#' @param SSPscen SSP or SDP scenarios
#' @param transportPolScen EDGE-T transport policy scenarios
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param gdxPath Path to a GDX file to load price signals from a REMIND run
#' @param hybridElecShare Share of electricity in Hybrid electric vehicles
#' @returns list with different input data sets
#' @import data.table
#' @importFrom mrtransport toolPrepareTransportSubsidies
#' @export

toolLoadInputs <- function(SSPscen, transportPolScen, demScen, gdxPath, hybridElecShare, policyStartYear) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- univocalName <- test <- . <- NULL

  ### load inputs  ------------------------------------------------------------

  ## from mrtransport
  # ToDo temporary default to SSP2, to be removed again
  mrtransportData <- toolLoadmrtransportData()
  # vehicle types that feature fleet tracking get a different temporal resolution
  dtTimeRes <- unique(mrtransportData$energyIntensityRaw[, c("univocalName", "period")])
  highRes <- unique(dtTimeRes$period)
  lowResUnivocalNames <- copy(dtTimeRes)
  lowResUnivocalNames <- lowResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  lowResUnivocalNames <- lowResUnivocalNames[test == FALSE, univocalName]
  lowTimeRes <- unique(dtTimeRes[univocalName %in% lowResUnivocalNames]$period)

  ### edgeTransport package data
  ## ToDo Function now takes two scenario specifiers, fist one is temporarily fixed
  packageData <- toolLoadPackageData(c("SSP2", SSPscen), c("Mix2", transportPolScen), demScen)
  # categories for filtering data
  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_2W", "trn_freight_road", "trn_pass", "trn_freight")
  filterEntries <- getFilterEntriesUnivocalName(categories, packageData$decisionTree)
  filterEntries[["trackedFleet"]] <- c(filterEntries[["trn_pass_road_LDV_4W"]], filterEntries[["trn_freight_road"]],
                                       getFilterEntriesUnivocalName("Bus", packageData$decisionTree)[["Bus"]])

  # mappings and other helpers
  helpers <- list(
    decisionTree = packageData$decisionTree,
    regionmappingISOto21to12 = packageData$regionmappingISOto21to12,
    mitigationTechMap = packageData$mitigationTechMap,
    mapEdgeToREMIND = packageData$mapEdgeToREMIND,
    filterEntries = filterEntries,
    dtTimeRes = dtTimeRes,
    lowTimeRes = lowTimeRes,
    reportingNames = packageData$reportingNames,
    reportingAggregation = packageData$reportingAggregation
  )

  ## from mrdrivers
  mrdriversData <- toolLoadmrdriversData(c("SSP2", SSPscen), helpers, policyStartYear)

  ## from REMIND
  REMINDfuelCosts <- toolLoadREMINDfuelCosts(gdxPath, hybridElecShare, helpers)

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
    REMINDfuelCosts = REMINDfuelCosts,
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
