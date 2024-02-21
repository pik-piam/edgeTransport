#' Read and build the complete structure of the edgeTransport decision tree
#' @author Johanna Hoppe
#' @param regionAggregation one of the different options for regional aggregation (iso|regionCode21|regionCode12)
#' @returns data.table of full spatially extended edgeTransport decision tree
#' @import data.table
#' @export

toolLoadInputs <- function(SSPscen, transportPolScen, demScen, gdxPath, hybridElecShare) {

  ### load inputs  ------------------------------------------------------------

  ## from mrtransport
  mrtransportData <- toolLoadmrtransportData(SSPscen)
  # vehicle types that feature fleet tracking get a different temporal resolution
  dtTimeRes <- unique(mrtransportData$energyIntensity[, c("univocalName", "period")])
  highRes <- unique(dtTimeRes$period)
  lowResUnivocalNames <- copy(dtTimeRes)
  lowResUnivocalNames <- lowResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  lowResUnivocalNames <- lowResUnivocalNames[test == FALSE, univocalName]
  lowTimeRes <- unique(dtTimeRes[univocalName %in% lowResUnivocalNames]$period)

  ### edgeTransport package data
  packageData <- toolLoadPackageData(SSPscen, transportPolScen, demScen)
  # categories for filtering data
  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_2W", "trn_freight_road", "trn_pass", "trn_freight")
  filterEntries <- getFilterEntriesUnivocalName(categories, packageData$decisionTree)
  filterEntries[["trackedFleet"]] <- c(filterEntries[["trn_pass_road_LDV_4W"]], filterEntries[["trn_freight_road"]], getFilterEntriesUnivocalName("Bus", packageData$decisionTree)[["Bus"]])

  # mappings and other helpers
  helpers <- list(
    decisionTree = packageData$decisionTree,
    regionmappingISOto21to12 = packageData$regionmappingISOto21to12,
    mitigationTechMap = packageData$mitigationTechMap,
    filterEntries = filterEntries,
    dtTimeRes = dtTimeRes,
    lowTimeRes = lowTimeRes
  )

  ## from mrcommons
  mrdriversData <- toolLoadmrdriversData(SSPscen, helpers)

  ## from REMIND
  REMINDdata <- toolLoadREMINDfuelCosts(gdxPath, hybridElecShare, helpers)

  # from mrremind (soon to be replaced by mrtransport data)
  mrremindData <- toolLoadmrremindData(packageData$decisionTree, helpers)

  ### structure inputs  ------------------------------------------------------------

  # general model parameters
  genModelPar <- list(
    lambdasDiscreteChoice = packageData$lambdasDiscreteChoice,
    baselinePrefTrends = packageData$baselinePrefTrends,
    incoCostStartVal = packageData$incoCostStartVal,
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
    energyIntensity = mrtransportData$energyIntensity,
    loadFactor = mrtransportData$loadFactor,
    annualMileage = mrtransportData$annualMileage,
    CAPEXtrackedFleet = mrtransportData$CAPEXtrackedFleet,
    nonFuelOPEXtrackedFleet = mrtransportData$nonFuelOPEXtrackedFleet,
    CAPEXother = mrtransportData$CAPEXother,
    nonFuelOPEXother = mrtransportData$nonFuelOPEXother,
    fuelCosts = REMINDdata$fuelCosts,
    timeValueCosts = mrtransportData$timeValueCosts,
    subsidies = mrremindData$subsidies,
    GDPpcMER = mrdriversData$GDPpcMER,
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
