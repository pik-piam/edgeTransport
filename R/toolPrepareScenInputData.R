#' Apply transport scenario specific adjustments to input data
#' @author Johanna Hoppe
#' @param genModelPar General model parameters
#' @param scenModelPar Transport scenario (SSPscen + demScen + polScen) specific model parameters
#' @param inputDataRaw Raw input data
#' @param policyStartYear Year when scenario differentiation sets in
#' @param GDPcutoff GDP cutoff to differentiate between regions
#' @param helpers List with helpers
#' @param isICEban optional enabling of ICE ban
#' @returns List of data.tables with scenario specific input data
#' @import data.table
#' @export

toolPrepareScenInputData <- function(genModelPar, scenModelPar, inputDataRaw, policyStartYear, GDPcutoff, helpers, isICEban, cm_startyear = 2025) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- variable <- level <- unit <- NULL

  # Preparation of baseline preference trends -------------------------------------
  # change to long-format
  basePrefTrends <- melt(genModelPar$baselinePrefTrends, variable.name = "period",
                         id.vars = c("region", "level", "sector", "subsectorL1",
                                     "subsectorL2", "subsectorL3", "vehicleType", "technology","startYearCat"))
  # interpolate all timesteps
  # get rid of levels as period is treated as a factor after using melt (not supported by approx_dt)
  basePrefTrends[, period := as.numeric(as.character(period))]
  basePrefTrends <- toolApplyMixedTimeRes(basePrefTrends, helpers)
  if ("final" %in% unique(basePrefTrends$startYearCat)) {
    basePrefTrends <- basePrefTrends[(period >= 2020 & period <= cm_startyear & startYearCat == 'origin')|(period > cm_startyear & startYearCat == 'final')][, startYearCat := NULL]
  } else {
    basePrefTrends <- basePrefTrends[period >= 2020][, startYearCat := NULL]
  }
  # order
  basePrefTrends <- basePrefTrends[, c("region", "period", "technology", "vehicleType",
                                       "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "value")]
  basePrefTrends[, variable := paste0("Preference|", level)][, unit := "-"]
  # Application of policy induced changes to baseline preference trends, here scenSpecPrefTrends changes from a table of levers to actual time dependent PrefTrends --------------
  if (!is.null(scenModelPar$scenParPrefTrends)) {
    scenSpecPrefTrends <- toolApplyScenPrefTrends(basePrefTrends, scenModelPar$scenParPrefTrends,
                                                  inputDataRaw$GDPpcMER, policyStartYear, GDPcutoff, helpers, isICEban)
    print("Policy induced changes to baseline preference trends were applied")
  } else {
    scenSpecPrefTrends <- basePrefTrends
    print("No policy induced changes to the baseline preference trends")
  }

  # Application of policy induced changes on load factor ----------------------------
  if (!is.null(scenModelPar$scenParLoadFactor)) {
    scenSpecLoadFactor <- toolApplyScenSpecLoadFactor(inputDataRaw$loadFactorRaw, scenModelPar$scenParLoadFactor,
                                                      policyStartYear, helpers)
    scenSpecLoadFactor[, variable := "Load factor"]
    print("Policy induced changes to the loadfactor were applied")
  } else {
    scenSpecLoadFactor <- copy(inputDataRaw$loadFactorRaw)[, variable := "Load factor"]
    print("No policy induced changes to the loadfactor")
  }

  # Application of policy induced changes on energy intensity ----------------------------
  if (!is.null(scenModelPar$scenParEnergyIntensity)) {
    scenSpecEnIntensity <- toolApplyScenSpecEnInt(inputDataRaw$energyIntensityRaw,
                                                  scenModelPar$scenParEnergyIntensity, policyStartYear, helpers)
    scenSpecEnIntensity[, variable := "Energy intensity sales"]
    print("Policy induced changes to the energy intensity were applied")
  } else {
    scenSpecEnIntensity <- copy(inputDataRaw$energyIntensityRaw)[, variable := "Energy intensity sales"]
    print("No policy induced changes to the energyIntensity")
  }

  # Annualization and formatting of monetary costs ---------------------------------------------------
  annuity <- toolCalculateAnnuity(genModelPar$annuityCalc, helpers)
  transportCosts <- toolCombineCAPEXandOPEX(inputDataRaw$CAPEXtrackedFleet,
                                            inputDataRaw$nonFuelOPEXtrackedFleet,
                                            inputDataRaw$CAPEXother,
                                            inputDataRaw$nonFuelOPEXother,
                                            inputDataRaw$REMINDfuelCosts,
                                            inputDataRaw$subsidies,
                                            scenSpecEnIntensity,
                                            scenSpecLoadFactor,
                                            inputDataRaw$annualMileage,
                                            annuity,
                                            helpers)

  # Annualization and formatting of non-monetary costs -------------------------------------------------
  initialIncoCosts <- toolCalculateInitialIncoCost(transportCosts$combinedCAPEXandOPEX,
                                                   genModelPar$incoCostStartVal, annuity, scenSpecLoadFactor,
                                                   inputDataRaw$annualMileage, helpers)

  scenSpecInputData <- list(
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecEnIntensity,
    combinedCAPEXandOPEX = transportCosts$combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = transportCosts$upfrontCAPEXtrackedFleet,
    initialIncoCosts = initialIncoCosts)

  return(scenSpecInputData)
}
