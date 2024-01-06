

toolPrepareScenInputData <- function(generalPar, scenarioPar, RawInputData, years, policyStartYear, helpers) {

  # Preparation of baseline preference trends -------------------------------------
  # change to long-format
  basePrefTrends <- melt(generalPar$baselinePrefTrends, variable.name = "period", id.vars = c("region", "level", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology"))
  # interpolate all timesteps
  # get rid of levels as period is treated as a factor after using melt (not supported by approx_dt)
  basePrefTrends[, period := as.numeric(as.character(period))]
  basePrefTrends <- approx_dt(basePrefTrends, years[years >= 2020], "period", "value", idxcols = setdiff(names(basePrefTrends), c("period", "value")), extrapolate = TRUE)
  # order
  basePrefTrends <- basePrefTrends[, c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "value")]

  # Application of policy induced changes to baseline preference trends --------------
  if (!is.null(scenarioPar$scenParPrefTrends)) {
    scenSpecPrefTrends <- toolApplyScenPrefTrends(basePrefTrends, scenarioPar$scenParPrefTrends, RawInputData$GDPpcMER, years, policyStartYear, helpers)
    print("Policy induced changes to baseline preference trends were applied")
  } else {
    scenSpecPrefTrends <- basePrefTrends
    print("No policy induced changes to the baseline preference trends")
  }

  # Application of policy induced changes on load factor ----------------------------
  if (!is.null(scenarioPar$scenParLoadFactor)) {
    scenSpecLoadFactor <- toolApplyScenSpecLoadFactor(RawInputData$loadFactor, scenarioPar$scenParLoadFactor,
                                                      policyStartYear, helpers)
    print("Policy induced changes to the loadfactor were applied")
  } else {
    scenSpecLoadFactor <- RawInputData$loadFactor
    print("No policy induced changes to the loadfactor")
  }

  # Application of policy induced changes on energy intensity ----------------------------
  if (!is.null(scenarioPar$scenParEnergyIntensity)) {
    scenSpecEnIntensity <- toolApplyScenSpecEnInt(RawInputData$energyIntensity, scenarioPar$scenParEnergyIntensity, years, policyStartYear, helpers)
    print("Policy induced changes to the energy intensity were applied")
  } else {
    scenSpecEnIntensity <- RawInputData$energyIntensity
    print("No policy induced changes to the energyIntensity")
  }

  # Annualization and formatting of monetary costs ---------------------------------------------------
  annuity <- toolCalculateAnnuity(generalPar$annuityCalc, helpers)
  combinedCAPEXandOPEX <- toolCombineCAPEXandOPEX(RawInputData$CAPEXtrackedFleet, RawInputData$nonFuelOPEXtrackedFleet, RawInputData$CAPEXother, RawInputData$nonFuelOPEXother,
                                                  RawInputData$fuelCosts, RawInputData$subsidies, scenSpecEnIntensity, scenSpecLoadFactor, RawInputData$annualMileage, annuity, years, helpers)

  # Annualization and formatting of non-monetary costs -------------------------------------------------
  initialIncoCosts <- toolCalcInitialIncoCost(copy(combinedCAPEXandOPEX), generalPar$incoCostStartVal, annuity, scenSpecLoadFactor, RawInputData$annualMileage, years, helpers)

  scenSpecInputData <- list(
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecEnIntensity,
    combinedCAPEXandOPEX = combinedCAPEXandOPEX,
    initialIncoCosts = initialIncoCosts
   )

  return(scenSpecInputData)
}
