

toolPrepareScenInputData <- function(generalPar, scenarioPar, RawInputData, policyStartYear, helpers) {

  # Preparation of baseline preference trends -------------------------------------
  # change to long-format
  basePrefTrends <- melt(generalPar$baselinePrefTrends, variable.name = "period", id.vars = c("region", "level", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology"))
  # interpolate all timesteps
  # get rid of levels as period is treated as a factor after using melt (not supported by approx_dt)
  basePrefTrends[, period := as.numeric(as.character(period))]
  basePrefTrends <- toolApplyMixedTimeRes(basePrefTrends, helpers)
  basePrefTrends <- basePrefTrends[period >= 2020]

  # order
  basePrefTrends <- basePrefTrends[, c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "value")]
  basePrefTrends[, variable := paste0("Preference|", level)][, unit := "-"]
  # Application of policy induced changes to baseline preference trends --------------
  if (!is.null(scenarioPar$scenParPrefTrends)) {
    scenSpecPrefTrends <- toolApplyScenPrefTrends(basePrefTrends, scenarioPar$scenParPrefTrends, RawInputData$GDPpcMER, policyStartYear, helpers)
    print("Policy induced changes to baseline preference trends were applied")
  } else {
    scenSpecPrefTrends <- basePrefTrends
    print("No policy induced changes to the baseline preference trends")
  }

  # Application of policy induced changes on load factor ----------------------------
  if (!is.null(scenarioPar$scenParLoadFactor)) {
    scenSpecLoadFactor <- toolApplyScenSpecLoadFactor(RawInputData$loadFactor, scenarioPar$scenParLoadFactor,
                                                      policyStartYear, helpers)
    scenSpecLoadFactor[, variable := "Load factor"]
    print("Policy induced changes to the loadfactor were applied")
  } else {
    scenSpecLoadFactor <- RawInputData$loadFactor[, variable := "Load factor"]
    print("No policy induced changes to the loadfactor")
  }

  # Application of policy induced changes on energy intensity ----------------------------
  if (!is.null(scenarioPar$scenParEnergyIntensity)) {
    scenSpecEnIntensity <- toolApplyScenSpecEnInt(RawInputData$energyIntensity, scenarioPar$scenParEnergyIntensity,policyStartYear, helpers)
    scenSpecEnIntensity[, variable := "Energy intensity sales"]
    print("Policy induced changes to the energy intensity were applied")
  } else {
    scenSpecEnIntensity <- RawInputData$energyIntensity[, variable := "Energy intensity sales"]
    print("No policy induced changes to the energyIntensity")
  }

  # Annualization and formatting of monetary costs ---------------------------------------------------
  annuity <- toolCalculateAnnuity(generalPar$annuityCalc, helpers)
  transportCosts <- toolCombineCAPEXandOPEX(RawInputData$CAPEXtrackedFleet, RawInputData$nonFuelOPEXtrackedFleet, RawInputData$CAPEXother,
                                                  RawInputData$nonFuelOPEXother, RawInputData$fuelCosts, RawInputData$subsidies, scenSpecEnIntensity,
                                                  scenSpecLoadFactor, RawInputData$annualMileage, annuity, helpers)

  # Annualization and formatting of non-monetary costs -------------------------------------------------
  initialIncoCosts <- toolCalculateInitialIncoCost(transportCosts$combinedCAPEXandOPEX, generalPar$incoCostStartVal, annuity, scenSpecLoadFactor, RawInputData$annualMileage, helpers)

  scenSpecInputData <- list(
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecEnIntensity,
    combinedCAPEXandOPEX = transportCosts$combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = transportCosts$upfrontCAPEXtrackedFleet,
    initialIncoCosts = initialIncoCosts)

  return(scenSpecInputData)
}
