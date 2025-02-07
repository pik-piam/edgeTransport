#' Apply scenario specific adjustments to the preference trends
#' @author Johanna Hoppe
#' @param baselinePrefTrends Baseline preference trends
#' @param scenParPrefTrends Scenario parameters to be applied on the preference trends
#' @param GDPpcMER Per capita GDP based on market exchange rate
#' @param policyStartYear Year from which the scenario parameters are applied on the baseline preference trends
#' @param GDPcutoff Threshold used to categorize countries into different mitigation groups based on their GDP
#' @param helpers List containing several helpers used throughout the model
#' @param isICEban Switch to turn on ICE phase out policies
#' @returns Scenario specific preference trends
#' @import data.table

toolApplyScenPrefTrends <- function(baselinePrefTrends, scenParPrefTrends, GDPpcMER, policyStartYear, GDPcutoff, helpers, isICEban, cm_startYear = 2025) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- region <- variable <- unit <- level <- vehicleType <- FVvehvar <- regionCat <- symmyr <- speed <- target <- old <- NULL

  # function to apply mitigation factors
  applyLogisticTrend <- function(year, final, ysymm, speed, initial = 1) {
    fct <- exp((year - ysymm) / speed) / (exp((year - ysymm) / speed) + 1)
    initial + fct * (final - initial)
  }

  # Check if a transportPol or SSPscen change is introduced with cm_startYear
  # If both stay the same, set cm_startYear out of bounds such that it does not affect the calculation here
  if (!"final" %in% scenParPrefTrends$startYearCat){
    cm_startYear <- 2200
  }

  # restructure mitigation factors provided in scenParPrefTrends
  # resolve techmap
  GDPpcMER <- copy(GDPpcMER)[, c("variable", "unit") := NULL]
  mitigationFactors <- merge(helpers$mitigationTechMap[, c("vehicleType", "FVvehvar")], scenParPrefTrends, by = "FVvehvar", all.y = TRUE, allow.cartesian = TRUE)
  mitigationFactors[is.na(vehicleType), vehicleType := ""][, FVvehvar := NULL]
  # implement differentiation by GDP and treatment of single region entries
  GDPpcMER <- GDPpcMER[period == 2020][, period := NULL]
  GDPpcMER[, regionCat := ifelse(value > GDPcutoff, "above GDP cutoff", "below GDP cutoff")]
  GDPpcMER <- GDPpcMER[, c("region", "regionCat")]
  # some regions have individual mitigation factors
  individualReg <- unique(mitigationFactors[!regionCat %in% c("above GDP cutoff", "below GDP cutoff")]$regionCat)
  GDPpcMER[, regionCat := ifelse(region %in% individualReg, region, regionCat)]
  mitigationFactors <- merge(mitigationFactors, GDPpcMER, by = "regionCat", allow.cartesian = TRUE, all.x = TRUE)[, regionCat := NULL]
  # apply mitigation factors
  checkMitigation <- copy(baselinePrefTrends)
  setnames(checkMitigation, "value", "old")

  # Assemble PrefTrends according to scenario before and after the startyear
  PrefTrends <- merge(baselinePrefTrends[period <= cm_startYear], mitigationFactors[startYearCat == "origin"], by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  PrefTrendsF <- merge(baselinePrefTrends[period > cm_startYear], mitigationFactors[startYearCat == "final"], by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  PrefTrends <- rbind(PrefTrends, PrefTrendsF)
  PrefTrends[, "startYearCat" := NULL]
  PrefTrends[period  >= policyStartYear & !is.na(target), value := value * applyLogisticTrend(period, target, symmyr, speed)][, c("target", "symmyr", "speed") := NULL]

  check <- merge(checkMitigation, PrefTrends, by = intersect(names(checkMitigation), names(PrefTrends)), all = TRUE)
  check[, diff := abs(value - old)]
  if (max(check$diff) < 0.001) stop("Mitigation preference factors have not been applied correctly. Please check toolApplyScenPrefTrends()")

  PrefTrends[, variable := paste0("Preference|", level)][, unit := "-"]
  # order
  PrefTrends <- PrefTrends[, c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "variable", "unit", "value")]

return(PrefTrends)

}
