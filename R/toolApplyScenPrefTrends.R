#' Apply scenario specific adjustments to the preference trends
#' @author Johanna Hoppe, Alex K. Hagen
#' @param baselinePrefTrends Baseline preference trends
#' @param scenParPrefTrends Scenario parameters to be applied on the preference trends
#' @param GDPpcMER Per capita GDP based on market exchange rate
#' @param allEqYear Year after which scenario differentiation sets in
#' @param GDPcutoff Threshold used to categorize countries into different mitigation groups based on their GDP
#' @param helpers List containing several helpers used throughout the model
#' @returns Scenario specific preference trends
#' @import data.table

toolApplyScenPrefTrends <- function(baselinePrefTrends, scenParPrefTrends, GDPpcMER, allEqYear, GDPcutoff, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- region <- variable <- unit <- level <- vehicleType <- NULL
  FVvehvar <- regionCat <- symmyr <- speed <- target <- old <- startYearCat <- NULL
  subsectorL1 <- subsectorL2 <- technology <- NULL

  # function to apply mitigation factors
  applyLogisticTrend <- function(year, final, ysymm, speed, initial = 1) {
    fct <- exp((year - ysymm) / speed) / (exp((year - ysymm) / speed) + 1)
    initial + fct * (final - initial)
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
  if (!"full" %in% scenParPrefTrends$startYearCat){
  PrefTrends <- merge(baselinePrefTrends[period <= allEqYear], mitigationFactors[startYearCat == "origin"], by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  PrefTrendsF <- merge(baselinePrefTrends[period > allEqYear], mitigationFactors[startYearCat == "final"], by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  PrefTrends <- rbind(PrefTrends, PrefTrendsF)
  setkey(PrefTrends, region, level, subsectorL1, subsectorL2, vehicleType, technology)
  PrefTrends[, "startYearCat" := NULL]
  } else {
    PrefTrends <- merge(baselinePrefTrends, mitigationFactors[startYearCat == "full"], by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  }
  generalTransportPolicyOnset <- 2020
  PrefTrends[period > generalTransportPolicyOnset & !is.na(target), value := value * applyLogisticTrend(period, target, symmyr, speed)][, c("target", "symmyr", "speed") := NULL]

  check <- merge(checkMitigation, PrefTrends, by = intersect(names(checkMitigation), names(PrefTrends)), all = TRUE)
  check[, diff := abs(value - old)]
  if (max(check$diff) < 0.001) stop("Mitigation preference factors have not been applied correctly. Please check toolApplyScenPrefTrends()")

  PrefTrends[, variable := paste0("Preference|", level)][, unit := "-"]
  # order
  PrefTrends <- PrefTrends[, c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "variable", "unit", "value")]

return(PrefTrends)

}
