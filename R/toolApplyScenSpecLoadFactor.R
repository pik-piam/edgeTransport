#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param loadFactor load factor input data supplied by mrtransport
#' @param scenParLoadFactor Scenario specific parameters to be applied don the baseline load factor
#' @param policyStartYear Year when scenario differentiation sets in
#' @param helpers List with helpers
#' @returns Scenario specific load factor
#' @import data.table

toolApplyScenSpecLoadFactor <- function(loadFactor, scenParLoadFactor, policyStartYear, helpers) {

  loadFactor <- copy(loadFactor)
  percentChange <- scenParLoadFactor$percentChange
  targetYear <- scenParLoadFactor$targetYear

  if (length(percentChange) > 1) {
    stop("Scenario specific load factor changes are not unambiguously defined")
  }

  loadFactor[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      period >= policyStartYear &
      period <= targetYear,
    value := value * (1 + percentChange * (period - policyStartYear)/(targetYear - policyStartYear))]

  loadFactor[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      period >= policyStartYear &
      period >= targetYear,
    value := value * (1 + percentChange)]

  return(loadFactor)
}
