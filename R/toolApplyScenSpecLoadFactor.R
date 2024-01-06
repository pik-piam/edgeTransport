#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param loadFactor load factor input data supplied by mrtransport
#' @param demandScen tranport demand scenario
#' @param SSPscenario shared socioeconomic pathway
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated univocalNames
#' @import data.table


toolApplyScenSpecLoadFactor <- function(loadFactor, scenParLoadFactor, policyStartYear, helpers) {

percentChange <- scenParLoadFactor$percentChange
targetYear <- scenParLoadFactor$targetYear

if (length(percentChange) > 1) {
  stop("Scenario specific load factor changes are not unambiguously defined")
}

  loadFactor[
    univocalName %in% helpers$filter$trn_pass_road_LDV_4W &
      period >= policyStartYear &
      period <= targetYear,
    value := value * (1 + percentChange * (period - policyStartYear)/(targetYear - policyStartYear))]

  loadFactor[
    univocalName %in% helpers$filter$trn_pass_road_LDV_4W &
      period >= policyStartYear &
      period >= targetYear,
    value := value * (1 + percentChange)]

  return(loadFactor)
}
