#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param loadFactor load factor input data supplied by mrtransport
#' @param scenParLoadFactor Scenario specific parameters to be applied on the baseline load factor
#' @param allEqYear Year after which scenario differentiation sets in
#' @param helpers List with helpers
#' @returns Scenario specific load factor
#' @import data.table

toolApplyScenSpecLoadFactor <- function(loadFactor, scenParLoadFactor, allEqYear, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- univocalName <- NULL

  loadFactor <- copy(loadFactor)
  percentChange <- scenParLoadFactor$percentChange
  targetYear <- scenParLoadFactor$targetYear

  if (length(percentChange) > 1) {
    stop("Scenario specific load factor changes are not unambiguously defined")
  }
  if ("origin" %in% scenParLoadFactor$startYearCat) {
    warning("Error in demand scenario specific changes: only delayed switch-on with allEqYear possible. Please check toolApplyScenSpecLoadFactor()")
  }

  # apply scenario specific load factor adjustments for LDW 4W
  # linear phase-in of percentage factor between max(2020, allEqYear) and targetYear
  loadFactor[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      period > max(2020, allEqYear) &
      period <= targetYear,
    value := value * (1 + percentChange * (period - 2021) / (targetYear - 2021))]

  # constant application of percentage factor after targetYear
  loadFactor[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      period > max(2020, allEqYear) &
      period >= targetYear,
    value := value * (1 + percentChange)]

    return(loadFactor)
}
