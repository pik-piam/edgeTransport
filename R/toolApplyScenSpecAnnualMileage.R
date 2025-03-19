#' Apply demand scenario specific adjustments to the annual mileage
#' @author Jarusch Muessel
#' @param annualMileage annual mileage input data supplied by mrtransport
#' @param scenParAnnualMileage Scenario specific parameters to be applied don the baseline annual mileage
#' @param policyStartYear Year when scenario differentiation sets in
#' @param helpers List with helpers
#' @returns Scenario specific annual mileage
#' @import data.table
                                    

toolApplyScenSpecAnnualMileage <- function(annualMileage, scenParAnnualMileage, policyStartYear, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- univocalName <- NULL

  annualMileage <- copy(annualMileage)
  percentChange <- scenParAnnualMileage$percentChange
  targetYear <- scenParAnnualMileage$targetYear

  if (length(percentChange) > 1) {
    stop("Scenario specific annual mileage changes are not unambiguously defined")
  }

  annualMileage[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      period >= policyStartYear &
      period <= targetYear & 
      region == "IND",
      value := value * (1 + percentChange * (period - policyStartYear) / (targetYear - policyStartYear))]

  annualMileage[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      period >= policyStartYear &
      period >= targetYear & 
      region == "IND",
    value := value * (1 + percentChange)]

  return(annualMileage)
}
