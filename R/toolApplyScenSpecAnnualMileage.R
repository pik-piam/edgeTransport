#' Apply demand scenario specific adjustments to the annual mileage
#' @author Jarusch Muessel
#' @param annualMileage annual mileage input data supplied by mrtransport
#' @param scenParAnnualMileage Scenario specific parameters to be applied don the baseline annual mileage
#' @param policyStartYear Year when scenario differentiation sets in
#' @param helpers List with helpers
#' @returns Scenario specific annual mileage
#' @import data.table
                                    

toolApplyScenSpecAnnualMileage <- function(annualMileage, scenParAnnualMileage, allEqYear, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- univocalName <- NULL

  annualMileage <- copy(annualMileage)
  percentChange <- scenParAnnualMileage$percentChange
  targetYear <- scenParAnnualMileage$targetYear

  if (length(percentChange) > 1) {
    stop("Scenario specific annual mileage changes are not unambiguously defined, check toolApplyScenSpecAnnualMileage()")
  }
  if ("origin" %in% scenParAnnualMileage$startYearCat) {
    stop("Error in demand scenario specific changes: only delayed switch-on with allEqYear possible. Please check toolApplyScenSpecLoadFactor()")
  }

  annualMileage[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
      region == "IND" & 
      period > max(2020, allEqYear) &
      period <= targetYear,
    value := value * (1 + percentChange * (period - 2021) / (targetYear - 2021))]

  annualMileage[
    univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W &
    region == "IND" & 
    period > max(2020, allEqYear) &
    period > targetYear,
    value := value * (1 + percentChange)]
    
  return(annualMileage)
}
