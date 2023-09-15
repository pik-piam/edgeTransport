#' Calculate annuity for different vehicle types
#' 
#' @author Johanna Hoppe
#' @param annuityCalc input data from mrtransport 
#' @param mitigationTechMap map to disaggregate vehicle types
#' @import data.table


toolCalculateAnnuity <- function(annuityCalc, mitigationTechMap) {

annuity <- merge(mitigationTechMap, annuityCalc, by = "FVvehvar", all.y = TRUE)[, FVvehvar := NULL]
# Calculate annuity factor to annualize CAPEX
annuity[, annuity := (1 + interestRate ^ serviceLife  * interestRate)/((1 + ainterestRate) ^ serviceLife - 1)][, c("interestRate", "serviceLife") := NULL]

return(annuity)
}