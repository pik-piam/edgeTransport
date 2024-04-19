#' Calculate annuity for different vehicle types
#'
#' @author Johanna Hoppe
#' @param annuityCalc input data for interest rate and service Life from edgeTransport
#' @param mitigationTechMap map to disaggregate vehicle types
#' @import data.table


toolCalculateAnnuity <- function(annuityCalc, helpers) {

annuity <- merge(helpers$mitigationTechMap[, c("FVvehvar", "univocalName")], annuityCalc, by = "FVvehvar", all.y = TRUE)[, FVvehvar := NULL]
# Calculate annuity factor to annualize CAPEX
annuity[, annuity := ((1 + interestRate) ^ serviceLife  * interestRate)/((1 + interestRate) ^ serviceLife - 1)][, c("interestRate", "serviceLife") := NULL]

return(annuity)
}
