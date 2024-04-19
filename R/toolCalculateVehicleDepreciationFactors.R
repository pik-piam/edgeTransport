#' Calculate vehicle depreciation factors for fleet tracking
#'
#' @author Johanna Hoppe
#' @param annuityCalc parameters for annualization
#' @param helpers list with helpers
#' @returns data.table including vehicle depreciation factors over service life#'
#' @import data.table

toolCalculateVehicleDepreciationFactors <- function(annuityCalc, helpers) {

  # calculate depreciation factors
  vehServiceLifes <- annuityCalc[, c("FVvehvar", "serviceLife")]
  depreciationTimeSteps <- data.table(indexUsagePeriod = seq(0, max(unique(vehServiceLifes$serviceLife), 1)))
  depreciationTimeSteps[, timesteps := "all"]
  vehServiceLifes[, timesteps := "all"]
  depreciationFactors <- merge(vehServiceLifes, depreciationTimeSteps, by = "timesteps", allow.cartesian = TRUE)
  depreciationFactors[, timesteps := NULL]
  depreciationFactors <- depreciationFactors[indexUsagePeriod <= serviceLife]
  depreciationFactors[, depreciationFactor := 1 - ((indexUsagePeriod - 0.5) / serviceLife) ^ 4]
  depreciationFactors[indexUsagePeriod == 0, depreciationFactor := 1]
  depreciationFactors <- merge(depreciationFactors, helpers$mitigationTechMap[, c("FVvehvar", "univocalName")],
                               by = "FVvehvar", allow.cartesian = TRUE)
  depreciationFactors[, FVvehvar := NULL]

  return(depreciationFactors)
}
