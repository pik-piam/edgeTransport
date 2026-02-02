#' Calculate shares for all levels of the decision tree for a variable on FV level
#'
#' @author Johanna Hoppe
#' @param dtVariable data.table of a transport variable, e.g., energy service demand with data at the most detailed level of of the decision tree (FV = fuel-vehicle)
#' @param helpers list of helpers

#'
#' @returns data.table including shares of the variable for all levels of the decision tree
#' @export


toolCalculateSharesDecisionTree <- function(dtVariable, helpers) {

  # Note: The solution right now is hard-coded and does not react to a change of levels/structure in the decision tree elsewhere.
  # This could be easily changed.
  # Right now changes in the levels are not done regularly and the structure is still quite easy to grasp.
  # Hence we stay with the explicit representation for now

  # Current levels and groupValues of the decision tree
  # levels <- c("FV", "VS3", "S3S2", "S2S1", "S1S")
  # groupValue <- c("vehicleType", "subsectorL3", "subsectorL2", "subsectorL1")
  dtVariable <- merge(dtVariable, helpers$decisionTree, by = intersect(names(dtVariable), names(helpers$decisionTree)), all.x = TRUE)
  dtVariable[, univocalName := NULL]

  FV <- copy(dtVariable)[, c("unit", "variable") := NULL]
  FV[, share := value / sum(value), by = c("region", "period", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  FV[is.nan(share) & value == 0, share := 0]
  FV[, level := "FV"][, value := NULL]

  VS3 <- copy(dtVariable)[, c("unit", "variable") := NULL]
  VS3 <- VS3[, .(value = sum(value)), by = c("region", "period", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  VS3[, technology := ""]
  VS3[, share := value / sum(value), by = c("region", "period", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  VS3[is.nan(share) & value == 0, share := 0]
  VS3[, level := "VS3"][, value := NULL]

  S3S2 <- copy(dtVariable)[, c("unit", "variable") := NULL]
  S3S2 <- S3S2[, .(value = sum(value)), by = c("region", "period", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  S3S2[, c("technology", "vehicleType") := ""]
  S3S2[, share := value / sum(value), by = c("region", "period", "subsectorL2", "subsectorL1", "sector")]
  S3S2[is.nan(share) & value == 0, share := 0]
  S3S2[, level := "S3S2"][, value := NULL]

  S2S1 <- copy(dtVariable)[, c("unit", "variable") := NULL]
  S2S1 <- S2S1[, .(value = sum(value)), by = c("region", "period", "subsectorL2", "subsectorL1", "sector")]
  S2S1[, c("technology", "vehicleType", "subsectorL3") := ""]
  S2S1[, share := value / sum(value), by = c("region", "period", "subsectorL1", "sector")]
  S2S1[is.nan(share) & value == 0, share := 0]
  S2S1[, level := "S2S1"][, value := NULL]

  S1S <- copy(dtVariable)[, c("unit", "variable") := NULL]
  S1S <- S1S[, .(value = sum(value)), by = c("region", "period", "subsectorL1", "sector")]
  S1S[, c("technology", "vehicleType", "subsectorL3", "subsectorL2") := ""]
  S1S[, share := value / sum(value), by = c("region", "period", "sector")]
  S1S[is.nan(share) & value == 0, share := 0]
  S1S[, level := "S1S"][, value := NULL]

  return(rbind(FV, VS3, S3S2, S2S1, S1S))
}
