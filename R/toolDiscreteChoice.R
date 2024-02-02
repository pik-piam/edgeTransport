#' Calculate vehicle sales shares and mode shares for all levels of the decisionTree.
#'
#' Function that traverses each level of the decision tree and calculates the shares according to the discrete choice approach
#'
#' @author Johanna Hoppe
#' @param dt data.table containing
#' @param category
#' @returns data.table
#' @import data.table
#' @export


toolDiscreteChoice <- function(input, generalModelPar, updatedEndoCosts, years, helpers) {

  # calculate all FV shares --------------------------------------------------------------------
  CAPEXandOPEX <- copy(input$combinedCAPEXandOPEX)
  CAPEXandOPEX <- merge(CAPEXandOPEX, helpers$decisionTree, by = c("region", "univocalName", "technology"), all = TRUE)
  # detailed resolution of CAPEX and OPEX not needed
  CAPEXandOPEX[, type := "Monetary Costs"]
  CAPEXandOPEX <- CAPEXandOPEX[, .(value = sum(value)), by = c(setdiff(names(CAPEXandOPEX), c("value", "variable")))]
  CAPEXandOPEX[, variable := "CAPEX and OPEX"]
  # vehicles that have endogenous inconvenience costs receive these in addition
  updatedEndoCosts[, type := "Inconvenience costs"]
  # time resolution of monetary costs for vehicles that have endogenous inconvenience costs needs to be adjusted
  timesteps <- unique(updatedEndoCosts$period)
  CAPEXandOPEXEndo <- CAPEXandOPEX[univocalName %in% unique(updatedEndoCosts$univocalName)]
  CAPEXandOPEXEndo <- approx_dt(CAPEXandOPEXEndo, timesteps, "period", "value",
                             c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType","technology", "univocalName", "variable", "unit", "type"),
                              extrapolate = FALSE)
  CAPEXandOPEX <- rbind(CAPEXandOPEX[!univocalName %in% unique(updatedEndoCosts$univocalName)], CAPEXandOPEXEndo)

  allCostsFV <- rbind(CAPEXandOPEX, updatedEndoCosts)
  # vehicles that have preference trends receive these instead
  setnames(input$prefTrends, "value", "pref")
  input$prefTrends[, c("variable", "unit") := NULL]
  preftrends <- input$prefTrends[level == "FV"][, level := NULL]
  FVshares <- merge(allCostsFV, preftrends, by = intersect(names(allCostsFV), names(preftrends)), all.x = TRUE, allow.cartesian = TRUE)
  # vehicleTypes with endogenous inconvenience costs have no preferences, which means that all preferences are set to 1 (equivalent expression)
  FVshares[vehicleType %in% unique(updatedEndoCosts$vehicleType), pref := 1]
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "FV"][, level := NULL]
  FVshares <- merge(FVshares, lambdas, by = intersect(names(FVshares), names(lambdas)), all.x = TRUE)
  # no technology decision for active modes, hence no lambda is supplied
  FVshares[subsectorL1 %in% c("Cycle", "Walk"), lambda := -15]

  FVshares <- FVshares[, .(totPrice = sum(value)), by = setdiff(names(FVshares), c("variable", "type", "value"))]
  FVshares[, share := calculateShares(totPrice, lambda, pref),
          by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")][, c("totPrice", "lambda", "pref") := NULL]

  FVshares[, test := sum(share), by = c("region", "period", "vehicleType")]
  if (nrow(FVshares[test < 0.9999 | test > 1.0001]) > 0 | anyNA(FVshares)) stop("FV shares in toolDiscreteChoice() were not calculated correctly")
  FVshares[, c("test", "univocalName") := NULL][, level := "FV"]

  # calculate all VS3 shares --------------------------------------------------------------------

  allCostsFV <- allCostsFV[type == "Monetary Costs"][, univocalName := NULL]
  allCostsFV <- merge(allCostsFV, FVshares[, -c("level")],  by = intersect(names(allCostsFV), names(FVshares)))
  # only FV level features detailed yearly resolution for some vehicle types. Set resolution back to years
  allCostsFV <- allCostsFV[period %in% years]
  allCostsVS3 <- toolTraverseDecisionTree(allCostsFV, "vehicleType", helpers$decisionTree)
  # time value costs only need to be added starting from level VS3. If there is no decision in the level (only a single branch) the time value costs are kept
  # and aggregated with a share of one to the upper level
  timeValueCosts <- merge(input$timeValueCosts, unique(helpers$decisionTree[, -c("technology")]), by = c("region", "univocalName"), all.x = TRUE)
  timeValueCosts[, type := "Travel time"][, univocalName := NULL]
  allCostsVS3 <- rbind(allCostsVS3, timeValueCosts)

  preftrends <- input$prefTrends[level == "VS3"][, level := NULL]
  VS3shares <- merge(allCostsVS3, preftrends, by = intersect(names(allCostsVS3), names(preftrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "VS3"]
  lambdas <- lambdas[, c("subsectorL3", "lambda")]
  VS3shares <- merge(VS3shares, lambdas, by = intersect(names(VS3shares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore get no lambda and pref as input. The share equals one with every lambda that is chosen.
  VS3shares[grepl(".*tmp.*", vehicleType), lambda := -15]
  VS3shares[grepl(".*tmp.*", vehicleType), pref := 1]
  VS3shares[grepl(".*tmp.*", vehicleType), technology := ""]
  if (anyNA(VS3shares)) stop("VS3 preferences are missing in toolDiscreteChoice()")

  VS3shares <- VS3shares[, .(totPrice = sum(value)), by = setdiff(names(VS3shares), c("variable", "type", "value"))]
  VS3shares[, share := calculateShares(totPrice, lambda, pref),
           by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")][, c("totPrice", "lambda", "pref") := NULL]

  VS3shares[, test := sum(share), by = c("region", "period", "subsectorL3")]
  if (nrow(VS3shares[test < 0.9999 | test > 1.0001]) > 0 | anyNA(VS3shares) | nrow(VS3shares) == 0) stop("VS3 shares in toolDiscreteChoice() were not calculated correctly")
  VS3shares[, test := NULL][, level := "VS3"]

  # calculate all S3S2 shares --------------------------------------------------------------------

  allCostsVS3 <- merge(allCostsVS3, VS3shares[, -c("level")],  by = intersect(names(allCostsVS3), names(VS3shares)))
  allCostsS3S2 <- toolTraverseDecisionTree(allCostsVS3, "subsectorL3", helpers$decisionTree)

  preftrends <- input$prefTrends[level == "S3S2"][, level := NULL]
  S3S2shares <- merge(allCostsS3S2, preftrends, by = intersect(names(allCostsS3S2), names(preftrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "S3S2"]
  lambdas <- lambdas[, c("subsectorL2", "lambda")]
  S3S2shares <- merge(S3S2shares, lambdas, by = intersect(names(S3S2shares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore get no lambda as input. The share equals one with every lambda that is chosen.
  S3S2shares[grepl(".*tmp.*", subsectorL3), lambda := -15]
  S3S2shares[grepl(".*tmp.*", subsectorL3), pref := 1]
  S3S2shares[grepl(".*tmp.*", subsectorL3), c("technology", "vehicleType") := ""]
  if (anyNA(S3S2shares)) stop("S3S2 preferences are missing in toolDiscreteChoice()")

  S3S2shares <- S3S2shares[, .(totPrice = sum(value)), by = setdiff(names(S3S2shares), c("variable", "type", "value"))]
  S3S2shares[, share := calculateShares(totPrice, lambda, pref),
            by = c("region", "period", "sector", "subsectorL1", "subsectorL2")][, c("totPrice", "lambda", "pref") := NULL]

  S3S2shares[, test := sum(share), by = c("region", "period", "subsectorL2")]
  if (nrow(S3S2shares[test < 0.9999 | test > 1.0001]) > 0 | anyNA(S3S2shares) | nrow(S3S2shares) == 0) stop("S3S2 shares in toolDiscreteChoice() were not calculated correctly")
  S3S2shares[, test := NULL][, level := "S3S2"]

  # calculate all S2S1 shares --------------------------------------------------------------------

  allCostsS3S2 <- merge(allCostsS3S2, S3S2shares[, -c("level")],  by = intersect(names(allCostsS3S2), names(S3S2shares)))
  allCostsS2S1 <- toolTraverseDecisionTree(allCostsS3S2, "subsectorL2", helpers$decisionTree)

  preftrends <- input$prefTrends[level == "S2S1"][, level := NULL]
  S2S1shares <- merge(allCostsS2S1, preftrends, by = intersect(names(allCostsS2S1), names(preftrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "S2S1"][, level := NULL]
  lambdas <- lambdas[, c("subsectorL1", "lambda")]
  S2S1shares <- merge(S2S1shares, lambdas, by = intersect(names(S2S1shares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore get no lambda as input. The share equals one with every lambda that is chosen.
  S2S1shares[grepl(".*tmp.*", subsectorL2), lambda := -15]
  S2S1shares[grepl(".*tmp.*", subsectorL2), pref := 1]
  S2S1shares[grepl(".*tmp.*", subsectorL2), c("technology", "vehicleType", "subsectorL3") := ""]
  if (anyNA(S2S1shares)) stop("S2S1 preferences are missing in toolDiscreteChoice()")

  S2S1shares <- S2S1shares[, .(totPrice = sum(value)), by = setdiff(names(S2S1shares), c("variable", "type", "value"))]
  S2S1shares[, share := calculateShares(totPrice, lambda, pref),
             by = c("region", "period", "sector", "subsectorL1")][, c("totPrice", "lambda", "pref") := NULL]

  S2S1shares[, test := sum(share), by = c("region", "period", "subsectorL1")]
  if (nrow(S2S1shares[test < 0.9999 | test > 1.0001]) > 0 | anyNA(S2S1shares) | nrow(S2S1shares) == 0) stop("S2S1 shares in toolDiscreteChoice() were not calculated correctly")
  S2S1shares[, test := NULL][, level := "S2S1"]

  # calculate all S1S shares --------------------------------------------------------------------

  allCostsS2S1 <- merge(allCostsS2S1, S2S1shares[, -c("level")],  by = intersect(names(allCostsS2S1), names(S2S1shares)))
  allCostsS1S <- toolTraverseDecisionTree(allCostsS2S1, "subsectorL1", helpers$decisionTree)

  preftrends <- input$prefTrends[level == "S1S"][, level := NULL]
  S1Sshares <- merge(allCostsS1S, preftrends, by = intersect(names(allCostsS1S), names(preftrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "S1S"][, level := NULL]
  lambdas <- lambdas[, c("sector", "lambda")]
  S1Sshares <- merge(S1Sshares, lambdas, by = intersect(names(S1Sshares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore get no lambda as input. The share equals one with every lambda that is chosen.
  S1Sshares[grepl(".*tmp.*", subsectorL1), lambda := -15]
  S1Sshares[grepl(".*tmp.*", subsectorL1), pref := 1]
  S1Sshares[grepl(".*tmp.*", subsectorL1), c("technology", "vehicleType", "subsectorL3", "subsectorL2") := ""]
  if (anyNA(S2S1shares)) stop("S1S preferences are missing in toolDiscreteChoice()")

  S1Sshares <- S1Sshares[, .(totPrice = sum(value)), by = setdiff(names(S1Sshares), c("variable", "type", "value"))]
  S1Sshares[, share := calculateShares(totPrice, lambda, pref),
             by = c("region", "period", "sector")][, c("totPrice", "lambda", "pref") := NULL]

  S1Sshares[, test := sum(share), by = c("region", "period", "sector")]
  if (nrow(S1Sshares[test < 0.9999 | test > 1.0001]) > 0 | anyNA(S1Sshares) | nrow(S1Sshares) == 0) stop("S1S shares in toolDiscreteChoice() were not calculated correctly")
  S1Sshares[, test := NULL][, level := "S1S"]

  # format --------------------------------------------------------------------
  shares <- rbind(FVshares, VS3shares, S3S2shares, S2S1shares, S1Sshares)[, unit := "-"]
  toolCheckAllLevelsComplete(copy(shares), helpers$decisionTree, "vehicle sales and mode shares")

  return(shares)

}
