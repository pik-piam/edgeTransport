#' Calculate vehicle sales shares and mode shares for all levels of the decisionTree.
#'
#' Function that traverses each level of the decision tree and calculates the shares according to the discrete choice approach
#'
#' @author Johanna Hoppe
#' @param input dataset for discrete choice module
#' @param generalModelPar general model parameter
#' @param updatedEndoCosts updated endogenous costs
#' @param helpers list of helpers
#' @returns calculated shares
#' @import data.table
#' @export


toolDiscreteChoice <- function(input, generalModelPar, updatedEndoCosts, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  type <- level <- vehicleType <- subsectorL1 <- pref <- lambda <- . <- value <- zeroTypes <- share <- NULL
  totPrice <- testShares <- variable <- univocalName <- period <- technology <- subsectorL3 <- subsectorL2 <- unit <- NULL

  # calculate all FV shares --------------------------------------------------------------------
  CAPEXandOPEX <- copy(input$combinedCAPEXandOPEX)
  CAPEXandOPEX <- merge(CAPEXandOPEX, helpers$decisionTree, by = c("region", "univocalName", "technology"), all.x = TRUE)
  # detailed resolution of CAPEX and OPEX not needed
  CAPEXandOPEX[, type := "Monetary Costs"]
  # vehicles that have endogenous inconvenience costs receive these in addition
  updatedEndoCosts[, type := "Inconvenience costs"]
  allCostsFV <- rbind(CAPEXandOPEX, updatedEndoCosts)
  # vehicles that have preference trends receive these instead
  prefTrends <- copy(input$scenSpecPrefTrends)
  setnames(prefTrends, "value", "pref")
  prefTrends[, c("variable", "unit") := NULL]
  prefTrendsFV <- prefTrends[level == "FV"][, level := NULL]
  FVshares <- merge(allCostsFV, prefTrendsFV, by = intersect(names(allCostsFV), names(prefTrends)), all.x = TRUE, allow.cartesian = TRUE)
  # vehicleTypes with endogenous inconvenience costs have no preferences, which means that all preferences
  # are set to 1 (equivalent expression) as there is no decision for cycling and walking, they have to receive 1 as well
  FVshares[vehicleType %in% unique(updatedEndoCosts$vehicleType) | subsectorL1 %in% c("Cycle", "Walk"), pref := 1]
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "FV"][, level := NULL]
  FVshares <- merge(FVshares, lambdas, by = intersect(names(FVshares), names(lambdas)), all.x = TRUE)
  # no technology decision for active modes, hence no lambda is supplied
  FVshares[subsectorL1 %in% c("Cycle", "Walk"), lambda := -15]
  FVshares <- FVshares[, .(totPrice = sum(value)), by = setdiff(names(FVshares), c("variable", "type", "value"))]
  # Some vehicleTypes are just not present at all in certain regions for certain years
  # They have to be filtered out and get a share of zero
  FVshares[, zeroTypes := sum(pref), by = c("region", "period", "vehicleType")]
  FVsharesZero <-  FVshares[zeroTypes == 0][, share := 0][, c("totPrice", "lambda", "pref", "zeroTypes", "univocalName") := NULL]
  FVshares <- FVshares[!zeroTypes == 0][, c("univocalName", "zeroTypes") := NULL]
  FVshares[, share := calculateSharesDiscreteChoice(totPrice, lambda, pref),
         by = setdiff(names(FVshares), c("technology", "totPrice", "lambda", "pref", "unit"))][, c("totPrice", "lambda", "pref") := NULL]
  FVshares[, testShares := sum(share), by = c("region", "period", "vehicleType")]

  if (nrow(FVshares[testShares < 0.9999 | testShares > 1.0001]) > 0 || anyNA(FVshares))
    stop("FV shares in toolDiscreteChoice() were not calculated correctly")
  FVshares[, c("testShares") := NULL]
  FVshares <- rbind(FVshares, FVsharesZero)[, level := "FV"]
  # Discrete choice cost structure
  storeAllCostsFV <- copy(allCostsFV)
  storeAllCostsFV[, variable := paste0("Logit cost|FV|", variable)][, type := NULL]
  costsDiscreteChoice <- list(allCostsFV = storeAllCostsFV)

  # calculate all VS3 shares --------------------------------------------------------------------
  allCostsFV <- allCostsFV[type == "Monetary Costs"][, univocalName := NULL]
  allCostsFV <- merge(allCostsFV, FVshares[, -c("level")],  by = intersect(names(allCostsFV), names(FVshares)))
  # only FV level features detailed yearly resolution for some vehicle types. Set resolution back to years
  allCostsFV <- allCostsFV[period %in% helpers$lowTimeRes]
  allCostsVS3 <- toolTraverseDecisionTree(allCostsFV, "vehicleType", helpers$decisionTree)
  # time value costs only need to be added starting from level VS3. If there is no decision in the level
  # (only a single branch) the time value costs are kept and aggregated with a share of one
  # to the upper level
  timeValueCosts <- merge(input$timeValueCosts[period %in% helpers$lowTimeRes], unique(helpers$decisionTree[, -c("technology")]), by = c("region", "univocalName"), all.x = TRUE)
  timeValueCosts[, type := "Travel time"][, univocalName := NULL]
  allCostsVS3 <- rbind(allCostsVS3, timeValueCosts)

  prefTrendsVS3 <- prefTrends[level == "VS3"][, "level" := NULL]

  VS3shares <- merge(allCostsVS3, prefTrendsVS3, by = intersect(names(allCostsVS3), names(prefTrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "VS3"]
  lambdas <- lambdas[, c("subsectorL3", "lambda")]
  VS3shares <- merge(VS3shares, lambdas, by = intersect(names(VS3shares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and
  # therefore get no lambda and pref as input. The share equals one with every lambda that is chosen.
  VS3shares[grepl(".*tmp.*", vehicleType), lambda := -15]
  VS3shares[grepl(".*tmp.*", vehicleType), pref := 1]
  VS3shares[grepl(".*tmp.*", vehicleType), technology := ""]

  if (anyNA(VS3shares)) stop("VS3 preferences are missing in toolDiscreteChoice()")
  VS3shares <- VS3shares[, .(totPrice = sum(value)), by = setdiff(names(VS3shares), c("variable", "type", "value"))]
  # Some vehicleTypes are just not present at all in certain regions for certain years
  # They have to be filtered out and get a share of zero
  VS3sharesZero <-  VS3shares[totPrice == 0 | pref == 0][, share := 0][, c("totPrice", "lambda", "pref") := NULL]
  VS3shares <- VS3shares[!(totPrice == 0 | pref == 0)]
  VS3shares[, share := calculateSharesDiscreteChoice(totPrice, lambda, pref),
           by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")][, c("totPrice", "lambda", "pref") := NULL]

  VS3shares[, testShares := sum(share), by = c("region", "period", "subsectorL3")]
  if (nrow(VS3shares[testShares < 0.9999 | testShares > 1.0001]) > 0 || anyNA(VS3shares) || nrow(VS3shares) == 0)
    stop("VS3 shares in toolDiscreteChoice() were not calculated correctly")
  VS3shares[, testShares := NULL]
  VS3shares <- rbind(VS3shares, VS3sharesZero)[, level := "VS3"]
  storeAllCostsVS3 <- copy(allCostsVS3)
  storeAllCostsVS3[, variable := paste0("Logit cost|VS3|", variable)][, type := NULL]
  costsDiscreteChoice <- c(costsDiscreteChoice, list(allCostsVS3 = storeAllCostsVS3))

  # calculate all S3S2 shares --------------------------------------------------------------------
  allCostsVS3 <- merge(allCostsVS3, VS3shares[, -c("level")],  by = intersect(names(allCostsVS3), names(VS3shares)))
  allCostsS3S2 <- toolTraverseDecisionTree(allCostsVS3, "subsectorL3", helpers$decisionTree)

  prefTrendsS3S <- prefTrends[level == "S3S2"][, level := NULL]
  S3S2shares <- merge(allCostsS3S2, prefTrendsS3S, by = intersect(names(allCostsS3S2), names(prefTrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "S3S2"]
  lambdas <- lambdas[, c("subsectorL2", "lambda")]
  S3S2shares <- merge(S3S2shares, lambdas, by = intersect(names(S3S2shares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore
  # get no lambda as input. The share equals one with every lambda that is chosen.
  S3S2shares[grepl(".*tmp.*", subsectorL3), lambda := -15]
  S3S2shares[grepl(".*tmp.*", subsectorL3), pref := 1]
  S3S2shares[grepl(".*tmp.*", subsectorL3), c("technology", "vehicleType") := ""]
  if (anyNA(S3S2shares)) stop("S3S2 preferences are missing in toolDiscreteChoice()")

  S3S2shares <- S3S2shares[, .(totPrice = sum(value)), by = setdiff(names(S3S2shares), c("variable", "type", "value"))]
  S3S2shares[, share := calculateSharesDiscreteChoice(totPrice, lambda, pref),
            by = c("region", "period", "sector", "subsectorL1", "subsectorL2")][, c("totPrice", "lambda", "pref") := NULL]

  S3S2shares[, testShares := sum(share), by = c("region", "period", "subsectorL2")]
  if (nrow(S3S2shares[testShares < 0.9999 | testShares > 1.0001]) > 0 || anyNA(S3S2shares) || nrow(S3S2shares) == 0)
    stop("S3S2 shares in toolDiscreteChoice() were not calculated correctly")
  S3S2shares[, testShares := NULL][, level := "S3S2"]
  storeAllCostsS3S2 <- copy(allCostsS3S2)
  storeAllCostsS3S2[, variable := paste0("Logit cost|S3S2|", variable)][, type := NULL]
  costsDiscreteChoice <- c(costsDiscreteChoice, list(allCostsS3S2 = storeAllCostsS3S2))

  # calculate all S2S1 shares --------------------------------------------------------------------
  allCostsS3S2 <- merge(allCostsS3S2, S3S2shares[, -c("level")],  by = intersect(names(allCostsS3S2), names(S3S2shares)))
  allCostsS2S1 <- toolTraverseDecisionTree(allCostsS3S2, "subsectorL2", helpers$decisionTree)

  prefTrendsS2S1 <- prefTrends[level == "S2S1"][, level := NULL]
  S2S1shares <- merge(allCostsS2S1, prefTrendsS2S1, by = intersect(names(allCostsS2S1), names(prefTrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "S2S1"][, level := NULL]
  lambdas <- lambdas[, c("subsectorL1", "lambda")]
  S2S1shares <- merge(S2S1shares, lambdas, by = intersect(names(S2S1shares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore
  # get no lambda as input. The share equals one with every lambda that is chosen.
  S2S1shares[grepl(".*tmp.*", subsectorL2), lambda := -15]
  S2S1shares[grepl(".*tmp.*", subsectorL2), pref := 1]
  S2S1shares[grepl(".*tmp.*", subsectorL2), c("technology", "vehicleType", "subsectorL3") := ""]
  if (anyNA(S2S1shares)) stop("S2S1 preferences are missing in toolDiscreteChoice()")

  S2S1shares <- S2S1shares[, .(totPrice = sum(value)), by = setdiff(names(S2S1shares), c("variable", "type", "value"))]
  S2S1shares[, share := calculateSharesDiscreteChoice(totPrice, lambda, pref),
             by = c("region", "period", "sector", "subsectorL1")][, c("totPrice", "lambda", "pref") := NULL]

  S2S1shares[, testShares := sum(share), by = c("region", "period", "subsectorL1")]
  if (nrow(S2S1shares[testShares < 0.9999 | testShares > 1.0001]) > 0 || anyNA(S2S1shares) || nrow(S2S1shares) == 0)
    stop("S2S1 shares in toolDiscreteChoice() were not calculated correctly")
  S2S1shares[, testShares := NULL][, level := "S2S1"]
  storeAllCostsS2S1 <- copy(allCostsS2S1)
  storeAllCostsS2S1[, variable := paste0("Logit cost|S2S1|", variable)][, type := NULL]
  costsDiscreteChoice <- c(costsDiscreteChoice, list(allCostsS2S1 = storeAllCostsS2S1))

  # calculate all S1S shares --------------------------------------------------------------------
  allCostsS2S1 <- merge(allCostsS2S1, S2S1shares[, -c("level")],  by = intersect(names(allCostsS2S1), names(S2S1shares)))
  allCostsS1S <- toolTraverseDecisionTree(allCostsS2S1, "subsectorL1", helpers$decisionTree)

  prefTrendsS1S <- prefTrends[level == "S1S"][, level := NULL]
  S1Sshares <- merge(allCostsS1S, prefTrendsS1S, by = intersect(names(allCostsS1S), names(prefTrends)), all.x = TRUE)
  lambdas <- generalModelPar$lambdasDiscreteChoice[level == "S1S"][, level := NULL]
  lambdas <- lambdas[, c("sector", "lambda")]
  S1Sshares <- merge(S1Sshares, lambdas, by = intersect(names(S1Sshares), names(lambdas)), all.x = TRUE)
  # modes marked with a "tmp" have only a single branch in this level of the decision tree and therefore
  # get no lambda as input. The share equals one with every lambda that is chosen.
  S1Sshares[grepl(".*tmp.*", subsectorL1), lambda := -15]
  S1Sshares[grepl(".*tmp.*", subsectorL1), pref := 1]
  S1Sshares[grepl(".*tmp.*", subsectorL1), c("technology", "vehicleType", "subsectorL3", "subsectorL2") := ""]
  if (anyNA(S2S1shares)) stop("S1S preferences are missing in toolDiscreteChoice()")

  S1Sshares <- S1Sshares[, .(totPrice = sum(value)), by = setdiff(names(S1Sshares), c("variable", "type", "value"))]
  S1Sshares[, share := calculateSharesDiscreteChoice(totPrice, lambda, pref),
             by = c("region", "period", "sector")][, c("totPrice", "lambda", "pref") := NULL]

  S1Sshares[, testShares := sum(share), by = c("region", "period", "sector")]
  if (nrow(S1Sshares[testShares < 0.9999 | testShares > 1.0001]) > 0 || anyNA(S1Sshares) || nrow(S1Sshares) == 0)
    stop("S1S shares in toolDiscreteChoice() were not calculated correctly")
  S1Sshares[, testShares := NULL][, level := "S1S"]
  storeAllCostsS1S <- copy(allCostsS1S)
  storeAllCostsS1S[, variable := paste0("Logit cost|S1S|", variable)][, type := NULL]
  costsDiscreteChoice <- c(costsDiscreteChoice, list(allCostsS1S = storeAllCostsS1S))

  # format --------------------------------------------------------------------
  shares <- rbind(FVshares, VS3shares, S3S2shares, S2S1shares, S1Sshares)[, unit := "-"]
  # toolCheckAllLevelsComplete(shares, helpers$decisionTree, "vehicle sales and mode shares")

  return(list(shares = shares,
              costsDiscreteChoice = costsDiscreteChoice))

}
