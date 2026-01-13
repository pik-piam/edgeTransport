#' Calibrate the logit share weights to historical data.
#'
#' @param combinedCosts Annualized total cost of ownership
#' @param histESdemand Historical energy service demand data
#' @param timeValueCost Time value cost for passenger transport modes
#' @param lambdas Exponents for discrete choice function
#' @param helpers list with helpers
#' @returns data.table with calibrated historical preferences
#'
toolCalibrateHistPrefs <- function(combinedCosts, histESdemand, timeValueCost, lambdas, helpers){
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- preference <- lambda <- share <- . <- value <- univocalName <- level <- subsectorL3 <- variable <- unit <- NULL

  # Optimization function: Non-linear set of equations to solve. Sha are the calculated shares from empirical data used to calibrate preferences x.
  # Equations are derived by solving logit function to caluclate shares (Rottoli et al.) for zero.
  # (1/min(pri ^ lamb)) is a factor applied to both sides of the equation to keep it closer to one
  optFunction <- function(x, pri, sha, lamb){
    sha * sum(x * pri ^ lamb) / min(pri ^ lamb) - x * pri ^ lamb / min(pri ^ lamb)
  }

  # rootFunction wraps a multivariate root solver (rootSolve::multiroot) that:
  # - Starts from an initial guess factor (vector of starting values for preferences).
  # - Solves optFunction(...) = 0 for x, subject to positivity.
  # - Returns results$root, the calibrated preferences for that node.
  rootFunction <- function(prices, shares, lambda, factor){
    results <- suppressWarnings(rootSolve::multiroot(f = optFunction, start = factor, pri = prices, sha = shares,
                                    lamb = lambda, positive = T))
    return(results$root)
  }

  # function that compares the shares you obtained with the calibrated preference and the theoretical shares, known
  checkShares = function(df , groupingValue){
    shareCheck <- shareDiff <-  NULL

    df[, shareCheck := preference * totPrice ^ lambda / sum(preference * totPrice ^ lambda),
       by = c("region", "period", groupingValue)]

    df[, shareDiff := (shareCheck - share) ^ 2]
  }

  # for loop that tries multiple initial points for the preference calibration.
  # The function stops when all the preference have been successfully calculated
  # Returns the dt with the calculated preference, already normalized

  calculatePreferences <- function(dfPreference, groupingValue, varyGuess, tol = 0.01){
    fac <- shareDiff <- NULL

    # loops through all the initial points suggested until suitable preferences are found
    for (variation in varyGuess) {
      # if this is the first iteration, an empty column is needed
      if(is.null(dfPreference$preference)){dfPreference[, preference := NaN]}
      # treats all the lambdas separately (is this necessary?)
      for (lamb in unique(dfPreference$lambda)) {
        # only for the nodes that have the specific lambda AND preference that are still not
        # calculated/did not work out the calculation
        dfPreference[lambda == lamb & is.nan(preference),
                     # provides a starting point
                     # Heuristic works as follows: Larger share ->larger initial preference.
                     # Incorporation of relative price in the guess depending on chosen variation
                     # variation = 0, no influence of the price on the guess
                     # variation > 0, the greater, the higher the influence:
                     # cheap price (compared to maximum in branch) lowers guess:
                     # Final share is suspected to be more price driven and less preference driven
                     # Expensive options can be chosen due to high preferences (luxury cars, more comfort) despite higher prices
                     guess := share / max(share) * (totPrice / max(totPrice)) ^ variation,
                     by = c("region", "period", groupingValue)]
        # apply the root function
        dfPreference[lambda == lamb  & is.nan(preference),
                     preference := rootFunction(totPrice, share, lamb, guess),
                     by = c("region", "period", groupingValue)]
      }

      # normalize preference. If the maximum is 0, this is going to give NaN,
      # which is recognized from the loop as "still work in progress"
      dfPreference[,
                   preference := preference / max(preference),
                   by = c("region", "period",groupingValue)]

      # Check solutions
      checkShares(dfPreference, groupingValue)
      # Don't accept bad solutions
      dfPreference[shareDiff >= tol, preference := NaN]

      # exit the loop if all the preference are calculated and there are no NaNs
      if (!any(is.nan(dfPreference$preference)) & !is.null(dfPreference$preference)) {
        break
      }
    }
    return(dfPreference)
  }

  dataStructureDummy <- names(helpers$decisionTree)
  dataStructureDummy <- dataStructureDummy[!dataStructureDummy %in% c("univocalName")]

  histESdemand <- copy(histESdemand)
  setnames(histESdemand, "value", "histESdemand")
  histESdemand <- merge(histESdemand, helpers$decisionTree, by = c("region", "univocalName", "technology"))
  histESdemand[, share := histESdemand / sum(histESdemand), by = c("region", "univocalName", "period")]
  histESdemand[is.nan(share) & histESdemand == 0, share := 0]
  histESdemand[, c("variable", "unit") := NULL]
  totPrice <- copy(combinedCosts)
  totPrice <- totPrice[, .(totPrice = sum(value)), by = c("region", "univocalName", "technology", "period")]
  FVpreference <- merge(histESdemand, totPrice, by = c("region", "univocalName", "technology", "period"))
  prefFVzero <- FVpreference[histESdemand == 0][, preference := 0][, shareCheck := 0][, shareDiff := 0]
  prefFVactive <- FVpreference[univocalName %in% c("Cycle", "Walk")][, preference := 1][, shareCheck := 1][, shareDiff := 0]
  FVpreference <- FVpreference[!univocalName %in% c("Cycle", "Walk") & !histESdemand == 0]
  FVpreference <- merge(FVpreference, lambdas[level == "FV"],
                        by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType"),
                        all.x = TRUE)[, level := NULL]
  # varyGuess inluences the impact of the price on the heuristic that determines the guesses for the solver.
  # The entries in the varyGuess vector are independent of each other and are applied to the heuristic/
  # tried out with the solver one after the other (without influencing the subsequent guess)
  FVpreference <- suppressMessages(calculatePreferences(dfPreference = FVpreference,
                                                        varyGuess = c(2, 1, 3, 4, 5, 6, 7), groupingValue = "vehicleType"))
  FVpreference[, c("guess", "lambda") := NULL]

  FVpreference  <- rbind(FVpreference, prefFVzero, prefFVactive)
  VS3preference <- copy(FVpreference)
  FVpreference <- FVpreference[, c(dataStructureDummy, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "FV"]

  ## -----------------------------------------------------------------------------------------
  VS3preference <- VS3preference[period %in% helpers$lowTimeRes]
  VS3preference <- VS3preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),
                                 by = c("region", "period", "sector", "subsectorL1", "subsectorL2",
                                        "subsectorL3", "vehicleType", "univocalName")]
  VS3preference[, share := histESdemand / sum(histESdemand),
                by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  VS3preference[is.nan(share) & histESdemand == 0, share := 0]
  timeValueCost <- copy(timeValueCost)
  setnames(timeValueCost, c("value"), c("timeValueCost"))
  timeValueCost[, c("variable", "unit") := NULL]
  VS3preference <- merge(VS3preference, timeValueCost, by = c("region", "period", "univocalName"), all.x = TRUE)
  VS3preference[is.na(timeValueCost), timeValueCost := 0]
  VS3preference[, totPrice := totPrice + timeValueCost]
  prefVS3zero <- VS3preference[share == 0 & histESdemand == 0][, preference := 0][, shareCheck := 0][, shareDiff := 0]
  VS3preference <- merge(VS3preference, lambdas[level == "VS3",
                                                c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "lambda")],
                         by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3"), all.x = TRUE)
  VS3preference[is.na(lambda), lambda := -10]

  VS3preference <-  VS3preference[!(share == 0 & histESdemand == 0)]
  VS3preference <- calculatePreferences(dfPreference = VS3preference, varyGuess =  c(15, 10, 1, 0, 2, 6, 5, 3),
                                        groupingValue = "subsectorL3")
  VS3preference[, c("guess", "lambda") := NULL]

  VS3preference  <- rbind(VS3preference, prefVS3zero)
  S3S2preference <- copy(VS3preference)
  VS3preference[, dataStructureDummy[!dataStructureDummy %in% names(VS3preference)] := ""]
  VS3preference <- VS3preference[,  c(dataStructureDummy, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "VS3"]

  ## -----------------------------------------------------------------------------------------
  S3S2preference <- S3S2preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),
                                   by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  S3S2preference[, share := histESdemand / sum(histESdemand),
                 by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  S3S2preference[is.nan(share) & histESdemand == 0, share := 0]
  prefS3S2zero <- S3S2preference[share == 0 & histESdemand == 0][, preference := 0][, shareCheck := 0][, shareDiff := 0]
  S3S2preference <- merge(S3S2preference, lambdas[level == "S3S2",
                                                  c("sector", "subsectorL1", "subsectorL2", "lambda")],
                          by = c("sector", "subsectorL1", "subsectorL2"), all.x = TRUE)
  S3S2preference[is.na(lambda), lambda := -10]

  S3S2preference <-  S3S2preference[!(share == 0 & histESdemand == 0)]
  S3S2preference <- calculatePreferences(dfPreference = S3S2preference,
                                         varyGuess =  c(1, 0.5, 2, 3, 1, 4), groupingValue = "subsectorL2")
  S3S2preference[, c("guess", "lambda") := NULL]

  S3S2preference  <- rbind(S3S2preference, prefS3S2zero)
  S2S1preference <- copy(S3S2preference)
  S3S2preference[, dataStructureDummy[!dataStructureDummy %in% names(S3S2preference)] := ""]
  S3S2preference <- S3S2preference[, c(dataStructureDummy, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "S3S2"]

  ## -----------------------------------------------------------------------------------------
  S2S1preference <- S2S1preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),
                                   by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  S2S1preference[, share := histESdemand / sum(histESdemand),  by = c("region", "period", "sector", "subsectorL1")]
  S2S1preference[is.nan(share) & histESdemand == 0, share := 0]
  prefS2S1zero <- S2S1preference[share == 0 & histESdemand == 0][, preference := 0][, shareCheck := 0][, shareDiff := 0]
  S2S1preference <- merge(S2S1preference, lambdas[level == "S2S1", c("sector", "subsectorL1", "lambda")],
                          by = c("sector", "subsectorL1"), all.x = TRUE)
  S2S1preference[is.na(lambda), lambda := -10]

  S2S1preference <-  S2S1preference[!(share == 0 & histESdemand == 0)]
  S2S1preference <- calculatePreferences(dfPreference = S2S1preference,
                                         varyGuess =  c(2, 3, 4, 2, 1), groupingValue = "subsectorL1")
  S2S1preference[, c("guess", "lambda") := NULL]

  S2S1preference  <- rbind(S2S1preference, prefS2S1zero)
  S1Spreference <- copy(S2S1preference)
  S2S1preference[, dataStructureDummy[!dataStructureDummy %in% names(S2S1preference)] := ""]
  S2S1preference <- S2S1preference[, c(dataStructureDummy, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "S2S1"]

  ## -----------------------------------------------------------------------------------------
  S1Spreference <- S1Spreference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),
                                 by = c("region", "period", "sector", "subsectorL1")]
  S1Spreference[, share := histESdemand / sum(histESdemand),  by = c("region", "period", "sector")]
  S1Spreference[is.nan(share) & histESdemand == 0, share := 0]
  prefS1Szero <- S1Spreference[share == 0 & histESdemand == 0][, preference := 0][, shareCheck := 0][, shareDiff := 0]

  S1Spreference <- merge(S1Spreference, lambdas[level == "S3S2", c("sector", "lambda")], by = c("sector"), all.x = TRUE)
  S1Spreference[is.na(lambda), lambda := -10]

  S1Spreference <-  S1Spreference[!(share == 0 & histESdemand == 0)]
  S1Spreference <- calculatePreferences(dfPreference = S1Spreference,
                                        varyGuess =  c(10, 5, 0, 1, 2, 7, 3, 4, 20, 15), groupingValue = "sector")
  S1Spreference[, c("guess", "lambda") := NULL]
  S1Spreference  <- rbind(S1Spreference, prefS1Szero)
  S1Spreference[, dataStructureDummy[!dataStructureDummy %in% names(S1Spreference)] := ""]
  S1Spreference <- S1Spreference[, c(dataStructureDummy, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "S1S"]

  historicalPreferences <- rbind(FVpreference, VS3preference, S3S2preference, S2S1preference, S1Spreference)
  calibrationReport <- copy(historicalPreferences)
  historicalPreferences[, c("shareCheck", "shareDiff", "share") := NULL]
  toolCheckAllLevelsComplete(historicalPreferences, helpers$decisionTree, "Historical preferences")

  historicalPreferences <-  historicalPreferences[!(subsectorL3 == "trn_pass_road_LDV_4W" & level == "FV")]

  historicalPreferences[, variable := paste0("Preference|", level)][, unit := "-"]
  setnames(historicalPreferences, "preference", "value")

  if (nrow(calibrationReport[shareDiff >= 0.01]) >= 1) stop(paste0("Calibrated shares differ
                                                      by more than 0.01 from historical data. Please provide better guesses for the calibration.
                                                      Affected levels:", unique(calibrationReport$levels)))

  result <- list(historicalPreferences = historicalPreferences,
                 calibrationReport = calibrationReport)

  return(result)
}
