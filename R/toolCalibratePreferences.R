#' Calibrate the logit share weights to historical data.
#'
#' @param sharesToBeCalibrated Shares at each level of the decision tree for which preferences are to be calibrated
#' @param combinedCosts Annualized total cost of ownership
#' @param timeValueCost Time value cost for passenger transport modes
#' @param lambdas Exponents for discrete choice function
#' @param helpers list with helpers
#' @returns data.table with calibrated historical preferences
#' @export

toolCalibratePreferences <- function(sharesToBeCalibrated, combinedCosts, timeValueCost, lambdas, helpers){
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
     invisible(capture.output(
      suppressWarnings(
        suppressMessages({
          results <- rootSolve::multiroot(
            f = optFunction,
            start = factor,
            pri = prices,
            sha = shares,
            lamb = lambda, positive = T)
        })
      )
    ))
    return(results$root)
  }

  # function that compares the shares you obtained with the calibrated preference and the theoretical shares, known
  checkShares <- function(df, groupingValue){
    shareCheck <- shareDiff <-  NULL
    df[, shareCheck := calculateSharesDiscreteChoice(totPrice, lambda, preference),
       by = c("region", "period", groupingValue)]
    df[, shareDiff := (shareCheck - share) ^ 2]
  }

  # for loop that tries multiple initial points for the preference calibration.
  # The function stops when all the preference have been successfully calculated
  # Returns the dt with the calculated preference, already normalized
  # Only accepts solutions, when the absolute difference between shares
  # calculated using the calibrated preferences and transferred shares to be calibrated < tol

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
                     by = c("region", "period", eval(groupingValue))]
        # apply the root function
        dfPreference[lambda == lamb  & is.nan(preference),
                     preference := rootFunction(totPrice, share, lamb, guess),
                     by = c("region", "period", groupingValue)]
      }

      # normalize preference. If the maximum is 0, this is going to give NaN,
      # which is recognized from the loop as "still work in progress"
      dfPreference[,
                   preference := preference / max(preference),
                   by = c("region", "period", groupingValue)]

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

  calibrateLevel <- function(levelToCalibrate,
                             totPrice,
                             lambdas,
                             sharesToBecalibrated,
                             variationOfGuesses,
                             colsDecisionTRee,
                             levelsDecisionTree) {

    # get data for level to calibrate
    groupValue <- levelsDecisionTree[level == levelToCalibrate]$groupValue
    shares <- sharesToBeCalibrated[level == levelToCalibrate][, level := NULL]
    empty_cols <- names(shares)[sapply(shares, function(x) all(x == "", na.rm = TRUE))]
    shares[, (empty_cols) := NULL]
    lambdas <- copy(lambdas)[level == levelToCalibrate][, level := NULL]
    empty_cols <- names(lambdas)[sapply(lambdas, function(x) all(x == "", na.rm = TRUE))]
    lambdas[, (empty_cols) := NULL]
    variationOfGuesses <- copy(variationOfGuesses)[[eval(levelToCalibrate)]]

    decisionFunctionData <- merge(totPrice, shares, by = intersect(names(totPrice), names(shares)), all.x = TRUE)
    # Treat zero entries outside the optimization
    prefZero <- decisionFunctionData[share == 0][, preference := 0][, shareCheck := 0][, shareDiff := 0]
    decisionFunctionData <- decisionFunctionData[!share == 0]
    # On fuel vehilce level (before time value price is applied, active modes have prices of zero and need to be treated separately)
    if (levelToCalibrate == "FV") {
      prefActive <- decisionFunctionData[grepl(".*Walk.*|.*Cycle.*", subsectorL3)][, preference := 1][, shareCheck := 1][, shareDiff := 0]
      decisionFunctionData <- decisionFunctionData[!grepl(".*Walk.*|.*Cycle.*", subsectorL3)]
    }
    decisionFunctionData <- merge(decisionFunctionData, lambdas,
                                  by = intersect(names(decisionFunctionData), names(lambdas)),
                                  all.x = TRUE)
    # Assign a lambda to branches where no decision is taken in the respective level.
    # The value of that lambda is not relevant
    decisionFunctionData[is.na(lambda) & share == 1, lambda := -10]
    # varyGuess inluences the impact of the price on the heuristic that determines the guesses for the solver.
    # The entries in the varyGuess vector are independent of each other and are applied to the heuristic/
    # tried out with the solver one after the other (without influencing the subsequent guess)
    preferences <- suppressMessages(calculatePreferences(dfPreference = decisionFunctionData,
                                                         varyGuess = variationOfGuesses, groupingValue = groupValue))
    preferences[, c("guess", "lambda") := NULL]

    preferences  <- rbind(preferences, prefZero)
    if (levelToCalibrate == "FV") preferences <- rbind(preferences, prefActive)
    return(preferences)
  }

  #---preparation---------------------
  # Dummy data structure to move through decision tree
  colsDecisionTRee <- names(helpers$decisionTree)
  colsDecisionTRee <- colsDecisionTRee[!colsDecisionTRee %in% c("univocalName")]
  levelsDecisionTree <- data.table(groupValue = c("vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector"),
                                   level = c("FV", "VS3", "S3S2", "S2S1", "S1S"))

  # Sum up different cost variables
  totPrice <- merge(combinedCosts, helpers$decisionTree, by = intersect(names(combinedCosts), names(helpers$decisionTree)))
  totPrice <- totPrice[, .(totCAPEXOPEX = sum(value)), by = c("region", "period", "univocalName", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  timeValueCost <- copy(timeValueCost)
  setnames(timeValueCost, c("value"), c("timeValueCost"))
  timeValueCost[, c("variable", "unit") := NULL]
  totPrice <- merge(totPrice, timeValueCost, intersect(names(totPrice), names(timeValueCost)), all.x = TRUE)
  totPrice[is.na(timeValueCost), timeValueCost := 0]

  # guesses
  # Note: The guesses are provided to vary the influence of the prices in the heuristic for the algorithm
  # They do not follow any structured approach and are tested one after another
  # (entries do not interact with each other; the order is only important for what is tested first)
  # Please make sure to provide sufficient guesses - if no solution within the tolerance is found the function will throw an error
  variationOfGuesses <- list(
    FV   = c(2, 1, 3, 4, 5, 6, 7),
    VS3  = c(15, 10, 1, 0, 2, 6, 5, 3),
    S3S2 = c(1, 0.5, 2, 3, 1, 4),
    S2S1 = c(2, 3, 4, 2, 1),
    S1S  = c(10, 5, 0, 1, 2, 7, 3, 4, 20, 15)
    )

  # Start of preference calibration for each level of the decision tree
  # Note: This is section could be further integrated to react and adjust according to changes in the structure of the decision tree,
  # but explicit coding preferred here:
  # - FV skips time value costs
  # - Prizes aggregate upward using lower-level shares
  # - Decision tree rarely changes (readability > configurability)

  #---FV level---------------------
  levelToCalibrate <- "FV"
  # Time value cost are not considered for fuel/technology choice
  # Total Price in decision function equals tot CAPEX and OPEX
  totPriceFV <- copy(totPrice)
  setnames(totPriceFV, "totCAPEXOPEX", "totPrice")
  FV <- calibrateLevel(levelToCalibrate,
                        totPriceFV,
                        lambdas,
                        sharesToBeCalibrated,
                        variationOfGuesses,
                        colsDecisionTRee,
                        levelsDecisionTree)
  FVpreferences <-  FV[, c(colsDecisionTRee, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "FV"]
  #---VS3 level---------------------
  levelToCalibrate <- "VS3"
  totPriceVS3 <- copy(FV)
  # Detailed time resolution is only kept on FV level
  totPriceVS3 <- totPriceVS3[period %in% helpers$lowTimeRes]

  # total CAPEX and OPEX for options in respective branches of the decision tree
  # needs to be calculated according to shares in levels below (eg. LDV 4W total price depends on share of BEV)
  totPriceVS3 <- totPriceVS3[, .(totPrice = sum(share * totPrice)),
           by = c("region", "period", "sector", "subsectorL1", "subsectorL2",
                  "subsectorL3", "vehicleType", "univocalName", "timeValueCost")]
  # Time value cost are applied from level VS3 onwards and do not vary by technology. They need to be added just one time, as shares are anyway 1 if there is no decision on that level.
  # Then prices are kept for the upper levels
  totPriceVS3[, totPrice := totPrice + timeValueCost][, timeValueCost := NULL]
  VS3 <- calibrateLevel(levelToCalibrate,
                       totPriceVS3,
                       lambdas,
                       sharesToBecalibrated,
                       variationOfGuesses,
                       colsDecisionTRee,
                       levelsDecisionTree)
  VS3preferences <- copy(VS3)[, colsDecisionTRee[!colsDecisionTRee %in% names(VS3)] := ""]
  VS3preferences <- VS3preferences[,  c(colsDecisionTRee, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "VS3"]
  #---S3S2 level---------------------
  levelToCalibrate <- "S3S2"
  totPriceS3S2 <- VS3[, .(totPrice = sum(share * totPrice)),
              by = c("region", "period", "sector", "subsectorL1", "subsectorL2",
                     "subsectorL3")]
  S3S2 <- calibrateLevel(levelToCalibrate,
                        totPriceS3S2,
                        lambdas,
                        sharesToBecalibrated,
                        variationOfGuesses,
                        colsDecisionTRee,
                        levelsDecisionTree)
  S3S2preferences <- copy(S3S2)[, colsDecisionTRee[!colsDecisionTRee %in% names(S3S2)] := ""]
  S3S2preferences <- S3S2preferences[,  c(colsDecisionTRee, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "S3S2"]
  #---S2S1 level---------------------
  levelToCalibrate <- "S2S1"
  totPriceS2S1 <- S3S2[, .(totPrice = sum(share * totPrice)),
               by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  S2S1 <- calibrateLevel(levelToCalibrate,
                         totPriceS2S1,
                         lambdas,
                         sharesToBecalibrated,
                         variationOfGuesses,
                         colsDecisionTRee,
                         levelsDecisionTree)
  S2S1preferences <- copy(S2S1)[, colsDecisionTRee[!colsDecisionTRee %in% names(S2S1)] := ""]
  S2S1preferences <- S2S1preferences[,  c(colsDecisionTRee, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "S2S1"]
  #---S1S level---------------------
  levelToCalibrate <- "S1S"
  totPriceS1S <- S2S1[, .(totPrice = sum(share * totPrice)),
               by = c("region", "period", "sector", "subsectorL1")]
  S1S <- calibrateLevel(levelToCalibrate,
                         totPriceS1S,
                         lambdas,
                         sharesToBecalibrated,
                         variationOfGuesses,
                         colsDecisionTRee,
                         levelsDecisionTree)

  S1Spreferences <- copy(S1S)[, colsDecisionTRee[!colsDecisionTRee %in% names(S1S)] := ""]
  S1Spreferences <- S1Spreferences[,  c(colsDecisionTRee, "period", "preference", "shareCheck", "shareDiff", "share"), with = FALSE][, level := "S1S"]

  # ---Combination of all levels----
  calibratedPreferences <- rbind(FVpreferences, VS3preferences, S3S2preferences, S2S1preferences, S1Spreferences)
  # Keep data and share differences in calibrationReport for later assessment
  calibrationReport <- copy(calibratedPreferences)
  if (nrow(calibrationReport[shareDiff >= 0.01]) >= 1) stop(paste0("Calibrated shares differ
                                                      by more than 0.01 from provided shares. Please provide better guesses for the calibration.
                                                      Affected levels:", unique(calibrationReport$levels)))


  calibratedPreferences[, c("shareCheck", "shareDiff", "share") := NULL]
  toolCheckAllLevelsComplete(calibratedPreferences, helpers$decisionTree, "calibratedPreferences")
  calibratedPreferences[, variable := paste0("Preference|", level)][, unit := "-"]
  setnames(calibratedPreferences, "preference", "value")

  result <- list(calibratedPreferences = calibratedPreferences,
                 calibrationReport = calibrationReport)

  return(result)
}
