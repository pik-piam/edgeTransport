#' Calibrate the logit share weights to historical data.
#'
#' @param combinedCosts total cost of ownership in US$2005/(p|t)km
#' @param histESdemand historical energy service demand data
#' @param timeValueCost time value cost for passenger transport modes in US$2005/pkm
#' @param lambdas exponents in discrete choice function
#'
#' @importFrom rootSolve multiroot


toolCalibrateHistPrefs <- function(combinedCosts, histESdemand, timeValueCost, lambdas){
  share <- tot_VOT_price <- fuel_price_pkm <- non_fuel_price <- lambda <- region <- vehicleType <- technology <- preference <- `.` <- subsectorL3 <- subsectorL2 <- subsectorL1 <- sector <- V1 <- V2 <- time_price <- totPrice <- NULL
  ##==== functions ====

  ##function that determines the shares of the alternatives in the nodes
  optFunction <- function(x, pri, sha, lamb){
    sha * sum(x * pri ^ lamb) / min(pri ^ lamb) - x * pri ^ lamb / min(pri ^ lamb)
  }

  jacobian <- function(x, pri, sha, lamb){
    matrix(sha) %*% t(matrix(pri)) / min(pri ^ lamb) - pri * diag(length(pri)) / min(pri ^ lamb)
  }

  ##applies optFunction, and to help the solver uses a factor as starting point
  rootFunction <- function(prices, shares, lambda,factor){
    multiroot(f = optFunction, start = factor, pri = prices, sha = shares, lamb=lambda, positive=T, jacfunc = jacobian)$root
  }

  ##function that compares the shares you obtained with the calibrated preference and the theoretical shares, known
  checkShares = function(df , groupingValue){
   browser()
    shareCheck <- preference <- totPrice <- lambda <- shareDiff <- share <- NULL
    tmp <- df[period <= 2010]

    tmp[, shareCheck := preference * totPrice ^ lambda / sum(preference * totPrice ^ lambda),
        by = c("region", "period", groupingValue)]

    tmp[,shareDiff := (shareCheck - share) ^ 2]
    tmp <- tmp[shareDiff > 1e-3]
    return(tmp[shareDiff == max(shareDiff)][, c("region","period","shareCheck", "share")])
  }

  ## for loop that tries multiple initial points for the preference calibration.
  ## The function stops when all the preference have been successfully calculated
  ## Returns the dt with the calculated preference, already normalized

  calculatePreferences <- function(dfPreference, groupingValue, expectedPrices){
   fac <- preference <- NULL
   browser()
    for (expectedPrice in expectedPrices) {                                             ## loops through all the initial points suggested
      if(is.null(dfPreference$preference)){dfPreference[, preference := NaN]}           ## if this is the first iteration, an empty column is needed

      for (lamb in unique(dfPreference$lambda)) {                                       ## treats all the lambdas separately
        dfPreference[lambda == lamb & is.nan(preference),                               ## only for the nodes that have the specific lambda AND preference that are still not calculated/did not work out the calculation
              fac := share / max(share) * (totPrice / max(totPrice)) ^ expectedPrice,   ## provides a starting point
              by = c("region", "period", groupingValue)]

        dfPreference[lambda == lamb  & is.nan(preference),
                               preference := rootFunction(totPrice, share, lamb, fac),  ## apply the root function
                               by = c("region", "period", groupingValue)]
      }

      dfPreference[,
            preference := preference / max(preference),                                 ## normalize preference. If the maximum is 0, this is going to give NaN, which is recognized from the loop as "still work in progress"
            by = c("region", "period",groupingValue)]

      if (!any(is.nan(dfPreference$preference)) & !is.null(dfPreference$preference)) {  ## needs to exit the loop if all the preference are calculated and there are no NaNs
        break
      }


    }

    if(any(is.na(dfPreference$preference))){
      print(paste0("There are NaNs in ", groupingValue, ", other initial values are needed")) ## error message that tells you that not all the preference are correctly calibrated
      stop()
    } else {print(paste0("Max difference in shares for ",groupingValue,": "))
            print(checkShares(dfPreference,groupingValue))}## check maximum difference in expected shares
    return(dfPreference)

  }

  setnames(histESdemand, "value", "histESdemand")
  histESdemand <- merge(histESdemand, decisionTree, by = c("region", "univocalName", "technology"))
  histESdemand <- histESdemand[histESdemand > 0]
  histESdemand[, share := histESdemand / sum(histESdemand), by = c("region", "univocalName", "period")]
  histESdemand[, c("variable", "unit") := NULL]
  totPrice <- copy(combinedCosts)
  totPrice <- totPrice[, .(totPrice = sum(value)), by = c("region", "univocalName", "technology", "period")]
  FVpreference <- merge(histESdemand, totPrice, by = c("region", "univocalName", "technology", "period"))
  prefFVzero <- FVpreference[histESdemand == 0][, preference := 0]
  prefFVactive <- FVpreference[univocalName %in% c("Cycle", "Walk")][, preference := 1]
  FVpreference <- FVpreference[!univocalName %in% c("Cycle", "Walk")]
  FVpreference <- merge(FVpreference, lambdas[level == "FV"], by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName"), all.x = TRUE)[, level := NULL]
  FVpreference <- calculatePreferences(dfPreference = FVpreference, expectedPrices = c(2, 1, 3, 4, 5, 6, 7), groupingValue = "vehicleType")
  FVpreference[, c("fac", "lambda") := NULL]

  FVpreference  <- rbind(FVpreference, prefFVzero, prefFVactive)
  VS3preference <- copy(FVpreference)
  FVpreference <- FVpreference[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "technology", "period", "preference")][, level := "FV"]

  ## -----------------------------------------------------------------------------------------
  VS3preference <- VS3preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName")]
  VS3preference[, share := histESdemand / sum(histESdemand),  by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  setnames(timeValueCost, c("value"), c("timeValueCost"))
  timeValueCost[, c("variable", "unit") := NULL]
  VS3preference <- merge(VS3preference, timeValueCost, by = c("region", "period", "univocalName"), all.x = TRUE)
  VS3preference[is.na(timeValueCost), timeValueCost := 0]
  VS3preference[, totPrice := totPrice + timeValueCost]
  VS3preference <- merge(VS3preference, lambdas[level == "VS3", c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "lambda")], by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3"), all.x = TRUE)
  VS3preference[is.na(lambda), lambda := -10]

  VS3preference <- calculatePreferences(dfPreference = VS3preference, expectedPrices =  c(7, 1, 2, 6, 5, 3), groupingValue = "subsectorL3")
  VS3preference[, c("fac", "lambda") := NULL]

  S3S2preference <- copy(VS3preference)
  VS3preference <- VS3preference[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName", "period", "preference")][, level := "VS3"]

  ## -----------------------------------------------------------------------------------------
  S3S2preference <- S3S2preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  S3S2preference[, share := histESdemand / sum(histESdemand),  by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  S3S2preference <- merge(S3S2preference, lambdas[level == "S3S2", c("sector", "subsectorL1", "subsectorL2", "lambda")], by = c("sector", "subsectorL1", "subsectorL2"), all.x = TRUE)
  S3S2preference[is.na(lambda), lambda := -10]

  S3S2preference <- calculatePreferences(dfPreference = S3S2preference, expectedPrices =  c(1, 0.5, 2, 3, 1, 4), groupingValue = "subsectorL2")
  S3S2preference[, c("fac", "lambda") := NULL]

  S2S1preference <- copy(S3S2preference)
  S3S2preference <- S3S2preference[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "period", "preference")][, level := "S3S2"]

  ## -----------------------------------------------------------------------------------------
  S2S1preference <- S2S1preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  S2S1preference[, share := histESdemand / sum(histESdemand),  by = c("region", "period", "sector", "subsectorL1")]
  S2S1preference <- merge(S2S1preference, lambdas[level == "S3S2", c("sector", "subsectorL1", "subsectorL2", "lambda")], by = c("sector", "subsectorL1"), all.x = TRUE)
  S2S1preference[is.na(lambda), lambda := -10]

  S2S1preference <- calculatePreferences(dfPreference = S2S1preference, expectedPrices =  c(2, 3, 4, 2, 1), groupingValue = "subsectorL1")
  S2S1preference[, c("fac", "lambda") := NULL]

  S1Spreference <- copy(S2S1preference)
  S2S1preference <- S2S1preference[, c("region", "sector", "subsectorL1", "subsectorL2", "period", "preference")][, level := "S2S1"]

  ## -----------------------------------------------------------------------------------------
  S1Spreference <- S1Spreference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "period", "sector", "subsectorL1")]
  S1Spreference[, share := histESdemand / sum(histESdemand),  by = c("region", "period", "sector")]
  S1Spreference <- merge(S1Spreference, lambdas[level == "S3S2", c("sector", "lambda")], by = c("sector"), all.x = TRUE)
  S1Spreference[is.na(lambda), lambda := -10]

  S1Spreference <- calculatePreferences(dfPreference = S1Spreference, expectedPrices =  c(1, 2, 5, 3, 1, 4), groupingValue = "sector")
  S1Spreference <- S1Spreference[, c("region", "sector", "subsectorL1", "period", "preference")][, level := "S1S"]




  return()
}
