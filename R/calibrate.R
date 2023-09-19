#' Calibrate the logit share weights to historical data.
#'
#' @param prices full prices (fuel, non-fuel) of each technology-vehicle type
#' @param tech_output regional historical demand
#' @param logit_exp_data logit exponents input data
#' @param vot_data value of time input data
#' @param price_nonmot non motorized technologies price
#'
#' @importFrom rootSolve multiroot


toolCalibrateEDGEinconv <- function(combinedCosts, histESdemand, lambdas, timeValueCost){
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

  ## historical values of the demand, fuel level
  calibr_demand=tech_output
  logit_exponentFV=logit_exp_data[["logit_exponentFV"]]
  logit_exponent_VS3=logit_exp_data[["logit_exponent_VS3"]]
  logit_exponent_S3S2=logit_exp_data[["logit_exponent_S3S2"]]
  logit_exponent_S2S1=logit_exp_data[["logit_exponent_S2S1"]]
  logit_exponent_S1S=logit_exp_data[["logit_exponent_S1S"]]

  value_timeFV=vot_data[["value_timeFV"]]
  value_time_VS3=vot_data[["value_time_VS3"]]
  value_time_S3S2=vot_data[["value_time_S3S2"]]
  value_time_S2S1=vot_data[["value_time_S2S1"]]
  value_time_S1S=vot_data[["value_time_S1S"]]
  base_preference=calibr_demand[tech_output>0,]
  base_preference=base_preference[,share:=tech_output/sum(tech_output),by=c("region","period","vehicleType")]
  base_preference=merge(base_preference,logit_exponentFV,all.x=TRUE,by=c("sector", "subsectorL3", "vehicleType", "subsectorL2", "subsectorL1"))

  priceFV = prices[period <= 2010]
  priceFV= merge(priceFV, price_nonmot,all=TRUE, by=c("totPrice","region","period","technology","vehicleType","subsectorL3","subsectorL2","subsectorL1","sector"))

  priceFV[,tot_VOT_price := 0]
  #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
  priceFV[is.na(fuel_price_pkm), fuel_price_pkm := 0]
  priceFV[is.na(non_fuel_price), non_fuel_price := 0]

  dups <- duplicated(priceFV, by=c("region", "technology", "vehicleType","period"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(priceFV[dups])
    priceFV <- unique(priceFV, by = c("region", "technology", "vehicleType", "period"))
  }

  setnames(histESdemand, "value", "histESdemand")
  histESdemand[, share := histESdemand / sum(histESdemand), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName")]
  histESdemand[, c("variable", "unit") := NULL]
  combinedCosts[, .(totPrice = sum(value)), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName")]
  FVpreference <- merge(FVshares, combinedCosts, by = c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector"))
  FVpreference <- merge(FVpreference, lambdas[level == "FV"], by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType"), all.x = TRUE)[, level := NULL]

  FVpreference <- calculatePreferences(dfPreference = FVpreference, expectedPrices = c(2, 1, 3, 4, 5, 6, 7), groupingValue = "vehicleType")
  FVpreference[, level := "FV"]

  VS3preference <- copy(FVpreference)
  VS3preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName")]
  VS3preference[, share := histESdemand / sum(histESdemand),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "univocalName")]
  setnames(timeValueCost, "value", "timeValueCost")
  VS3preference <- merge(VS3preference, timeValueCost, by = "univocalName")
  VS3preference[is.na(timeValueCosts), timeValueCost := 0]
  VS3preference[, totPrice := totPrice + timeValueCost]
  VS3preference <- merge(VS3preference, lambdas[level == "VS3"], by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3"), all.x = TRUE)[, level := NULL]

  VS3preference <- calculatePreferences(dfPreference = VS3preference, expectedPrices =  c(7, 1, 2, 6, 5, 3), groupingValue = "subsectorL3")
  VS3preference[, level := "VS3"]

  S3S2preference <- copy(FVpreference)
  S3S2preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "univocalName")]
  S3S2preference[, share := histESdemand / sum(histESdemand),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "univocalName")]
  setnames(timeValueCost, "value", "timeValueCost")
  S3S2preference <- merge(S3S2preference, timeValueCost, by = "univocalName")
  S3S2preference[is.na(timeValueCosts), timeValueCost := 0]
  S3S2preference[, totPrice := totPrice + timeValueCost]
  S3S2preference <- merge(S3S2preference, lambdas[level == "S3S2"], by = c("region", "sector", "subsectorL1", "subsectorL2"), all.x = TRUE)[, level := NULL]

  S3S2preference <- calculatePreferences(dfPreference = S3S2preference, expectedPrices =  c(1, 0.5, 2, 3, 1, 4), groupingValue = "subsectorL3")
  S3S2preference[, level := "S3S2"]

  S2S1preference <- copy(FVpreference)
  S2S1preference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "univocalName")]
  S2S1preference[, share := histESdemand / sum(histESdemand),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "univocalName")]
  setnames(timeValueCost, "value", "timeValueCost")
  S2S1preference <- merge(S2S1preference, timeValueCost, by = "univocalName")
  S2S1preference[is.na(timeValueCosts), timeValueCost := 0]
  S2S1preference[, totPrice := totPrice + timeValueCost]
  S2S1preference <- merge(S2S1preference, lambdas[level == "S2S1"], by = c("region", "sector", "subsectorL1", "subsectorL2"), all.x = TRUE)[, level := NULL]

  S2S1preference <- calculatePreferences(dfPreference = S2S1preference, expectedPrices =  c(2, 3, 4, 2, 1), groupingValue = "subsectorL3")
  S2S1preference[, level := "S2S1"]

  S1Spreference <- copy(FVpreference)
  S1Spreference[, .(totPrice = sum(share * totPrice), histESdemand = sum(histESdemand)),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "univocalName")]
  S1Spreference[, share := histESdemand / sum(histESdemand),  by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "univocalName")]
  setnames(timeValueCost, "value", "timeValueCost")
  S1Spreference <- merge(S1Spreference, timeValueCost, by = "univocalName")
  S1Spreference[is.na(timeValueCosts), timeValueCost := 0]
  S1Spreference[, totPrice := totPrice + timeValueCost]
  S1Spreference <- merge(S1Spreference, lambdas[level == "S1S"], by = c("region", "sector", "subsectorL1", "subsectorL2"), all.x = TRUE)[, level := NULL]

  S1Spreference <- calculatePreferences(dfPreference = S1Spreference, expectedPrices =  c(1, 2, 5, 3, 1, 4), groupingValue = "subsectorL3")
  S1Spreference[, level := "S1S"]

  return(list(list_preference = list(S2S1_final_preference=S2S1_final_preference,
                                 S1S_final_preference=S1S_final_preference,
                                 S3S2_final_preference=S3S2_final_preference,
                                 VS3_final_preference=VS3_final_preference,
                                 FV_final_preference=FV_final_preference)))
}
