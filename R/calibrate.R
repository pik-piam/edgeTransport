#' Calibrate the logit share weights to historical data.
#'
#' @param prices full prices (fuel, non-fuel) of each technology-vehicle type
#' @param tech_output regional historical demand
#' @param logit_exp_data logit exponents input data
#' @param vot_data value of time input data
#' @param price_nonmot non motorized technologies price
#'
#' @importFrom rootSolve multiroot


toolCalibrateEDGEinconv <- function(prices, tech_output, logit_exp_data, vot_data, price_nonmot){
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
    shareCheck <- preference <- totPrice <- lambda <- share_diff <- share <- NULL
    tmp <- df[period <= 2010]

    tmp[, shareCheck := preference * totPrice ^ lambda / sum(preference * totPrice ^ lambda),
        by = c("region", "period", groupingValue)]

    tmp[,share_diff := (shareCheck - share) ^ 2]
    tmp <- tmp[share_diff > 1e-3]
    return(tmp[share_diff == max(share_diff)][, c("region","period","shareCheck", "share")])
  }

  ## for loop that tries multiple initial points for the preference calibration.
  ## The function stops when all the preference have been successfully calculated
  ## Returns the dt with the calculated preference, already normalized

  calculatePreferences <- function(dfPreference, groupingValue, expectedPrices){
    lambda <- fac <- share <- totPrice <- tech_output <- preference <- NULL
    for (expectedPrice in expectedPrices) {                                         ## loops through all the initial points suggested
      if(is.null(dfPreference$preference)){dfPreference[, preference := NaN]}                                ## if this is the first iteration, an empty column is needed

      for (lamb in unique(dfPreference$lambda)) {                          ## treats all the lambdas separately
        dfPreference[lambda==lamb & is.nan(preference),                            ## only for the nodes that have the specific lambda AND preference that are still not calculated/did not work out the calculation
              fac := share / max(share) * (totPrice / max(totPrice)) ^ expectedPrice,   ## provides a starting point
              by = c("region", "period",groupingValue)]

        dfPreference[lambda == lamb  & is.nan(preference),
                               preference:= rootFunction(totPrice, share, lamb,fac),                   ## apply the root function
                               by = c("region", "period",groupingValue)]
      }

      dfPreference[,
            preference := preference / max(preference),                                               ## normalize preference. If the maximum is 0, this is going to give NaN, which is recognized from the loop as "still work in progress"
            by = c("region", "period",groupingValue)]

      if (!any(is.nan(dfPreference$preference)) & !is.null(dfPreference$preference)) {                    ## needs to exit the loop if all the preference are calculated and there are no NaNs
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
  logit_exponent_FV=logit_exp_data[["logit_exponent_FV"]]
  logit_exponent_VS3=logit_exp_data[["logit_exponent_VS3"]]
  logit_exponent_S3S2=logit_exp_data[["logit_exponent_S3S2"]]
  logit_exponent_S2S1=logit_exp_data[["logit_exponent_S2S1"]]
  logit_exponent_S1S=logit_exp_data[["logit_exponent_S1S"]]

  value_time_FV=vot_data[["value_time_FV"]]
  value_time_VS3=vot_data[["value_time_VS3"]]
  value_time_S3S2=vot_data[["value_time_S3S2"]]
  value_time_S2S1=vot_data[["value_time_S2S1"]]
  value_time_S1S=vot_data[["value_time_S1S"]]
  base_preference=calibr_demand[tech_output>0,]
  base_preference=base_preference[,share:=tech_output/sum(tech_output),by=c("region","period","vehicleType")]
  base_preference=merge(base_preference,logit_exponent_FV,all.x=TRUE,by=c("sector", "subsectorL3", "vehicleType", "subsectorL2", "subsectorL1"))

  price_FV = prices[period <= 2010]
  price_FV= merge(price_FV, price_nonmot,all=TRUE, by=c("totPrice","region","period","technology","vehicleType","subsectorL3","subsectorL2","subsectorL1","sector"))

  price_FV[,tot_VOT_price := 0]
  #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
  price_FV[is.na(fuel_price_pkm), fuel_price_pkm := 0]
  price_FV[is.na(non_fuel_price), non_fuel_price := 0]

  dups <- duplicated(price_FV, by=c("region", "technology", "vehicleType","period"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(price_FV[dups])
    price_FV <- unique(price_FV, by=c("region", "technology", "vehicleType","period"))
  }

  FV_preference=merge(price_FV,base_preference,all=FALSE,by=c("region","period","technology","vehicleType","subsectorL3","subsectorL2","subsectorL1","sector"))
  FV_preference=FV_preference[!is.na(tech_output),] ## minor adjusments, TODO check if needed/why needed!
  ## needs rando lambdas for the sectors that are not explicitly calculated
  FV_preference[,lambda:=ifelse(is.na(lambda),-10,lambda)]

  FV_preference=calculatePreferences( dfPreference=FV_preference, expectedPrices = c(2,1,3,4,5,6,7), groupingValue="vehicleType")

  ## merge value of time and assign 0 to the entries that don't have it
  FV_preference=merge(FV_preference,value_time_FV,all.x=TRUE,by=c("region", "period", "vehicleType","subsectorL3"))
  FV_preference[,time_price:=ifelse(is.na(time_price),0,time_price)]
  FV_preference[,totPrice:=totPrice+time_price]

  ## reshape, save and store the preference at this level
  FV_final_preference=FV_preference[,.(region,period,technology,totPrice,vehicleType,subsectorL3,subsectorL2,subsectorL1,sector,preference,lambda)]

  ## calculate price of one level up
  FV_preference=FV_preference[,.(totPrice=sum(share*totPrice),tech_output=sum(tech_output)),by = .(region,period,vehicleType,subsectorL3,subsectorL2,subsectorL1,sector)]

  ## now we go from V to SubS VSubS
  VS3_preference=merge(FV_preference,logit_exponent_VS3,all.x=TRUE,by = c("subsectorL3", "subsectorL2", "subsectorL1", "sector"))
  ## needs random lambdas for the sectors that are not explicitly calculated
  VS3_preference[,lambda:=ifelse(is.na(lambda),-10,lambda)]
  ## calculate shares at this level
  VS3_preference[,share:=tech_output/sum(tech_output),by=c("region","period","subsectorL3")]


  ## merge value of time and assign 0 to the entries that don't have it
  VS3_preference=merge(VS3_preference,value_time_VS3,all.x=TRUE,by=c("region", "period", "vehicleType", "subsectorL3"))
  VS3_preference[,time_price:=ifelse(is.na(time_price),0,time_price)]
  VS3_preference[,totPrice:=totPrice+time_price]
  VS3_preference[,time_price:=NULL]

  ## optimize
  VS3_preference=calculatePreferences(dfPreference = VS3_preference, expectedPrices = c(7,1,2,6,5,3),groupingValue = "subsectorL3")

  ## reshape, save and store the preference at this level
  VS3_final_preference=VS3_preference[,.(region,period,vehicleType,subsectorL3,subsectorL2,subsectorL1,sector,preference,totPrice,lambda)]


  VS3_preference=VS3_preference[,.(totPrice=sum(share*totPrice),tech_output=sum(tech_output)),by = .(region,period,subsectorL3,subsectorL2,subsectorL1,sector)]

  ## rename the database, now it's going from V to S1
  S3S2_preference=merge(VS3_preference,logit_exponent_S3S2,all.x=TRUE)
  S3S2_preference[,share:=tech_output/sum(tech_output),by=c("region","period","sector","subsectorL1","subsectorL2")]

  ## needs rando lambdas for the sectors that are not explicitly calculated
  S3S2_preference[,lambda:=ifelse(is.na(lambda),-10,lambda)]


  S3S2_preference=merge(S3S2_preference,value_time_S3S2,all.x = TRUE,by = c("region", "period", "subsectorL3","subsectorL2"))
  S3S2_preference[,time_price:=ifelse(is.na(time_price),0,time_price)]
  S3S2_preference[,totPrice:=totPrice+time_price]
  S3S2_preference[,time_price:=NULL]
  S3S2_preference=calculatePreferences(S3S2_preference,expectedPrices = c(1,0.5,2,3,1,4),groupingValue = "subsectorL2")

  ## reshape, save and store the preference at this level
  S3S2_final_preference=S3S2_preference[,.(region,period,subsectorL3,subsectorL2,subsectorL1,sector,preference,totPrice,lambda)]

  S3S2_preference=S3S2_preference[,.(totPrice=sum(share*totPrice),tech_output=sum(tech_output)),by = .(region,period,subsectorL2,subsectorL1,sector)]

  ## rename the database, now it's going from S2 to S3
  S2S1_preference=merge(S3S2_preference,logit_exponent_S2S1,all.x=TRUE)
  S2S1_preference[,share:=tech_output/sum(tech_output),by=c("region","period","sector","subsectorL1")]

  ## needs rando lambdas for the sectors that are not explicitly calculated
  S2S1_preference[,lambda:=ifelse(is.na(lambda),-10,lambda)]

  S2S1_preference=merge(S2S1_preference,value_time_S2S1,all.x = TRUE,by = c("region", "period", "subsectorL2","subsectorL1"))
  S2S1_preference[,time_price:=ifelse(is.na(time_price),0,time_price)]
  S2S1_preference[,totPrice:=totPrice+time_price]
  S2S1_preference[,time_price:=NULL]
  S2S1_preference=calculatePreferences(dfPreference = S2S1_preference, expectedPrices = c(2,3,4,2,1), groupingValue = "subsectorL1")

  ## reshape, save and store the preference at this level
  S2S1_final_preference=S2S1_preference[,.(region,period,subsectorL2,subsectorL1,sector,preference, totPrice, lambda)]


  S2S1_preference=S2S1_preference[,.(totPrice=sum(share*totPrice),tech_output=sum(tech_output)),by = .(region,period,subsectorL1,sector)]

  ## rename the database, now it's going from S2 to S3
  S1S_preference=merge(S2S1_preference,logit_exponent_S1S,all.x=TRUE)
  S1S_preference[,share:=tech_output/sum(tech_output),by=c("region","period","sector")]

  S1S_preference=merge(S1S_preference,value_time_S1S,all.x = TRUE,by = c("region", "period", "sector","subsectorL1"))
  S1S_preference[,time_price:=ifelse(is.na(time_price),0,time_price)]
  S1S_preference[,totPrice:=totPrice+time_price]
  S1S_preference[,time_price:=NULL]

  S1S_preference=calculatePreferences(dfPreference = S1S_preference, expectedPrices = c(1,2,5,3,1,4), groupingValue = "sector")

  ## reshape, save and store the preference at this level
  S1S_final_preference=S1S_preference[,.(region,period,subsectorL1,sector,preference,totPrice,lambda)]

  return(list(list_preference = list(S2S1_final_preference=S2S1_final_preference,
                                 S1S_final_preference=S1S_final_preference,
                                 S3S2_final_preference=S3S2_final_preference,
                                 VS3_final_preference=VS3_final_preference,
                                 FV_final_preference=FV_final_preference)))
}
