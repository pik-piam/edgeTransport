#' Calibrate the logit share weights to historical data.
#'
#' @param prices full prices (fuel, non-fuel) of each technology-vehicle type
#' @param tech_output regional historical demand
#' @param logit_exp_data logit exponents input data
#' @param vot_data value of time input data
#' @param price_nonmot non motorized technologies price
#' 
#' @importFrom rootSolve multiroot


lvl1_calibrateEDGEinconv <- function(prices, tech_output, logit_exp_data, vot_data, price_nonmot){
  share <- tot_VOT_price <- fuel_price_pkm <- non_fuel_price <- logit.exponent <- region <- vehicle_type <- technology <- sw <- `.` <- subsector_L1 <- subsector_L2 <- subsector_L3 <- sector <- V1 <- V2 <- time_price <- tot_price <- NULL
  ##==== functions ====

  ##function that determines the shares of the alternatives in the nodes
  opt_func <- function(x, pri, sha, lamb){
    sha*sum(x*pri^lamb)/min(pri^lamb) - x*pri^lamb/min(pri^lamb)
  }

  jacobian <- function(x, pri, sha, lamb){
    matrix(sha) %*% t(matrix(pri))/min(pri^lamb) - pri * diag(length(pri))/min(pri^lamb)
  }

  ##applies opt_func, and to help the solver uses a factor as starting point
  root_func <- function(prices, shares, lambda,factor){
    multiroot(f = opt_func, start = factor, pri = prices, sha = shares, lamb=lambda, positive=T, jacfunc = jacobian)$root
  }

  ##function that compares the shares you obtained with the calibrated sw and the theoretical shares, known
  check_shares=function(df,grouping_value){
    share_check <- sw <- tot_price <- logit.exponent <- share_diff <- share <- NULL
    tmp=df[year<=2010,]

    tmp[, share_check := sw*tot_price^logit.exponent/sum(sw*tot_price^logit.exponent),
        by=c("region", "year",grouping_value)]

    tmp[,share_diff:=(share_check-share)^2]
    tmp = tmp[share_diff>1e-3]
    return(tmp[share_diff == max(share_diff)][,c("region","year","share_check","share")])
  }
  ## for loop that tries multiple initial points for the SW calibration.
  ## The function stops when all the SW have been successfully calculated
  ## Returns the dt with the calculated SW, already normalized

  sw_calc=function(df_sw,grouping_value,exp_prices){
    logit.exponent <- fac <- share <- tot_price <- tech_output <- sw <- NULL
    for (exp_price in exp_prices) {                                         ## loops through all the initial points suggested
      if(is.null(df_sw$sw)){df_sw[,sw:=NaN]}                                ## if this is the first iteration, an empty column is needed
      
      for (lamb in unique(df_sw$logit.exponent)) {                          ## treats all the lambdas separately
        df_sw[logit.exponent==lamb & is.nan(sw),                            ## only for the nodes that have the specific lambda AND SW that are still not calculated/did not work out the calculation
              fac:=share/max(share)*(tot_price/max(tot_price))^exp_price,   ## provides a starting point
              by=c("region", "year",grouping_value)]
        
        df_sw[logit.exponent==lamb  & is.nan(sw),
                               sw:= root_func(tot_price, share, lamb,fac),                   ## apply the root function
                               by=c("region", "year",grouping_value)]
      }

      df_sw[,
            sw := sw/max(sw),                                               ## normalize SW. If the maximum is 0, this is going to give NaN, which is recognized from the loop as "still work in progress"
            by=c("region", "year",grouping_value)]
      
      if (!any(is.nan(df_sw$sw)) & !is.null(df_sw$sw)) {                    ## needs to exit the loop if all the SW are calculated and there are no NaNs        
        break
      }


    }

    if(any(is.na(df_sw$sw))){
      print(paste0("There are NaNs in ", grouping_value, ", other initial values are needed")) ## error message that tells you that not all the SW are correctly calibrated
    } else {print(paste0("Max difference in shares for ",grouping_value,": "))
            print(check_shares(df_sw,grouping_value))}## check maximum difference in expected shares

    return(df_sw)

  }

  ## historical values of the demand, fuel level
  calibr_demand=tech_output
  logit_exponent_FV=logit_exp_data[["logit_exponent_FV"]]
  logit_exponent_VS1=logit_exp_data[["logit_exponent_VS1"]]
  logit_exponent_S1S2=logit_exp_data[["logit_exponent_S1S2"]]
  logit_exponent_S2S3=logit_exp_data[["logit_exponent_S2S3"]]
  logit_exponent_S3S=logit_exp_data[["logit_exponent_S3S"]]

  value_time_FV=vot_data[["value_time_FV"]]
  value_time_VS1=vot_data[["value_time_VS1"]]
  value_time_S1S2=vot_data[["value_time_S1S2"]]
  value_time_S2S3=vot_data[["value_time_S2S3"]]
  base_SW=calibr_demand[tech_output>0,]
  base_SW=base_SW[,share:=tech_output/sum(tech_output),by=c("region","year","vehicle_type")]
  base_SW=merge(base_SW,logit_exponent_FV,all.x=TRUE,by=c("sector", "subsector_L1", "vehicle_type", "subsector_L2", "subsector_L3"))

  price_FV = prices[year <= 2010]
  price_FV= merge(price_FV, price_nonmot,all=TRUE, by=c("tot_price","region","year","technology","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector"))

  price_FV[,tot_VOT_price := 0]
  #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
  price_FV[is.na(fuel_price_pkm), fuel_price_pkm := 0]
  price_FV[is.na(non_fuel_price), non_fuel_price := 0]

  dups <- duplicated(price_FV, by=c("region", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(price_FV[dups])
    price_FV <- unique(price_FV, by=c("region", "technology", "vehicle_type","year"))
  }

  FV_SW=merge(price_FV,base_SW,all=FALSE,by=c("region","year","technology","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector"))
  FV_SW=FV_SW[!is.na(tech_output),] ## minor adjusments, TODO check if needed/why needed!
  ## needs rando lambdas for the sectors that are not explicitly calculated
  FV_SW[,logit.exponent:=ifelse(is.na(logit.exponent),-10,logit.exponent)]

  FV_SW=sw_calc( df_sw=FV_SW, exp_prices = c(2,1,3,4,5,6,7), grouping_value="vehicle_type")

  ## reshape, save and store the sw at this level
  FV_final_SW=FV_SW[,.(region,year,technology,tot_price,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector,sw,logit.exponent)]

  ## merge value of time and assign 0 to the entries that don't have it
  FV_SW=merge(FV_SW,value_time_FV,all.x=TRUE,by=c("region", "year", "vehicle_type","subsector_L1"))
  FV_SW[,time_price:=ifelse(is.na(time_price),0,time_price)]
  FV_SW[,tot_price:=tot_price+time_price]

  ## calculate price of one level up
  FV_SW=FV_SW[,.(tot_price=sum(share*tot_price),tech_output=sum(tech_output)),by = .(region,year,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector)]

  ## now we go from V to SubS VSubS
  VS1_SW=merge(FV_SW,logit_exponent_VS1,all.x=TRUE,by = c("subsector_L1", "subsector_L2", "subsector_L3", "sector"))
  ## needs random lambdas for the sectors that are not explicitly calculated
  VS1_SW[,logit.exponent:=ifelse(is.na(logit.exponent),-10,logit.exponent)]
  ## calculate shares at this level
  VS1_SW[,share:=tech_output/sum(tech_output),by=c("region","year","subsector_L1")]
  ## optimize
  VS1_SW=sw_calc(df_sw = VS1_SW, exp_prices = c(7,1,2,6,5,3),grouping_value = "subsector_L1")

  ## reshape, save and store the sw at this level
  VS1_final_SW=VS1_SW[,.(region,year,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector,sw,tot_price,logit.exponent)]

  ## merge value of time and assign 0 to the entries that don't have it
  VS1_SW=merge(VS1_SW,value_time_VS1,all.x=TRUE,by=c("region", "year", "vehicle_type", "subsector_L1"))
  VS1_SW[,time_price:=ifelse(is.na(time_price),0,time_price)]
  VS1_SW[,tot_price:=tot_price+time_price]

  VS1_SW=VS1_SW[,.(tot_price=sum(share*tot_price),tech_output=sum(tech_output)),by = .(region,year,subsector_L1,subsector_L2,subsector_L3,sector)]

  ## rename the database, now it's going from V to S1
  S1S2_sw=merge(VS1_SW,logit_exponent_S1S2,all.x=TRUE)
  S1S2_sw[,share:=tech_output/sum(tech_output),by=c("region","year","sector","subsector_L3","subsector_L2")]

  ## needs rando lambdas for the sectors that are not explicitly calculated
  S1S2_sw[,logit.exponent:=ifelse(is.na(logit.exponent),-10,logit.exponent)]

  S1S2_sw=sw_calc(S1S2_sw,exp_prices = c(1,0.5,2,3,1,4),grouping_value = "subsector_L2")

  ## reshape, save and store the sw at this level
  S1S2_final_SW=S1S2_sw[,.(region,year,subsector_L1,subsector_L2,subsector_L3,sector,sw,tot_price,logit.exponent)]

  S1S2_sw=merge(S1S2_sw,value_time_S1S2,all.x = TRUE,by = c("region", "year", "subsector_L1","subsector_L2"))
  S1S2_sw[,time_price:=ifelse(is.na(time_price),0,time_price)]
  S1S2_sw[,tot_price:=tot_price+time_price]

  S1S2_sw=S1S2_sw[,.(tot_price=sum(share*tot_price),tech_output=sum(tech_output)),by = .(region,year,subsector_L2,subsector_L3,sector)]

  ## rename the database, now it's going from S2 to S3
  S2S3_sw=merge(S1S2_sw,logit_exponent_S2S3,all.x=TRUE)
  S2S3_sw[,share:=tech_output/sum(tech_output),by=c("region","year","sector","subsector_L3")]

  ## needs rando lambdas for the sectors that are not explicitly calculated
  S2S3_sw[,logit.exponent:=ifelse(is.na(logit.exponent),-10,logit.exponent)]

  S2S3_sw=sw_calc(df_sw = S2S3_sw, exp_prices = c(2,3,4,2,1), grouping_value = "subsector_L3")

  ## reshape, save and store the sw at this level
  S2S3_final_SW=S2S3_sw[,.(region,year,subsector_L2,subsector_L3,sector,sw, tot_price, logit.exponent)]
  S2S3_sw=merge(S2S3_sw,value_time_S2S3,all.x = TRUE,by = c("region", "year", "subsector_L2","subsector_L3"))
  S2S3_sw[,time_price:=ifelse(is.na(time_price),0,time_price)]
  S2S3_sw[,tot_price:=tot_price+time_price]

  S2S3_sw=S2S3_sw[,.(tot_price=sum(share*tot_price),tech_output=sum(tech_output)),by = .(region,year,subsector_L3,sector)]

  ## rename the database, now it's going from S2 to S3
  S3S_sw=merge(S2S3_sw,logit_exponent_S3S,all.x=TRUE)
  S3S_sw[,share:=tech_output/sum(tech_output),by=c("region","year","sector")]

  S3S_sw=sw_calc(df_sw = S3S_sw, exp_prices = c(1,2,5,3,1,4), grouping_value = "sector")

  ## reshape, save and store the sw at this level
  S3S_final_SW=S3S_sw[,.(region,year,subsector_L3,sector,sw,tot_price,logit.exponent)]

  return(list(list_SW = list(S2S3_final_SW=S2S3_final_SW,
                                 S3S_final_SW=S3S_final_SW,
                                 S1S2_final_SW=S1S2_final_SW,
                                 VS1_final_SW=VS1_final_SW,
                                 FV_final_SW=FV_final_SW)))
}
