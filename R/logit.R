#' Evaluate logit tree shares from real prices and inconvenience costs.
#' Inconvenience costs are updated based on shares from previous timesteps (endogenous formulation).
#'
#' @param prices logit prices
#' @param vot_data value-of-time data
#' @param pref_data inconvenience cost data and SWs
#' @param logit_params contains logit exponents
#' @param intensity_data logit level intensity data
#' @param price_nonmot price of non-motorized modes in the logit tree
#' @param ptab4W inconvenience cost factors for LDVs
#' @param logit_data_4W the output of the 4W logit calculation
#' @import data.table
#' @export

toolCalculateLogitSW <- function(prices,
                                        vot_data,
                                        pref_data,
                                        logit_params,
                                        intensity_data,
                                        price_nonmot,
                                        ptab4W,
                                        logit_data_4W) {

  tot_price <- non_fuel_price <- subsector_L3 <- logit.exponent <- share <- sw <- time_price <- NULL
  tot_VOT_price <- `.` <- fuel_price_pkm <- subsector_L1 <- D <- index_yearly <- pinco <- NULL
  shareVS1 <- sw <- region <- vehicle_type <- shareFS1 <- weighted_sharessum <- NULL
  technology <- cluster <- combined_shareEL <- combined_shareLiq <- tail <- NULL
  sector <- subsector_L2 <- MJ_km <- EJ_Mpkm_final <- type <- dpp_nfp <- fuel_price <- value_time <- NULL
  logit_type <- pchar <- pinco_tot <- pmod_av <- prange <- pref <- prisk <- NULL

  ## X2Xcalc is used to traverse the logit tree, calculating shares and intensities
  X2Xcalc <- function(prices, pref_data, logit_params, value_time, mj_km_data, level_base, level_next, group_value) {
    final_pref <- pref_data[[paste0(level_next, "_final_pref")]]
    logit_exponent <- logit_params[[paste0("logit_exponent_", level_next)]]

    ## data contains all the prices in the beginning
    all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                        "subsector_L3", "sector")
    ## takes the right files using the names as inputs
    value_time <- vot_data[[paste0("value_time_", level_next)]]
    ## joins the df containing the prices with the df containing the logit exponents
    df <- merge(prices, logit_exponent,
                by=intersect(names(prices), names(logit_exponent)), all.x = TRUE)

    ## joins the previous df with gathe df containing the inconvenience costs
    df <- merge(df, final_pref, by=intersect(names(df), names(final_pref)), all.y=TRUE)
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]

    ## needs random lambdas for the sectors that are not explicitly calculated
    df <- df[ is.na(logit.exponent), logit.exponent := -10]

    ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)

    df <- df[is.na(time_price), time_price := 0]
    df <- df[, tot_VOT_price := time_price + tot_VOT_price]
    df <- df[, tot_price := tot_price + time_price]

    ## calculate the shares given prices, lambda and inco
    df <- df[, share := sw*tot_price^logit.exponent/(sum(sw*tot_price^logit.exponent)),
             by = c(group_value, "region", "year")]

    nas <- df[is.na(share)]
    if(nrow(nas) > 0) {
      print("NAs found in SWs.")
      browser()
    }
    MJ_km <- merge(df, mj_km_data, by=intersect(names(df),names(mj_km_data)), all = FALSE)

    MJ_km <- MJ_km[, .(MJ_km = sum(share * MJ_km)),
                   by = c("region", "year", "technology", group_value)]

    ## get rid of the ( misleading afterwards) columns
    df_shares <- copy(df)

    df_shares <- df_shares[
      , c("share", "region", "year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)],
          "tot_VOT_price",
          "fuel_price_pkm",
          "non_fuel_price",
          "tot_price"), with = FALSE]

    ## calculate 'one level up' database with the useful columns only
    df <- df[
      , c("share","tot_price","tot_VOT_price",
          "fuel_price_pkm","non_fuel_price","region","year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)]), with = FALSE]

    ## calculate prices of one level up
    df=df[,.(tot_price=sum(share*tot_price),
             tot_VOT_price=sum(share*tot_VOT_price),
             fuel_price_pkm=sum(share*fuel_price_pkm),
             non_fuel_price=sum(share*non_fuel_price)),
          by = c("region","year",
                 all_subsectors[
                   seq(match(group_value,all_subsectors),
                       length(all_subsectors),1)])]

    return(list(df = df, MJ_km = MJ_km, df_shares = df_shares))

  }


  F2Vcalc <- function(prices, pref_data, ptab4W, logit_params, value_time,
                      mj_km_data, group_value) {
    vehicles_number <- param <- value <- NULL
    final_prefFV <- pref_data[["FV_final_pref"]]
    final_prefVS1 <- pref_data[["VS1_final_pref"]]
    logit_exponentFV <- logit_params[["logit_exponent_FV"]]
    logit_exponentVS1 <- logit_params[["logit_exponent_VS1"]]
    ## create single datatable for F->V with columns separated for inconvenience and sw
    final_prefFV = dcast(final_prefFV, region + year + technology + vehicle_type +
                                       subsector_L1 + subsector_L2 + subsector_L3 + sector ~
                                         logit_type, value.var = "value")

    ## data contains all the prices in the beginning
    all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                        "subsector_L3", "sector")
    ## takes the right files using the names as inputs
    value_time <- vot_data[[paste0("value_time_FV")]]
    ## joins the df containing the prices with the df containing the logit exponents
    df <- merge(prices, logit_exponentFV,
                by = intersect(names(prices), names(logit_exponentFV)), all.x = TRUE)

    ## joins the previous df with gathe df containing the inconvenience costs
    df <- merge(df, final_prefFV, by=intersect(names(df),names(final_prefFV)), all.y = TRUE)
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]
    ## needs random lambdas for the sectors that are not explicitly calculated
    df <- df[ is.na(logit.exponent), logit.exponent := -10]

    ## all modes other then 4W calculated with exogenous sws
    dfother = df[(subsector_L1 != "trn_pass_road_LDV_4W"), c("region", "year", "subsector_L2", "subsector_L3", "sector", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent", "sw", "tot_VOT_price", "fuel_price_pkm", "non_fuel_price")]

    ##HOTfix: Truck size classes are not all included in the input data -> they need to be added manually
    ##ATTENTION: This should be changed in the refactoring process
    dfother[grepl("^Truck", vehicle_type), logit.exponent := -4]

    ## for all entries other than 4wheelers, shares based on SW are calculated
    dfother[, share := sw*tot_price^logit.exponent/(sum(sw*tot_price^logit.exponent)),
            by = c(group_value, "year", "region")]
    ## 4W and all other entries are merged
    df <- dfother[, .(region, year, share, technology, vehicle_type, subsector_L1, subsector_L2, subsector_L3, sector, fuel_price_pkm, non_fuel_price, tot_price)]

    ## merge energy intensity
    MJ_km <- merge(df, mj_km_data, by=intersect(names(df),names(mj_km_data)),all = FALSE)
    MJ_km <- MJ_km[, .(MJ_km = sum(share * MJ_km)),
                   by = c("region", "year", "technology", group_value)]

    ## save complete dt at this level
    df_shares <- copy(df)
    df_shares <- df_shares[,tot_VOT_price:=0]
    ## get rid of the ( misleading afterwards) columns
    df_shares <- df_shares[
      , c("share", "region", "year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)],
          "tot_VOT_price",
          "fuel_price_pkm",
          "non_fuel_price",
          "tot_price"), with = FALSE]

     ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)
    df <- df[is.na(time_price), time_price := 0]
    df <- df[, tot_VOT_price := time_price]
    df <- df[, tot_price := tot_price + time_price]

    ## calculate 'one level up' database with the useful columns only
    df <- df[
      , c("share","tot_price","tot_VOT_price",
          "fuel_price_pkm","non_fuel_price","region","year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)]), with = FALSE]

    ## calculate prices of one level up
    df=df[,.(tot_price=sum(share*tot_price),
             tot_VOT_price=sum(share*tot_VOT_price),
             fuel_price_pkm=sum(share*fuel_price_pkm),
             non_fuel_price=sum(share*non_fuel_price)),
          by = c("region","year",
                 all_subsectors[
                   seq(match(group_value,all_subsectors),
                       length(all_subsectors),1)])]

    return(list(df = df, MJ_km = MJ_km, df_shares = df_shares, pref_data = pref_data))

  }


  ## FV load technology prices and merge with value of time (~technology price for
  ## non-motorized)
  ## non-fuel prices
  base <- merge(prices[!is.na(tot_price)], price_nonmot, all = TRUE,
                by = c("tot_price","region","year",
                       "technology","vehicle_type",
                       "subsector_L1","subsector_L2","subsector_L3","sector"))

  base[,tot_VOT_price := 0]
  #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
  base[is.na(fuel_price_pkm), fuel_price_pkm := 0]
  base[is.na(non_fuel_price), non_fuel_price := 0]

  ## energy intensity
  mj_km_data <- intensity_data[, MJ_km := EJ_Mpkm_final
                               * 1e12 # to MJ
                               * 1e-6 # MJ/km
                               ]
  mj_km_data <- mj_km_data[,-"EJ_Mpkm_final"]

  ## FV
  FV_non4W <- F2Vcalc(prices = base,
                    pref_data = pref_data,
                    ptab4W = ptab4W,
                    logit_params = logit_params,
                    value_time = value_time,
                    mj_km_data = mj_km_data,
                    group_value = "vehicle_type")

  FV <- rbind(FV_non4W[["df"]], logit_data_4W[["df"]], use.names=T)
  MJ_km_FV <- rbind(FV_non4W[["MJ_km"]], logit_data_4W[["MJ_km"]], use.names=T)
  FV_shares <- rbind(FV_non4W[["df_shares"]], logit_data_4W[["df_shares"]], use.names=T)
  annual_sales <- logit_data_4W[["annual_sales"]]
  nas <- FV_shares[is.na(share)]
  if(nrow(nas) > 0){
    print("NAs found in FV shares.")
    nas
    browser()
  }
  # VS1
  VS1_all <- X2Xcalc(prices = FV,
                     pref_data = pref_data,
                     logit_params = logit_params,
                     value_time = value_time,
                     mj_km_data = MJ_km_FV,
                     level_base = "FV",
                     level_next = "VS1",
                     group_value = "subsector_L1")

  VS1 <- VS1_all[["df"]]
  MJ_km_VS1 <- VS1_all[["MJ_km"]]
  VS1_shares <- VS1_all[["df_shares"]]
  VS1_shares=VS1_shares[,-c("sector","subsector_L2","subsector_L3")]

  # S1S2
  S1S2_all <- X2Xcalc(prices = VS1,
                      pref_data = pref_data,
                      logit_params = logit_params,
                      value_time = value_time,
                      mj_km_data = MJ_km_VS1,
                      level_base = "VS1",
                      level_next = "S1S2",
                      group_value = "subsector_L2")
  S1S2 <- S1S2_all[["df"]]
  MJ_km_S1S2 <- S1S2_all[["MJ_km"]]
  S1S2_shares <- S1S2_all[["df_shares"]]


  # S2S3
  S2S3_all <- X2Xcalc(prices = S1S2,
                      pref_data = pref_data,
                      logit_params = logit_params,
                      value_time = value_time,
                      mj_km_data = MJ_km_S1S2,
                      level_base = "S1S2",
                      level_next = "S2S3",
                      group_value = "subsector_L3")

  S2S3 <- S2S3_all[["df"]]
  MJ_km_S2S3 <- S2S3_all[["MJ_km"]]
  S2S3_shares <- S2S3_all[["df_shares"]]

  # S3S
  S3S_all <- X2Xcalc(prices = S2S3,
                     pref_data = pref_data,
                     logit_params = logit_params,
                     value_time = value_time,
                     mj_km_data = MJ_km_S2S3,
                     level_base = "S2S3",
                     level_next = "S3S",
                     group_value = "sector")

  S3S <- S3S_all[["df"]]
  MJ_km_S3S <- S3S_all[["MJ_km"]]
  S3S_shares <- S3S_all[["df_shares"]]

  share_list=list(S3S_shares=S3S_shares,
                  S2S3_shares=S2S3_shares,
                  S1S2_shares=S1S2_shares,
                  VS1_shares=VS1_shares,
                  FV_shares=FV_shares)

  prices_list=list(S3S=S3S,
                   S2S3=S2S3,
                   S1S2=S1S2,
                   VS1=VS1,
                   FV=FV,
                   base=base)

  result=list(mj_km_data = mj_km_data,
              prices_list = prices_list,
              share_list = share_list,
              pref_data = pref_data,
              annual_sales = annual_sales)

  return(result)
}

#' Evaluate 4W logit tree shares from real prices and inconvenience costs.
#' Inconvenience costs are updated based on shares from previous timesteps (endogenous formulation).
#'
#' @param prices logit prices
#' @param vot_data value-of-time data
#' @param pref_data inconvenience cost data and SWs
#' @param logit_params contains logit exponents
#' @param intensity_data logit level intensity data
#' @param price_nonmot price of non-motorized modes in the logit tree
#' @param ptab4W inconvenience cost factors for LDVs
#' @param totveh total demand for LDVs by tecnology, in million veh
#' @param tech_scen technology scenario
#' @import data.table
#' @export

toolCalculateLogitIncost <- function(prices,
                              vot_data,
                              pref_data,
                              logit_params,
                              intensity_data,
                              price_nonmot,
                              ptab4W,
                              tech_scen,
                              totveh = NULL ){

   `.` <- D <- EJ_Mpkm_final <- fuel_price_pkm <- index_yearly <- logit.exponent <- logit_type <-
    non_fuel_price <- pchar <- pinco_tot <- pmod_av <- prange <- pref <- prisk <- region <- sector <-
      share <- shareFS1 <- shareVS1 <- subsector_L1 <- subsector_L2 <- subsector_L3 <- sw <- tail <-
        technology <- time_price <- tot_VOT_price <- tot_price <- vehicle_type <- weighted_sharessum <- NULL
  ## FV load technology prices and merge with value of time (~technology price for
  ## non-motorized)
  ## non-fuel prices
  prices <- merge(prices[!is.na(tot_price)], price_nonmot, all = TRUE,
                by = c("tot_price","region","year",
                       "technology","vehicle_type",
                       "subsector_L1","subsector_L2","subsector_L3","sector"))

  prices[,tot_VOT_price := 0]
  #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
  prices[is.na(fuel_price_pkm), fuel_price_pkm := 0]
  prices[is.na(non_fuel_price), non_fuel_price := 0]

  ## energy intensity
  mj_km_data <- intensity_data[, MJ_km := EJ_Mpkm_final
                               * 1e12 # to MJ
                               * 1e-6 # MJ/km
                               ]
  mj_km_data <- mj_km_data[,-"EJ_Mpkm_final"]

  vehicles_number <- param <- value <- NULL
  final_prefFV <- pref_data[["FV_final_pref"]]
  final_prefVS1 <- pref_data[["VS1_final_pref"]]
  logit_exponentFV <- logit_params[["logit_exponent_FV"]]
  logit_exponentVS1 <- logit_params[["logit_exponent_VS1"]]
  ## create single datatable for F->V with columns separated for inconvenience and sw
  final_prefFV = dcast(final_prefFV, region + year + technology + vehicle_type + subsector_L1 + subsector_L2 + subsector_L3 + sector ~ logit_type, value.var = "value")
  ## liquids don't have pref, prange, pchar, pmod_av, prisk. Other techs don't have the generic pinco_tot
  final_prefFV[, pinco_tot := ifelse(is.na(pinco_tot), 0, pinco_tot)]
  final_prefFV[, prange := ifelse(is.na(prange), 0, prange)]
  final_prefFV[, pchar := ifelse(is.na(pchar), 0, pchar)]
  final_prefFV[, pref := ifelse(is.na(pref), 0, pref)]
  final_prefFV[, pmod_av := ifelse(is.na(pmod_av), 0, pmod_av)]
  final_prefFV[, prisk := ifelse(is.na(prisk), 0, prisk)]

  intensity_data = intensity_data[EJ_Mpkm_final>0]

  ## data contains all the prices in the beginning
  all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                      "subsector_L3", "sector")
  ## takes the right files using the names as inputs
  value_time <- vot_data[["value_time_FV"]]
  ## joins the df containing the prices with the df containing the logit exponents
  df <- merge(prices, logit_exponentFV,
              by = intersect(names(prices), names(logit_exponentFV)), all.x = TRUE)

  ## joins the previous df with gathe df containing the inconvenience costs
  df <- merge(df, final_prefFV, by=intersect(names(df),names(final_prefFV)), all.y = TRUE)
  ## delete entries have tot_price NA (e.g. 1900 BEV)
  df <- df[ !(is.na(tot_price))]
  ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
  df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]
  ## needs random lambdas for the sectors that are not explicitly calculated
  df <- df[ is.na(logit.exponent), logit.exponent := -10]


  ## define the years on which the inconvenience price will be calculated on the basis of the previous time steps sales
  futyears_all = seq(2020, 2101, 1)
  ## all modes other then 4W calculated with exogenous sws
  dfhistorical4W = df[(subsector_L1 == "trn_pass_road_LDV_4W" & year < 2020), c("region", "year", "subsector_L2", "subsector_L3", "sector", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent", "pinco_tot", "prange", "pref", "pmod_av", "prisk", "pchar", "tot_VOT_price", "fuel_price_pkm", "non_fuel_price")]

  ## 4W are calculated separately
  df4W = df[subsector_L1 == "trn_pass_road_LDV_4W", c("region", "year", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent")]

  ## extrapolate for all years
  df4W = approx_dt(dt = df4W, xdata = futyears_all,
                   xcol = "year", ycol = "tot_price",
                   idxcols = c("region",  "subsector_L1", "vehicle_type", "technology"),
                   extrapolate=T)

  ## other price components for 4W are useful later but will not be carried on in the yearly calculation
  dfprices4W = prices[subsector_L1 == "trn_pass_road_LDV_4W",  c("region", "year", "subsector_L1", "vehicle_type", "technology", "fuel_price_pkm", "non_fuel_price")]

  ## starting value for inconvenience cost of 2020 for 4W is needed as a starting point for the iterative calculations
  dfpinco2020 = df[subsector_L1 == "trn_pass_road_LDV_4W" & year == 2020, c("region", "subsector_L1", "vehicle_type", "technology", "pinco_tot", "prange", "pref", "pmod_av", "prisk", "pchar")]

  ## merge the yearly values and the starting inconvenience cost
  df = merge(df4W, dfpinco2020, all = TRUE, by = c("region", "subsector_L1", "vehicle_type", "technology"))
  ## apply the same logit exponent to all the years
  df[, logit.exponent := as.double(logit.exponent)]
  df[, logit.exponent := ifelse(is.na(logit.exponent), mean(logit.exponent, na.rm = TRUE), logit.exponent), by = c("vehicle_type")]

  ## for 4W the value of V->S1 market shares is needed on a yearly basis
  final_prefVS1cp = final_prefVS1[subsector_L1 == "trn_pass_road_LDV_4W"]

  final_prefVS1cp = approx_dt(dt = final_prefVS1cp, xdata = futyears_all,
                              xcol = "year", ycol = "sw",
                              idxcols = c("region", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector"),
                              extrapolate=T)

  ## initialize values needed for the for loop
  tmp2past = NULL
  tmp1 = df[year < 2020,]
  tmp1[, share := NA]

  ## define the weight that has to be attributed to each year
  paux = 15   ## approximate lifetime of a car
  Ddt = data.table(index_yearly = seq(1,paux,1))
  Ddt[, D := 1-((index_yearly-0.5)/paux)^4]
  Ddt[, D := ifelse(D<0,0,D)]

  ## in case the total vehicle number is not provided, 1 is attributed
  if (is.null(totveh)) {
    totveh = CJ(region =unique(df[, region]), year = seq(2020, 2100, 1))
    totveh[, vehicles_number := 1]
  } else {
    ## in case the total vehicle number is provided, the values have to be interpolated to get yearly values
    totveh = approx_dt(totveh,
                       seq(2020, 2100, 1),
                       xcol = "year", ycol = "vehicles_number",
                       idxcols = c("region"),
                       extrapolate=T)
  }

  start <- Sys.time()
  for (t in futyears_all[futyears_all>2020]) {

    if (t > 2021) {
      tmp <- df[year == t,][, c("share") := NA]
      tmp <- merge(tmp, tmp1, all = TRUE, by = intersect(names(tmp), names(tmp1)))
    } else {
      tmp <- df[year %in% c(2020, 2021),]
    }
    tmp <- tmp[year == (t-1), share := (tot_price + pinco_tot + prange + pref + pmod_av + prisk  + pchar)^logit.exponent/(sum((tot_price + pinco_tot + prange + pref + pmod_av + prisk + pchar)^logit.exponent)),
               by = c("vehicle_type", "year", "region")]
    tmp2 <- tmp[year == (t-1)]
    tmp2 <- tmp2[,.(tot_price=sum(share*tot_price)),
                 by = c("region","year","vehicle_type","subsector_L1")]

    ## calculate the average share FS1 (the vehicle type is not important)
    tmp2 <- merge(tmp2, final_prefVS1cp, by=intersect( names(tmp2),names(final_prefVS1cp)), all.y=TRUE)
    tmp2 <- merge(tmp2, logit_exponentVS1,
                  by=intersect(names(tmp2), names(logit_exponentVS1)), all.x = TRUE)
    tmp2 <- tmp2[, shareVS1 := sw*tot_price^logit.exponent/sum(sw*tot_price^logit.exponent),
                 by = c("subsector_L1", "year", "region")]
    tmp2 <- tmp2[,.(subsector_L1, year, region, vehicle_type, shareVS1)]

    ## market share is calculated on the basis of the values in t-1
    tmp2 <- merge(tmp2, tmp[year %in% (t-1),], by = c("region", "year", "vehicle_type", "subsector_L1"))

    ## calculate the share from fuel to S1 (FS1)
    tmp2[, shareFS1 := share*shareVS1]

    ## calculate the share of all fuels on the subsector_L1 (I don't care anymore about the vehicle_type)
    tmp2 <- tmp2[,.(shareFS1=sum(shareFS1)),by=c("region", "technology", "subsector_L1","year")]

    ## merge with previous years' values
    if (!is.null(tmp2past)) {
      tmp3 <- rbind(tmp2, tmp2past)
    } else {
      tmp3 <- copy(tmp2)
    }

    ## save the values of the past for the next iteration
    tmp2past <- copy(tmp3)

    ## merge the FS1 shares to the FV shares
    tmp <- merge(tmp,tmp3, by= c("technology", "year", "region", "subsector_L1"), all=TRUE)

    ## find depreciation to attribute to each time step
    Ddt1 = copy(Ddt)
    Ddt1 = Ddt1[, year := seq(t-1,t-paux,-1)]
    Ddt1 = Ddt1[year>=2020]

    tmp = merge(tmp, Ddt1, all.x = TRUE, by = "year")
    tmp[is.na(D), D := 0]
    tmp[is.na(shareFS1), shareFS1 := 0]

    tmp = merge(tmp, totveh, by = c("region", "year"))

    ## weighted share depends on the composition of the fleet, which is a weighted average of the previous years' composition
    tmp[, weighted_sharessum := ifelse(year == (t-1), sum(shareFS1[year<t]*D[year<t]*vehicles_number[year<t])/sum(D[year<t]*vehicles_number[year<t]), 0), by = c("region", "technology", "vehicle_type", "subsector_L1")]
    tmp[, vehicles_number := NULL]
    ## for 2020, we assume the value was constant for all the previous years (hence the value for 2020 coincides with the share)
    if (t == 2021) {
      tmp_2020 = tmp[year == 2020,]
    }

    tmp = rbind(tmp[year!=2020], tmp_2020)

    ## coefficients of the intangible costs trend
    bfuelav = -20    ## value based on Greene 2001
    bmodelav = -12   ## value based on Greene 2001
    coeffrisk = 3800 ## value based on Pettifor 2017


    ## Hotfix: CHN has very low costs for NG, which leads to unstable NG behavior. Temporarily constrained to 2020 values
    tmp[technology == "NG", pref := ifelse(year == t,
                                           pmax(pref[year == 2020], pref[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bfuelav)),
                                           pref), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    ## range anxiety for BEVs
    tmp[, prange := ifelse(year == t,
                           prange[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bfuelav),
                           prange), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    ## Calculate trend of refuelling availability costs component
    tmp[, pref := ifelse(year == t,
                         pref[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bfuelav),
                         pref), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    ## the phase-in of BEVs should not be too abrupt
    linDecrease <- function(x, x0, y0, x1, y1) {
      return(min(y0, max(y1, (y1 - y0)/(x1 - x0) * (x - x0) + y0)))
    }

    mult <- linDecrease(t, ptab4W[param == "startYeBEV", value], ptab4W[param == "startValBEV", value], ptab4W[param == "targetYeBEV", value], ptab4W[param == "targetValBEV", value])


    tmp[technology == "BEV", prange :=ifelse(year == t,
                                             pmax(mult*prange[year == 2020], prange),
                                             prange), by = c("region", "technology", "vehicle_type", "subsector_L1")]


    ## the policymaker bans ICEs increasingly more strictly
    linIncrease <- function(x, x0, y0, x1, y1) {
      return(min(y1, max(y0, (y1 - y0)/(x1 - x0) * (x - x0) + y0)))
    }

    floor <- linIncrease(t, ptab4W[param == "startYeICE", value], 0.0, ptab4W[param == "targetYeICE", value], ptab4W[param == "targetValICE", value])

    ## inconvenience cost for liquids is allowed to increase
    tmp[technology == "Liquids", pinco_tot := ifelse(year == t,
                                                     0.5*exp(1)^(weighted_sharessum[year == (t-1)]*bmodelav),
                                                     pinco_tot), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    if(tech_scen %in% c("PhOP", "Mix3", "Mix4", "HydrHype4", "ECEMF_HighEl_ModEff", "ECEMF_HighEl_HighEff", "ECEMF_HighEl_LifestCha", "ECEMF_HighH2_ModEff", "ECEMF_HighH2_HighEff", "ECEMF_HighH2_LifestCha") & t>= 2030){
      ## phase-out of all light-duty vehicle ICEs
      EUreg <- c("DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "EU27", "EUR")

      if(t>=2030 & t<=2032){
        floorEU <- linIncrease(t, 2030, floor, 2032, 0.35)
      } else if(t>2032 & t<=2034){
        floorEU <- linIncrease(t, 2032, 0.35, 2034, 0.95)
      } else if (t== 2035){
        floorEU <- 0.95
      } else {
        floorEU <- 1
      }

      tmp[technology == "Liquids" & region %in% EUreg, pinco_tot := ifelse(year == t,
                                                                           pmax(pinco_tot, floorEU),
                                                                           pinco_tot), by = c("region", "technology", "vehicle_type", "subsector_L1")]

      tmp[technology == "Liquids"  & !region %in% EUreg, pinco_tot := ifelse(year == t,
                                                                             pmax(pinco_tot, floor),
                                                                             pinco_tot), by = c("region", "technology", "vehicle_type", "subsector_L1")]

      tmp[technology == "NG" & region %in% EUreg, pref := ifelse(year == t,
                                                                 pmax(pref, floorEU),
                                                                 pref), by = c("region", "technology", "vehicle_type", "subsector_L1")]

      tmp[technology == "Hybrid Electric" & region %in% EUreg, pmod_av := ifelse(year == t,
                                                                                 pmax(pmod_av, floorEU),
                                                                                 pmod_av), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    } else {

      tmp[technology == "Liquids", pinco_tot := ifelse(year == t,
                                                       pmax(pinco_tot, floor),
                                                       pinco_tot), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    }


    ## hybrid electric inconvenience cost cannot decrease below 50% of 2020 value
    ratioPHEV = ptab4W[param == "ratioPHEV", value]
    tmp[technology %in% c("Hybrid Electric"), pmod_av := ifelse(year == t,
                                                                pmax(pmod_av, ratioPHEV*pmod_av[year == 2020]),
                                                                pmod_av), by = c("region", "technology", "vehicle_type", "subsector_L1")]

    ## annual sales, needed for reporting purposes
    if (t == 2101) {
      annual_sales = tmp[year<=2100, c("region", "year", "technology", "shareFS1", "vehicle_type", "subsector_L1", "share")]
    }

    ## remove "temporary" columns
    tmp[, c("shareFS1","D",  "weighted_sharessum", "index_yearly") := NULL]

    if (t<tail(futyears_all,1)) {
      tmp1 = copy(tmp)
    }

  }
  print(paste("Iterative logit calculation finished in",
              difftime(Sys.time(), start, units="mins"),
              "Minutes"))
  ## tmp1 needs the same structure as dfhistorical4W to produce the complete trend in time of 4wheelers
  tmp1[, c("subsector_L2", "subsector_L3", "sector") := list("trn_pass_road_LDV", "trn_pass_road", "trn_pass")]
  tmp1[, share := NULL]
  ## merge tmp1 and historical 4W
  tmp1 = rbind(tmp1, dfhistorical4W[, c("year", "technology", "region", "subsector_L1", "vehicle_type", "tot_price", "logit.exponent", "pinco_tot", "prange", "pref", "pmod_av", "prisk", "pchar", "subsector_L2", "subsector_L3", "sector")])
  tmp1 <- tmp1[, share := (tot_price+pinco_tot+prange+pref+pmod_av+prisk+pchar)^logit.exponent/(sum((tot_price+pinco_tot+prange+pref+pmod_av+prisk+pchar)^logit.exponent)),
               by = c("vehicle_type", "year", "region")]

  ## merge with prices
  tmp1 <- merge(tmp1[year %in% unique(dfprices4W$year)], dfprices4W, all.x = TRUE,
                by = intersect(names(tmp1), names(dfprices4W)))

  ## values after 2100 are set to be equal to 2100
  df <- rbind(tmp1,
              tmp1[year==2100][,year:=2110],
              tmp1[year==2100][,year:=2130],
              tmp1[year==2100][,year:=2150])
  ## copy of tmp1 is needed to create the updated version of preferences trend
  inconv = copy(df[,.(year, region, sector, subsector_L3, subsector_L2, subsector_L1, vehicle_type, technology, pinco_tot, prange, pref, pmod_av, prisk, pchar)])
  ## inconv and final_prefFV need the same structure as pref_data[["FV_final_pref"]]
  inconv = melt(inconv, id.vars = c("year", "region", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology"))
  setnames(inconv, old = "variable", new = "logit_type")
  final_prefFV = melt(final_prefFV, id.vars = c("year", "region", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology"))
  setnames(final_prefFV, old = "variable", new = "logit_type")
  final_prefFV = rbind(final_prefFV[subsector_L1 != "trn_pass_road_LDV_4W" & logit_type == "sw"],
                       inconv[(technology == "Liquids" & logit_type == "pinco_tot")|
                              (technology == "BEV" & logit_type %in% c("prange", "prisk", "pchar", "pmod_av"))|
                              (technology == "FCEV" & logit_type %in% c("pref", "prisk", "pmod_av"))|
                              (technology == "NG" & logit_type %in% c("pref", "prisk", "pmod_av"))|
                              (technology == "Hybrid Electric" & logit_type %in% c("prisk", "pchar", "pmod_av"))|
                              (technology == "Hybrid Liquids" & logit_type %in% c("prisk", "pmod_av"))])
  final_prefNonMot = final_prefFV[vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),]
  final_prefFV = merge(final_prefFV, unique(intensity_data[, c("region", "vehicle_type")]),
                       by = c("region", "vehicle_type"), all.y = TRUE)
  final_prefFV = rbind(final_prefNonMot, final_prefFV)
  ## overwrite the preferences with the market based ones
  pref_data[["FV_final_pref"]] = final_prefFV

  ## merge energy intensity
  MJ_km <- merge(df, mj_km_data, by=intersect(names(df),names(mj_km_data)),all = FALSE)
  MJ_km <- MJ_km[, .(MJ_km = sum(share * MJ_km)),
                 by = c("region", "year", "technology", "vehicle_type")]

  ## get rid of the ( misleading afterwards) columns
  df <- df[
  , c("share", "region", "year",
      all_subsectors,
      "fuel_price_pkm",
      "non_fuel_price",
      "tot_price"), with = FALSE]
  ## save complete dt at this level
  df_shares <- copy(df)
  df_shares[, tot_VOT_price:=0]

  ## merge value of time for the selected level and assign 0 to the entries that don't have it
  df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)
  df <- df[is.na(time_price), time_price := 0]
  df <- df[, tot_VOT_price := time_price]
  df <- df[, tot_price := tot_price + time_price]

  ## calculate prices of one level up
  df <- df[,.(tot_price=sum(share*tot_price),
           tot_VOT_price=sum(share*tot_VOT_price),
           fuel_price_pkm=sum(share*fuel_price_pkm),
           non_fuel_price=sum(share*non_fuel_price)),
        by = c("region","year",
               all_subsectors[
                 seq(match("vehicle_type",all_subsectors),
                     length(all_subsectors),1)])]

  return(list(df = df, MJ_km = MJ_km, df_shares = df_shares,
              pref_data = pref_data, annual_sales = annual_sales))

}
