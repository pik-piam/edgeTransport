#' Calculate a trend for share weights and inconvenience costs based on the EDGE scenario
#'
#' @param SWS preference factors
#' @param calibdem calibration demand
#' @param incocost inconvenience costs for 4wheelers
#' @param years time steps
#' @param smartlifestyle switch activating sustainable lifestyles
#' @param tech_scen technology at the center of the policy packages
#'
#' @importFrom zoo na.approx na.spline
#' @return projected trend of preference factors
#' @author Alois Dirnaichner, Marianna Rottoli


lvl1_preftrend <- function(SWS, preftab, calibdem, incocost, years,
                           smartlifestyle, tech_scen, SSP_scen){
  subsector_L1 <- gdp_pop <- technology <- tot_price <- sw <- logit.exponent <- NULL
  logit_type <- `.` <- region <- vehicle_type <- subsector_L2 <- subsector_L3 <- NULL
  sector <- V1 <- tech_output <- V2 <- GDP_cap <- value <- convsymmBEVlongDist <- NULL


  ## function that extrapolate constant values
  filldt <- function(dt, proxy){
    yrs_toadd <- setdiff(years, unique(dt$year))
    for (yr in yrs_toadd) {
      tmp_dt <- dt[year == proxy]
      tmp_dt[, year := yr]
      tmp_dt[, sw := NA]
      dt <- rbind(dt, tmp_dt)
    }
    return(dt)
  }

  ## load pref table
  ptab <- fread(preftab, header=T)[techscen == tech_scen]
  ptab <- melt(ptab, value.name = "sw", variable.name = "year", id.vars = colnames(ptab)[1:10])
  ptab[, year := as.numeric(as.character(year))]
  ## add missing years
  ptab <- filldt(ptab, 2020)
  
  ## merge tech prefs
  FVdt <- SWS$FV_final_SW
  
  FVtarget <- ptab[level == "FV"]
  ## insert historical values
  FVtarget[FVdt, sw := i.sw, on=c("region", "year", "vehicle_type", "technology")]
  FVtarget[year <= 2010 & is.na(sw), sw := 0]

  FVtarget[subsector_L3 == "HSR", sw := 1]

  tmps <- filldt(FVdt[grepl("_tmp_", technology)], 2010)[
    , `:=`(sw=1, level="FV", techscen=unique(FVtarget$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  ## merge placeholder
  FVtarget <- rbind(FVtarget, tmps)

  FVtarget[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "vehicle_type", "technology")]
  FVtarget[sw < 0, sw := 0]
  ## introduces NA for sw == 0
  FVtarget[, sw := ifelse(max(sw) == 0, 0, sw/max(sw)),
           by = c("region", "year", "vehicle_type")]
  
  setnames(FVtarget, "sw", "value")
  FVtarget[, logit_type := "sw"]
  FVtarget[, c("techscen", "level", "approx") := NULL]

  ## merge with incocost, this should be moved elsewhere in the future
  FV_inco = FVdt[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids" & year <= 2020]
  FV_inco[, value := tot_price*(sw^(1/logit.exponent)-1)]
  FV_inco[, logit_type := "pinco_tot"]
  FV_inco = FV_inco[,.(region,year,technology,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector,logit_type, value)]
  ## add also values for 2015 and 2020 for Liquids, as the other technologies have them
  FV_inco = rbind(FV_inco, FV_inco[year == 2010][, year := 2015], FV_inco[year == 2010][, year := 2020])
  ## merge to the "normally calculated" pinco, and create the output at this level
  incocost = merge(incocost, unique(FV_inco[,c("region", "vehicle_type")]), all = FALSE)
  FVtarget = rbind(FVtarget, FV_inco, incocost)

  ## merge size prefs
  VSdt <- SWS$VS1_final_SW
  
  VStarget <- ptab[level == "VS1"]
  VStarget[, technology := NULL]
  ## insert historical values
  VStarget[VSdt, sw := i.sw, on=c("region", "year", "sector", "subsector_L1",
                                  "subsector_L2", "subsector_L3", "vehicle_type")]
  VStarget[year <= 2010 & is.na(sw), sw := 0]

  ## merge placeholder
  tmps <- filldt(VSdt[grepl("_tmp_", vehicle_type)], 2010)[
    , `:=`(sw=1, level="VS1", techscen=unique(VStarget$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  ## add missing placeholders (HSR and rail)
  tmps <- unique(
    rbind(
      tmps, rbindlist(
              lapply(c("IND", "MEA", "REF"),
                     function(reg){
                       tmps[region == "JPN"][, region := reg]
                     }))))
  VStarget <- rbind(VStarget, tmps)

  VStarget[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector", "subsector_L1",
                "subsector_L2", "subsector_L3", "vehicle_type")]
  VStarget[sw < 0, sw := 0]
  VStarget[, c("techscen", "level", "approx") := NULL]

  ## merge L1 sws (4W vs 2W)
  S1dt <- SWS$S1S2_final_SW
  
  S1target <- ptab[level == "S1S2"]
  S1target[, c("technology", "vehicle_type") := NULL]
  ## insert historical values
  S1target[S1dt, sw := i.sw, on=c("region", "year", "sector", "subsector_L1",
                                  "subsector_L2", "subsector_L3")]
  S1target[year <= 2010 & is.na(sw), sw := 0]

  ## merge placeholder
  tmps <- filldt(S1dt[grepl("_tmp_", subsector_L1)], 2010)[
    , `:=`(sw=1, level="S1S2", techscen=unique(S1target$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  ## add missing placeholders (HSR and rail)
  tmps <- unique(
    rbind(
      tmps, rbindlist(
              lapply(c("IND", "MEA", "REF"),
                     function(reg){
                       tmps[region == "JPN"][, region := reg]
                     }))))
  S1target <- rbind(S1target, tmps)

  S1target[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector", "subsector_L1",
                "subsector_L2", "subsector_L3")]
  S1target[sw < 0, sw := 0]
  S1target[, c("techscen", "level", "approx") := NULL]
  
  ## merge L2 sws 
  S2dt <- SWS$S2S3_final_SW
  
  S2target <- ptab[level == "S2S3"]
  S2target[, c("technology", "vehicle_type", "subsector_L1") := NULL]
  ## insert historical values
  S2target[S2dt, sw := i.sw, on=c("region", "year", "sector",
                                  "subsector_L2", "subsector_L3")]
  S2target[year <= 2010 & is.na(sw), sw := 0]

  ## merge placeholder
  tmps <- filldt(S2dt[grepl("_tmp_", subsector_L2)], 2010)[
    , `:=`(sw=1, level="S2S3", techscen=unique(S2target$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  tmps <- unique(
    rbind(
      tmps, rbindlist(
              lapply(c("IND", "MEA", "REF"),
                     function(reg){
                       tmps[region == "JPN"][, region := reg]
                     }))))
  S2target <- rbind(S2target, tmps)

  S2target[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector",
                "subsector_L2", "subsector_L3")]
  S2target[sw < 0, sw := 0]
  S2target[, c("techscen", "level", "approx") := NULL]
  
  ## merge L3 sws 
  S3dt <- SWS$S3S_final_SW
  
  S3target <- ptab[level == "S3S"]
  S3target[, c("technology", "vehicle_type", "subsector_L1", "subsector_L2") := NULL]
  ## insert historical values
  S3target[S3dt, sw := i.sw, on=c("region", "year", "sector", "subsector_L3")]
  S3target[year <= 2010 & is.na(sw), sw := 0]

  ## no placeholder on S3

  S3target[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector", "subsector_L3")]
  S3target[sw < 0, sw := 0]
  S3target[, c("techscen", "level", "approx") := NULL]
  

  ## The values of SWS have to be normalized again
  return(list(
    S3S_final_pref=S3target[, sw := ifelse(max(sw) == 0, 0, sw/max(sw)),
                            by = c("region", "year", "sector")],
    S2S3_final_pref=S2target[, sw := ifelse(max(sw) == 0, 0, sw/max(sw)),
                             by = c("region", "year", "subsector_L3")],
    S1S2_final_pref=S1target[, sw := ifelse(max(sw) == 0, 0, sw/max(sw)),
                             by = c("region", "year", "subsector_L2")],
    VS1_final_pref=VStarget[, sw := ifelse(max(sw) == 0, 0, sw/max(sw)),
                            by = c("region", "year", "subsector_L1")],
    FV_final_pref=FVtarget
  ))
}
