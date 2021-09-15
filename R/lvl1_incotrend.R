#' Calculate a trend for share weights and inconvenience costs based on the EDGE scenario
#'
#' @param SWS preference factors
#' @param calibdem calibration demand
#' @param incocost inconvenience costs for 4wheelers
#' @param years time steps
#' @param GDP GDP regional level
#' @param GDP_POP_MER GDP per capita MER
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param smartlifestyle switch activating sustainable lifestyles
#' @param techswitch technology at the center of the policy packages
#' @return projected trend of preference factors
#' @author Alois Dirnaichner, Marianna Rottoli


lvl1_preftrend <- function(SWS, calibdem, incocost, years, GDP, GDP_POP_MER, EDGE_scenario, smartlifestyle, techswitch){
  subsector_L1 <- gdp_pop <- technology <- tot_price <- sw <- logit.exponent <- logit_type <- `.` <- region <- vehicle_type <- subsector_L2 <- subsector_L3 <- sector <- V1 <- tech_output <- V2 <- GDP_cap <- value <- NULL
  ## load gdp as weight
  gdp <- copy(GDP)
  gdpcap <- copy(GDP_POP_MER)

  eu_regions <- c("EUR", "DEU", "ENC", "ESC", "ESW", "ECS", "ECE", "EWN", "FRA", "UKI")

  apply_logistic_trends <- function(initial, yrs, ysymm, speed){
    logistic_trend <- function(year){
      a <- speed
      b <- ysymm

      exp(a * (year - b))/(exp(a * (year - b)) + 1)
    }

    scl <- sapply(yrs, logistic_trend)

    initial + scl * (1 - initial)
  }


  ## function that extrapolate constant values
  extr_const <- function(dt){
    tmp_dt <- dt[year==2010]
    for (yr in years[years > 2010]) {
      tmp_dt[, year := yr]
      dt <- rbind(dt, tmp_dt)
    }
    return(dt)
  }

  ##function that attributes SW==0 or SW==1 to those alternatives that are in theory available but have demand=0 (e.g. HSR  needs an initial SW=0 to develop in the future; Rail (if null) needs a SW=0 at the top level, otherwise is excluded from the future)
  addmissingSW=function(x, calibr_demand, grouping_value){
    `.` <- tech_output <- sw <- NULL
    if (grouping_value != "technology") {
      allcol=c("vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector")
      col=c("region","year",allcol[which(allcol==grouping_value):length(allcol)])
      tmp=calibr_demand[,.(tech_output=sum(tech_output)),by = col]
      tmp=tmp[tech_output==0,]
      tmp=unique(tmp[,col,with=FALSE])     ## col are perceived as column names with with=FALSE
      if (grouping_value=="subsector_L3") {
        tmp[,sw:=0]         ## on the last level, I need them to be 0 because they are not "carried up" but used instead
      }else(
        tmp[,sw:=1]         ## on every other level, giving 1 allows the entry to be "carried up" one level
      )
      x = x[,c(col, "sw"), with = FALSE]
      x = rbind(tmp,x)
    }

    return(x)
  }


  ## entries that are to be treated with inconvenience costs (4wheelers)
  FV_inco = SWS$FV_final_SW[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids" & year <= 2020]
  FV_inco[, value := tot_price*(sw^(1/logit.exponent)-1)]
  FV_inco[, logit_type := "pinco_tot"]
  FV_inco = FV_inco[,.(region,year,technology,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector,logit_type, value)]
  ## add also values for 2015 and 2020 for Liquids, as the other technologies have them
  FV_inco = rbind(FV_inco, FV_inco[year == 2010][, year := 2015], FV_inco[year == 2010][, year := 2020])
  ## merge to the "normally calculated" pinco, and create the output at this level
  incocost = merge(incocost, unique(FV_inco[,c("region", "vehicle_type")]), all = FALSE)
  FV_inco = rbind(FV_inco, incocost)
  ## entries that are to be treated with sws (all vehicle types other than 4wheelers)
  SWS$FV_final_SW = SWS$FV_final_SW[subsector_L1 != "trn_pass_road_LDV_4W"]
  SWS$FV_final_SW=SWS$FV_final_SW[,.(region,year,technology,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector,sw)]

  ## all technologies that have 0 demand have to be included before assuming the SW trend:
  ## some technologies need to start with a pinco=pinco_start (e.g. Mini Car FCEV: it has alternatives-Liquids, NG...-)
  tmp=copy(calibdem[subsector_L1 != "trn_pass_road_LDV_4W"])
  tmp=tmp[, V1 := (sum(tech_output) != 0), by = c("region","year","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector")]
  tmp=tmp[, V2 := (tech_output == 0), by = c("region","year","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector")]

  tmp[V1==TRUE & V2==TRUE,sw:=0]
  tmp=tmp[sw==0,]
  tmp=tmp[,V2:=NULL]

  ## some technologies need a sw=1 (e.g. HSR_tmp_vehicle_type Electric: has no alternatives)
  tmp1=copy(calibdem[subsector_L1 != "trn_pass_road_LDV_4W"])
  tmp1=tmp1[,V1:=(sum(tech_output)==0),by=c("region","year","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector")]
  tmp1[V1==TRUE,sw:=1]
  tmp1=tmp1[sw==1]

  ## merge this 2 sub-cases
  tmp=rbind(tmp,tmp1)[,c("V1","tech_output") := NULL]

  ## merge to the "normally calculated" sws, and create the output at this level
  SWS$FV_final_SW = rbind(SWS$FV_final_SW, tmp)
  ve_type = unique(SWS$VS1_final_SW[subsector_L1 %in% c("trn_pass_road_LDV_4W", "trn_freight_road_tmp_subsector_L1"), c("vehicle_type", "region")])
  SWS <- SWS[c("FV_final_SW","VS1_final_SW", "S1S2_final_SW", "S2S3_final_SW", "S3S_final_SW")]
  ## add the missing entries to all levels
  SWS = mapply(addmissingSW, SWS, calibr_demand = list(calibdem = calibdem, calibdem = calibdem, calibdem = calibdem, calibdem = calibdem, calibdem = calibdem), grouping_value = c("technology","vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3"))
  ve = SWS$VS1_final_SW[subsector_L1 %in% c("trn_pass_road_LDV_4W", "trn_freight_road_tmp_subsector_L1")]
  ve = merge(ve, ve_type, all.y = TRUE, by = c("region", "vehicle_type"))
  rest=SWS$VS1_final_SW[!(subsector_L1 %in% c("trn_pass_road_LDV_4W", "trn_freight_road_tmp_subsector_L1"))]
  SWS$VS1_final_SW = rbind(ve, rest)
  ## constant trends for all techs
  SWS <- lapply(SWS, extr_const)


  SWS$S3S_final_SW[region %in% c("EUR", "ESC", "EWN", "FRA") & subsector_L3 == "Domestic Aviation"  & year >=2015,
                   sw := 0.1*sw[year==2015], by = c("region","subsector_L3")]

  SWS$FV_final_SW = melt(SWS$FV_final_SW, id.vars = c("region", "year", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector"))
  setnames(SWS$FV_final_SW, old = "variable", new = "logit_type")
  SWS$FV_final_SW = rbind(SWS$FV_final_SW, FV_inco)

  ## from now on, SWs and inconvenience costs will coexist. Names of the entries will reflect that, and the generic label "preference" is preferred
  names(SWS) = gsub(pattern = "SW", "pref", names(SWS))

  ## apply S-type trends for alternative vehicles

  if (techswitch == "Liq") {
    convsymmBEV = 2045
    speedBEV = 0.2
    convsymmHydrogenAir = 2100
    speedFCEV = 0.2
    convsymmFCEV = 2045
  } else if (techswitch == "Liq_El"){
    convsymmBEV = 2045
    speedBEV = 0.3
    convsymmHydrogenAir = 2100
    speedFCEV = 0.2
    convsymmFCEV = 2045
  } else if (techswitch == "BEV"){
    convsymmBEV = 2035
    speedBEV = 0.4
    convsymmHydrogenAir = 2100
    speedFCEV = 0.2
    convsymmFCEV = 2045
  } else if (techswitch == "FCEV"){
    convsymmBEV = 2045
    speedBEV = 0.2
    convsymmHydrogenAir = 2080
    speedFCEV = 0.4
    convsymmFCEV = 2035
  }

  ## small trucks
  smtruck = c("Truck (0-3.5t)", "Truck (7.5t)")

  SWS$FV_final_pref[technology == "FCEV" & year >= 2025 & vehicle_type %in% smtruck,
                    value := apply_logistic_trends(value[year == 2025], year, ysymm = convsymmFCEV, speed = speedFCEV),
                    by=c("region","vehicle_type","technology")]

  SWS$FV_final_pref[technology == "FCEV" & year >= 2025 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                             (!vehicle_type %in% smtruck & subsector_L1 == "trn_freight_road_tmp_subsector_L1")),
                    value := apply_logistic_trends(value[year == 2025], year, ysymm = (convsymmFCEV + 10), speed = speedFCEV),
                    by=c("region","vehicle_type","technology")]
  SWS$FV_final_pref[technology == "Electric" & year >= 2025 & vehicle_type %in% smtruck,
                    value := apply_logistic_trends(value[year == 2025], year, ysymm = convsymmBEV, speed = speedBEV),
                    by=c("region","vehicle_type","technology")]

  SWS$FV_final_pref[technology == "Electric" & year >= 2025 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                             (!vehicle_type %in% smtruck & subsector_L1 == "trn_freight_road_tmp_subsector_L1")),
                    value := apply_logistic_trends(value[year == 2025], year, ysymm = (convsymmBEV + 10), speed = speedBEV),
                    by=c("region","vehicle_type","technology")]

  ## hydrogen airplanes develop following an S-shaped curve
  ## in optimistic scenarios, the percentage of hydrogen-fuelled aviation can be around 40% https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf
  SWS$FV_final_pref[technology == "Hydrogen" & year >= 2025 & subsector_L3 == "Domestic Aviation",
                    value := apply_logistic_trends(value[year == 2025], year, ysymm = convsymmHydrogenAir, speed = 0.1),
                    by=c("region","vehicle_type","technology")]


if (techswitch %in% c("BEV", "FCEV")) {
  if (techswitch == "FCEV") {
    ## BEV are constrained, for long distance application
    SWS$FV_final_pref[technology == "Electric" & year >= 2025 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                                   (!vehicle_type %in% smtruck & subsector_L1 == "trn_freight_road_tmp_subsector_L1")),
                      value := apply_logistic_trends(value[year == 2025], year, ysymm = (convsymmBEV + 20), speed = speedBEV),
                      by=c("region","vehicle_type","technology")]
  }

  ## dislike for NG fuelled trucks and buses
  SWS$FV_final_pref[technology %in% c("NG") & year >= 2020 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                                           subsector_L1 == "trn_freight_road_tmp_subsector_L1"),
                    value := ifelse(year <= 2100, value[year==2020] + (0.01*value[year==2020]-value[year==2020]) * (year-2020)/(2100-2020), 0.1*value[year==2020]),
                    by=c("region", "vehicle_type", "technology")]
  ## normalize again
  SWS$FV_final_pref[year >= 2020 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                                 subsector_L1 == "trn_freight_road_tmp_subsector_L1"),
                    value := value/max(value),
                    by=c("region", "vehicle_type", "year")]
}

  ## electric trains develop linearly to 2100
  SWS$FV_final_pref[technology == "Electric" & year >= 2020 & subsector_L3 %in% c("Passenger Rail", "HSR", "Freight Rail"),
                    value := value[year==2020] + (1-value[year==2020]) * (year-2020)/(2100-2020),
                    by=c("region","vehicle_type","technology")]


  ## nat. gas increase linearly for Buses and Trucks (very slowly, 1 is reached in 2400)
  SWS$FV_final_pref[technology %in% "NG" & year >= 2020 & logit_type == "sw",
                    value := value[year==2020] + (1-value[year==2020]) * (year-2020) / (2400-2020),
                    by=c("region","vehicle_type","technology", "logit_type")]

  SWS$FV_final_pref[region%in% c("ECE", "ECS") &technology %in% c("NG") & year >= 2010 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                                                            subsector_L1 == "trn_freight_road_tmp_subsector_L1"),
                    value := 0,
                    by=c("region", "vehicle_type", "technology")]


  SWS$FV_final_pref[region%in% c("ECE", "ECS") &technology %in% c("Liquids") & year >= 2020 & (vehicle_type %in% c("Bus_tmp_vehicletype")|
                                                                                            subsector_L1 == "trn_freight_road_tmp_subsector_L1"),
                    value := ifelse(year <= 2030, value[year==2020] + (1-value[year==2020]) * (year-2020)/(2030-2020), 1),
                    by=c("region", "vehicle_type", "technology")]

  ## CHA has very low prices for 2W. this leads to crazy behaviours, hence their preferenc efactor is set to contant
  SWS$S1S2_final_pref[region %in% c("OAS", "CHA", "IND") & subsector_L1 == "trn_pass_road_LDV_2W"  & year >=2010 & year <= 2020,
                      sw := ifelse(year <= 2020, sw[year==2010] + (0.05*sw[year==2010]-sw[year==2010]) * (year-2010) / (2020-2010), sw),
                      by = c("region","subsector_L1")]
  SWS$S1S2_final_pref[region %in% c("OAS", "CHA", "IND") & subsector_L1 == "trn_pass_road_LDV_2W"  & year >=2020 & year <= 2100,
                      sw := ifelse(year >= 2020, sw[year==2020] + (0.05*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), sw),
                      by = c("region","subsector_L1")]
  SWS$S1S2_final_pref[region %in% c("OAS", "CHA", "IND") & subsector_L1 == "trn_pass_road_LDV_2W"  & year >=2100,
                      sw := sw[year==2100], by = c("region","subsector_L1")]


  ## preference for buses high in EU after 2030
  target_reduction = 0.67
  SWS$S2S3_final_pref[region %in% eu_regions & subsector_L2 == "Bus" & year >= 2020,
                      sw := sw*pmax((1 - target_reduction*(year - 2020)/30), 1-target_reduction)]
  SWS$S2S3_final_pref[region %in% c("DEU") & subsector_L2 == "Bus"  & year >=2010 & year <= 2020,
                      sw := ifelse(year >= 2010, sw[year==2010] + (0.3*sw[year==2010]-sw[year==2010]) * (year-2010) / (2020-2010), sw),
                      by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("DEU") & subsector_L2 == "Bus"  & year >=2020,
                      sw := sw[year==2020], by = c("region","subsector_L2")]

  ## preference for buses extremely high in OAS and IND
  SWS$S2S3_final_pref[region %in% c("SSA", "NES", "LAM", "MEA") & subsector_L2 == "Bus"  & year >=2020 & year <= 2030,
                      sw := ifelse(year <= 2030, sw[year==2020] + (0.5*sw[year==2020]-sw[year==2020]) * (year-2020) / (2030-2020), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("SSA", "NES", "LAM", "MEA") & subsector_L2 == "Bus"  & year >=2030 & year <= 2050,
                      sw := ifelse(year <= 2050, sw[year==2030] + (0.3*sw[year==2030]-sw[year==2030]) * (year-2030) / (2050-2030), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("SSA", "NES", "LAM", "MEA") & subsector_L2 == "Bus"  & year >=2050 & year <= 2100,
                      sw := ifelse(year <= 2100, sw[year==2050] + (0.2*sw[year==2050]-sw[year==2050]) * (year-2050) / (2100-2050), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("SSA", "NES", "LAM", "MEA") & subsector_L2 == "Bus"  & year >=2100,
                      sw := sw[year==2100], by = c("region","subsector_L2")]

  SWS$S2S3_final_pref[region %in% c("IND", "CHA", "OAS") & subsector_L2 == "Bus"  & year >=2010 & year <= 2020,
                      sw := ifelse(year <= 2020, sw[year==2010] + (0.01*sw[year==2010]-sw[year==2010]) * (year-2010) / (2020-2010), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("IND", "CHA", "OAS") & subsector_L2 == "Bus"  & year >=2020 & year <= 2100,
                      sw := ifelse(year >= 2020, sw[year==2020] + (0.01*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("IND", "CHA", "OAS") & subsector_L2 == "Bus"  & year >=2100,
                      sw := sw[year==2100], by = c("region","subsector_L2")]

  ## public transport preference in European countries increases (Rail)
  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% eu_regions & region!= "DEU" & year >= 2020,
                     sw := ifelse(year <= 2100, sw[year==2020] + (0.2*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 0.2*sw[year==2020]),
                     by=c("region", "subsector_L3")]
  SWS$S3S_final_pref[subsector_L3 %in% c("Passenger Rail", "HSR") & region %in% c("ECE", "ECS") & year >= 2020,
                     sw := ifelse(year <= 2100, sw[year==2020] + (0.01*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 0.01*sw[year==2020]),
                     by=c("region", "subsector_L3")]
  SWS$S3S_final_pref[subsector_L3 %in% c("Passenger Rail", "HSR") & region %in% c("DEU") & year >= 2010,
                     sw := ifelse(year <= 2020, sw[year==2020] + (0.2*sw[year==2010]-sw[year==2010]) * (year-2010) / (2020-2010), 0.2*sw[year==2020]),
                     by=c("region", "subsector_L3")]



  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% c("MEA") & year >= 2010,
                     sw := sw[year == 2010],
                     by=c("region")]

  SWS$S3S_final_pref[subsector_L3 %in% c("Passenger Rail") & region %in% c("IND", "CHA") & year >= 2010 & year <= 2020,
                     sw := sw[year==2010] + (0.05*sw[year==2010]-sw[year==2010]) * (year-2010) / (2020-2010),
                     by=c("region", "subsector_L3")]

  SWS$S3S_final_pref[subsector_L3 %in% c("Passenger Rail") & region %in% c("IND", "CHA") & year >= 2020 & year <= 2100,
                     sw := sw[year==2020] + (0.05*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020),
                     by=c("region", "subsector_L3")]


  SWS$S3S_final_pref[subsector_L3 %in% c("Passenger Rail", "HSR") & region %in% c("ECE", "ECS") & year >= 2020,
                     sw := ifelse(year <= 2100, sw[year==2020] + (0.01*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 0.01*sw[year==2020]),
                     by=c("region", "subsector_L3")]

  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% c("JPN") & year >= 2010,
                     sw :=  ifelse(year <= 2100, sw[year==2010] + (0.2*sw[year==2010]-sw[year==2010]) * (year-2010) / (2100-2010), 0.2*sw[year==2010]),
                     by=c("region", "subsector_L3")]


  SWS$S3S_final_pref[subsector_L3 == "Cycle" & region %in% "REF" & year >= 2010,
                     sw := ifelse(year==2010, sw, 3*sw[year==2010]),
                     by=c("region")]
  SWS$S3S_final_pref[subsector_L3 == "Walk" & region %in% "REF" & year >= 2010,
                     sw := ifelse(year==2010, sw, 3*sw[year==2010]),
                     by=c("region")]


  SWS$S3S_final_pref[subsector_L3 %in% c("Domestic Ship") & region %in% "IND" & year >= 2010,
                     sw := ifelse(year == 2010, sw, 0.1*sw[year == 2010]),
                     by=c("region", "subsector_L3")]

  SWS$S3S_final_pref[subsector_L3 %in% c("Domestic Aviation") & region %in% c("IND", "CHA") & year >= 2010,
                     sw := 0.01*sw[year==2010],
                     by=c("region", "subsector_L3")]

  SWS$S3S_final_pref[region %in% c("CHA") & subsector_L3 == "Domestic Aviation"  & year >= 2010 & year <2015,
                     sw := 100*sw[year==2010], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("CHA") & subsector_L3 == "Domestic Aviation"  & year >= 2015 & year <=2020,
                     sw := 10*sw[year==2015], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("IND") & subsector_L3 == "Domestic Aviation"  & year >= 2010 & year <2015,
                     sw := 150*sw[year==2010], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("IND") & subsector_L3 == "Domestic Aviation"  & year >= 2015 & year <2030,
                     sw := 10*sw[year==2015], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("IND") & subsector_L3 == "Domestic Aviation"  & year >= 2030 & year <2040,
                     sw := 5*sw[year==2030], by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("IND") & subsector_L3 == "Domestic Aviation"  & year >= 2040,
                     sw := 2*sw[year==2040], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("JPN") & subsector_L3 == "Domestic Aviation"  & year >= 2025,
                     sw := 2*sw[year==2025], by = c("region","subsector_L3")]

  ## domestic aviation grows way to much, reduce it
  SWS$S3S_final_pref[region %in% c("NEU") & subsector_L3 == "Domestic Aviation"  & year == 2015,
                     sw := 0.001, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("NEU") & subsector_L3 == "Domestic Aviation"  & year == 2020,
                     sw := 0.0001, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("NEU") & subsector_L3 == "Domestic Aviation"  & year == 2025,
                     sw := 0.0001, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("NEU") & subsector_L3 == "Domestic Aviation"  & year >= 2030,
                     sw := 0.0001, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("NEU", "MEA") & subsector_L3 == "trn_pass_road"  & year >=2015,
                     sw := 1, by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("ECE", "ECS") & subsector_L3 == "Domestic Aviation"  & year >=2015,
                   sw := 0.05*sw[year==2015], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("DEU") & subsector_L3 == "Domestic Aviation"  & year >=2020,
                     sw := 1.1*sw[year==2020], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("ESC") & subsector_L3 == "Domestic Aviation"  & year >=2030,
                     sw := 1.1*sw[year==2030], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("FRA") & subsector_L3 == "Domestic Aviation"  & year ==2010,
                     sw := 1.5*sw[year==2010], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("FRA") & subsector_L3 == "Domestic Aviation"  & year ==2015,
                     sw := 5*sw[year==2015], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("UKI") & subsector_L3 == "Domestic Aviation"  & year >=2015,
                     sw := 1.1*sw[year==2015], by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("ESW") & subsector_L3 == "Domestic Aviation"  & year >=2010,
                     sw := 0.4*sw[year==2010], by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("ENC") & subsector_L3 == "Domestic Aviation"  & year >=2010,
                     sw := 0.9*sw[year==2010], by = c("region","subsector_L3")]

  if (smartlifestyle) {
    ## roughly distinguish countries by GDPcap
    richregions = unique(unique(gdpcap[year == 2010 & GDP_cap > 25000, region]))

    ## Preference for Walking increases assuming that the infrastructure and the services are smarter closer etc.
    SWS$S3S_final_pref[subsector_L3 %in% c("Walk") & year >= 2020 & region %in% richregions,
                       sw := sw[year==2020] + (4*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), by = c("region","subsector_L3")]

    ## Preference for Cycling sharply increases in rich countries assuming that the infrastructure and the services are smarter closer etc.
    SWS$S3S_final_pref[subsector_L3 %in% c("Cycle") & year >= 2020 & region %in% richregions,
                       sw := sw[year==2020] + (20*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), by = c("region","subsector_L3")]

    ## Preference for Walking increases assuming that the infrastructure and the services are smarter closer etc.
    SWS$S3S_final_pref[subsector_L3 %in% c("Walk") & year >= 2020 & !(region %in% richregions),
                       sw := sw[year==2020] + (2*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), by = c("region","subsector_L3")]

    ## Preference for Cycling sharply increases in rich countries assuming that the infrastructure and the services are smarter closer etc.
    SWS$S3S_final_pref[subsector_L3 %in% c("Cycle") & year >= 2020 & !(region %in% richregions),
                       sw := sw[year==2020] + (10*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), by = c("region","subsector_L3")]


    ## Preference for Cycling sharply increases in rich countries assuming that the infrastructure and the services are smarter closer etc.
    SWS$S3S_final_pref[subsector_L3 %in% c("Cycle", "Walk") & year >= 2020 & region %in% richregions,
                       sw := sw[year==2020] + (sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), by = c("region","subsector_L3")]

    SWS$S3S_final_pref[subsector_L3 %in% c("trn_pass_road") & year >= 2020 & region %in% richregions,
                       sw := ifelse(year<=2100, sw[year==2020] + (0.9*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 3*sw[year==2020]), by = c("region","subsector_L3")]

    ## public transport preference in European countries increases (Buses)
    SWS$S2S3_final_pref[subsector_L2 == "Bus" & region %in%  richregions & year >= 2020,
                        sw := ifelse(year <= 2100, sw[year==2020] + (1.5*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 2*sw[year==2020]),
                        by=c("region")]

    ## public transport preference in European countries increases (Rail)
    SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% richregions & year >= 2020,
                       sw := ifelse(year <= 2100, sw[year==2020] + (1.5*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 2*sw[year==2020]),
                       by=c("region")]

    ## Small-large cars preference increase 5-fold
    SWS$VS1_final_pref[vehicle_type %in% c("Compact Car", "Mini Car", "Subcompact Car") & year >= 2020,
                       sw := ifelse(year <= 2100, sw[year==2020] + (5*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 2*sw[year==2020]),
                       by=c("region", "vehicle_type")]


  }


  ## linear convergence is fixed if goes beyond 0 or above 1
  SWS$FV_final_pref[value > 1 & logit_type == "sw", value := 1]
  SWS$S2S3_final_pref[, sw := ifelse(year >2100 & sw<0, sw[year == 2100], sw), by = c("region", "subsector_L2")]
  SWS$S3S_final_pref[, sw := ifelse(year >2100 & sw<0, sw[year == 2100], sw), by = c("region", "subsector_L3")]

  ## The values of SWS have to be normalized again
  SWS$S3S_final_pref[, sw := sw/max(sw),
                     by = c("region", "year", "sector")]

  SWS$S2S3_final_pref[, sw := sw/max(sw),
                      by = c("region", "year", "subsector_L3")]

  SWS$S1S2_final_pref[, sw := sw/max(sw),
                      by = c("region", "year", "subsector_L2")]

  SWS$VS1_final_pref[, sw := sw/max(sw),
                     by = c("region", "year", "subsector_L1")]

  SWS$FV_final_pref[logit_type =="sw", value := value/max(value),
                     by = c("region", "year", "vehicle_type")]

  return(SWS)
}
