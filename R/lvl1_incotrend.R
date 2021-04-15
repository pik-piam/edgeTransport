#' Calculate a trend for share weights and inconvenience costs based on the EDGE scenario and the regional clusters.
#'
#' @param SWS preference factors
#' @param calibdem calibration demand
#' @param incocost inconvenience costs for 4wheelers
#' @param clusters clusters of regions based on geographical structure
#' @param years time steps
#' @param GDP GDP regional level
#' @param GDP_POP GDP per capita
#' @param REMIND_scenario SSP scenario
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param smartlifestyle switch activating sustainable lifestyles
#' @param techswitch technology at the center of the policy packages
#' @return projected trend of preference factors
#' @author Alois Dirnaichner, Marianna Rottoli


lvl1_preftrend <- function(SWS, calibdem, incocost, clusters, years, GDP, GDP_POP, REMIND_scenario, EDGE_scenario, smartlifestyle, techswitch){
  subsector_L1 <- gdp_pop <- technology <- tot_price <- sw <- logit.exponent <- logit_type <- `.` <- region <- vehicle_type <- subsector_L2 <- subsector_L3 <- sector <- V1 <- tech_output <- V2 <- GDP_cap <- value <- NULL
  ## load gdp as weight
  gdp <- copy(GDP)
  gdpcap <- copy(GDP_POP)
  ## function to converge to average generic rich region (useful for vehicle types and subsector_L1)
  aveval=function(dtin,              ## dt of which I recalculate the SW
                  gdpcap            ## per capita gdp
                  ){
    weight <- POP_val <- GDP <- yearconv <- time <- year_at_yearconv <- sw_new <- sw_conv <- NULL
    ## data contains all the prices in the beginning
    all_subsectors <- c("vehicle_type", "subsector_L1", "subsector_L2",
                        "subsector_L3", "sector")
    groupval = intersect(names(dtin), all_subsectors)[2]
    ## merge demand with gdp capita
    dt = merge(dtin, gdpcap, by = c("region", "year"))
    ## define rich regions
    richregions = unique(unique(dt[year == 2010 & GDP_cap > 25000, region]))
    ## calculate average sw (averaged on GDP) across rich countries and find total GDP and population
    richave = dt[region %in% richregions & sw > 0,]
    richave = richave[, .(sw = sum(sw*weight)/sum(weight)), by = c(all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))], "year")]
    richave[, sw := sw/max(sw), by = .(year, get(all_subsectors[match(groupval,
                                                                      all_subsectors)])) ]## normalize to 1 again

    gdpcap = gdpcap[, .(GDP = sum(weight), POP_val = sum(POP_val)), by = c("year")]
    richave = merge(richave, gdpcap, by = c("year"))
    ## average gdp per capita of the cluster regions
    richave[, GDP_cap := GDP/POP_val]

    ## dt on which the GDPcap is checked
    tmp1 = dt[!region %in% richregions, c("region", "year", "sw", "GDP_cap", all_subsectors[seq(match(groupval, all_subsectors) - 1,length(all_subsectors), 1)]), with = FALSE]
    ## dt contaning the gdp towards which to converge
    tmp2 = richave[, c("year", "GDP_cap")]
    tmp2 = unique(tmp2[, c("year", "GDP_cap")])
    ## dt containing the sw for rich countries
    tmp3 = richave[, c("GDP_cap", "POP_val") := NULL]
    ## names has to be different across dts for roll join
    setnames(tmp2, old = c("year"), new = c("time"))
    setnames(tmp3, old = c("year", "sw"), new = c("time", "sw_new"))
    setkey(tmp1,GDP_cap)
    setkey(tmp2,GDP_cap)
    ## find the time step at which the GDPcap matches the GDPcap of the rich countries
    tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

    ## merge with non fuel price of corresponding values
    tmp2 = merge(tmp2, tmp3, by = c("time", all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))]))

    ## find year closest to 2010 for each region, this is the year at which is going to converge
    tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("region")]

    ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
    tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("region", "time")]
    tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("region", "time")]
    ## if year of convergence is 2010, 2015 is selected
    tmp2[yearconv == 2010, yearconv := 2020]
    tmp2[yearconv == 2015, yearconv := 2020]
    ## year at which the convergence happens
    tmp2[, year_at_yearconv := year[time == yearconv], by = c("region",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]
    tmp2[is.na(year_at_yearconv), year_at_yearconv := year[time == yearconv + 5], by = c("region",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]
    ## values of GDPcap equal to GDPcap_rich have the same values as non_fuel_prices of rich countries
    tmp2[year >= year_at_yearconv & year > 2010, sw := sw_new, by = c("region",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]

    ## value of yearconv represents the convergence value
    tmp2[, sw_conv := sw_new[time==yearconv], by = c("region",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]
    tmp2[is.na(sw_conv), sw_conv := sw_new[time==yearconv + 5], by = c("region",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]

    ## convergence is linear until the value corresponding to 2010 is reached
    tmp2[year <= year_at_yearconv & year >= 2010, sw := sw[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(sw_conv-sw[year == 2010]), by =c(all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))], "region")]

    ## select only useful columns
    tmp2 = tmp2[,c("region", "year", "sw", all_subsectors[seq(match(groupval, all_subsectors) - 1,length(all_subsectors), 1)]), with = F]

    ## rich countries need to be reintegrated
    dtout = rbind(tmp2, dtin[region %in% richregions])

    return(dtout)
  }

  ## function to converge to average cluster leader region (useful for subsector_L2, subsector_L3, sector)
  aveval_cluster=function(dtin,               ## dt of which I recalculate the SW
                          gdpcap,             ## per capita gdp
                          clusters            ## clusters with region leader and similar regions
                          ){
    year_at_yearconv <- sw_new <- sw_conv <- yearconv <- i.region_leader <- GDP <- POP_val <- region_leader <- region <- time <- weight <- NULL
    ## data contains all the prices in the beginning
    all_subsectors <- c("subsector_L2", "subsector_L3", "sector")
    groupval = intersect(names(dtin), all_subsectors)[2]
    ## merge demand with gdp capita
    dt = merge(dtin, gdpcap, by = c("region", "year"))
    dt = merge(dt, clusters, by = c("region"))
    ## define rich regions
    richregions = unique(unique(dt[year == 2010 & GDP_cap > 25000 & region %in% unique(clusters$region_leader), region]))
    ## calculate average sw (averaged on GDP) across rich countries and find total GDP and population
    richave = dt[region %in% richregions & sw > 0,]
    richave = richave[, .(sw = sum(sw*weight)/sum(weight)), by = c(all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))], "year", "region_leader")]
    richave[, sw := sw/max(sw), by = .(year, region_leader, get(all_subsectors[match(groupval,
                                                                      all_subsectors)])) ]## normalize to 1 again
    gdpcap_regionleader = gdpcap[region %in% unique(richave$region_leader)]
    gdpcap_regionleader = gdpcap_regionleader[,region_leader := region]
    gdpcap_regionleader[, region:=NULL]
    richave = merge(richave, gdpcap_regionleader, by = c("year", "region_leader"))
    ## dt on which the GDPcap is checked
    tmp1 = dt[!region %in% richregions, c("region", "year", "sw", "GDP_cap", "region_leader", all_subsectors[seq(match(groupval, all_subsectors) - 1,length(all_subsectors), 1)]), with = FALSE]
    ## dt contaning the gdp towards which to converge
    tmp2 = unique(richave[, c("year", "GDP_cap", "region_leader")])
    ## dt containing the sw for rich countries
    tmp3 = richave[, c("GDP_cap", "POP_val") := NULL]
    ## names has to be different across dts for roll join
    setnames(tmp2, old = c("year"), new = c("time"))
    setnames(tmp3, old = c("year", "sw"), new = c("time", "sw_new"))
    setkey(tmp1,GDP_cap)
    setkey(tmp2,GDP_cap)

    ## find the time step at which the GDPcap matches the GDPcap of the rich countries
    tmp5 = NULL
    for (reg in unique(tmp1$region_leader)) {
      tmp4 = tmp2[region_leader == reg]
      tmp4 <- tmp4[tmp1[region_leader == reg], roll = "nearest", on = .(GDP_cap)]
      tmp5 = rbind(tmp5, tmp4)
    }
    tmp5[,`i.region_leader` := NULL]

    ## merge with non fuel price of corresponding values
    tmp2 = merge(tmp5, tmp3, by = c("time", "region_leader", all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))]))
    ## find year closest to 2010 for each region, this is the year at which is going to converge
    tmp2[, yearconv := time[which.min(abs(time - 2020))], by = c("region")]

    ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
    tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("region", "time", "region_leader")]
    tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("region", "time", "region_leader")]
    tmp2[yearconv == 2010, yearconv := 2015]
    # tmp2[yearconv == 2015, yearconv := 2020]
    ## year at which the convergence happens
    tmp2[, year_at_yearconv := year[time == yearconv], by = c("region", "region_leader", all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]
    ## TODO: this one still does not work but would be better to have a more generic one!
    tmp2[is.na(year_at_yearconv), year_at_yearconv := year[time == (yearconv+5)], by = c("region", "region_leader", all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]

    ## values of GDPcap equal to GDPcap_rich have the same values as non_fuel_prices of rich countries
    tmp2[year >= year_at_yearconv & year > 2010, sw := sw_new, by = c("region", "region_leader", all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]

    ## value of yearconv represents the convergence value
    tmp2[, sw_conv := sw_new[time==yearconv], by = c("region", "region_leader",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]
    tmp2[is.na(sw_conv), sw_conv := sw_new[time==yearconv+5], by = c("region", "region_leader",all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))])]

    ## convergence is linear until the value corresponding to 2010 is reached
    tmp2[year <= year_at_yearconv & year >= 2010, sw := sw[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(sw_conv-sw[year == 2010]), by =c(all_subsectors[seq(match(groupval, all_subsectors) - 1, match(groupval, all_subsectors))], "region")]

    ## select only useful columns
    tmp2 = tmp2[,c("region", "year", "sw", all_subsectors[seq(match(groupval, all_subsectors) - 1,length(all_subsectors), 1)]), with = F]

    ## rich countries need to be reintegrated
    if (groupval == "subsector_L3") {
      dtout = rbind(tmp2, dtin[region %in% richregions])
    } else {
      dtout = rbind(tmp2, dtin[region %in% richregions])
    }

    return(dtout)
  }



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

  SWS <- SWS[c("FV_final_SW","VS1_final_SW", "S1S2_final_SW", "S2S3_final_SW", "S3S_final_SW")]
  ## add the missing entries to all levels
  SWS = mapply(addmissingSW, SWS, calibr_demand = list(calibdem = calibdem, calibdem = calibdem, calibdem = calibdem, calibdem = calibdem, calibdem = calibdem), grouping_value = c("technology","vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3"))
  ## constant trends for all techs
  SWS <- lapply(SWS, extr_const)


  SWS$S3S_final_SW[region %in% c("EUR", "ESC", "EWN", "FRA") & subsector_L3 == "Domestic Aviation"  & year >=2015,
                   sw := 0.1*sw[year==2015], by = c("region","subsector_L3")]


  ups1 = list(VS1_final_SW = SWS$VS1_final_SW, S1S2_final_SW = SWS$S1S2_final_SW)
  ups2 = list(S2S3_final_SW = SWS$S2S3_final_SW, S3S_final_SW = SWS$S3S_final_SW)
  ups1 <- lapply(X=ups1, FUN=aveval, gdpcap = gdpcap)
  ups2 <- lapply(X=ups2, FUN=aveval_cluster, gdpcap = gdpcap, clusters = clusters)

  SWS_upperlevs = list(VS1_final_SW = ups1$VS1_final_SW, S1S2_final_SW = ups1$S1S2_final_SW, S2S3_final_SW = ups2$S2S3_final_SW, S3S_final_SW = ups2$S3S_final_SW)
  ## substitute the original dts with those with converging values
  SWS$VS1_final_SW = SWS_upperlevs$VS1_final_SW
  SWS$S1S2_final_SW=SWS_upperlevs$S1S2_final_SW
  SWS$S2S3_final_SW=SWS_upperlevs$S2S3_final_SW
  SWS$S3S_final_SW=SWS_upperlevs$S3S_final_SW

  SWS$FV_final_SW = melt(SWS$FV_final_SW, id.vars = c("region", "year", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector"))
  setnames(SWS$FV_final_SW, old = "variable", new = "logit_type")
  SWS$FV_final_SW = rbind(SWS$FV_final_SW, FV_inco)

  ## from now on, SWs and inconvenience costs will coexist. Names of the entries will reflect that, and the generic label "preference" is preferred
  names(SWS) = gsub(pattern = "SW", "pref", names(SWS))

  ## apply S-type trends for renewables

  ## convergence year for FCEV Buses and Trucks is more optimistic in the HydrHype case
  if (techswitch == "FCEV") {
    convsymmFCEV = 2035
    convsymmHydrogenAir = 2100
    speedFCEV = 0.1
  } else {
    convsymmFCEV = 2075
    convsymmHydrogenAir = 2150
    speedFCEV = 0.1
  }

  ## convergence base year for electric Buses and Trucks is more optimistic in the ElecEra case
  if (techswitch == "BEV") {
    convsymmBEV = 2035
    speedBEV = 0.5
    } else {
    convsymmBEV = 2075
    speedBEV = 0.1
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
  SWS$S1S2_final_pref[region %in% c("CHA") & subsector_L1 == "trn_pass_road_LDV_2W"  & year >=2010, sw := sw[year == 2010], by = c("region")]

  ## preference for buses extremely high in OAS and IND
  SWS$S2S3_final_pref[region %in% c("OAS", "IND") & subsector_L2 == "Bus"  & year >=2020 & year <= 2030,
                      sw := ifelse(year <= 2030, sw[year==2020] + (0.5*sw[year==2020]-sw[year==2020]) * (year-2020) / (2030-2020), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("OAS", "IND") & subsector_L2 == "Bus"  & year >=2030 & year <= 2050,
                      sw := ifelse(year <= 2050, sw[year==2030] + (0.3*sw[year==2030]-sw[year==2030]) * (year-2030) / (2050-2030), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("OAS", "IND") & subsector_L2 == "Bus"  & year >=2050 & year <= 2100,
                      sw := ifelse(year <= 2100, sw[year==2050] + (0.2*sw[year==2050]-sw[year==2050]) * (year-2050) / (2100-2050), sw), by = c("region","subsector_L2")]
  SWS$S2S3_final_pref[region %in% c("OAS", "IND") & subsector_L2 == "Bus"  & year >=2100,
                      sw := sw[year==2100], by = c("region","subsector_L2")]

  SWS$S2S3_final_pref[region %in% c("CHA") & subsector_L2 == "Bus"  & year == 2015,
                      sw := 0.18, by = c("region","subsector_L2")]

  SWS$S2S3_final_pref[region %in% c("CHA") & subsector_L2 == "Bus"  & year == 2020,
                      sw := 0.072, by = c("region","subsector_L2")]

  SWS$S2S3_final_pref[region %in% c("CHA") & subsector_L2 == "Bus"  & year == 2025,
                      sw := 0.013, by = c("region","subsector_L2")]

  SWS$S2S3_final_pref[region %in% c("CHA") & subsector_L2 == "Bus"  & year >= 2030,
                      sw := 0.008, by = c("region","subsector_L2")]

  SWS$S2S3_final_pref[region %in% c("CHA") & subsector_L2 == "trn_pass_road_LDV"  & year >= 2015 & year <= 2025,
                      sw := 1, by = c("region","subsector_L2")]

  ## public transport preference in European countries increases (Rail)
  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% c("EUR", "DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI") & year >= 2020,
                     sw := ifelse(year <= 2100, sw[year==2020] + (0.2*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), 0.2*sw[year==2020]),
                     by=c("region", "subsector_L3")]
  ## public transport preference in European countries increases (Buses)
  SWS$S2S3_final_pref[subsector_L2 == "Bus" & region %in% c("EUR", "DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI") & year >= 2010,
                      sw := ifelse(year <= 2020, sw[year==2010] + (0.2*sw[year==2010]-sw[year==2010]) * (year-2010) / (2020-2010), 0.2*sw[year==2010]),
                      by=c("region", "subsector_L2")]

  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% c("IND", "MEA", "CHA") & year >= 2010,
                     sw := sw[year == 2010],
                     by=c("region")]

  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% c("JPN") & year >= 2010,
                     sw :=  ifelse(year <= 2100, sw[year==2010] + (0.2*sw[year==2010]-sw[year==2010]) * (year-2010) / (2100-2010), 0.2*sw[year==2010]),
                     by=c("region", "subsector_L3")]

  SWS$S3S_final_pref[subsector_L3 == "Passenger Rail" & region %in% "REF" & year >= 2010,
                     sw := ifelse(year==2010, sw, 20*sw[year==2010]),
                     by=c("region")]

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
  ## domestic aviation grows way to much, reduce it
  SWS$S3S_final_pref[region %in% c("USA", "LAM") & subsector_L3 == "Domestic Aviation"  & year == 2015,
                   sw := 0.001, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("USA", "LAM") & subsector_L3 == "Domestic Aviation"  & year == 2020,
                   sw := 0.001, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("USA", "LAM") & subsector_L3 == "Domestic Aviation"  & year == 2025,
                   sw := 0.0005, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("USA", "LAM") & subsector_L3 == "Domestic Aviation"  & year >= 2030,
                   sw := 0.0005, by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("USA", "LAM") & subsector_L3 == "trn_pass_road"  & year >=2015,
                   sw := 1, by = c("region","subsector_L3")]

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

  SWS$S3S_final_pref[region %in% c("CAZ") & subsector_L3 == "Domestic Aviation"  & year >=2015,
                     sw := 0.5*sw[year==2015], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("NEN") & subsector_L3 == "Domestic Aviation"  & year >=2010,
                     sw := sw[year==2010], by = c("region","subsector_L3")]

  SWS$S3S_final_pref[region %in% c("NES") & subsector_L3 == "Domestic Aviation"  & year >=2010 & year < 2020,
                     sw := 0.5*sw[year==2010], by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("NES") & subsector_L3 == "Domestic Aviation"  & year >=2020 & year < 2030,
                     sw := 0.3*sw[year==2020], by = c("region","subsector_L3")]
  SWS$S3S_final_pref[region %in% c("NES") & subsector_L3 == "Domestic Aviation"  & year >=2030,
                     sw := 0.1*sw[year==2030], by = c("region","subsector_L3")]


  #
  # SWS$S3S_final_pref[region %in% c("ENC") & subsector_L3 == "Domestic Aviation"  & year >=2010,
  #                    sw := 0.9*sw[year==2010], by = c("region","subsector_L3")]


  SWS$S3S_final_pref[region %in% c("NEN") & subsector_L3 == "trn_pass_road"  & year >=2015,
                     sw := 1, by = c("region","subsector_L3")]
  SWS$S2S3_final_pref[region %in% c("NEN") & subsector_L3 == "trn_pass_road_LDV"  & year >=2015,
                     sw := 1, by = c("region","subsector_L2")]

  SWS$S3S_final_pref[region %in% c("NEN") & subsector_L3 == "Passenger Rail"  & year >=2010,
                     sw := sw[year==2010], by = c("region","subsector_L3")]

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
