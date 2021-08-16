lvl0_convergence = function(GCAM_data, merged_data, GDP_MER_country, POP_country){
  speed = copy(GCAM_data[["speed"]])
  load_factor = copy(merged_data$LF)

  ## speed converges
  GDP_POP = merge(GDP_MER_country, POP_country, by = c("iso", "year"))

  GDP_POP_cap = GDP_POP[,GDP_cap := weight/value]

  tmp = merge(speed, GDP_POP_cap, by = c("iso", "year"))
  ## define rich regions
  richregions = unique(unique(tmp[year == 2010 & GDP_cap > 25000, iso]))
  ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
  richave = tmp[iso %in% richregions & speed > 0,]
  richave = richave[, .(speed = sum(speed*weight)/sum(weight)), by = c("tranSubsector", "supplysector", "year")]
  GDP_POP = GDP_POP_cap[iso %in% richregions,]
  GDP_POP = GDP_POP[, .(GDP = sum(weight), POP_val = sum(value)), by = c("year")]
  richave = merge(richave, GDP_POP, by = "year")
  ## average gdp per capita of the rich countries
  richave[, GDP_cap := GDP/POP_val]
  ## dt on which the GDPcap is checked
  tmp1 = tmp[!iso %in% richregions, c("iso", "year",
                                      "GDP_cap", "supplysector", "tranSubsector",
                                      "speed")]
  ## dt contaning the gdp towards which to converge
  tmp2 = richave[, c("year", "GDP_cap")]
  ## dt containing the non fuel price for rich countries
  tmp3 = richave[, c("year", "supplysector", "tranSubsector")]
  ## names has to be different across dts for roll join
  setnames(tmp2, old = c("year"), new = c("time"))
  setnames(tmp3, old = c("year"), new = c("time"))

  setkey(tmp1,GDP_cap)
  setkey(tmp2,GDP_cap)
  ## find the time step at which the GDPcap matches the GDPcap of the rich countries
  tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

  ## merge with non fuel price of corresponding values
  tmp2 = merge(tmp2, tmp3, by = c("time", "supplysector", "tranSubsector"))

  ## find year closest to 2010 for each region, this is the year at which is going to converge
  tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("iso")]

  ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
  tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("iso", "time")]
  tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("iso", "time")]
  ## year at which the convergence happens
  tmp2[, year_at_yearconv := year[time == yearconv], by = c("iso","supplysector", "tranSubsector")]

  ## value of speed after the convergence
  tmp3 = richave[, c("year", "speed", "supplysector", "tranSubsector")]
  setnames(tmp3, old = c("speed", "year"), new = c("speed_trend", "year_at_yearconv"))
  tmp2 = merge(tmp2,tmp3,by=c("year_at_yearconv", "supplysector", "tranSubsector"), all.x=TRUE)

  ## after the year of convergence, the values are the "average" developed countries values
  tmp2[year >= year_at_yearconv & year > 2010, speed := speed_trend, by = c("iso","supplysector", "tranSubsector")]

  ## value of yearconv represents the convergence value
  tmp2[, speed_conv := speed_trend[time==yearconv], by = c("iso","supplysector", "tranSubsector")]
  ## convergence is linear until the value corresponding to 2010 is reached
  tmp2[year <= year_at_yearconv & year >= 2010, speed := speed[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(speed_conv-speed[year == 2010]), by =c("supplysector", "tranSubsector", "iso")]
  tmp2[is.na(speed), speed := speed_trend]
  ## select only useful columns
  tmp2 = tmp2[,.(iso, year, speed, supplysector, tranSubsector)]
  ## rich countries need to be reintegrated
  speed = rbind(tmp2, speed[iso %in% richregions])

  ## load factor convergence
  tmp = merge(merged_data$LF, GDP_POP_cap, by = c("iso", "year"))
  ## define rich regions
  richregions = unique(unique(tmp[year == 2010 & GDP_cap > 25000, iso]))
  ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
  richave = tmp[iso %in% richregions & loadFactor > 0,]
  richave = richave[, .(loadFactor = sum(loadFactor*weight)/sum(weight)), by = c("vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "year")]
  GDP_POP = GDP_POP_cap[iso %in% richregions,]
  GDP_POP = GDP_POP[, .(GDP = sum(weight), POP_val = sum(value)), by = c("year")]
  richave = merge(richave, GDP_POP, by = "year")
  ## average gdp per capita of the rich countries
  richave[, GDP_cap := GDP/POP_val]
  ## dt on which the GDPcap is checked
  tmp1 = tmp[!iso %in% richregions, c("iso", "year",
                                      "GDP_cap", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1",
                                      "loadFactor")]
  ## dt contaning the gdp towards which to converge
  tmp2 = richave[, c("year", "GDP_cap")]
  ## dt containing the non fuel price for rich countries
  tmp3 = richave[, c("year", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]
  ## names has to be different across dts for roll join
  setnames(tmp2, old = c("year"), new = c("time"))
  setnames(tmp3, old = c("year"), new = c("time"))

  setkey(tmp1,GDP_cap)
  setkey(tmp2,GDP_cap)
  ## find the time step at which the GDPcap matches the GDPcap of the rich countries
  tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

  ## merge with non fuel price of corresponding values
  tmp2 = merge(tmp2, tmp3, by = c("time", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1"))

  ## find year closest to 2010 for each region, this is the year at which is going to converge
  tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("iso")]

  ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
  tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("iso", "time")]
  tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("iso", "time")]
  ## year at which the convergence happens
  tmp2[, year_at_yearconv := year[time == yearconv], by = c("iso","vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]

  ## value of speed after the convergence
  tmp3 = richave[, c("year", "loadFactor", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]
  setnames(tmp3, old = c("loadFactor", "year"), new = c("loadFactor_trend", "year_at_yearconv"))
  tmp2 = merge(tmp2,tmp3,by=c("year_at_yearconv", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1"))

  ## after the year of convergence, the values are the "average" developed countries values
  tmp2[year >= year_at_yearconv & year > 2010, loadFactor := loadFactor_trend, by = c("iso","vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]

  ## value of yearconv represents the convergence value
  tmp2[, loadFactor_conv := loadFactor_trend[time==yearconv], by = c("iso","vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]
  ## convergence is linear until the value corresponding to 2010 is reached
  tmp2[year <= year_at_yearconv & year >= 2010, loadFactor := loadFactor[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(loadFactor_conv-loadFactor[year == 2010]), by =c("vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "iso")]
  tmp2[is.na(loadFactor), loadFactor := loadFactor_trend]
  ## select only useful columns
  tmp2 = tmp2[,.(iso, year, loadFactor, vehicle_type, technology, sector, subsector_L3, subsector_L2, subsector_L1)]
  ## rich countries need to be reintegrated (only for non 4W -> as the LF for 4W has been previously defined)
  load_factor = rbind(tmp2[subsector_L1 != "trn_pass_road_LDV_4W"],
                      load_factor[(iso %in% richregions)|(subsector_L1 == "trn_pass_road_LDV_4W")])


  return(list(speed = speed, LF = load_factor))
}


lvl0_REMINDdat = function(merged_data, conv_data, VOT_lambdas, REMIND2ISO_MAPPING, GDP_country){
  gdp = copy(GDP_country)

  ## LF
  LF = copy(conv_data$LF)
  LF = aggregate_dt(data = LF,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "loadFactor",
                    datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                    weights = gdp)

  ## AM
  AM = copy(merged_data$AM)
  AM = aggregate_dt(data = AM,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "vkm.veh",
                    datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                    weights = gdp)

  ## intensity
  int = copy(merged_data$int)
  int = aggregate_dt(data = int,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "conv_pkm_MJ",
                    datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                    weights = gdp)
  int[, sector_fuel := "refined liquids enduse"]
  int[technology == "FCEV", sector_fuel := "H2 enduse"]
  int[technology == "NG", sector_fuel := "delivered gas"]
  int[technology == "Hybrid Electric", sector_fuel := "Liquids-Electricity"]
  int[technology %in% c("BEV", "LA-BEV", "Electric"), sector_fuel := "elect_td_trn"]

  ## costs
  costs = copy(merged_data$costs)
  setnames(costs, old = "variable", new = "var")
  costs = aggregate_dt(data = costs,
                     mapping = REMIND2ISO_MAPPING,
                     valuecol = "value",
                     datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "var"),
                     weights = gdp[year%in%unique(costs$year)])
  setnames(costs, old = "var", new = "variable")


  ## demand
  dem = copy(merged_data$dem)
  dem = aggregate_dt(data = dem,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "tech_output",
                       datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                       weights = NULL)


  NFcost = costs[,.(non_fuel_price = sum(value)), by = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "year", "region")]


  return(list(LF = LF, AM = AM, int = int, costs = costs, NFcost = NFcost, dem = dem))

}

