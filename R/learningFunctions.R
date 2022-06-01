#' Applies learning to BEV purchase cost
#'
#' @param gdx input gdx file
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param ES_demandpr the ES demand of the previous iteration
#' @param non_fuel_costs total non fuel costs
#' @param capcost4W purchase prices of 4W on which learning is applied
#' @param demand_learntmp the demand for vehicles from the previous iteration
#' @param ES_demand the ES demand of the current iteration
#'
#' @import data.table
#' @export

applylearning <- function(non_fuel_costs, capcost4W, gdx, EDGE2teESmap, demand_learntmp, ES_demandpr, ES_demand){
  `.` <- ratio <- demandpr <- vehicles_number <- cumul <- technology <- subsector_L1 <- non_fuel_price <- b <- initialyear <- NULL
  Capital_costs_purchase <- totalNE_cost <- price_component <- NULL
  ## find the estimated number of cars
  demand = merge(ES_demand, ES_demandpr)
  demand = demand[, ratio := demand/demandpr][,-c("demand", "demandpr")] ## ratio from previous iteration of total demand
  demand = merge(demand, demand_learntmp, all.x =TRUE, by = c("region", "year"))
  demand[, vehicles_number := vehicles_number*ratio]

  ## find the cumulative capacity and the reduction factor of costs
  demand = demand[,.(vehicles_number = sum(vehicles_number)), by = c("technology", "year")]
  demand = demand[!is.nan(vehicles_number) & !is.na(vehicles_number)]
  demand[, cumul := cumsum(vehicles_number), by = "technology"]
  initialcap = demand[cumul!=0 & !is.na(demand$cumul),]
  initialcap = initialcap[ , .SD[which.min(cumul)], by = "technology"]
  initialcap = initialcap[,.(technology, initialcap = cumul, initialyear = year)]
  demand = merge(demand, initialcap, by = c("technology"))
  demand[, cumul := cumul/initialcap, by = "technology"]
  demand[, cumul := ifelse(year>2100, cumul[year == 2100], cumul), by = "technology"]
  demand[, cumul := ifelse(year<=initialyear, cumul[year == initialyear], cumul), by = "technology"]

  exponent = data.table(b = c(-2.5, -2.5), technology =  c("BEV", "FCEV")) ## according to https://about.bnef.com/blog/behind-scenes-take-lithium-ion-battery-prices/  LR ~ 18% -> b=ln(LR)/ln(2)~-2.5
                  ## same for https://www.sciencedirect.com/science/article/pii/S0959652618337211 (LR = 23%)
  ## for FCEVs: https://www.sciencedirect.com/science/article/abs/pii/S0360544218302998

  demand = merge(exponent, demand, by = "technology")
  demand[, factor := cumul^b]
  demand[, b := NULL]
  demand[, factor := ifelse(is.infinite(factor), 1, factor)] ## factor is 1 if there is no increase in cumulated demand (->no decrease in costs)
  ## only BEV car costs are affected
  capcost4W = capcost4W[technology %in% c("BEV", "FCEV") & subsector_L1 == "trn_pass_road_LDV_4W",]
  capcost4W = merge(capcost4W, unique(non_fuel_costs[subsector_L1 == "trn_pass_road_LDV_4W", c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)
  ## substract purchase price to the non fuel price to obtain the non fuel price not affected by learning
  nonfuel_costslearn = rbind(non_fuel_costs[subsector_L1 == "trn_pass_road_LDV_4W" & technology %in% c("BEV", "FCEV") & year >= 2020][, price_component := "totalNE_cost"], capcost4W[year >= 2020])
  nonfuel_costslearn = dcast(nonfuel_costslearn, region + year + technology + vehicle_type + subsector_L1 + subsector_L2 + subsector_L3 + sector + type ~ price_component, value.var = "non_fuel_price")
  nonfuel_costslearn[, non_fuel_price := totalNE_cost - Capital_costs_purchase]
  nonfuel_costslearn[, c("Capital_costs_purchase", "price_component", "totalNE_cost") := list(NULL, "remainingprice", NULL)]

  ## capcost for 4wheelers of 2020 is to be applied to all time steps
  capcost4W[year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("region", "vehicle_type", "technology")]

  capcost4W = merge(demand, capcost4W, all.x = TRUE, by = c("year", "technology"))
  ## powertrain represents ~20% of the total purchase price
  batterycomponent = 0.2 ##
  ## powertrain represents ??% of the average purchase price
  fuelcellcomponent = 0.4 ## average https://www.energy.gov/sites/prod/files/2014/03/f9/fcev_status_prospects_july2013.pdf
  capcost4W[year >= 2020 & technology == "BEV", non_fuel_price := ifelse(!is.na(factor), factor*batterycomponent*non_fuel_price + (1-batterycomponent)*non_fuel_price, non_fuel_price)]
  capcost4W[year >= 2020 & technology == "FCEV", non_fuel_price := ifelse(!is.na(factor), factor*fuelcellcomponent*non_fuel_price + (1-fuelcellcomponent)*non_fuel_price, non_fuel_price)]
  capcost4W = capcost4W[year >= 2020]
  capcost4W[,c("factor", "cumul", "vehicles_number", "initialcap", "initialyear"):= NULL]
  ## merge with other components of non fuel price and calculate total non fuel price
  nonfuel_costslearn = rbind(nonfuel_costslearn, capcost4W)
  nonfuel_costslearn = nonfuel_costslearn[,.(non_fuel_price = sum(non_fuel_price)), by = c("region", "year", "type", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector")]

  ## technologies subjected to learning
  techlearn = c("BEV", "FCEV")
  ## integrate non fuel costs from the input data  with the updated values from learning
  nonfuel_costs = nonfuel_costs[!(technology %in% techlearn & subsector_L1 =="trn_pass_road_LDV_4W" & year >=2020),]
  nonfuel_costs = rbind(nonfuel_costs, nonfuel_costslearn[technology %in% techlearn])

  nonfuel_costs = list(capcost4W = capcost4W, nonfuel_costs = nonfuel_costs)
  return(nonfuel_costs)
}

#' Calculate number of vehicles scaling up the normalized demand with the aggregate total demand
#'
#' @param norm_dem normalized demand shares
#' @param intensity energy intensity of CES nodes, in million pkm/EJ
#' @param ES_demand_all total demand for ESs
#' @param loadFactor load factor of vehicles
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param rep if is it a stand alone run or the last iteration of the coupled run->set to TRUE
#' @import data.table
#' @export



calc_num_vehicles_stations <- function(norm_dem, intensity, ES_demand_all, loadFactor, EDGE2teESmap, rep){
  demand_F <- demand <- annual_mileage <- region <- `.` <- vehicles_number <- vehicle_type <- demand_F <- technology <- statnum <- fracst <- NULL
  cost_st_km <- cost <- en_int <- value <- teEs <- NULL
  if (!rep) {
    norm_dem = merge(norm_dem, ES_demand_all, by = c("region", "year", "sector"))
    ## scale the normalized demand
    norm_dem[, demand_F := demand_F*   ## normalized to 1
                           demand]     ## in million km (total passenger demand)
  }
  LDVdem = merge(norm_dem, loadFactor, all.x = TRUE, by = c("region", "year", "vehicle_type"))

  LDVdem[, annual_mileage := 13000]

  LDVdem[, vehicles_number := demand_F             ## in millionpkm
                              /loadFactor          ## in millionvkm
                              /annual_mileage]     ## in million veh

  LDVdem = LDVdem[, .(region, year, vehicles_number, technology, vehicle_type, demand_F)]

  alltechdem = LDVdem[,.(vehicles_number = sum(vehicles_number)), by = c("region", "year")]

  learntechdem = LDVdem[technology %in% c("BEV", "FCEV"),][, .(region, year, vehicles_number, vehicle_type, technology)]

  ## calculate the cost of refueling/recharging infrastructure
  intensity = merge(intensity, EDGE2teESmap, all.x=TRUE, by = "CES_node")

  ## costs H2 https://www.osti.gov/pages/servlets/purl/1393842 ->4-6 $/kg
  ## https://www.researchgate.net/publication/318056062_Impact_of_hydrogen_refueling_configurations_and_market_parameters_on_the_refueling_cost_of_hydrogen ->6-15 $kg
  ## costs NG, Liquids -> assumed half of the hydrogen, as no compressor and refrigeration are required
  ## energy content
  ## 120 MJ/kg = 120*1e-12EJ/kg Hydrogen
  ## 44 MJ/kg = 44*1e-12EJ/kg Liquids
  ## 50 MJ/kg = 50*1e-12EJ/kg NG

  charact_stations = data.table(teEs = c("te_eselt_pass_sm", "te_esgat_pass_sm", "te_espet_pass_sm", "te_esh2t_pass_sm"),
                                cost = c(NA, 4, 4, 8),                                ## costs associated to each station (dollars per kg for H2, NG and Liquids; dollars per MWh for BEVs)
                                en_int = c(3.6e-9, 50*1e-12, 44*1e-12, 120*1e-12))      ## energy intensity in EJ/kg (for H2, Liquids and NG and liquids), EJ/MWh for BEVs

  ## calculation of recharging points costs
  ## data for purchase/installation costs from: https://theicct.org/sites/default/files/publications/ICCT_EV_Charging_Cost_20190813.pdf
  ##                                            https://afdc.energy.gov/files/u/publication/evse_cost_report_2015.pdf
  discount_rate_veh = 0.05   ## Consumer discount rate for recharger purchase (assumed)
  nper_amort_veh = 5         ## Number of periods (years) over which vehicle capital payments are amortized (assumed)
  fcr_veh = discount_rate_veh + discount_rate_veh/(((1+discount_rate_veh)^nper_amort_veh)-1)

  purchase_cost = 45000 ## dollars/charger (~25k$ purchase, ~20k$installation)
  cap = 50 ## kW/charger
  util_rate = 0.05 ## utilization rate of the charger (Benchmarking Charging Infrastructure Utilization, Wolbertus, 2016)

  cost_ch = purchase_cost*fcr_veh/  ## in $/charger (levelized)
            cap/                    ## in $/kW
            (365*24*util_rate)/     ## in $/kWh
            1000                    ## in $/MWh

  charact_stations[teEs == "te_eselt_pass_sm", cost := cost_ch]

  ## merge costs with number of stations
  stations = merge(intensity, charact_stations, by = "teEs")

  stations[, cost_st_km := cost/       ## in dollars/kg (or dollars/MWh)
                           en_int/     ## in dollars/EJ
                           value*      ## in dollars/million pkm
                           1e-6]       ## in dollars/km

  stations = stations[,.(region, year, cost_st_km, teEs)]
  return(list(learntechdem = learntechdem, stations = stations, alltechdem = alltechdem))
}
