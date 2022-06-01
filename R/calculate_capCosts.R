#' Evaluate the costs of REMIND technology placeholders (e.g. te_eselt_pass_sm).
#'
#' @param base_price base prices from logit calculations
#' @param Fdemand_ES normalized ES demand
#' @param stations costs of stations in $/km
#' @param EDGE2CESmap map from top level EDGE-T/GCAM categories to REMIND CES nodes
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param REMINDyears range of REMIND timesteps
#' @param scenario EDGE-T scenario name
#' @import data.table
#' @importFrom rmndt aggregate_dt approx_dt
#' @export

calculate_capCosts <-function(base_price, Fdemand_ES, stations,
                              EDGE2CESmap,
                              EDGE2teESmap,
                              REMINDyears,
                              scenario){

  teEs <- region <- variable <- value <- demand_F <- `.` <- subsector_L3 <- vehicle_type <- NULL
  vehicles_number <- annual_mileage <- load_factor <- demand <- technology <- cost_st_km <- NULL
  subsector_L2 <- NULL

  ## the non fuel price is to be calculated only for motorized entries
  Fdemand_ES=Fdemand_ES[!subsector_L3 %in% c("Walk","Cycle"),]
  base_price=base_price[!subsector_L3 %in% c("Walk","Cycle"),]

  ## remove O&M costs
  ## cars (from the UCD data)
  ## CAPEX share for liquids is approx. 40%
  base_price[subsector_L2 == "trn_pass_road_LDV" & technology %in% c("Liquids", "NG"), non_fuel_price := non_fuel_price*0.4]
  ## CAPEX for el. cars is 70%
  base_price[subsector_L2 == "trn_pass_road_LDV" & technology %in% c("BEV", "FCEV", "Hybrid Electric"), non_fuel_price := non_fuel_price*0.7]

  ## trucks
  ## https://theicct.org/sites/default/files/publications/TCO-BETs-Europe-white-paper-v4-nov21.pdf
  ## p. 11: retail price = 150k for diesel, 500 - 200k for BEV
  ## p. 22: TCO 550 for diesel, TCO = 850 - 500k for BEV
  ## CAPEX share diesel = 27%, 60-40% for BEV -> 50%
  base_price[subsector_L3 == "trn_freight_road" & technology %in% c("Liquids", "NG"), non_fuel_price := non_fuel_price*0.3]
  base_price[subsector_L3 == "trn_freight_road" & technology %in% c("Electric", "FCEV"), non_fuel_price := non_fuel_price*0.5]

  ## trains
  ## https://www.unescap.org/sites/default/files/1.%20Part%20A.%20Point%20to%20point%20railway%20traffic%20costing%20model.pdf
  ## O&M 80% for low traffic lines
  ## 50% for high traffic lines
  ## -> 60% O&M -> CAPEX share = 40%
  base_price[subsector_L3 %in% c("Freight Rail", "Passenger Rail", "HSR"), non_fuel_price := non_fuel_price*0.4]

  ## busses
  ## https://mdpi-res.com/d_attachment/wevj/wevj-11-00056/article_deploy/wevj-11-00056.pdf?version=1597829235
  ## for electric busses: veh + batt. = 25% of TCO
  base_price[subsector_L2 == "Bus" & technology %in% c("Electric", "FCEV"), non_fuel_price := non_fuel_price*0.25]
  ## -> diesel busses: 15% of TCO
  base_price[subsector_L2 == "Bus" & technology %in% c("Liquids", "NG"), non_fuel_price := non_fuel_price*0.15]

  ## planes
  ## domestic (from UCD data)
  ## CAPEX share is approx 40%
  base_price[subsector_L3 == "Domestic Aviation", non_fuel_price := non_fuel_price*0.4]

  ## intl. (from UCD data)
  ## capex share is 50%
  base_price[subsector_L3 == "International Aviation", non_fuel_price := non_fuel_price*0.5]

  ## ships
  ## CCS ships doi:10.1016/j.egypro.2014.11.285
  ## CAPEX ~ 30%
  base_price[subsector_L3 %in% c("Domestic Ship", "International Ship"), non_fuel_price := non_fuel_price*0.3]

  ## merge prices and demand
  ## TODO at the moment, Hybrid Electric veh cannot be included in this calculation because they have 2 fuels (elec, liq) and cannot be mapped to one
  ## fuel only. This has to be fixed.
  data=merge(base_price,Fdemand_ES[technology != "Hybrid Electric"],all.y=TRUE,by=intersect(names(base_price),names(Fdemand_ES)))

  ## merge with mappings
  data=merge(data,EDGE2CESmap,all.x=TRUE,by=intersect(names(data),names(EDGE2CESmap)))
  data=merge(data,EDGE2teESmap,all=TRUE,by=intersect(names(data),names(EDGE2teESmap)))
  ## summarise and find the average prices
  data=data[,.(non_fuel_price=sum(non_fuel_price*demand_F/sum(demand_F))), by=c("region","year","teEs")]

  ## merge with the stations costs
  data = merge(data, stations, all = TRUE, by = c("teEs", "region", "year"))
  data[is.na(cost_st_km), cost_st_km := 0]
  ## temporarily set to 0 the station costs
  data[, cost_st_km := NULL]

  non_fuel_price = melt(data, id.vars = c("region", "year", "teEs"),
                            measure.vars = c("non_fuel_price"))

  setcolorder(non_fuel_price, c("region","year","teEs","variable","value"))

  #rows with NaNs are deleted (meaning: no price, and no demand, in the specific region/year)
  non_fuel_price=non_fuel_price[!is.nan(value),]

  non_fuel_price = approx_dt(non_fuel_price, REMINDyears,
                     xcol = "year", ycol = "value",
                     idxcols = c("region", "teEs", "variable"),
                     extrapolate=T)

  non_fuel_price=non_fuel_price[variable=="non_fuel_price",]
  non_fuel_price[,variable:=NULL]
  non_fuel_price=non_fuel_price[order(region,year,teEs)]

  return(non_fuel_price)
}
