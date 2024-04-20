

toolReportREMINDinputDataVarSet <- function(fleetESdemand, fleetFEdemand, fleetEnergyIntensity, fleetCapCosts,
                                      combinedCAPEXandOPEX, scenSpecPrefTrends, scenSpecEnIntensity, initialIncoCosts,
                                      annualMileage, timeValueCosts, demScen, SSPscen, transportPolScen, helpers) {


  prepareForREMIND <- function(dt, demScen, SSPscen, transportPolScen) {
    cols <- names(copy(dt))
    cols <- cols[!cols %in% c("region", "period", "value")]
    dt[, DEM_scenario := paste0("gdp_", demScen)]
    dt[, GDP_scenario := paste0("gdp_", SSPscen)]
    dt[, EDGE_scenario := transportPolScen]
    setcolorder(dt, c("region", "period", "GDP_scenario", "EDGE_scenario", "DEM_scenario", cols, "value"))
    return(dt)
  }

  #############################################################
  ## Input data for transport module GAMS code
  #############################################################
  # See needed inputs in REMIND/modules/35_transport/edge_esm/datainput.gms
  # Unit conversion
  EJtoTwa <- 31.71e-03

  # Capital costs for the transport system [2005US$/pkm or 2005US$/tkm]
  # p35_esCapCost(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)
  p35_esCapCost <- copy(fleetCapCosts)
  p35_esCapCost <- merge(p35_esCapCost, unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")]),
                         by = c("univocalName", "technology"))

  p35_esCapCost <- p35_esCapCost[, .(value = sum(value)), by = c("region", "period", "all_teEs")]

  # Aggregate energy efficiency of transport fuel technologies [trn pkm/Twa or trn tkm/Twa]
  # p35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)
  p35_fe2es <- copy(fleetEnergyIntensity)
  # convert to TWa/trn (pkm|tkm)
  p35_fe2es[, value := value * EJtoTwa * 1e03]
  # convert to trn (pkm|tkm)/TWa
  p35_fe2es[, value := 1 / value]
  p35_fe2es <- merge(p35_fe2es, unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")]),
                     by = c("univocalName", "technology"))
  # aggregate with fleet ES demand as weight
  fleetESdemand <- copy(fleetESdemand)
  setnames(fleetESdemand, "value", "ESdemand")
  fleetESdemand[, c("unit", "variable") := NULL]
  p35_fe2es <- merge(p35_fe2es, fleetESdemand, by = intersect(names(p35_fe2es), names(fleetESdemand)))
  p35_fe2es[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  p35_fe2es <- p35_fe2es[, .(value = sum(value * ESdemand / sumES)),  by = c("region", "period", "all_teEs")]

  # Aggregate FE Demand per transport fuel technology [TWa]
  # p35_demByTech(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_enty, all_in, all_teEs)
  # convert to TWa
  p35_demByTech <- copy(fleetFEdemand)
  p35_demByTech[, value := value * EJtoTwa]
  p35_demByTech <- merge(p35_demByTech, unique(helpers$mapEdgeToREMIND[, c("all_enty", "all_in", "all_teEs", "univocalName", "technology")]),
                         by = c("univocalName", "technology"))
  p35_demByTech <- p35_demByTech[, .(value = sum(value)), by = c("region", "period", "all_teEs")]

  # Input data for edgeTransport iterative that is coupled to REMIND--------------------------------------------
  ####################################################################
  ## Input data for edgeTransport iterative that is coupled to REMIND
  ####################################################################
  # -> can be supplied as it is
  # CAPEXandNonFuelOPEX
  # Fuel costs are added from the fulldata.gdx of the last REMIND iteration in the iterative script
  CAPEXandNonFuelOPEX <- copy(combinedCAPEXandOPEX)
  CAPEXandNonFuelOPEX <- CAPEXandNonFuelOPEX[!variable == "Fuel costs"]
  # scenSpecPrefTrends
  # scenSpecLoadFactor
  # scenSpecEnIntensity
  # initialIncoCosts
  # annualMileage
  # timeValueCosts

  output <- list(
    p35_esCapCost = p35_esCapCost,
    p35_fe2es = p35_fe2es,
    p35_demByTech = p35_demByTech,
    CAPEXandNonFuelOPEX = CAPEXandNonFuelOPEX,
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecEnIntensity = scenSpecEnIntensity,
    initialIncoCosts = initialIncoCosts,
    annualMileage = annualMileage,
    timeValueCosts = timeValueCosts
  )

  output <- lapply(output, prepareForREMIND, demScen, SSPscen, transportPolScen)

  return(output)
}
