

toolReportREMINDinputData <- function(fleetESdemand, fleetFEdemand, fleetEnergyIntensity, fleetCapCosts, helpers) {

  # Input data for transport module GAMS code
  # See needed inputs in REMIND/modules/35_transport/edge_esm/datainput.gms

  # Unit conversion
  EJtoTwa <- 31.71e-03

  # Capital costs for the transport system [2005US$/pkm or 2005US$/tkm]
  # p35_esCapCost(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)
  p35_esCapCost <- copy(fleetCapCosts)
  fleetCapCosts <- merge(fleetCapCosts, unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")]))
  fleetCapCosts <- fleetCapCosts[, .(value = sum(value)), by = c("region", "period", "all_teEs")]

  # Aggregate energy efficiency of transport fuel technologies [trn pkm/Twa or trn tkm/Twa]
  # p35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)
  p35_fe2es <- copy(fleetEnergyIntensity)
  # convert to TWa/trn (pkm|tkm)
  fleetEnergyIntensity[, value := value * EJtoTwa * 1e03]
  # convert to trn (pkm|tkm)/TWa
  fleetEnergyIntensity[, value := 1 / value]
  fleetEnergyIntensity <- merge(fleetEnergyIntensity, unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")]))
  # aggregate with fleet ES demand as weight
  fleetESdemand <- copy(fleetESdemand)
  setnames(fleetESdemand, "value", "ESdemand")
  fleetEnergyIntensity <- merge(fleetEnergyIntensity, fleetESdemand, by = intersect(names(fleetEnergyIntensity), names(fleetESdemand)))
  fleetEnergyIntensity[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  fleetEnergyIntensity <- fleetEnergyIntensity[, .(value = sum(value * ESdemand / sumESdemand)),  by = c("region", "period", "all_teEs")]

  # Aggregate FE Demand per transport fuel technology [TWa]
  # p35_demByTech(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_enty, all_in, all_teEs)
  # convert to TWa
  p35_demByTech <- copy(fleetFEdemand)
  fleetFEdemand[, value := value * EJtoTwa]
  fleetFEdemand <- merge(fleetFEdemand, unique(helpers$mapEdgeToREMIND[, c("all_enty", "all_in", "all_teEs", "univocalName", "technology")]))
  fleetFEdemand <- fleetFEdemand[, .(value = sum(value)), by = c("region", "period", "all_teEs")]

  # Input data for edgeTransport iterative that is coupled to REMIND


}
