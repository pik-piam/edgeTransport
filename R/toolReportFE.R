#'Report variables in relation to the vehicle fleet.
#'
#'Variables like energy intensity and capital costs are linked to the
#'construction year of a vehicle.
#'As energy intensity and capital costs change over time for new sales, the composition
#'of the fleet from vehicles of different construction years needs to be taken into account
#'to report these variables.
#'
#' @param fleetEnergyIntensity Energy intensity linked to the vehicle fleet
#' @param fleetESdemand Energy service on technology level linked to the vehicle fleet
#' @param loadFactor Persons or tons per vehicle
#' @param hybridElecShare Share of electric driving in hybrid electric vehicles
#' @param helpers list of helpers
#'
#' @returns Final energy consumed by all modes and technologies
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolReportFE <- function(fleetEnergyIntensity, fleetESdemand, loadFactor, hybridElecShare, helpers){

  fleetEnergyIntensity <- copy(fleetEnergyIntensity)[, c("variable", "unit") := NULL]
  setnames(fleetEnergyIntensity, "value", "energyIntensity")
  fleetESdemand <- copy(fleetESdemand)[, c("variable", "unit") := NULL]
  setnames(fleetESdemand, "value", "ESdemand")
  loadFactor <- copy(loadFactor)[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  fleetFEdemand <- merge(fleetESdemand, fleetEnergyIntensity, by = intersect(names(fleetESdemand), names(fleetEnergyIntensity)))
  fleetFEdemand <- merge(fleetFEdemand, loadFactor, by = intersect(names(fleetFEdemand), names(loadFactor)))
  fleetFEdemand[, value := (energyIntensity / loadFactor) * ESdemand * 1e-3][, unit := "EJ/yr"][, variable := "FE"]
  # There is no final energy type "hybrid electric". So hybrids need to be split in liquids and electricity
  fleetFEdemand <- rbind(fleetFEdemand, copy(fleetFEdemand[technology == "Hybrid electric"])[, value := value * hybridElecShare][, technology := "BEV"])
  fleetFEdemand[technology == "Hybrid electric", value := value * (1 - hybridElecShare)]
  fleetFEdemand[technology == "Hybrid electric", technology := "Liquids"]
  fleetFEdemand[technology %in% c("BEV", "Electric"), technology := "Electricity"]
  fleetFEdemand[technology == "FCEV", technology := "Hydrogen"]
  cols <- names(fleetFEdemand)
  cols <- cols[!cols %in% c("value","loadFactor", "energyIntensity", "ESdemand")]
  fleetFEdemand <- fleetFEdemand[, .(value = sum(value)), by = cols]
  fleetFEdemand[univocalName %in% c("Cycle", "Walk"), value := 0]

  return(fleetFEdemand)
}
