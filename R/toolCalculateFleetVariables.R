
toolCalculateFleetVariables <- function(salesData, vehiclesConstrYears, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  value <- share <- . <- subsectorL3 <- NULL

  salesData <- merge(salesData, helpers$decisionTree, by = c(intersect(names(salesData), names(helpers$decisionTree))))
  vehiclesConstrYears <- vehiclesConstrYears[, sum := sum(value), by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]
  vehiclesConstrYears <- vehiclesConstrYears[, share := value / sum, by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "constrYear")]
  # No weights needed to aggregate over construction years when there are no vehicles (e.g. the energy intensity for BEVs on fleet level should be kept, even if there is no demand)
  vehiclesConstrYears[is.nan(share), share := 1]
  vehiclesConstrYears[, c("value", "sum", "unit", "variable") := NULL]
  setnames(salesData, "period", "constrYear")
  salesDataTrackedVeh <- merge(salesData, vehiclesConstrYears, by = intersect(names(salesData), names(vehiclesConstrYears)))
  fleetData <- salesDataTrackedVeh[, .(value = sum(value * share)), by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "unit", "variable")]
  fleetData <- rbind(fleetData, salesData[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)])
  fleetData <- toolOrderandCheck(fleetData, helpers$decisionTree)

  return(fleetData)
}
