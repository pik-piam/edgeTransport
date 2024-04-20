#'Report variables in relation to the vehicle fleet.
#'
#'Variables like energy intensity and capital costs are linked to the
#'construction year of a vehicle.
#'As energy intensity and capital costs change over time for new sales, the composition
#'of the fleet from vehicles of different construction years needs to be taken into account
#'to report these variables.
#'
#' @param salesData
#' @param vehiclesConstrYears
#' @param helpers
#'
#' @returns
#' @author Johanna Hoppe
#' @import data.table
#' @export

reportExtendedVarSet <- function(data, timeResReporting, reportTransportData = TRUE, reportExtendedTransportData = FALSE, reportAnalytics = FALSE){

  # Calculate useful energy
  fleetUEdemand <- toolCalculateUE(fleetFEdemand, data$helpers)

  # Calculate vintages (stock without sales)
  vintages <- copy(data$fleetSizeAndComposition$fleetVehNumbersConstrYears[!period == constrYear])
  vintages[, variable := "Vintages"][, constrYear := NULL]
  cols <- names(vintages)
  vintages <- vintages[, .(value = sum(value)), by = eval(cols[!cols %in% c("value", "constrYear")])]
  vintages <- approx_dt(vintages, timeResReporting, "period", "value", extrapolate = TRUE)
  loadFactor <- merge(data$loadFactor, data$helpers$decisionTree, by = intersect(names(data$loadFactor), names(data$helpers$decisionTree)))

  extendedExt <- list(fleetUEdemand = fleetUEdemand,
                      vintages = vintages
  )

  extendedInt <- list(loadFactor = loadFactor,
                      operatingCostNonFuel = fleetCost[variable == "Operating costs (total non-fuel)"],
                      fuelCost = fleetCost[variable == "Fuel costs"],
                      FEsplitShares = mixedCarrierSplit$splitShares
  )

  outputVarsExt <- append(outputVarsExt, extendedExt)
  outputVarsInt <- append(outputVarsInt, extendedInt)

outputVars <- list(ext = outputVarsExt,
                   int = outputVarsInt)
}
