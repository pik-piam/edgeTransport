#'Report variables in relation to the vehicle fleet.
#'
#'Variables like energy intensity and capital costs are linked to the
#'construction year of a vehicle.
#'As energy intensity and capital costs change over time for new sales, the composition
#'of the fleet from vehicles of different construction years needs to be taken into account
#'to report these variables.
#'
#' @param salesData variables linked to new sales in the respective years
#' @param vehiclesConstrYears vehicle numbers and their construction years for the respective years
#' @param helpers list of helpers
#'
#' @returns
#' @author Johanna Hoppe
#' @import data.table
#' @export


toolReportFleetVariables <- function(salesData, vehiclesConstrYears, helpers) {

  # Calculate vehicle shares of different construction years for each period---------------------
  vehiclesConstrYears<- copy(vehiclesConstrYears)
  cols <- names(vehiclesConstrYears)
  vehiclesConstrYears <- vehiclesConstrYears[, sum := sum(value),
                                             by = eval(cols[!cols %in% c("constrYear", "variable", "value")])]
  vehiclesConstrYears <- vehiclesConstrYears[, share := value / sum,
                                             by = eval(cols[!cols %in% c("value")])]
  # No weights needed to aggregate over construction years when there are no vehicles
  # (e.g. the energy intensity for BEVs on fleet level should be kept, even if there is no demand)
  vehiclesConstrYears[is.nan(share) & period == constrYear, share := 1]
  vehiclesConstrYears[is.nan(share) & !period == constrYear, share := 0]
  vehiclesConstrYears[, c("value", "sum", "unit", "variable") := NULL]

  # Merge with the data linked to new sales -----------------------------------------------------
  # The period of the sales data now needs to be matched with the construction years.
  # The value of the variables linked to the vehicle fleet is now calculated based on the shares of the
  # different construction years
  salesData <- merge(salesData, helpers$decisionTree,
                     by = c(intersect(names(salesData), names(helpers$decisionTree))))
  salesDataTrackedVeh <- salesData[grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)]
  #Interpolate the missing timesteps between 1990 and 2005
  salesDataTrackedVeh <- approx_dt(salesDataTrackedVeh, unique(vehiclesConstrYears$constrYear),
                                   "period", "value", extrapolate = TRUE)
  setnames(salesDataTrackedVeh, "period", "constrYear")
  salesDataTrackedVeh <- merge(salesDataTrackedVeh, vehiclesConstrYears,
                               by = intersect(names(salesDataTrackedVeh), names(vehiclesConstrYears)),
                               all.x = TRUE, allow.cartesian = TRUE)
  cols <- names(salesDataTrackedVeh)
  fleetData <- salesDataTrackedVeh[, .(value = sum(value * share)),
                                   by = eval(cols[!cols %in% c("share", "constrYear", "value")])]
  #Timesteps after 2100 need to be interpolated
  fleetData <- approx_dt(fleetData, unique(salesData$period), "period", "value", extrapolate = TRUE)
  fleetData <- rbind(fleetData, salesData[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)])
  fleetData[, variable := gsub(" sales", "", variable)]

  return(fleetData)
}
