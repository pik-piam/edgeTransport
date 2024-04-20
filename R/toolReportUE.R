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


toolReportUE <- function(FEdemand, helpers){
  browser()
  # Note that this is a really rough assumptions (as the aircarft burning hydrogen is getting the same efficiency than
  # a fuel cell electric truck)
  MappUE <- data.table(
    technology = c("Electricity", "Hydrogen", "Liquids", "Gases"),
    UEefficiency = c(0.64, 0.25, 0.23, 0.23))
  
  UEdemand <- copy(FEdemand)
  UEdemand <- merge(UEdemand, MappUE, by = "technology")
  UEdemand[univocalName %in% c("Cycle", "Walk"), UEefficiency := 1]
  UEdemand[, value := value * UEefficiency][, UEefficiency := NULL]
  UEdemand[, variable := "Useful energy"]
  
  return(fleetFEdemand)
}