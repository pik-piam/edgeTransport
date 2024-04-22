#'Report cost variables in aggregated levels:
#'Capital costs sales, operating costs (total non-fuel), fuel costs
#'
#' @param combinedCAPEXandOPEX detailed data on cost variables
#'
#' @returns aggregated cost variables
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolReportAggregatedCosts <- function (combinedCAPEXandOPEX){
  combinedCAPEXandOPEX <- copy(combinedCAPEXandOPEX)
  combinedCAPEXandOPEX[grepl("Capital.*", variable), type := "Capital costs sales"]
  combinedCAPEXandOPEX[grepl("Operating.*", variable), type := "Operating costs (total non-fuel)"]
  combinedCAPEXandOPEX[grepl("Fuel.*", variable), type := "Fuel costs"]
  if (anyNA(combinedCAPEXandOPEX) == TRUE) stop("Some cost mixedCarrierTypes did not receive an aggregated
                                                level and would get lost in the aggregation. Please check toolAggregateCosts()")
  combinedCAPEXandOPEX[, variable := NULL]
  setnames(combinedCAPEXandOPEX, "type", "variable")
  cols <- names(combinedCAPEXandOPEX)
  combinedCAPEXandOPEX <- combinedCAPEXandOPEX[, .(value = sum(value)), by = eval(cols[cols != "value"])]

  return(combinedCAPEXandOPEX)
}
