#' toolPrepareReporting
#'
#' Brings all data that will be included in the reporting in the right format and checks for completeness
#'
#' @author Johanna Hoppe
#' @param
#' @returns list of data.tables in egdeTransport quitte format
#' @import data.table
#' @export


toolPrepareReporting <- function() {

  toolAggregateCosts <- function (combinedCAPEXandOPEX){

    combinedCAPEXandOPEX <- copy(inputData$combinedCAPEXandOPEX)
    combinedCAPEXandOPEX[grepl("Capital.*|Subsidy", variable), type := "Capital costs sales"]
    combinedCAPEXandOPEX[grepl("Operational.*", variable), type := "Operating costs (total non-fuel)"]
    combinedCAPEXandOPEX[grepl("Fuel.*", variable), type := "Fuel costs"]
    activeModes <- combinedCAPEXandOPEX[univocalName %in% c("Walk", "Cycle")][, type := "Capital costs sales"]
    #Insert placeholders for active modes
    combinedCAPEXandOPEX <- rbind(combinedCAPEXandOPEX[!univocalName %in% c("Walk", "Cycle")],
                                  activeModes,
                                  copy(activeModes)[, type := "Operational costs (non fuel)"],
                                  copy(activeModes)[, type := "Fuel costs"])[, variable := NULL]
    if (anyNA(combinedCAPEXandOPEX$type) == TRUE) stop("Some cost types did not receive an aggregaten level and would get lost in the aggregation. Please check toolAggregateCosts()")
    setnames(combinedCAPEXandOPEX, "type", "variable")
    cols <- names(combinedCAPEXandOPEX)
    combinedCAPEXandOPEX <- combinedCAPEXandOPEX[, .(value = sum(value)), by = eval(cols[cols != "value"])]

    return(combinedCAPEXandOPEX)
  }

  # Aggregate costs
  aggregatedCosts <- toolAggregateCosts(copy(inputData$combinedCAPEXandOPEX))


}
