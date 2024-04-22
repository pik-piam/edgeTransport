#'Report basic variable set needed to report REMIND input data and detailed transport data
#'
#' @param data List that contains at least the model results to report the basic variable set
#' @param timeResReporting Timesteps to be reported
#'
#' @returns list of intensive and extensive output variables
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolReportBaseVarSet <- function(data, timeResReporting){

  # aggregate costs---------------------------------------------------------------------------
  aggregatedCosts <- toolReportAggregatedCosts(data$combinedCAPEXandOPEX)
  aggregatedCosts <- merge(aggregatedCosts, data$helpers$decisionTree,
                           by = intersect(names(aggregatedCosts), names(data$helpers$decisionTree)))

  # Move from sales to fleet reporting for affected variables---------------------------------
  # (in the variables named fleet other modes are still included)
  # Energy service demand on fleet level deviates from the sales level
  # regarding the share that each technology gets
  fleetESdemand <- rbind(data$ESdemandFVsalesLevel[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)],
                         data$fleetSizeAndComposition$fleetESdemand)
  # Energy intensity and Capital costs are tied to the construction year and have to be recalculated
  # to reflect the value for each year referring to the vehicle stock
  fleetVariables <- list(fleetEnergyIntensity = data$enIntensity,
                         fleetCapCosts = aggregatedCosts[variable == "Capital costs sales"])
  fleetData <- lapply(fleetVariables, toolReportFleetVariables,
                      data$fleetSizeAndComposition$fleetVehNumbersConstrYears, data$helpers)
  fleetCost <- rbind(fleetData$fleetCapCosts, aggregatedCosts[!variable == "Capital costs sales"])

  # Switch from mixed time resolution to the reporting time resolution for all vars------------
  # (this needs to happen after the reporting of the fleet data)
  fleetESdemand <- fleetESdemand[period %in% timeResReporting]
  fleetData <- lapply(fleetData, FUN = function(x) x <- x[period %in% timeResReporting])
  loadFactor <- copy(data$loadFactor)[period %in% timeResReporting]
  fleetCost <- fleetCost[period %in% timeResReporting]

  # Calculate final energy---------------------------------------------------------------------
  fleetFEdemand <- toolReportFE(fleetEnergyIntensity = fleetData$fleetEnergyIntensity, fleetESdemand = fleetESdemand, loadFactor = loadFactor,
                                hybridElecShare = data$hybridElecShare, helpers = data$helpers)
  outputVarsExt <- list(
    fleetESdemand = fleetESdemand,
    fleetFEdemand = fleetFEdemand
  )
  outputVarsInt <- list(
    fleetEnergyIntensity = fleetData$fleetEnergyIntensity,
    fleetCost = fleetCost
  )

  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)

  return(outputVars)

}
