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

reportTransportVarSet <- function(data, timeResReporting, reportTransportData = TRUE, reportExtendedTransportData = FALSE, reportAnalytics = FALSE){
  
  # Switch from mixed time resolution to the reporting time resolution for all vars
  data$ESdemandFVsalesLevel <- data$ESdemandFVsalesLevel[period %in% timeResReporting]
  data$fleetSizeAndComposition <- lapply(data$fleetSizeAndComposition, FUN = function(x) x <- x[period %in% timeResReporting])
  data$enIntensity <- data$enIntensity[period %in% timeResReporting]
  data$upfrontCAPEXtrackedFleet <- data$upfrontCAPEXtrackedFleet[period %in% timeResReporting]
  aggregatedCosts <- aggregatedCosts[period %in% timeResReporting]
  data$combinedCAPEXandOPEX <- data$combinedCAPEXandOPEX[period %in% timeResReporting]
  
  # Calculate liquids and gases split
  browser()
  mixedCarrierSplit <- toolReportLiquidsAndGasesComposition(fleetFEdemand[technology %in% c("Liquids", "Gases")], data$gdxPath, data$helpers)
  fleetFEdemandsplittedCarriers <- copy(fleetFEdemand[!technology %in% c("Liquids", "Gases")])[, fuel := NA]
  fleetFEdemandsplittedCarriers <- rbind(fleetFEdemandsplittedCarriers, mixedCarrierSplit$splittedCarriers)
  outputVarsExt$fleetFEdemand <- fleetFEdemandsplittedCarriers
  
  # Calculate emissions
  cols <- names(fleetFEdemand)
  FEtailpipe <- fleetFEdemand[, .(sum = sum(value)), by = cols[!cols == "fuel"]]
  FEfossil <- fleetFEdemand[fuel == "Fossil", .(sum = sum(value)), by = cols[!cols == "fuel"]]
  fleetEmissionsTailpipe <- toolCalculateEmissions(FEtailpipe, data$gdxPath, "Tailpipe", data$helpers)
  fleetEmissionsDemand <- toolCalculateEmissions(FEfossil, data$gdxPath, "Demand", data$helpers)
  fleetEmissions <- rbind(fleetEmissionsTailpipe, fleetEmissionsDemand)
  
  # Calculate vehicle sales
  sales <- copy(data$fleetSizeAndComposition$fleetVehNumbersConstrYears[period == constrYear])
  sales[, variable := "Sales"][, constrYear := NULL]
  sales <- approx_dt(sales, timeResReporting, "period", "value", extrapolate = TRUE)
  
  # Calculate yearly investment costs
  fleetES <- copy(fleetESdemand)
  fleetES[, c("variable", "unit") := NULL]
  setnames(fleetES, "value", "ESdemand")
  fleetCost <- rbind(fleetData$fleetCapCosts, aggregatedCosts[!variable == "Capital costs sales"])
  fleetYrlCosts <- merge(fleetCost, fleetES, by = intersect(names(fleetCost), names(fleetES)))
  fleetYrlCosts[, value := value * ESdemand][, unit := "billion US$2005/yr"][, ESdemand := NULL]
  fleetYrlCosts[variable == "Capital costs", variable := "Annualized fleet investments"]
  fleetYrlCosts[variable == "Operating costs (total non-fuel)",
                variable := "Operating costs fleet (total non-fuel)"]
  fleetYrlCosts[variable == "Fuel costs",
                variable := "Operating costs fleet (fuel)"]
  aggregatedOperatingCosts <- fleetYrlCosts[variable %in% c("Operating costs fleet (total non-fuel)",
                                                            "Operating costs fleet (fuel)")]
  byCols <- names(aggregatedOperatingCosts)
  byCols <- byCols[!byCols %in% c("value", "variable")]
  aggregatedOperatingCosts <- aggregatedOperatingCosts[, .(value = sum(value)), by = byCols][, variable := "Operating costs fleet"]
  fleetYrlCosts <- rbind(fleetYrlCosts, aggregatedOperatingCosts)
  
  # Calculate upfront capital cost for vehicle sales
  data$upfrontCAPEXtrackedFleet <- copy(data$upfrontCAPEXtrackedFleet)
  data$upfrontCAPEXtrackedFleet <- merge(data$upfrontCAPEXtrackedFleet, data$helpers$decisionTree, by = intersect(names(data$upfrontCAPEXtrackedFleet), names(data$helpers$decisionTree)))
  
  # Differentiate between intensive and extensive variables (those that can be aggregated without a weight)
  
  addOutputVarsExt <- list(
    #FEsplittedCarriers = mixedCarrierSplit$splittedCarriers,
    fleetEmissions = fleetEmissions,
    sales = sales,
    stock = data$fleetSizeAndComposition$fleetVehNumbers,
    fleetYrlCosts = fleetYrlCosts
  )
  addOutputVarsInt <- list(
    upfrontCAPEXtrackedFleet = data$upfrontCAPEXtrackedFleet
  )
  
  outputVarsExt <- append(outputVarsExt, addOutputVarsExt)
  outputVarsInt <- append(outputVarsInt, addOutputVarsInt)
  
  
}