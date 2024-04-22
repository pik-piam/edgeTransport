#'Report detailed transport variable set
#'
#' @param data List that contains the model results to report the detailed transport variable set
#' @param baseVarSet Basic output variable set
#' @param timeResReporting Timesteps to be reported
#'
#' @returns detailed transport output variable set
#' @author Johanna Hoppe
#' @import data.table
#' @export

reportTransportVarSet <- function(data, baseVarSet, timeResReporting){
  browser()
  # Switch from mixed time resolution to the reporting time resolution for all vars
  data$ESdemandFVsalesLevel <- data$ESdemandFVsalesLevel[period %in% timeResReporting]
  data$fleetSizeAndComposition <- lapply(data$fleetSizeAndComposition, FUN = function(x) x <- x[period %in% timeResReporting])
  data$enIntensity <- data$enIntensity[period %in% timeResReporting]
  data$upfrontCAPEXtrackedFleet <- data$upfrontCAPEXtrackedFleet[period %in% timeResReporting]

  # Calculate liquids and gases split
  browser()
  mixedCarrierSplit <- toolReportLiquidsAndGasesComposition(dtFE = baseVarSet$ext$fleetFEdemand[technology %in% c("Liquids", "Gases")],
                                                            gdxPath = data$gdxPath,
                                                            timeResReporting = timeResReporting,
                                                            helpers = data$helpers)
  fleetFEdemandsplittedCarriers <- copy(baseVarSet$ext$fleetFEdemand[!technology %in% c("Liquids", "Gases")])[, fuel := NA]
  fleetFEdemandsplittedCarriers <- rbind(fleetFEdemandsplittedCarriers, mixedCarrierSplit$splittedCarriers)

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
  fleetES <- copy(baseVarSet$ext$fleetESdemand)
  fleetES[, c("variable", "unit") := NULL]
  setnames(fleetES, "value", "ESdemand")
  fleetCost <- rbind(baseVarSet$int$fleetCapCosts, aggregatedCosts[!variable == "Capital costs sales"])
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
    FEsplittedCarriers = mixedCarrierSplit$splittedCarriers,
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
