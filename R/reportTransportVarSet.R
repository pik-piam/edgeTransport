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

  # Switch from mixed time resolution to the reporting time resolution for all vars
  data$ESdemandFVsalesLevel <- data$ESdemandFVsalesLevel[period %in% timeResReporting]
  data$fleetSizeAndComposition <- lapply(data$fleetSizeAndComposition, FUN = function(x) x <- x[period %in% timeResReporting])
  data$enIntensity <- data$enIntensity[period %in% timeResReporting]
  data$upfrontCAPEXtrackedFleet <- data$upfrontCAPEXtrackedFleet[period %in% timeResReporting]

  # Calculate liquids and gases split
  mixedCarrierSplit <- toolReportLiquidsAndGasesComposition(dtFE = baseVarSet$ext$fleetFEdemand[technology %in% c("Liquids", "Gases")],
                                                            gdxPath = data$gdxPath,
                                                            timeResReporting = timeResReporting,
                                                            helpers = data$helpers)
  fleetFEdemandsplittedCarriers <- copy(baseVarSet$ext$fleetFEdemand[!technology %in% c("Liquids", "Gases")])[, fuel := NA]
  fleetFEdemandsplittedCarriers <- rbind(fleetFEdemandsplittedCarriers, mixedCarrierSplit$splittedCarriers)

  # Calculate emissions
  cols <- names(fleetFEdemandsplittedCarriers)
  byCols <- cols[!cols %in% c("fuel", "value")]
  # For the tailpipe emissions the different fuel production routes are not taken into account ´
  # "It counts what comes out of the exhaust"
  FEtailpipe <- fleetFEdemandsplittedCarriers[technology %in% c("Liquids", "Gases")]
  FEtailpipe <- FEtailpipe[, .(value = sum(value)), by = eval(byCols)]
  # For the demand emissions only the fuel from the fossil production route is taken into account ´
  FEfossil <- fleetFEdemandsplittedCarriers[technology %in% c("Liquids", "Gases") & fuel == "Fossil"]
  FEfossil <- FEfossil[, .(value = sum(value)), by = eval(byCols)]
  fleetEmissionsTailpipe <- toolReportEmissions(dtFE = FEtailpipe,
                                                gdxPath = data$gdxPath,
                                                prefix = "Tailpipe",
                                                helpers = data$helpers)
  fleetEmissionsDemand <- toolReportEmissions(dtFE = FEfossil,
                                              gdxPath = data$gdxPath,
                                              prefix = "Demand",
                                              helpers = data$helpers)
  fleetEmissions <- rbind(fleetEmissionsTailpipe, fleetEmissionsDemand)

  # Report vehicle sales
  sales <- copy(data$fleetSizeAndComposition$fleetVehNumbersConstrYears[period == constrYear])
  sales[, variable := "Sales"][, constrYear := NULL]
  sales <- approx_dt(sales, timeResReporting, "period", "value", extrapolate = TRUE)

  # Report yearly investment costs
  fleetES <- copy(baseVarSet$ext$fleetESdemand)
  fleetES[, c("variable", "unit") := NULL]
  setnames(fleetES, "value", "ESdemand")
  fleetYrlCosts <- merge(baseVarSet$int$fleetCost, fleetES, by = intersect(names(baseVarSet$int$fleetCost), names(fleetES)))
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

  # Report upfront capital cost for vehicle sales
  data$upfrontCAPEXtrackedFleet <- copy(data$upfrontCAPEXtrackedFleet)
  data$upfrontCAPEXtrackedFleet <- merge(data$upfrontCAPEXtrackedFleet, data$helpers$decisionTree, by = intersect(names(data$upfrontCAPEXtrackedFleet), names(data$helpers$decisionTree)))

  # Differentiate between intensive and extensive variables (those that can be aggregated without a weight)

  outputVarsExt <- list(
    FEsplittedCarriers = fleetFEdemandsplittedCarriers,
    fleetEmissions = fleetEmissions,
    sales = sales,
    stock = data$fleetSizeAndComposition$fleetVehNumbers,
    fleetYrlCosts = fleetYrlCosts
  )
  outputVarsInt <- list(
    upfrontCAPEXtrackedFleet = data$upfrontCAPEXtrackedFleet
  )

  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)

  return(outputVars)
}
