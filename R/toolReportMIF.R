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

toolReportMIF <- function(vars, GDPMER, helpers, scenario, model, gdx,  reportExtendedTransportData = FALSE) {

  applyReportingNames <- function(vars, mapNames) {

    rename <- function(columns, mapNames) {
      dt <- data.table(name = c(columns))
      dt[, rownum := .I]
      dt <- merge(dt, mapNames, by = "name", allow.cartesian = TRUE, all.x = TRUE)
      # Important to prevent change of row order
      setkey(dt, "rownum")
      dt <- dt[!is.na(reportName), name := reportName]
      dt[grepl(".*tmp", name), name := NA]
      dt[!is.na(name), name := paste0("|", name)]

      return(dt$name)
    }

    colsUnchanged <- c("region", "period", "variable", "univocalName", "technology", "fuel", "unit", "value")
    colOrder <- names(vars)
    colsToRename <- colOrder[!colOrder %in% colsUnchanged]
    dt <- vars[, ..colsToRename]
    dt <- dt[, lapply(.SD, rename, helpers$reportingNames)]
    varsNew <- cbind(vars[, ..colsUnchanged], dt)
    setcolorder(varsNew, colOrder)
    return(varsNew)
  }

  # Use ES demand as weight to aggregate over modes
  varsToMIFext <- rbindlist(vars$ext, fill = TRUE, use.names = TRUE)
  varsToMIFint <- vars$int[!names(vars) %in% c("FEsplitShares")]
  noAggregationvars <- vars$int[["FEsplitShares"]]
  varsToMIFint <- rbindlist(varsToMIFint, fill = TRUE, use.names = TRUE)

  # Apply variable naming convention
  varsToMIFext <- applyReportingNames(varsToMIFext, helpers$reportingNames)
  varsToMIFint[, fuel := NA]
  varsToMIFint <- applyReportingNames(varsToMIFint, helpers$reportingNames)

  if (!is.null(vars$updatedEndogenousCosts)) {
    varsToMIFanalytics <- list(vars$updatedEndogenousCosts, vars$policyMask, vars$rawEndogenousCost)
    varsToMIFanalytics <- lapply(varsToMIFanalytics, rename, helpers$reportingNames)
    varsToMIFanalytics <- lapply(varsToMIFanalytics,
                                 function(x){x[, variable := paste0(variable, "|", vehicleType, "|", technology)]
                                             x[, c("region", "period", "variable", "value", "unit")]})
  }
  toMIFext <- aggregate(varsToMIFext, helpers$reportingAggregation)
  weight <- varsToMIFext[variable == "ES"]
  toMIFint <- aggregate(varsToMIFint, helpers$reportingAggregation, weight)
  toMIFint <- rbind(noAggregationvars, toMIFint, fill = TRUE, use.names = TRUE)

  # Regional aggregation
  ## Aggregation to world is always supplied
  mapWorld <- unique(toMIFext[, c("region")])[, aggrReg := "World"]
  worldDataExt <- as.data.table(aggregate_map(toMIFext, mapWorld, by = "region"))
  weight <- copy(GDPMER)
  setnames(weight, "value", "weight_val_col")
  worldDataInt <- as.data.table(aggregate_map(toMIFint, mapWorld, by = "region", weights = weight, weight_item_col = "region"))

  ## if regionSubsetList != NULL -> gdx provides 21 region resolution
  regionSubsetList <- toolRegionSubsets(gdx)

  if (!is.null(regionSubsetList)){
  # ADD EU-27 region aggregation
    if("EUR" %in% names(regionSubsetList)){
      regionSubsetList <- c(regionSubsetList, list(
        "EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "ESW")
      ))
    }
    # Create Mapping for region Aggregation out of region SubsetList
    namesReg <- names(regionSubsetList)
    regSubsetAggregation <- data.table()
    for (i in 1:length(namesReg)){
      tmp <- data.table(region = regionSubsetList[[i]], aggrReg = namesReg[i])
      regSubsetAggregation <- rbind(regSubsetAggregation, tmp)
    }
    subsetDataExt <- as.data.table(aggregate_map(toMIFext[region %in% unique(regSubsetAggregation$region)], regSubsetAggregation, by = "region"))
    weight <- copy(GDPMER)
    setnames(weight, "value", "weight_val_col")
    subsetDataDataInt <- as.data.table(aggregate_map(toMIFint[region %in% unique(regSubsetAggregation$region)], regSubsetAggregation, by = "region",
                                       weights = weight, weight_item_col = "region"))

    # EUR and EU27 aggregation is always provided in the 21 region resolution
    toMIFint <- rbind(toMIFint, subsetDataDataInt[region %in% c("EUR", "EU27")])
    toMIFext <- rbind(toMIFext, subsetDataExt[region %in% c("EUR", "EU27")])
  }
  # World aggregation is always added
  toMIFint <- rbind(toMIFint, worldDataInt)
  toMIFext <- rbind(toMIFext, worldDataExt)

  # Other subset region are only provided in the extended reporting
  if (reportExtendedTransportData == TRUE) {
    toMIFint <- rbind(toMIFint, subsetDataDataInt[!region %in% c("EUR", "EU27")])
    toMIFext <- rbind(toMIFext, subsetDataExt[!region %in% c("EUR", "EU27")])
  }

  toMIF <- rbind(toMIFint, toMIFext)
  toMIF[, model := model][, scenario := scenario]
  toMIF <- as.quitte(toMIF)

  if (anyNA(toMIF)) stop("MIF output contains NAs.
                         Please check toolReportAndAggregatedMIF()")
  if (anyDuplicated(toMIF)) stop("MIF output contains duplicates.
                                 Please check toolReportAndAggregatedMIF()")

  return(toMIF)
}
