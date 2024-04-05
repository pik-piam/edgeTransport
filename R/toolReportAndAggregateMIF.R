
toolReportAndAggregateMIF <- function(vars, GDPMER, helpers, scenario, model, gdx,  reportExtendedTransportData = FALSE) {

  rename <- function(var, mapNames) {
    cols <- names(var)
    cols <- cols[!cols %in% c("univocalName", "region", "period", "variable", "value",  "unit")]
    for (colName in cols) {
      map <- copy(mapNames)
      setnames(map, "name", colName)
      var <- merge(var, map, by = eval(colName), all.x = TRUE)
      var[!is.na(reportName), eval(colName) := reportName][, reportName := NULL]
      var[!is.na(get(colName)), eval(colName) := paste0("|", get(colName))]
      var[grepl(".*tmp", get(colName)), eval(colName) := NA]
    }
  return(var)
  }

  aggregate <- function(var, mapAggregation, weight = NULL) {

    var <- merge(var, mapAggregation, by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
    var[, univocalName := NULL]
    aggrOrder <- c("sector", "aggrActiveModes", "aggrRail", "subsectorL1", "subsectorL2",
                   "subsectorL3", "aggrVehSizes", "vehicleType", "technology", "fuel")
    # Initialize aggregated vars
    aggregatedVars <- var[0][, eval(aggrOrder) := NULL]
    keep <- c("region",  aggrOrder, "variable", "unit", "period", "value")
    var <- var[, ..keep]
    if (!is.null(weight)) {
      weight <- merge(weight, mapAggregation, by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
      keepCols <- keep[keep != "fuel"]
      weight <- weight[, ..keepCols]
      weight[, c("variable", "unit") := NULL]
      setnames(weight, "value", "weight")
    }

    for (i in seq(0, length(aggrOrder) - 1)) {
      cols <- aggrOrder[1:(length(aggrOrder) - i)]
      last <- cols[length(cols)]
      byCols <- c("region",  cols, "variable", "unit", "period")
      aggrVar <- copy(var)

      if (!is.null(weight)) {
        aggrVar <- merge(aggrVar, weight, by = intersect(names(aggrVar), names(weight)), all.x = TRUE)
        aggrVar[, sum := sum(weight), by = eval(byCols)]
        aggrVar[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
        aggrVar <- aggrVar[!is.na(get(last)), .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
      } else {
        aggrVar <- aggrVar[!is.na(get(last)), .(value = sum(value)), by = eval(byCols)]
      }
      aggrVar[, variable := paste0(variable, "|Transport")]
      for (r in cols) aggrVar[!is.na(get(r)), variable := paste0(variable, get(r))]
      aggrVar[, eval(cols) := NULL]
      aggregatedVars <- rbind(aggregatedVars, aggrVar)
    }

    # Aggregate keeping technology level
    aggregateLeveltoTech <- c("sector", "subsectorL3", "subsectorL2", "aggrVehSizes")
    for (i in aggregateLeveltoTech) {
      cols <- aggrOrder[1:match(i, aggrOrder)]
      last <- cols[length(cols)]
      after <- aggrOrder[(match(i, aggrOrder) + 1)]
      cols <- c(cols, "technology")
      byCols <- c("region",  cols, "variable", "unit", "period")
      aggrVar <- copy(var)
      aggrVar <- aggrVar[!is.na(technology)]
      # To prevent duplicates mode types with now further breakdown needs to be filtered out
      # (e.g. Bus|BEV has been calculated above)
      aggrVar <- aggrVar[!is.na(get(last)) & !is.na(get(after)), .(value = sum(value)), by = eval(byCols)]
      aggrVar[, variable := paste0(variable, "|Transport")]
      for (r in cols) aggrVar[!is.na(get(r)), variable := paste0(variable, get(r))]
      aggrVar[, eval(cols) := NULL]
      aggregatedVars <- rbind(aggregatedVars, aggrVar)
    }

    if ("fuel" %in% names(var)) {
      # Aggregate keeping technology and fuel level
      aggregateLeveltoTech <- c("sector", "subsectorL3", "subsectorL2", "aggrVehSizes")
      for (i in aggregateLeveltoTech) {
        cols <- aggrOrder[1:match(i, aggrOrder)]
        last <- cols[length(cols)]
        after <- aggrOrder[(match(i, aggrOrder) + 1)]
        cols <- c(cols, "technology", "fuel")
        byCols <- c("region",  cols, "variable", "unit", "period")
        aggrVar <- copy(var)
        # To prevent duplicates mode types with now further breakdown needs to be filtered out
        # (e.g. Bus|BEV has been calculated above)
        aggrVar <- aggrVar[!is.na(fuel)]
        aggrVar <- aggrVar[!is.na(get(last)) & !is.na(get(after)), .(value = sum(value)), by = eval(byCols)]
        aggrVar[, variable := paste0(variable, "|Transport")]
        for (r in cols) aggrVar[!is.na(get(r)), variable := paste0(variable, get(r))]
        aggrVar[, eval(cols) := NULL]

        aggregatedVars <- rbind(aggregatedVars, aggrVar)
      }
    }
    # Aggregate with bunkers
    aggrVar <- copy(var)
    aggrVar[grepl(".*Pass.*", sector), sector := "|Pass with bunkers"]
    aggrVar[grepl(".*Freight.*", sector), sector := "|Freight with bunkers"]
    aggrVar <- aggrVar[, .(value = sum(value)), by = c("region",  "sector", "variable", "unit", "period")]
    aggrVar[, variable := paste0(variable, "|Transport", sector)][, sector := NULL]
    aggregatedVars <- rbind(aggregatedVars, aggrVar)
    # Aggregate with bunkers keeping technology level
    aggrVar <- copy(var)
    # Active modes need to be excluded as they dont have a technology
    aggrVar <- aggrVar[!is.na(technology)]
    aggrVar[grepl(".*Pass.*", sector), sector := "|Pass with bunkers"]
    aggrVar[grepl(".*Freight.*", sector), sector := "|Freight with bunkers"]
    aggrVar <- aggrVar[, .(value = sum(value)), by = c("region",  "sector", "technology", "variable", "unit", "period")]
    aggrVar[, variable := paste0(variable, "|Transport", sector, technology)][, c("sector", "technology") := NULL]
    aggregatedVars <- rbind(aggregatedVars, aggrVar)

    if (anyNA(aggregatedVars)) stop(paste0("Output variable contains NAs.
                                           Please check toolReportAndAggregatedMIF() variable: ",
                                           unique(var$variable)))
    return(aggregatedVars)
  }

  # Use ES demand as weight to aggregate over modes
  varsToMIFext <- rbindlist(vars$ext, fill = TRUE, use.names = TRUE)
  varsToMIFint <- vars$int[!names(vars) %in% c("FEsplitShares")]
  noAggregationVars <- vars$int[["FEsplitShares"]]
  varsToMIFint <- rbindlist(varsToMIFint, fill = TRUE, use.names = TRUE)

  # Apply variable naming convention
  varsToMIFext <- rename(varsToMIFext, helpers$reportingNames)
  varsToMIFint <- rename(varsToMIFint, helpers$reportingNames)
  varsToMIFint[, fuel := NA]
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
  toMIFint <- rbind(noAggregationVars, toMIFint, fill = TRUE, use.names = TRUE)

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
