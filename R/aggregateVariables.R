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

aggregateVariables <- function(vars, mapAggregation, weight = NULL) {
  
  createVariableEntry <- function(aggrvars, cols) {
    varNameCols <- c("variable", cols)
    varNames <- aggrvars[, ..varNameCols]
    varNames[, variable := paste0(variable, "|Transport")]
    varNames[!is.na(technology), technology := paste0("|", technology)]
    varNames[!is.na(technology), fuel := paste0("|", technology)]
    varNames[, variable := do.call(paste0, .SD), .SDcols = varNameCols]
    varNames[, variable := gsub("NA", "", variable)]
    return(varNames)
  }
  
  vars <- merge(vars, mapAggregation, by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
  vars[, univocalName := NULL]
  aggrOrder <- c("sector", "aggrActiveModes", "aggrRail", "subsectorL1", "subsectorL2",
                 "subsectorL3", "aggrVehSizes", "vehicleType", "technology", "fuel")
  # Initialize aggregated vars
  aggregatedvars <- vars[0][, eval(aggrOrder) := NULL]
  keep <- c("region",  aggrOrder, "variable", "unit", "period", "value")
  vars <- vars[, ..keep]
  if (!is.null(weight)) {
    weight <- merge(weight, mapAggregation, by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
    keepCols <- keep[keep != "fuel"]
    weight <- weight[, ..keepCols]
    weight[, c("variable", "unit") := NULL]
    setnames(weight, "value", "weight")
  }
  
  # Aggregate each level of the decision tree --------------------------------------------------------------------
  for (i in seq(0, length(aggrOrder) - 1)) {
    browser()
    cols <- aggrOrder[1:(length(aggrOrder) - i)]
    last <- cols[length(cols)]
    aggrvars <- vars[!is.na(get(last))]
    byCols <- c("region",  cols, "variable", "unit", "period")
    
    if (!is.null(weight)) {
      aggrvars <- merge(aggrvars, weight, by = intersect(names(aggrvars), names(weight)), all.x = TRUE)
      aggrvars[, sum := sum(weight), by = eval(byCols)]
      aggrvars[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
      aggrvars <- aggrvars[, .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
    } else {
      aggrvars <- aggrvars[, .(value = sum(value)), by = eval(byCols)]
    }
    
    varNames <- createVariableEntry(aggrvars, cols)
    aggrvars[, variable := NULL]
    aggrvars <- merge(aggrvars, varNames, by = intersect(names(aggrvars), names(varNames)))
    aggrvars[, eval(cols) := NULL]
    aggregatedvars <- rbind(aggregatedvars, aggrvars)
  }
  
  # Aggregate keeping technology level --------------------------------------------------------------------
  aggregateLeveltoTech <- c("sector", "subsectorL3", "subsectorL2", "aggrVehSizes")
  for (i in aggregateLeveltoTech) {
    browser()
    cols <- aggrOrder[1:match(i, aggrOrder)]
    last <- cols[length(cols)]
    aggrvars <- copy(vars)
    # Only keep entries for aggregation that feature the aggregation level
    aggrvars <- aggrvars[!is.na(last) & !is.na(technology)][, rownum := .I]
    # To prevent duplicates mode types with no further breakdown needs to be filtered out
    # (e.g. Bus|BEV has been calculated above)
    # Check whether all columns until technology are NA: if yes -> filter Out
    after <- aggrOrder[(match(i, aggrOrder) + 1) : (match("technology", aggrOrder) - 1)]
    filterDT <- aggrvars[, ..after]
    filterDT <- filterDT[, lapply(.SD, is.na)]
    filterDT[, sum := rowSums(.SD)]
    filterDT[, keep := ifelse(sum == 0, FALSE, TRUE)]
    filterDT <- filterDT[, c("keep")][, rownum := .I]
    aggrvars <- merge(aggrvars, filterDT, by = "rownum")
    aggrvars <- aggrvars[keep == TRUE][, keep := NULL]
    
    cols <- c(cols, "technology")
    byCols <- c("region",  cols, "variable", "unit", "period")
    
    aggrvars <- aggrvars[, .(value = sum(value)), by = eval(byCols)]
    varNames <- createVariableEntry(aggrvars, cols)
    aggrvars[, variable := NULL]
    aggrvars <- merge(aggrvars, varNames, by = intersect(names(aggrvars), names(varNames)))
    aggrvars[, eval(cols) := NULL]
    aggregatedvars <- rbind(aggregatedvars, aggrvars)
  }
  
  # Aggregate keeping fuel level --------------------------------------------------------------------
  if ("fuel" %in% names(vars)) {
    # Aggregate keeping technology and fuel level
    aggregateLeveltoTech <- c("sector", "subsectorL3", "subsectorL2", "aggrVehSizes")
    for (i in aggregateLeveltoTech) {
      browser()
      cols <- aggrOrder[1:match(i, aggrOrder)]
      last <- cols[length(cols)]
      aggrvars <- copy(vars)
      # Only keep entries for aggregation that feature the aggregation level
      aggrvars <- aggrvars[!is.na(last) & !is.na(fuel)][, rownum := .I]
      # To prevent duplicates mode types with no further breakdown needs to be filtered out
      # (e.g. Bus|BEV has been calculated above)
      # Check whether all columns until technology are NA: if yes -> filter Out
      after <- aggrOrder[(match(i, aggrOrder) + 1) : (match("technology", aggrOrder) - 1)]
      filterDT <- aggrvars[, ..after]
      filterDT <- filterDT[, lapply(.SD, is.na)]
      filterDT[, sum := rowSums(.SD)]
      filterDT[, keep := ifelse(sum == 0, FALSE, TRUE)]
      filterDT <- filterDT[, c("keep")][, rownum := .I]
      aggrvars <- merge(aggrvars, filterDT, by = "rownum")
      aggrvars <- aggrvars[keep == TRUE][, keep := NULL]
        aggrvars <- aggrvars[, .(value = sum(value)), by = eval(byCols)]
        varNames <- createVariableEntry(aggrvars, cols)
        aggrvars[, variable := NULL]
        aggrvars <- merge(aggrvars, varNames, by = intersect(names(aggrvars), names(varNames)))
        aggrvars[, eval(cols) := NULL]
        aggregatedvars <- rbind(aggregatedvars, aggrvars)
      }
    }

    # Aggregate with bunkers --------------------------------------------------------------------
    aggrvars <- copy(vars)
    browser()
    aggrvars[grepl(".*Pass.*", sector), sector := "|Pass with bunkers"]
    aggrvars[grepl(".*Freight.*", sector), sector := "|Freight with bunkers"]
    aggrvars <- aggrvars[, .(value = sum(value)), by = c("region",  "sector", "variable", "unit", "period")]
    aggrvars[, variable := paste0(variable, "|Transport", sector)][, sector := NULL]
    aggregatedvars <- rbind(aggregatedvars, aggrvars)
    # Aggregate with bunkers keeping technology level
    aggrvars <- copy(vars)
    # Active modes need to be excluded as they dont have a technology
    aggrvars <- aggrvars[!is.na(technology)]
    aggrvars[grepl(".*Pass.*", sector), sector := "|Pass with bunkers"]
    aggrvars[grepl(".*Freight.*", sector), sector := "|Freight with bunkers"]
    aggrvars <- aggrvars[, .(value = sum(value)), by = c("region",  "sector", "technology", "variable", "unit", "period")]
    aggrvars[, variable := paste0(variable, "|Transport", sector, technology)][, c("sector", "technology") := NULL]
    aggregatedvars <- rbind(aggregatedvars, aggrvars)

    if (anyNA(aggregatedvars)) stop(paste0("Output variable contains NAs.
                                           Please check toolReportAndAggregatedMIF() variable: ",
                                           unique(vars$variable)))
    return(aggregatedvars)
  }