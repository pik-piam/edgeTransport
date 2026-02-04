#' Calculate shares for all levels of the decision tree for a variable on FV level
#'
#' @author Johanna Hoppe
#' @param dtVariable data.table of a transport variable, e.g., energy service demand with data at the most detailed level of of the decision tree (FV = fuel-vehicle)
#' @param helpers list of helpers

#'
#' @returns data.table including shares of the variable for all levels of the decision tree
#' @export


toolCalculateSharesDecisionTree <- function(dtVariable, helpers) {

  # Note: The solution right now is hard-coded and does not react to a change of levels/structure in the decision tree elsewhere.
  # This could be easily changed.
  # Right now changes in the levels are not done regularly and the structure is still quite easy to grasp.
  # Hence we stay with the explicit representation for now

  # Current levels and groupValues of the decision tree
  # levels <- c("FV", "VS3", "S3S2", "S2S1", "S1S")
  # groupValue <- c("vehicleType", "subsectorL3", "subsectorL2", "subsectorL1")
  dtVariable <- merge(dtVariable, helpers$decisionTree, by = intersect(names(dtVariable), names(helpers$decisionTree)), all.x = TRUE)
  dtVariable[, univocalName := NULL]

  FV <- copy(dtVariable)[, c("unit", "variable") := NULL]
  FV[, share := value / sum(value), by = c("region", "period", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  FV[is.nan(share) & value == 0, share := 0]
  FV[, level := "FV"][, value := NULL]

  VS3 <- copy(dtVariable)[, c("unit", "variable") := NULL]
  VS3 <- VS3[, .(value = sum(value)), by = c("region", "period", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  VS3[, technology := ""]
  VS3[, share := value / sum(value), by = c("region", "period", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  VS3[is.nan(share) & value == 0, share := 0]
  VS3[, level := "VS3"][, value := NULL]

  S3S2 <- copy(dtVariable)[, c("unit", "variable") := NULL]
  S3S2 <- S3S2[, .(value = sum(value)), by = c("region", "period", "subsectorL3", "subsectorL2", "subsectorL1", "sector")]
  S3S2[, c("technology", "vehicleType") := ""]
  S3S2[, share := value / sum(value), by = c("region", "period", "subsectorL2", "subsectorL1", "sector")]
  S3S2[is.nan(share) & value == 0, share := 0]
  S3S2[, level := "S3S2"][, value := NULL]

  S2S1 <- copy(dtVariable)[, c("unit", "variable") := NULL]
  S2S1 <- S2S1[, .(value = sum(value)), by = c("region", "period", "subsectorL2", "subsectorL1", "sector")]
  S2S1[, c("technology", "vehicleType", "subsectorL3") := ""]
  S2S1[, share := value / sum(value), by = c("region", "period", "subsectorL1", "sector")]
  S2S1[is.nan(share) & value == 0, share := 0]
  S2S1[, level := "S2S1"][, value := NULL]

  S1S <- copy(dtVariable)[, c("unit", "variable") := NULL]
  S1S <- S1S[, .(value = sum(value)), by = c("region", "period", "subsectorL1", "sector")]
  S1S[, c("technology", "vehicleType", "subsectorL3", "subsectorL2") := ""]
  S1S[, share := value / sum(value), by = c("region", "period", "sector")]
  S1S[is.nan(share) & value == 0, share := 0]
  S1S[, level := "S1S"][, value := NULL]

  return(rbind(FV, VS3, S3S2, S2S1, S1S))
}


#' Normalize preferences so that the maximum in each branch of the decision tree equals 1
#'
#' @author Johanna Hoppe
#' @param preferenceTab data.table including preferences for all levels of the decision tree
#' @returns Normalized preferences
#' @import data.table
#' @export

toolNormalizePreferences <- function(preferenceTab) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  level <- value <- NULL

  # S1S: logit level: distances (e.g. short-medium, long)
  preferenceTab[level == "S1S", max := max(value), by = c("region", "period", "sector")]
  preferenceTab[level == "S1S" & max != 0, value := value / max(value), by = c("region", "period", "sector")]

  # S2S1: logit level: modes/categories (e.g. walk, road, rail)
  preferenceTab[level == "S3S2", max := max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  preferenceTab[level == "S2S1", max := max(value), by = c("region", "period", "sector", "subsectorL1")]
  preferenceTab[level == "S2S1" & max != 0, value := value / max(value),
                by = c("region", "period", "sector", "subsectorL1")]

  # S3S2: logit level: modes/technologies (e.g. LDV, bus, Liquids)
  preferenceTab[level == "S3S2" & max != 0, value := value / max(value),
                by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]

  # VS3: logit level: modes/technologies (e.g. cars, Liquids)
  preferenceTab[level == "VS3", max := max(value),
                by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  preferenceTab[level == "VS3" & max != 0, value := value / max(value),
                by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]


  # FV: logit level: vehicle type (e.g. large car, moped)
  preferenceTab[level == "FV", max := max(value),
                by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")]
  preferenceTab[level == "FV" & max != 0, value := value / max(value),
                by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")]

  preferenceTab[, max := NULL]

  if (anyNA(preferenceTab)) stop("Something went wrong with the normalization of the preference trends. Please check toolNormalizePreferences()")

  return(preferenceTab)
}


#' Read and build the complete structure of the edgeTransport decision tree
#'
#' @author Johanna Hoppe
#' @param regionAggregation choose one of the different options for regional aggregation (iso|regionCode21|regionCode12)
#' @returns data.table of full spatially extended edgeTransport decision tree
#' @import data.table
#' @export

toolLoadDecisionTree <- function(regionAggregation = "iso") {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  spatial <- present <- region <- sector <- subsectorL1 <- subsectorL2 <- subsectorL3 <- vehicleType <- technology <- univocalName <- NULL

  # decisionTree.csv contains all possible branches of the decision tree
  decisionTree <- fread(system.file("extdata/helpersDecisionTree.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  decisionTree[, spatial := "all"]
  # Not all countries feature the same branches of the decision tree - Some vehicleTypes and modes are not
  # available in certain countries
  # Here we create the full structure of the nested decision tree differentiated for all countries to make it testable
  regionMap <- system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                           package = "edgeTransport", mustWork = TRUE
  )
  regionMap <- fread(regionMap, skip = 0, header = TRUE)
  setnames(regionMap, "countryCode", "region")
  ISOcountries <- regionMap[, c("region")]
  ISOcountries[, spatial := "all"]

  completeDataSet <- merge.data.table(decisionTree, ISOcountries, by = "spatial", allow.cartesian = TRUE)
  completeDataSet[, spatial := NULL]
  mapCountrySpecificVehicleTypes <- fread(system.file("extdata/helpersMapCountrySpecificVehicleTypes.csv",
                                                      package = "edgeTransport", mustWork = TRUE), header = TRUE)

  completeDataSet <- merge.data.table(completeDataSet, mapCountrySpecificVehicleTypes,
                                      by = c("region", "univocalName"), all = TRUE)
  completeDataSet <- completeDataSet[present == 1][, present := NULL]

  # aggregate to the required regions
  switch (regionAggregation,
          "iso" = {
            completeDataSetAgg <- completeDataSet
          },
          "regionCode21" = {
            completeDataSetAgg <- merge(completeDataSet, regionMap, by = "region")
            completeDataSetAgg <- unique(completeDataSetAgg[, c("regionCode21", "sector", "subsectorL1",
                                                                "subsectorL2", "subsectorL3", "vehicleType",
                                                                "technology", "univocalName")])
            setnames(completeDataSetAgg, "regionCode21", "region")
          },
          "regionCode12" = {
            completeDataSetAgg <- merge(completeDataSet, regionMap, by = "region")
            completeDataSetAgg <- unique(completeDataSetAgg[, c("regionCode12", "sector", "subsectorL1",
                                                                "subsectorL2", "subsectorL3", "vehicleType",
                                                                "technology", "univocalName")])
            setnames(completeDataSetAgg, "regionCode12", "region")
          }
  )

  setkey(completeDataSetAgg, region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType,
         technology, univocalName)

  return(completeDataSetAgg)
}



#' List associated univocalNames for any entry of the decision tree
#' @author Johanna Hoppe
#' @param categories vector of categories to filter
#' @param decisionTree decision tree that contains the univocalNames associated to the category
#' @returns list of categories and their associated univocalNames e.g. trn_pass as list entry
#'          containing a vector c("Compact Car", "HSR", ..)
#' @import data.table
#' @export

getFilterEntriesUnivocalName <- function(categories, decisionTree) {

  findEntries <- function(category, dt) {
    test <- dt[, lapply(.SD, function(x) grepl(category, x))]
    entries <- unique(dt[rowSums(test) > 0]$univocalName)
  }

  filterEntries <- sapply(categories, findEntries, dt = decisionTree)

  return(filterEntries)
}

#' Calculate data for the next higher level of the decision tree.
#'
#' Function that aggregates the data for the next higher level of the decision tree
#'
#' @author Johanna Hoppe
#' @param data data.table containing the data of the lower level
#' @param upperLevel name of the upper level in the decision tree
#' @param decisionTree full edgeTransport decision tree
#' @returns data.table
#' @import data.table
#' @export

toolTraverseDecisionTree <- function(data, upperLevel, decisionTree) {
  share <- value <- . <- NULL

  decisionGroups <- names(decisionTree[, -c("region", "univocalName")])
  indexUpperLevel <- which(match(decisionGroups, upperLevel) == 1)
  remainingGroups <- decisionGroups[1:indexUpperLevel]
  aggregatedGroups <- setdiff(decisionGroups, remainingGroups)
  remainingNames <- setdiff(names(data), aggregatedGroups)
  data <- data[, .(value = sum(share * value)), by = setdiff(remainingNames, c("value", "share"))]

  return(data)
}


#' toolApplyMixedTimeRes
#'
#' Applies two different temporal resolutions on a data.table object in the
#' edgeTransport data structure and do a linear approximination for the highRes data that is not available
#'
#' @author Johanna Hoppe
#' @param data data.table containing data in in the edgeTransport data structure
#'              (at least featuring univocalName, period, value)
#' @param helpers list containing several helpers used throughout the model.
#'          It includes dtTimeRes, a data.table containing the temporal
#'          resolution for different univocalNames
#' @param idcols optional supply of idcols for using approx_dt
#' @returns data.table
#' @import data.table
#' @importFrom rmndt approx_dt
#' @export

toolApplyMixedTimeRes <- function(data, helpers, idcols = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- level <- test <- univocalName <- . <- NULL

  highRes <- unique(helpers$dtTimeRes$period)
  highResUnivocalNames <- copy(helpers$dtTimeRes)
  highResUnivocalNames <- highResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  highResUnivocalNames <- highResUnivocalNames[test == TRUE]$univocalName

  if (c("level") %in% names(data)) {
    # in this data format there is no univocalName
    # this needs to change temporarily
    dataFV <- merge(data[level == "FV"], helpers$decisionTree, by = intersect(names(data), names(helpers$decisionTree)))
    dataHighRes <- dataFV[univocalName %in% highResUnivocalNames][, univocalName := NULL]
    dataLowRes <- rbind(dataFV[!(univocalName %in% highResUnivocalNames)][, univocalName := NULL], data[!level == "FV"])
  } else {
    dataHighRes <- data[univocalName %in% highResUnivocalNames]
    dataLowRes <- data[!univocalName %in% highResUnivocalNames]
  }

  if (is.null(idcols)) {
    if (nrow(dataHighRes) > 0) dataHighRes <- approx_dt(dataHighRes, highRes, "period", "value", extrapolate = TRUE)
    if (nrow(dataLowRes) > 0) dataLowRes <- approx_dt(dataLowRes, helpers$lowTimeRes, "period", "value", extrapolate = TRUE)
  } else {
    if (nrow(dataHighRes) > 0) dataHighRes <- approx_dt(dataHighRes, highRes, "period", "value", idxcols = idcols, extrapolate = TRUE)
    if (nrow(dataLowRes) > 0) dataLowRes <- approx_dt(dataLowRes, helpers$lowTimeRes, "period", "value", idxcols = idcols, extrapolate = TRUE)
  }

  data <- rbind(dataHighRes, dataLowRes)

return(data)
}



#' toolCheckAllLevelsComplete
#'
#' Checks whether data is complete for all levels of decision tree
#'
#' @author Johanna Hoppe
#' @param data data.table containing data in all levels format that should be checked
#' @param decisionTree data.table containing full edgeTransport decision Tree
#' @param name name of variable to be checked
#' @returns data.table
#' @import data.table
#' @export

toolCheckAllLevelsComplete <- function(data, decisionTree, name) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  level <- univocalName <- technology <- NULL

  decisionFV <- copy(decisionTree)
  decisionFV[, univocalName := NULL][, level := "FV"]

  decisionVS3 <- copy(decisionFV)
  decisionVS3[, technology := ""]
  decisionVS3 <- unique(decisionVS3)[, level := "VS3"]

  decisionS3S2 <- copy(decisionVS3)
  decisionS3S2[, c("technology", "vehicleType") := ""]
  decisionS3S2 <- unique(decisionS3S2)[, level := "S3S2"]

  decisionS2S1 <- copy(decisionS3S2)
  decisionS2S1[, c("technology", "vehicleType", "subsectorL3") := ""]
  decisionS2S1 <- unique(decisionS2S1)[, level := "S2S1"]

  decisionS1S <- copy(decisionS2S1)
  decisionS1S[, c("technology", "vehicleType", "subsectorL3", "subsectorL2") := ""]
  decisionS1S <- unique(decisionS1S)[, level := "S1S"]

  allLevels <- rbind(decisionFV, decisionVS3, decisionS3S2, decisionS2S1, decisionS1S)

  test <- merge(data, allLevels, by = intersect(names(data), names(allLevels)), all = TRUE)

  if (anyNA(test) == TRUE) stop(paste0("Variable ", name, " is incomplete or contains unnessesary data"))

}

#' toolOrderandCheck
#'
#' sort data.table according to edgeTransport data structure and check for NAs
#' If checkCompletness is activated it is further checked, if the full data set is provided.
#' In case of fleetVars == TRUE the full data set is reduced to the vehicle types that feature fleet tracking
#'
#' @author Johanna Hoppe
#' @param data data.table containing data in all levels format that should be checked
#' @param decisionTree data.table containing full edgeTransport decision Tree
#' @param yrs temporal resolution
#' @param checkCompleteness if activated it is further checked, if the full data set is provided
#' @param fleetVars if activated the full data set is reduced to vehicle types that feature fleet tracking
#' @returns data.table
#' @import data.table
#' @export

toolOrderandCheck <- function(data, decisionTree, yrs = NULL, checkCompleteness = FALSE, fleetVars = FALSE) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- subsectorL3 <- allyears <- NULL

  data <- merge(data, decisionTree, by = c(intersect(names(data), names(decisionTree))))
  allCols <- c(names(decisionTree), "variable", "unit", "period", "value")
  isComplete <- all(names(data) %in% allCols) && all(allCols %in% names(data))
  if (isComplete == FALSE) stop(paste0(deparse(substitute(data)), " misses columns or has additional columns"))
  if (anyNA(data) == TRUE) stop(paste0(deparse(substitute(data)), " contains NAs"))
  data <- data[, allCols, with = FALSE]

  if (checkCompleteness == TRUE) {
    data <- data[period %in% yrs]
    if (fleetVars == TRUE) decisionTree <- decisionTree[grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)]
    allTimesteps <- data.table(period = yrs)[, allyears := "All"]
    decisionTree[, allyears := "All"]
    decisionTree <- merge(decisionTree,  allTimesteps, by = "allyears")
    test <- merge(data, decisionTree, by = intersect(names(data), names(decisionTree)), all = TRUE)
    if (anyNA(test) == TRUE) stop(paste0(deparse(substitute(data)), " is not complete"))
  }

  return(data)
}

#' Check a data.table for NAs and duplicates and throw an error if needed
#' @author Johanna Hoppe
#' @param dt data.table to be checked
#' @param varname name of the variable
#' @param codePosition position in the code to find the bug
#' @import data.table
#' @export

checkForNAsDups <- function(dt, varname, codePosition) {

  if (anyNA(dt)) {
    stop(paste0(varname, " in ", codePosition, " contains NAs."))
  }
  if (anyDuplicated(dt)) {
    stop(paste0(varname, " in ", codePosition, " contains duplicates."))
  }

}
