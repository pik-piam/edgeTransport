#' Read and build the complete structure of the edgeTransport decision tree
#'
#' @author Johanna Hoppe
#' @param regionAggregation choose one of the different options for regional aggregation (iso|regionCode21|regionCode12)
#' @returns data.table of full spatially extended edgeTransport decision tree
#' @import data.table
#' @export

toolLoadDecisionTree <- function(regionAggregation = "iso") {

  # decisionTree.csv contains all possible branches of the decision tree
  decisionTree <- fread(system.file("extdata/helpersDecisionTree.csv",
                                    package = "edgeTransport", mustWork = TRUE), header = TRUE)
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
            setnames(completeDataSetAgg, "regionCode21", "region")
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

  findEntries <- function(category, dt){
    test <- dt[, lapply(.SD, function(x) grepl(category, x))]
    entries <- unique(dt[rowSums(test) > 0]$univocalName)
  }

  filterEntries <- sapply(categories, findEntries, dt = decisionTree)

  return(filterEntries)
}


#' Calculate shares based on discrete choice model.
#'
#' Function works for the use of generic preferences as well as for inconvenience costs.
#' If no preferences are provided the function sets them to one which is equivalent to
#' pure description by inconvenience costs.
#'
#' @author Johanna Hoppe
#' @param totPrice total price of an option in a branch of the decision tree
#' @param lambda exponent that determines the price sensitivity of the decision model
#' @param pref optional use of generic preference fators
#' @returns share of option in a branch of the decision tree
#' @import data.table
#' @export

calculateShares <- function(totPrice, lambda, pref = NULL) {

  if (!is.null(pref)) {
    if (sum(totPrice) == 0) {
      share <- 1 # e.g. for active modes with totPrice of zero on FV level
    } else {
      share <- pref * totPrice ^ lambda / (sum(pref * totPrice ^ lambda))
    }
  } else {
    share <- totPrice ^ lambda / (sum(totPrice ^ lambda))
  }

  return(share)
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

  highRes <- unique(helpers$dtTimeRes$period)
  highResUnivocalNames <- copy(helpers$dtTimeRes)
  highResUnivocalNames <- highResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  highResUnivocalNames <- highResUnivocalNames[test == TRUE]$univocalName

  if (c("level") %in% names(data)){
    #in this data format there is no univocalName
    #this needs to change temporarily
    dataFV <- merge(data[level == "FV"], helpers$decisionTree, by = intersect(names(data), names(helpers$decisionTree)))
    dataHighRes <- dataFV[univocalName %in% highResUnivocalNames][, univocalName := NULL]
    dataLowRes <- rbind(dataFV[!(univocalName %in% highResUnivocalNames)][, univocalName := NULL], data[!level == "FV"])
  } else {
    dataHighRes <- data[univocalName %in% highResUnivocalNames]
    dataLowRes <- data[!univocalName %in% highResUnivocalNames]
  }

  if (is.null(idcols)) {
    dataHighRes <- approx_dt(dataHighRes, highRes, "period", "value", extrapolate = TRUE)
    dataLowRes <- approx_dt(dataLowRes, helpers$lowTimeRes, "period", "value", extrapolate = TRUE)
  } else {
    dataHighRes <- approx_dt(dataHighRes, highRes, "period", "value", idxcols = idcols, extrapolate = TRUE)
    dataLowRes <- approx_dt(dataLowRes, helpers$lowTimeRes, "period", "value", idxcols = idcols, extrapolate = TRUE)
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
#' @param checkCompleteness if activated it is further checked, if the full data set is provided
#' @param fleetVars if activated the full data set is reduced to vehicle types that feature fleet tracking
#' @returns data.table
#' @import data.table
#' @export

toolOrderandCheck <- function(data, decisionTree, yrs = NULL, checkCompleteness = FALSE, fleetVars = FALSE) {

  data <- merge(data, decisionTree, by = c(intersect(names(data), names(decisionTree))))
  allCols <- c(names(decisionTree), "variable", "unit", "period", "value")
  isComplete <- all(names(data) %in% allCols) && all(allCols %in% names(data))
  if (isComplete == FALSE) stop(paste0(deparse(substitute(data), " misses columns or has additional columns")))
  if (anyNA(data) == TRUE) stop(paste0(deparse(substitute(data), " contains NAs")))
  data <- data[, ..allCols]

  if (checkCompleteness == TRUE){
    data <- data[period %in% yrs]
    if (fleetVars == TRUE) decisionTree <- decisionTree[grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)]
    allTimesteps <- data.table(period = yrs)[, allyears := "All"]
    decisionTree[, allyears := "All"]
    decisionTree <- merge(decisionTree,  allTimesteps, by = "allyears")
    test <- merge(data, decisionTree, by = intersect(names(data), names(decisionTree)), all = TRUE)
    if (anyNA(test) == TRUE) stop(paste0(deparse(substitute(data), " is not complete")))
  }

  return(data)
}
