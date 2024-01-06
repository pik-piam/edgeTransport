#' Read and build the complete structure of the edgeTransport decision tree
#' @author Johanna Hoppe
#' @param regionAggregation one of the different options for regional aggregation (iso|regionCode21|regionCode12)
#' @returns data.table of full spatially extended edgeTransport decision tree
#' @import data.table
#' @export

toolLoadDecisionTree <- function(regionAggregation = "iso") {

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
            completeDataSetAgg <- unique(completeDataSetAgg[, c("regionCode21", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName")])
            setnames(completeDataSetAgg, "regionCode21", "region")
          },
          "regionCode12" = {
            completeDataSetAgg <- merge(completeDataSet, regionMap, by = "region")
            completeDataSetAgg <- unique(completeDataSetAgg[, c("regionCode12", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "univocalName")])
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
#' @returns list of categories and their associated univocalNames e.g. trn_pass as list entry containing a vector c("Compact Car", "HSR", ..)
#' @import data.table
#' @export

getFilterEntriesUnivocalName <- function(categories, decisionTree) {

  findEntries <- function(category, dt){
    test <- dt[, lapply(.SD, function(x) grepl(category, x))]
    entries <- unique(dt[rowSums(test) > 0]$univocalName)
  }

  filterEntries <- sapply(categories, findEntries, dt = decisionTree, USE.NAMES = TRUE)

}


#' Calculate shares based on discrete choice model.
#'
#' Function works for the use of generic preferences as well as for inconvenience costs.
#' If no preferences are provided the function sets them to one which is equivalent to pure description by inconvenience costs.
#'
#' @author Johanna Hoppe
#' @param dt data.table containing
#' @param category
#' @returns data.table
#' @import data.table
#' @export

calculateShares <- function(totPrice, lambda, pref = NULL) {

  if (!is.null(pref)) {
    share <- pref * totPrice ^ lambda / (sum(pref * totPrice ^ lambda))
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
#' @param dt data.table containing
#' @param category
#' @returns data.table
#' @import data.table
#' @export

aggregateNextHigherDecisionLevel <- function(dt, category) {



}
