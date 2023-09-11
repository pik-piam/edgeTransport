#' Read and build the complete structure of the edgeTransport decision tree
#' @author Johanna Hoppe
#' @param regionAggregation one of the different options for regional aggregation (iso|regionCode21|regionCode12)
#' @import data.table

toolLoadDecisiontree <- function(regionAggregation = "iso") {

  # decisionTree.csv contains all possible branches of the decision tree
  decisionTree <- fread(system.file("extdata/decisionTree.csv", package = "edgeTransport", mustWork = TRUE))
  decisionTree[, spatial := "all"]
  # Not all countries feature the same branches of the decision tree - Some vehicleTypes and modes are not
  # available in certain countries
  # Here we create the full structure of the nested decision tree differentiated for all countries to make it testable
  regionMap <- system.file("extdata", "regionmappingISOto21to12.csv",
                                 package = "mrtransport", mustWork = TRUE
  )
  regionMap <- fread(regionMap, skip = 0)
  ISOcountries <- regionMap[, c("countryCode")]
  setnames(ISOcountries, "CountryCode", "region")
  ISOcountries[, spatial := "all"]

  completeDataSet <- merge.data.table(decisionTree, ISOcountries, by = "spatial", allow.cartesian = TRUE)
  completeDataSet[, spatial := NULL]
  mapCountrySpecificVehicleTypes <- fread(system.file("extdata/mapCountrySpecificVehicleTypes.csv",
                                                      package = "edgeTransport", mustWork = TRUE))

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
         technology, univocalName, period)

  return(completeDataSetAgg)
  }
