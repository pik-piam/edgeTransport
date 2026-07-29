#' Assemble the list of helpers shared across the EDGE-T model
#'
#' Derives filterEntries (associated univocalNames per category) and the mixed temporal
#' resolution, then collects them together with the structural mappings into the helpers
#' list used throughout the model. Shared by toolLoadInputs() (full input preparation) and
#' toolReLoadInputs() (reload from RDS in iterative mode) so that the two paths cannot drift
#' apart.
#'
#' @author Alex K. Hagen
#' @param decisionTree data.table of the full edgeTransport decision tree
#' @param timeResDataBase data.table with univocalName and period used to derive the temporal
#'          resolution (see toolDeriveTimeRes())
#' @returns list of helpers used throughout the model
#' @import data.table
#' @export

toolBuildHelpers <- function(decisionTree,
                             timeResDataBase) {

  ## helpers
  mitigationTechMap <- fread(system.file("extdata", "helpersMitigationTechmap.csv",
                                         package = "edgeTransport"))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                package = "edgeTransport"))
  reportingNames <- fread(system.file("extdata", "helpersReportingNames.csv",
                                      package = "edgeTransport"), skip = 1)
  reportingAggregation <- fread(system.file("extdata", "helpersReportingAggregation.csv",
                                            package = "edgeTransport"), skip = 1)
  mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv",
                                       package = "edgeTransport", mustWork = TRUE))

  # categories for filtering data
  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_3W", "trn_pass_road_LDV_2W",
                  "trn_freight_road", "trn_pass", "trn_freight")
  filterEntries <- getFilterEntriesUnivocalName(categories, decisionTree)
  filterEntries[["trackedFleet"]] <- c(filterEntries[["trn_pass_road_LDV_4W"]],
                                       filterEntries[["trn_freight_road"]],
                                       getFilterEntriesUnivocalName("Bus", decisionTree)[["Bus"]])

  # vehicle types that feature fleet tracking get a different temporal resolution
  timeRes <- toolDeriveTimeRes(timeResDataBase)

  helpers <- list(
    decisionTree = decisionTree,
    regionmappingISOto21to12 = regionmappingISOto21to12,
    mitigationTechMap = mitigationTechMap,
    mapEdgeToREMIND = mapEdgeToREMIND,
    filterEntries = filterEntries,
    dtTimeRes = timeRes$dtTimeRes,
    lowTimeRes = timeRes$lowTimeRes,
    reportingNames = reportingNames,
    reportingAggregation = reportingAggregation
  )

  return(helpers)
}
