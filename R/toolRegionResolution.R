#' Disaggregate data from the 12 REMIND regions to the 21 EDGE-T regions
#' @author Alex K. Hagen
#' @param dt data.table on 12 regions with columns region, period and value
#' @param helpers list of helpers
#' @param weights optional data.table on 21 regions with columns region and value, plus any columns shared with dt
#'                along which the weights differ (e.g. period, sector). Without weights, the 12-region value is
#'                copied to each 21-region; with weights, it is split according to the weight shares
#' @returns data.table on 21 regions with the columns of dt, in the order of merge(dt, regionMap, by = "regionCode12"):
#'          rows sorted by the 12-region, the column region last
#' @details To save memory, the columns of dt and weights are renamed and dt is sorted in place.
#'          Reassign the result to the input.
#' @import data.table

toolDisaggregateRegions12to21 <- function(dt, helpers, weights = NULL) {
  regionMap <- unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")])

  cols <- setdiff(names(dt), "region")
  setnames(dt, "region", "regionCode12")
  # the disaggregation keeps the row order of dt, so sorting dt gives the row order of a merge on the 12-region
  setorderv(dt, "regionCode12")
  if (!is.null(weights)) {
    setnames(weights, c("region", "value"), c("regionCode21", "weight"))
  }
  dataColumns <- setdiff(names(dt), c("regionCode12", "value", names(weights)))

  data <- rmndt::disaggregate_dt(dt, regionMap,
                                 fewcol = "regionCode12", manycol = "regionCode21",
                                 datacols = dataColumns, weights = weights)
  if (anyNA(data$value)) {
    stop("Error in regional disaggregation 12 to 21 regions: weights are missing for some entries.")
  }
  setnames(data, "regionCode21", "region")
  setcolorder(data, c(cols, "region"))

  return(data)
}

#' Aggregate data from the 21 EDGE-T regions to the 12 REMIND regions
#' @author Alex K. Hagen
#' @param dt data.table on 21 regions with columns region, period and value
#' @param helpers list of helpers
#' @param weights optional data.table on 21 regions with columns region and value, plus any columns shared with dt
#'                along which the weights differ. Without weights, the 21-region values are summed (extensive
#'                variables); with weights, the weighted average is calculated (intensive variables)
#' @returns data.table on 12 regions with the columns of dt, in the order of rmndt::aggregate_dt(): the column
#'          region first
#' @details To save memory, the columns of dt and weights are renamed in place. Reassign the result to the input.
#' @import data.table

toolAggregateRegions21to12 <- function(dt, helpers, weights = NULL) {
  regionMap <- unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")])

  dataColumns <- setdiff(names(dt), c("region", "period", "value"))
  if (!is.null(weights)) {
    if (anyNA(weights$value) || !setequal(unique(dt$region), unique(weights$region))) {
      stop("Error in regional aggregation 21 to 12 regions: weights contain NA or do not cover all regions.")
    }
    setnames(weights, c("region", "value"), c("regionCode21", "weight"))
  }
  setnames(dt, "region", "regionCode21")

  data <- rmndt::aggregate_dt(dt, regionMap,
                              fewcol = "regionCode12", manycol = "regionCode21",
                              datacols = dataColumns, weights = weights, yearcol = "period")
  setnames(data, "regionCode12", "region")

  return(data)
}
