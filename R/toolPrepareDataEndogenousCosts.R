#' Format all cost components
#'
#' @author Johanna Hoppe
#' @param inputData List containing inputData
#' @param lambdas exponents for discrete choice calculation
#' @param helpers List with helpers
#' @returns data.table including all cost components
#' @import data.table
#' @export

toolPrepareDataEndogenousCosts <- function(inputData, lambdas, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  univocalName <- type <- . <- value <- region <- period <- technology <- unit <- variable <- NULL

  # format input data
  # merge input data with decision tree
  # select only modes that feature inconvenience costs
  # bind with inconvenience cost start values
  monetaryCosts <- inputData$combinedCAPEXandOPEX[univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W]
  monetaryCosts[, type := "Monetary costs"]
  monetaryCosts <- monetaryCosts[, .(value = sum(value)), by = .(region, period, technology, univocalName, type, unit)]
  monetaryCosts[, variable := "CAPEX and OPEX"]
  inconvenienceCosts <- inputData$initialIncoCosts
  inconvenienceCosts[, type := "Inconvenience costs"]

  # insert NAs for future years to be filled in updateEndogenousCosts()
  highTimeRes <- data.table(period = unique(helpers$dtTimeRes$period))
  prevTimeRes <- unique(inconvenienceCosts$period)
  inconvenienceCosts <- approx_dt(inconvenienceCosts, highTimeRes, "period", "value", extrapolate = TRUE)
  inconvenienceCosts[!period %in% prevTimeRes, value := NA]
  combinedCosts <- rbind(monetaryCosts, inconvenienceCosts)
  combinedCosts <- merge(combinedCosts, helpers$decisionTree, by = intersect(names(combinedCosts),
                                                                             names(helpers$decisionTree)), all.x = TRUE)

  # calculate FS3 share, hence the share of each technology in the overall car sales, in 2020
  # (needed to calculate the vehicle sales depreciating in time
  # to get a proxy for the fleet share in the iterative section)
  timesteps <- unique(combinedCosts$period)
  FS3share <- toolCalculateFS3share(combinedCosts, timesteps[timesteps <= 2020], inputData$timeValueCosts,
                                    inputData$scenSpecPrefTrends, lambdas, helpers)

  # merge back to combined costs
  combinedCosts <- merge(combinedCosts, FS3share,
                         by = intersect(names(combinedCosts), names(FS3share)),
                         all = TRUE)

  return(combinedCosts)
}
