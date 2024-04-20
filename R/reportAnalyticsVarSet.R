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

reportAnalyticsVarSet <- function() {
  
  updatedEndogenousCosts <- list()
  policyMask <- list()
  rawEndogenousCost <- list()
  for (i in 1:length(endogenousCostsIterations)) {
    updatedEndogenousCosts[i] <- endogenousCostsIterations[[i]]$updatedEndogenousCosts
    policyMask[i] <- endogenousCostsIterations[[i]]$policyMask
    rawEndogenousCost[i] <- endogenousCostsIterations[[i]]$rawEndogenousCost
  }
  updatedEndogenousCosts <- rbindlist(updatedEndogenousCosts)
  policyMask <- rbindlist(policyMask)
  rawEndogenousCost <- rbindlist(rawEndogenousCost)
  analyticsData <- list(updatedEndogenousCosts, policyMask, rawEndogenousCost)
  outputVars <- append(outputVars, analyticsData)
  
}