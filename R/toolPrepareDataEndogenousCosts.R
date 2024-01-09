
toolPrepareDataEndogenousCosts <- function(inputData, lambdas, policyStartYear, helpers) {

  # format input data
  # merge input data with decision tree
  # select only modes that feature inconvenience costs
  # bind with inconvenience cost start values
  # apply yearly resolution
  monetaryCosts <- inputData$combinedCAPEXandOPEX
  monetaryCosts[, type := "Monetary costs"]
  monetaryCosts <- monetaryCosts[, .(value = sum(value)), by = .(region, period, technology, univocalName, type, unit)]
  monetaryCosts[, variable := "CAPEX and OPEX"]
  inconvenienceCosts <- inputData$initialIncoCosts
  inconvenienceCosts[, type := "Inconvenience costs"]

  combinedCosts <- rbind(monetaryCosts, inconvenienceCosts)
  combinedCosts <- merge(combinedCosts, helpers$decisionTree, by = c("region", "univocalName", "technology"), all = TRUE)
  if (anyNA(combinedCosts)) {
    stop("Some data got lost on the way to the endogenous cost updates. Please check combinedCosts at the beginning of toolPrepareDataEndogenousCosts.")
  }

  # extend to one year timesteps. Inconvenience costs are NA after policy startyear and are updated endogenously in updateEndogenousCosts()
  combinedCosts <- combinedCosts[subsectorL3 == "trn_pass_road_LDV_4W"]
  timesteps <- seq(min(unique(combinedCosts$period)), 2100, 1)
  combinedCosts <- approx_dt(combinedCosts, timesteps, "period", "value",
                  c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType","technology", "univocalName", "variable", "unit", "type"),
                  extrapolate = FALSE, keepna = TRUE)

  # calculate FS3 share, hence the share of each technology in the overall car sales, in 2020 (needed to calculate the vehicle sales depreciating in time
  # to get a proxy for the fleet share in the iterative section)
  FS3share <- toolCalculateFS3share(copy(combinedCosts), timesteps[timesteps < policyStartYear], inputData$timeValueCosts, lambdas, helpers)

  # merge back to combined costs
  combinedCosts <- merge(combinedCosts, FS3share, by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "technology"), all = TRUE)

  return(combinedCosts)
}
