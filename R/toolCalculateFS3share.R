#' @title toolCalculateFS3share
#' @description Calculates fuel subsector L3 shares
#'
#' @param endoCostData data.table containing all cost components on technology level
#' @param timesteps years for which to calculate FS3 shares
#' @param timeValue data.table containing mode specific time value costs based on speed and gdp
#' @param lambdas data.table containing exponents for discrete choice calculation
#' @param preferences data.table containing preference trends
#' @param helpers list containing helpers like mappings, decisionTree etc.
#' @return data.table containing all cost components on technology level and their respective FS3 shares
#' @author Johanna Hoppe
#' @import data.table
#' @export


toolCalculateFS3share <- function(endoCostData, timesteps, timeValue, preferences, lambdas, helpers){

  #time costs in [US$2017/pkm] for traveling with mode X in region Y
  timeValueCosts <- merge(timeValue, unique(helpers$decisionTree[, -c("technology")]), by = c("region", "univocalName"), all.x = TRUE)
  timeValueCosts[, type := "Travel time"][, c("unit", "univocalName", "variable") := NULL]
  if (length(timesteps) > 1) {
    timeValueCosts <- approx_dt(timeValueCosts, timesteps, "period", "value",
                              c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "type"),
                                extrapolate = FALSE)
  } else {
    timeValueCosts <- timeValueCosts[period %in% timesteps]
  }

  # first the FV share needs to be calculated
  FVshare <- copy(endoCostData[period %in% timesteps])
  FVshare <- FVshare[, .(totPrice = sum(value)), by = .(region, period, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology)]
  FVshare <- merge(FVshare, lambdas[level == "FV"], by = c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType"))
  FVshare[, FVshare := calculateShares(totPrice, lambda), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")][, c("totPrice", "lambda", "level") := NULL]
  FVshare[, test := sum(FVshare), by = c("region", "period", "vehicleType")]
  if (nrow(FVshare[test < 0.9999 | test > 1.0001]) > 0) stop("FV shares in toolPrepareEndogenousCosts were not calculated correctly")
  FVshare[, test := NULL]
  # second the VS3 share needs to be calculated
  VS3share <- copy(endoCostData[period %in% timesteps & type == "Monetary costs"])
  VS3share <- merge(VS3share, FVshare, by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology"))
  VS3share <- VS3share[, .(value = sum(FVshare * value)), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "type")]
  VS3share <- rbind(VS3share, timeValueCosts[vehicleType %in% unique(VS3share$vehicleType)])
  VS3share <- VS3share[, .(totPrice = sum(value)), by = .(region, period, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType)]
  lambdasVS3 <- lambdas[level == "VS3"]
  lambdasVS3[, c("vehicleType", "level") := NULL]
  VS3share <- merge(VS3share, lambdasVS3, by = intersect(names(VS3share), names(lambdasVS3)))
  preftrends <- preferences[level == "VS3"][, c("level", "variable", "technology", "unit") := NULL]
  preftrends <- approx_dt(preftrends, unique(helpers$dtTimeRes$period), "period", "value")
  setnames(preftrends, "value", "pref")
  VS3share <- merge(VS3share, preftrends, by = intersect(names(VS3share), names(preftrends)), all.x = TRUE)
  VS3share[, VS3share := calculateShares(totPrice, lambda, pref), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  VS3share[, c("totPrice", "lambda") := NULL]
  VS3share[, test := sum(VS3share), by = c("region", "period", "subsectorL3")]
  if (nrow(VS3share[test < 0.9999 | test > 1.0001]) > 0) stop("VS3 shares in toolPrepareEndogenousCosts were not calculated correctly")
  VS3share[, test := NULL]
  shares <- merge(FVshare, VS3share, by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType"), allow.cartesian = TRUE)
  shares <- shares[, .(FS3share = sum(VS3share * FVshare)), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "technology")]
  shares[, test := sum(FS3share), by = c("region", "period", "subsectorL3")]
  if (nrow(shares[test < 0.9999 | test > 1.0001]) > 0) stop("FS3 shares in toolPrepareEndogenousCosts were not calculated correctly")
  shares[, test := NULL]

  return(shares)
}


