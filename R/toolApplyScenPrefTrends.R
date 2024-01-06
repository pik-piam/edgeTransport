#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param enInt energy enInt input data supplied by mrtransport
#' @param polScen tranport policy scenario
#' @import data.table
#' @importFrom rmndt approx_dt


toolApplyScenPrefTrends <- function(baselinePrefTrends, scenParPrefTrends, GDPpcMER, years, policyStartYear, helpers) {

  #function to apply mitigation factors
  applyLogisticTrend <- function(years, final, ysymm, speed, initial = 1){
    fct <- exp((years - ysymm)/speed)/(exp((years - ysymm)/speed) + 1)
    initial + fct * (final - initial)
  }

  # restructure mitigation factors provided in scenParPrefTrends
  # resolve techmap
  mitigationFactors <- merge(helpers$mitigationTechMap[, c("vehicleType", "FVvehvar")], scenParPrefTrends, by = "FVvehvar", all.y = TRUE, allow.cartesian = TRUE)
  mitigationFactors[is.na(vehicleType), vehicleType := ""][, FVvehvar := NULL]
  # implement differentiation by GDP and treatment of single region entries
  GDPpcMER <- GDPpcMER[period == 2020][, period := NULL]
  GDPpcMER[, regionCat := ifelse(value > GDPcutoff, "above GDP cutoff", "below GDP cutoff")]
  GDPpcMER <- GDPpcMER[, c("region", "regionCat")]
  # some regions have individual mitigation factors
  individualReg <- mitigationFactors[!regionCat %in% c("above GDP cutoff", "below GDP cutoff")]$regionCat
  GDPpcMER[, regionCat := ifelse(regionCat %in% individualReg, region, regionCat)]
  mitigationFactors <- merge(mitigationFactors, GDPpcMER, by = "regionCat", allow.cartesian = TRUE, all.x = TRUE)[, regionCat := NULL]
  # apply mitigation factors
  PrefTrends <- merge(baselinePrefTrends, mitigationFactors, by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  PrefTrends[period  >= policyStartYear & !is.na(target), value := value * applyLogisticTrend(period, target, symmyr, speed)][, c("target", "symmyr", "speed") := NULL]
  # approximate missing timesteps

  # normalize preferences in each level
  PrefTrends[level == "S1S", value := value/max(value), by = c("region", "period", "sector")]
  PrefTrends[level == "S2S1", value := value/max(value), by = c("region", "period", "sector", "subsectorL1")]
  PrefTrends[level == "S3S2", value := value/max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]
  PrefTrends[level == "VS3", value := value/max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  PrefTrends[level == "FV", value := value/max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")]

  # order
  PrefTrends <- PrefTrends[, c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "value")]

return(PrefTrends)

}
