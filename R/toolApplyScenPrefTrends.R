#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param enInt energy enInt input data supplied by mrtransport
#' @param polScen tranport policy scenario
#' @import data.table
#' @importFrom rmndt approx_dt


toolApplyScenPrefTrends <- function(historicalPrefs, baselinePrefTrends, mitigationFactors, mitigationTechMap, yrs, GDPpcMER, GDPcutoff) {

  #function to apply mitigation factors
  applyLogisticTrend <- function(yrs, final, ysymm, speed, initial = 1){
    fct <- exp((yrs - ysymm)/speed)/(exp((yrs - ysymm)/speed) + 1)
    initial + fct * (final - initial)
  }

  # change to long-format
  baselinePrefTrends <- melt(baselinePrefTrends, variable.name = "period", id.vars = c("region", "level", "sector", "subsectorL1", "subsectorL2", "vehicleType", "technology"))
  # get rid of levels as period is treated as a factor after using melt (not supported by approx_dt)
  baselinePrefTrends[, period := as.numeric(as.character(period))]

  baselinePrefTrends <- approx_dt(baselinePrefTrends, yrs, "period", "value", idxcols = setdiff(names(baselinePrefTrends), c("period", "value")), extrapolate = TRUE)

  # restructure mitigation factors
  # resolve techmap
  mitigationFactors <- merge(mitigationTechMap, mitigationFactors, by = "FVvehvar", all.y = TRUE)
  mitigationFactors[is.na(vehicleType), vehicleType := ""][, FVvehvar := NULL]
  # implement differentiation by GDP and treatment of single region entries
  GDPpcMER[, regionCat := ifelse(value > GDPcutoff, "above GDP cutoff", "below GDP cutoff")]
  GDPpcMER[, c("region", "regionCat")]
  # some regions have individual mitigation factors
  individualReg <- mitigationFactors[!regionCat %in% c("above GDP cutoff", "below GDP cutoff")]$regionCat
  GDPpcMER[, regionCat := ifelse(regionCat %in% individualReg, region, regionCat)]
  mitigationFactors <- merge(mitigationFactors, GDPpcMER, by = "regionCat", allow.cartesian = TRUE, all.x = TRUE)[, regionCat := NULL]
  # apply mitigation factors
  PrefTrends <- merge(baselinePrefTrends, mitigationFactors, by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE)
  PrefTrends[period > 2020 & !is.na(target), value := value * applyLogisticTrend(period, target, symmyr, speed)][, c("target", "symmyr", "speed") := NULL]
  # approximate missing timesteps

  # normalize preferences in each level
  PrefTrends[level == "S1S", value := value/max(value), by = c("region", "sector")]
  PrefTrends[level == "S2S1", value := value/max(value), by = c("region", "sector", "subsectorL1")]
  PrefTrends[level == "S3S2", value := value/max(value), by = c("region", "sector", "subsectorL1", "subsectorL2")]
  PrefTrends[level == "VS3", value := value/max(value), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3")]
  PrefTrends[level == "FV", value := value/max(value), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")]

return(PrefTrends)

}
