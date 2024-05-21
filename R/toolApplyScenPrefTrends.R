#' Apply scenario specific adjustments to the preference trends
#' @author Johanna Hoppe
#' @param baselinePrefTrends Baseline preference trends
#' @param scenParPrefTrends Scenario parameters to be applied on the preference trends
#' @param GDPpcMER Per capita GDP based on market exchange rate
#' @param policyStartYear Year from which the scenario parameters are applied on the baseline preference trends
#' @param GDPcutoff Threshold used to categorize countries into different mitigation groups based on their GDP
#' @param helpers List containing several helpers used throughout the model
#' @param isICEban Switch to turn on ICE phase out policies
#' @returns Scenario specific preference trends
#' @import data.table

toolApplyScenPrefTrends <- function(baselinePrefTrends, scenParPrefTrends, GDPpcMER, policyStartYear, GDPcutoff, helpers, isICEban) {

  #function to apply mitigation factors
  applyLogisticTrend <- function(year, final, ysymm, speed, initial = 1){
    fct <- exp((year - ysymm)/speed)/(exp((year - ysymm)/speed) + 1)
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
  individualReg <- unique(mitigationFactors[!regionCat %in% c("above GDP cutoff", "below GDP cutoff")]$regionCat)
  GDPpcMER[, regionCat := ifelse(region %in% individualReg, region, regionCat)]
  mitigationFactors <- merge(mitigationFactors, GDPpcMER, by = "regionCat", allow.cartesian = TRUE, all.x = TRUE)[, regionCat := NULL]
  # apply mitigation factors
  PrefTrends <- merge(baselinePrefTrends, mitigationFactors, by = c("region", "level", "subsectorL1", "subsectorL2", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  PrefTrends[period  >= policyStartYear & !is.na(target), value := value * applyLogisticTrend(period, target, symmyr, speed)][, c("target", "symmyr", "speed") := NULL]

  # normalize preferences in each level
  PrefTrends[level == "S1S", value := value/max(value), by = c("region", "period", "sector")] # S1S: logit level: distances (e.g. short-medium, long)
  PrefTrends[level == "S2S1", value := value/max(value), by = c("region", "period", "sector", "subsectorL1")] # S2S1: logit level: modes/categories (e.g. walk, road, rail)
  PrefTrends[level == "S3S2", value := value/max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2")] # S3S2: logit level: modes/technologies (e.g. LDV, bus, Liquids)
  PrefTrends[level == "VS3", value := value/max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3")] # VS3: logit level: modes/technologies (e.g. cars, Liquids)
  PrefTrends[level == "FV", value := value/max(value), by = c("region", "period", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType")]  # FV: logit level: vehicle type (e.g. large car, moped)

  # Apply ICE ban if switched on
  if (isICEban) {
    #Ban is applied to EU28
    affectedRegions <- unique(helpers$regionmappingISOto21to12[regionCode12 == "EUR"]$regionCode21)
    #affectedRegions <- affectedRegions[!affectedRegions == "UKI"] currently we apply the ban also to UK
    PrefTrends[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
               value := ifelse(period == 2025, 0.98 * value[period == 2015], value), by = c("region","technology")]
    PrefTrends[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
               value := ifelse(period == 2030, 0.75 * value[period == 2015], value), by = c("region","technology")]
    PrefTrends[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
               value := ifelse(period == 2035, 0.3 * value[period == 2015], value), by = c("region","technology")]
    PrefTrends[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
               value := ifelse(period == 2040, 0.2 * value[period == 2015], value), by = c("region","technology")]
    PrefTrends[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
               value := ifelse(period == 2045, 0.1  * value[period == 2015], value), by = c("region","technology")]
    PrefTrends[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
               value := ifelse(period > 2045, value[period == 2015] * 0.05, value), by = c("region","technology")]
  }

  PrefTrends[, variable := paste0("Preference|", level)][, unit := "-"]
  # order
  PrefTrends <- PrefTrends[, c("region", "period", "technology", "vehicleType", "subsectorL3", "subsectorL2", "subsectorL1", "sector", "level", "variable", "unit", "value")]

return(PrefTrends)

}
