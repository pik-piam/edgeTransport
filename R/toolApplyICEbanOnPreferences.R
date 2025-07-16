#' Apply ICE ban on vehicle types that feature preference factors
#'
#' @author Johanna Hoppe, Alex K. Hagen
#' @param preferenceTab data.table including preferences for all levels of the decision tree
#' @param helpers list of helpers
#' @param ICEbanYears sequence of years in which ICEban is applied
#' @returns Preferences in accordance to the ICE ban policy
#' @import data.table
#' @export

toolApplyICEbanOnPreferences <- function(preferenceTab, helpers, ICEbanYears) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  regionCode12 <- level <- region <- subsectorL1 <- subsectorL2 <- technology <- value <- period <- NULL
  # Ban is applied to EU28
  affectedRegions <- unique(helpers$regionmappingISOto21to12[regionCode12 == "EUR"]$regionCode21)
  # affectedRegions <- affectedRegions[!affectedRegions == "UKI"] currently we apply the ban also to UK
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2025 & period %in% ICEbanYears, 2 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2030 & period %in% ICEbanYears, 1 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2035 & period %in% ICEbanYears, 0.7 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2040 & period %in% ICEbanYears, 0.25 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2045 & period %in% ICEbanYears, 0.2  * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period > 2045 & period %in% ICEbanYears, value * 0.1, value), by = c("region", "technology")]

  if (anyNA(preferenceTab)) stop("Something went wrong with the ICE ban application. Please check toolApplyICEbanOnPreferences()")

  return(preferenceTab)
}
