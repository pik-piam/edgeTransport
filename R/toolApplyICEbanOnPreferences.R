#' Apply ICE ban on vehicle types that feature preference factors
#'
#' @author Johanna Hoppe
#' @param preferenceTab data.table including preferences for all levels of the decision tree
#' @param helpers list of helpers
#' @returns Preferences in accordance to the ICE ban policy
#' @import data.table
#' @export

toolApplyICEbanOnPreferences <- function(preferenceTab, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  regionCode12 <- level <- region <- subsectorL1 <- subsectorL2 <- technology <- value <- period <- NULL
  # Ban is applied to EU28
  affectedRegions <- unique(helpers$regionmappingISOto21to12[regionCode12 == "EUR"]$regionCode21)
  # affectedRegions <- affectedRegions[!affectedRegions == "UKI"] currently we apply the ban also to UK
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2025, 0.98 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2030, 0.75 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2035, 0.3 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2040, 0.2 * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period == 2045, 0.1  * value[period == 2015], value), by = c("region", "technology")]
  preferenceTab[level == "FV" & region %in% affectedRegions & (subsectorL1 == "trn_freight_road" | subsectorL2 == "Bus") & technology %in% c("Liquids", "Gases"),
             value := ifelse(period > 2045, value * 0.05, value), by = c("region", "technology")]

  if (anyNA(preferenceTab)) stop("Something went wrong with the ICE ban application. Please check toolApplyICEbanOnPreferences()")

  return(preferenceTab)
}
