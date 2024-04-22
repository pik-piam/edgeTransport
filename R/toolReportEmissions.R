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
#' @importFrom gdxrrw rgdx.scalar, rgdx.param
#' @import data.table
#' @export

toolReportEmissions <- function(dtFE, gdx, prefix, helpers) {
  browser()
  # Get emission factors from REMIND gdx
  GtCtoGtCO2 <- rgdx.scalar(gdx, "sm_c_2_co2", ts = FALSE)
  EJ2TWa <- rgdx.scalar(gdx, "sm_EJ_2_TWa", ts = FALSE)
  gdxColNames <- c("period", "region", "from", "to", "conversionTechnology", "emissionType", "value")
  emissionFactors <- as.data.table(rgdx.param(gdx, "pm_emifac", names = gdxColNames))
  # liquid fuels
  emissionFactors[from == "seliqfos" & to ==  "fedie" & emissionType == "co2",  emissionFactor := value * GtCtoGtCO2 *1e3 * EJ2TWa]
  emissionFactors[from == "seliqfos" & to ==  "fepet" & emissionType == "co2",  emissionFactor := value * GtCtoGtCO2 *1e3 * EJ2TWa]
  # gaseous fuels
  emissionFactors[from == "segafos" & to ==  "fegas" & emissionType == "co2",  emissionFactor := value * GtCtoGtCO2 *1e3 * EJ2TWa]
  # unit MtCO2/EJ
  emissionFactors <- emissionFactors[!is.na(emissionFactor)]
  emissionFactors <- emissionFactors[, c("region", "period", "to", "emissionFactor")]

  ## attribute explicitly fuel used to the FE values
  dtFE <- copy(dtFE)
  dtFE[univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                           helpers$filterEntries$trn_pass_road_LDV_2W) & technology %in% c("Liquids", "Hybrid electric"), to := "fepet"]
  dtFE[!(univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                             helpers$filterEntries$trn_pass_road_LDV_2W)) & technology == "Liquids", to := "fedie"]
  dtFE[technology == "Gases", to := "fegas"]

  emissionFactors[, period := as.double(as.character(period))]
  ## merge with emission factors
  emi <- merge(dtFE, emissionFactors, by = c("to", "region", "period"), all.x = TRUE)
  emi[is.na(emissionFactor), emissionFactor := 0]
  ## calculate emissions and attribute variable and unit names
  emi[, value := value * emissionFactor][, variable := paste0("Emi|CO2|", prefix)][, unit := "Mt CO2/yr"]
  emi[, c("to", "emissionFactor") := NULL]

  return(emi)
}
