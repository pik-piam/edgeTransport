#'Report emissions allocated to the transport sector.
#'Only direkt emissions from liquid or gaseous energy carriers used in ICEs are considered.
#'Indirect emissions from electricity and hydrogen are not allocated to the transport sector.
#'
#' @param dtFE Final energy data for liquids and gases
#' @param gdxPath Path to REMIND fulldata.gdx containing emission factors
#' @param prefix Prefix that specifies the emissions we are referring to in the variable name (either tailpipe or demand)
#' @param helpers
#'
#' @returns Emissions data for provided values in dtFE
#' @author Johanna Hoppe
#' @importFrom gdxrrw rgdx.scalar, rgdx.param
#' @import data.table
#' @export

toolReportEmissions <- function(dtFE, gdxPath, prefix, helpers) {

  # Get emission factors from REMIND gdx
  GtCtoGtCO2 <- rgdx.scalar(gdxPath, "sm_c_2_co2", ts = FALSE)
  EJ2TWa <- rgdx.scalar(gdxPath, "sm_EJ_2_TWa", ts = FALSE)
  gdxColNames <- c("period", "region", "from", "to", "conversionTechnology", "emissionType", "value")
  emissionFactors <- as.data.table(rgdx.param(gdxPath, "pm_emifac", names = gdxColNames))
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
                           helpers$filterEntries$trn_pass_road_LDV_2W) & technology == "Liquids", to := "fepet"]
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
