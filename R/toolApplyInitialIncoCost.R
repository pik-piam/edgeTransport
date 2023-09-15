#' Apply regional differences for inconvenience costs based on ICE cost differences
#' 
#' @author Johanna Hoppe
#' @param combinedCost CAPEX and OPEX data 
#' @param incoCostStartVal tranport policy scenario
#' @import data.table


toolApplyInitialIncoCost <- function(combinedCost, incoCostStartVal, annuity, loadFactor, annualMileage, regionmappingISOto21to12, decisionTree, mitigationTechMap, yrs) {
  
  incoCostStartVal <- melt(incoCostStartVal, id.vars = c("region", "incoCostType", "FVvehvar", "technology", "unit"), value.name = "period")
  
  # map incocost start values on regions
  # they are provided either globally (GLO) or for single regions (out of 12 or 21)
  # choose first the individual ones 
  individualIncoCost21 <- incoCostStartVal[region %in% regionmappingISOto21to12$regionCode21]
  individualIncoCost12 <- merge(incoCostStartVal, unique(regionmappingISOto21to12[, c("regionCode21", "regionCode12")]), 
                                by.x = "region", by.y = "regionCode12", allow.cartesian = TRUE)
  individualIncoCost12[, region := NULL]
  setnames(individualIncoCost12, "regionCode21", "region")
  GLO <- regionmappingISOto21to12[, "regionCode21"][, region := "GLO"]
  incoCostGLO <- merge(incoCostStartVal, GLO, by = "region", allow.cartesian = TRUE)
  incoCostGLO[, region := NULL]
  setnames(incoCostGLO, "regionCode21", "region")
  incoCostStartValReg <- rbind(individualIncoCost21, individualIncoCost12, incoCostGLO[!region %in% individualIncoCost21$region 
                                                                                       & !region %in% individualIncoCost12$region])
  
  ## use ICE price difference to DEU to introduce regional differentiation in all regions apart from EU regions
  # this is done for the 2020 value of model availability and range anxiety and for all years for risk aversion and sationsAvailability for BEV and hybrid electric
  # Q: This procedure is not really straight forward - maybe we get to a more systemic approach?
  EUreg <- unique(regionmappingISOto21to12[regionCode12 == "EUR"])$regionCode21
  reference <- combinedCost[! variable == "Fuel price" & period == 2020]
  reference <- reference[, .(value = sum(value), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                        "technology", "univocalName", "period"))]
  reference[, .(average := mean(value)), by = c("region", "technology")]
  
  reference[, ratio := average/average[technology == "Liquids" & region == "DEU"]]
  incoCostStartValReg <- merge(incoCostStartValReg, reference, by = c("region", "technology"), allow.cartesian = TRUE)
  # Q: The application of the ratio is also a bit weird
  incoCostStartValReg[!region %in% EUreg & period == 2020 & incoCostType %in% c("modelAvailability", "rangeAnxiety"), value := value * ((ratio - 1) / 2 + 1)]
  incoCostStartValReg[!region %in% EUreg & incoCostType == "riskAversion", value := value * ((ratio - 1) / 2 + 1)]
  incoCostStartValReg[!region %in% EUreg & incoCostType == "stationsAvailability" & technology %in% c("BEV", "Hybrid electric"), value := value * ((ratio - 1) / 2 + 1)]
  incoCostStartValReg[, ratio := NULL]
  
  # map data on decision tree and interpolate missing timesteps <= 2020
  incoCostStartValReg <- merge(mitigationTechMap, incoCostStartValReg, by = "FVvehvar", all.y = TRUE)[, FVvehvar := NULL]
  incoCostStartValReg <- approx_dt(incoCostStartValReg, yrs[yrs <= 2020], "period", "value", c("region", "incoCostType", "vehicleType", "technology", "unit"), extrapolate = TRUE)
  incoCostStartValReg <- merge(decisionTree, incoCostStartValReg, by = c("region", "vehicleType", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  
  # convert to US$2005/pkm
  # Annualize and discount to convert to US$2005/veh/yr
  annualizedincoCostStartVal <- merge(incoCostStartValReg, annuity, by = vehicleType, allow.cartesian = TRUE)
  annualizedincoCostStartVal[, value := value * annuity][, unit := "US$2005/veh/yr"]
  
  loadFactor[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  annualMileage[, c("variable", "unit") := NULL]
  setnames(annualMileage, "value", "annualMileage")
  
  annualizedincoCostStartVal <- merge(annualizedincoCostStartVal, loadFactor, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                                                "technology", "univocalName", "period"), all.x = TRUE)
  annualizedincoCostStartVal <- merge(annualizedincoCostStartVal, annualMileage, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                                                "technology", "univocalName", "period"), all.x = TRUE)
  annualizedincoCostStartVal[, value := value / (annualMileage * loadFactor)][, c(loadFactor, annualMileage) := NULL]
  annualizedincoCostStartVal[, unit := ifelse(sector %in% c("trn_pass", "trn_aviation_intl", "US$2005/pkm", "US$2005/tkm"))]
  
  if (anyNA(annualizedincoCostStartVal) == TRUE) {
    stop("Inconvenience cost start values contain NAs")
  }
  
  return(annualizedincoCostStartVal)
}