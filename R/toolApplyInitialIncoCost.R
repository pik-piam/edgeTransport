#' Apply regional differences for inconvenience costs based on ICE cost differences
#' 
#' @author Johanna Hoppe
#' @param combinedCost CAPEX and OPEX data 
#' @param incoCostStartVal tranport policy scenario
#' @import data.table


toolApplyInitialIncoCost <- function(combinedCost, incoCostStartVal, loadFactor, annualMileage, regionmappingISOto21to12) {
  
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
  
  
  ## use ICE price difference to DEU to introduce regional differentiation 
  reference <- combinedCost[! variable == "Fuel price" & period == 2020]
  reference <- reference[, .(value = sum(value), by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                        "technology", "univocalName", "period"))]
  reference[, .(average := mean(value)), by = c("region", "technology")]
  
  reference[, ratio := average/average[technology == "Liquids" & region == "DEU"]]
  allreg = merge(EU_val, REMINDp, by = "technology", allow.cartesian = T)
  allreg[!region %in% c("DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI"), value := value*((resc-1)/2+1)]
  allreg = allreg[, c("meanp", "resc") := NULL]

}