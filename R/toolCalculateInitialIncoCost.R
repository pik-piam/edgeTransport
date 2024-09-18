#' Apply regional differences for inconvenience cost start values based on ICE cost differences
#'
#' @author Johanna Hoppe
#' @param combinedCost total cost of ownership
#' @param incoCostStartVal start values for inconvenience costs
#' @param annuity calculated annuity for different vehicle types
#' @param loadFactor load factor data
#' @param annualMileage annual mileage data
#' @param helpers list with helpers
#' @import data.table
#' @returns data.table including initial inconvenience costs from 1990-2020 for LDV 4W US$2017/(p|t)km

toolCalculateInitialIncoCost <- function(combinedCost, incoCostStartVal, annuity, loadFactor, annualMileage, helpers) {

  incoCostStartVal <- copy(incoCostStartVal)
  incoCostStartVal <- melt(incoCostStartVal, id.vars = c("region", "incoCostType", "FVvehvar", "technology", "unit"),
                           variable.name = "period")
  incoCostStartVal[, value := as.double(value)]
  # map incocost start values on regions
  # they are provided either globally (GLO) or for single regions (out of 12 or 21)
  # choose first the individual ones
  individualIncoCost21 <- incoCostStartVal[region %in% helpers$regionmappingISOto21to12$regionCode21]
  individualIncoCost12 <- merge(incoCostStartVal,
                                unique(helpers$regionmappingISOto21to12[, c("regionCode21", "regionCode12")]),
                                by.x = "region", by.y = "regionCode12", allow.cartesian = TRUE)
  individualIncoCost12[, region := NULL]
  setnames(individualIncoCost12, "regionCode21", "region")
  GLO <- unique(helpers$regionmappingISOto21to12[, "regionCode21"])[, region := "GLO"]
  incoCostGLO <- merge(incoCostStartVal, GLO, by = "region", allow.cartesian = TRUE)
  incoCostGLO[, region := NULL]
  setnames(incoCostGLO, "regionCode21", "region")
  incoCostStartValReg <- rbind(individualIncoCost21, individualIncoCost12[!region %in% individualIncoCost21$region],
                               incoCostGLO[!region %in% individualIncoCost21$region
                                           & !region %in% individualIncoCost12$region])
  ## use ICE price difference to DEU to introduce regional differentiation in all regions apart from EU regions
  # this is done for the 2020 value of model availability and range anxiety and for all years for risk aversion
  # and sationsAvailability for BEV and Hybrid electric
  # Q: This procedure is not really straight forward - maybe we get to a more systemic approach?
  EUreg <- unique(helpers$regionmappingISOto21to12[regionCode12 == "EUR"])$regionCode21
  reference <- combinedCost[! variable == "Fuel price" & period == 2020]
  reference <- reference[, .(value = sum(value)), by = c("region", "univocalName", "technology", "period")]
  reference <- reference[, .(average = mean(value)), by = c("region", "technology")]

  reference[, ratio := average / average[technology == "Liquids" & region == "DEU"]][, average := NULL]
  incoCostStartValReg <- merge(incoCostStartValReg, reference, by = c("region", "technology"), allow.cartesian = TRUE)
  # Q: The application of the ratio is also a bit weird
  incoCostStartValReg[!region %in% EUreg & period == 2020 & incoCostType %in% c("modelAvailability", "rangeAnxiety"),
                      value := value * ((ratio - 1) / 2 + 1)]
  incoCostStartValReg[!region %in% EUreg & incoCostType == "riskAversion", value := value * ((ratio - 1) / 2 + 1)]
  incoCostStartValReg[!region %in% EUreg & incoCostType == "stationsAvailability"
                      & technology %in% c("BEV", "Hybrid electric"), value := value * ((ratio - 1) / 2 + 1)]
  incoCostStartValReg[, ratio := NULL]

  # map data on decision tree and interpolate missing timesteps <= 2020
  incoCostStartValReg <- merge(helpers$mitigationTechMap[, c("FVvehvar", "univocalName")], incoCostStartValReg,
                               by = "FVvehvar", all.y = TRUE, allow.cartesian = TRUE)[, FVvehvar := NULL]
  # get rid of levels for the years, as approx_dt cannot handle them
  incoCostStartValReg[, period := as.numeric(as.character(period))]
  incoCostStartValReg <- approx_dt(incoCostStartValReg, unique(helpers$dtTimeRes[period <= 2020]$period),
                                   "period", "value", idxcols = c("region", "incoCostType", "univocalName",
                                                                  "technology", "unit"), extrapolate = TRUE)

  # map on decision tree for LDV 4 Wheelers
  decTree <- unique(helpers$decisionTree[subsectorL3 == "trn_pass_road_LDV_4W", c("region", "univocalName", "technology")])
  incoCostStartValReg <- merge(decTree, incoCostStartValReg, by = c("region", "univocalName", "technology"),
                               all.x = TRUE, allow.cartesian = TRUE)

  incoCostStartValReg[, unit := "US$2017/veh/yr"]
  setnames(incoCostStartValReg, "incoCostType", "variable")

  # convert to US$2017/pkm
  # Annualize and discount to convert to US$2017/veh/yr
  annualizedincoCostStartVal <- merge(incoCostStartValReg, annuity, by = "univocalName", allow.cartesian = TRUE)
  annualizedincoCostStartVal[, value := value * annuity][, unit := "US$2017/veh/yr"][, annuity := NULL]

  loadFactor <- copy(loadFactor)
  loadFactor[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  annualMileage <- copy(annualMileage)
  annualMileage[, c("variable", "unit") := NULL]
  setnames(annualMileage, "value", "annualMileage")

  annualizedincoCostStartVal <- merge(annualizedincoCostStartVal, loadFactor,
                                      c("region", "univocalName", "technology", "period"), all.x = TRUE)
  annualizedincoCostStartVal <- merge(annualizedincoCostStartVal, annualMileage,
                                      c("region", "univocalName", "technology", "period"), all.x = TRUE)
  annualizedincoCostStartVal[, value := value / (annualMileage * loadFactor)][, c("loadFactor", "annualMileage") := NULL]
  #unit US$2017/pkm for passenger and unit US$2017/tkm for freight
  annualizedincoCostStartVal[, unit := ifelse(univocalName %in% c(helpers$filter$trn_pass, "International Aviation"),
                                              "US$2017/pkm", "US$2017/tkm")]

  if (anyNA(annualizedincoCostStartVal) == TRUE) {
    stop("Inconvenience cost start values contain NAs")
  }

  return(annualizedincoCostStartVal)
}

