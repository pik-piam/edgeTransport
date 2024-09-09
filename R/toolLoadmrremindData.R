#' Load data from mrremind
#' @param helpers list containg several helpers used throughout the model.
#'          It includes dtTimeRes, a data.table containing the temporal
#'          resolution for different univocalNames
#' @returns list of data.tables containing mrremind input data
#' @importFrom rmndt magpie2dt

toolLoadmrremindData <- function(helpers) {

  yrs <- unique(helpers$dtTimeRes$period)

  subsidies <- magpie2dt(readSource(type = "TransportSubsidies"))
  setnames(subsidies, "variable", "technology")
  #average between legal and private entities
  subsidies <- subsidies[, .(value = mean(value)), by = c("region", "period", "technology")]
  # exchange rate 2020: 1 euro = 1.12 dollar
  # conversion from EUR2020 to US$2005 : inflation/exchange rate = 1.3504/1.12 = 1.205714286
  subsidies[, value := value / 1.205714286] # in 2005 USD
  subsidies[, value := - value] # count subsidies negative
  completeSub <- unique(subsidies[, c("region", "technology")])[, temporal := "all"]
  temporal <- data.table(period = yrs)[, temporal := "all"]
  completeSub <- merge(completeSub, temporal, by = "temporal", allow.cartesian = TRUE)[, temporal := NULL]
  subsidies <- merge(subsidies, completeSub, all.y = TRUE, by = c("region", "technology", "period"))

  ## year where complete phase-out incentives occurs
  yearOut = 2030
  ## attribute first (to the countries that have them) the same incentives value until the phase out year
  subsidies[, value := ifelse(period >= 2020 & period <= yearOut, value[period == 2020], 0),
        by = c("region", "technology")]

  # map on decision tree, apply only on 4 wheelers
  subsidies <- merge(unique(helpers$decisionTree[subsectorL3 == "trn_pass_road_LDV_4W",
                                         c("region", "univocalName", "technology")]), subsidies,
                     by = c("region", "technology"), all.x = TRUE, allow.cartesian = TRUE)
  subsidies <- subsidies[!is.na(value)][, variable := "Capital costs subsidy"][, unit := "US$2005/veh"]
  #Q: How to include phase out of the incentives? Is that needed at all?

return(list(
  subsidies = subsidies
))
}
