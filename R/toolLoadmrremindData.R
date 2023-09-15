#' Load data from mrremind
#'
#' @importFrom rmndt magpie2dt

toolLoadmrremindData <- function(SSPscenario, yrs) {
  
  subsidies <- magpie2dt(readSource(type = "TransportSubsidies"))
  setnames(subsidies, "variable", "technology")
  #average between legal and private entities
  subsidies[, value := mean(value), by = c("region", "period", "technology")]
  subsidies <- unique(subsidies[, c("region", "period", "technology", "value")])
  # exchange rate 2020: 1 euro = 1.12 dollar
  # conversion from EUR2020 to US$2005 : inflation/exchange rate = 1.3504/1.12 = 1.205714286
  subsidies[, value := value/1.205714286] # in 2005 USD
  subsidies[, value := - value] # count subsidies negative
  completeSub <- unique(subsidies[, c("region", "technology")])[, temporal := "all"]
  temporal <- data.table(period = yrs)[, temporal := "all"]
  completeSub <- merge(completeSub, temporal, by = "temporal", allow.cartesian = TRUE)[, temporal := NULL]
  subsidies <- merge(subsidies, completeSub, all.y = TRUE, by = c("region", "technology", "period"))
  
  ## year where complete phase-out incentives occurs
  yearOut = 2030
  ## attribute first (to the countries that have them) the same incentives value until the phase out year
  subsidies[value := ifelse(period >= 2020 & period <= yearOut, value[period == 2020], 0),
        by = c("region", "technology", "period")]
  
  #Q: How to include phase out of the incentives?
}