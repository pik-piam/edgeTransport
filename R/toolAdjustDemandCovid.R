#' Function to adjust the sector ES demand on CES level to the covid decrease in 2020
#'
#' The adjustment is based on the relative differences of our model projections
#' in 2020 to the IEA data from 2020, as stored in inst/extdata/IEAdemandDifferencesCovid.csv
#'
#' The demand projections are constantly shifted by this absolute difference from 2020 onwards
#'
#' This function is applied in toolDemandRegression() directly after the regression calculation as a default
#'
#'
#' @param demandData the energy service demand on CES level: trn_aviation_intl, trn_shipping_intl, trn_freight, trn_pass
#'
#' @returns demandData adjusted for the decrease in demand related to covid
#' @export
#'

toolAdjustDemandCovid <- function(demandData) {

  # read in data for decrease in 2020 demand in IEA data in comparison to EDGET-REMIND projections
  IEAdemandDiffCovid <- fread(system.file("extdata/IEAdemandDifferencesCovid.csv",
                                          package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # drop unused columns
  IEAdemandDiffCovid[, `:=`(variable = NULL, unit = NULL)]

  demandData <- merge(demandData, IEAdemandDiffCovid, by = c("region", "period", "sector"), all.x = TRUE)

  # get absolute deviation in 2020
  demandData[, covidDecrease := (value * covidDecrease)]

  # no adjustments prior to 2020
  demandData[period < 2020, covidDecrease := 0]

  # constant forward fill of absolute decrease to later years
  demandData[, covidDecrease := zoo::na.locf(covidDecrease, na.rm = FALSE), by = c("region", "sector")]

  # apply constant covid decrease for period >= 2020
  demandData[, value := (value - covidDecrease)][, covidDecrease := NULL]

  return(demandData)

}
