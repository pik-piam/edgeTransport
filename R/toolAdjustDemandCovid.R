#' Function to adjust the sector ES demand on CES level to the covid decrease in 2020
#'
#' The adjustment is based on the relative differences of our FE model projections
#' in 2020 to the FE IEA data averaged over five years.
#' In addition, the deviation in 2020 is corrected for earlier diferences between IEA data and model projections,
#' such that only the decrease in deviation due to covid is regarded here. 
#' The final differences used for this correction are  stored in inst/extdata/IEAdemandDifferencesCovid.csv
#' For more context, the full data and calculations can be found 
#' in /p/projects/edget/adjustmentDataFiles/IEAdemandDifferencesCovid_compHistoricREMIND.xlsx
#'
#' The demand projections are constantly shifted by an absolute difference from 2020 onwards
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
  covidDecrease <- value <- period <- NULL

  # read in data for decrease in 2020 demand in IEA data in comparison to EDGET-REMIND projections
  IEAdemandDiffCovid <- fread(system.file("extdata/IEAdemandDifferencesCovid.csv",
                                          package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # drop unused columns
  IEAdemandDiffCovid[, `:=`(variable = NULL, unit = NULL)]

  demandData <- merge(demandData, IEAdemandDiffCovid, by = c("region", "period", "sector"), all.x = TRUE)

  # get absolute ES deviation in 2020 based on relative difference
  demandData[, covidDecrease := (value * covidDecrease)]

  # no adjustments prior to 2020
  demandData[period < 2020, covidDecrease := 0]

  # constant forward fill of absolute decrease to later years
  demandData[, covidDecrease := zoo::na.locf(covidDecrease, na.rm = FALSE), by = c("region", "sector")]

  # apply constant covid decrease for period >= 2020
  demandData[, value := (value - covidDecrease)][, covidDecrease := NULL]

  return(demandData)

}
