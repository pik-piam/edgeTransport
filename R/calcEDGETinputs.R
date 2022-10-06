#' Provide EDGE-Transport input parameters
#' @author Alois Dirnaichner
#' @param subtype one of the parameters required for EDGE-T
#' @param adjustments adjust historical data (boolean, defaults to TRUE)

calcEDGETinputs <- function(subtype, adjustments = TRUE) {

    switch(
      subtype,
      "annualMileage" = {
        am_TRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
        am_UCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)

        ## use same periods
        am_TRACCS <- am_TRACCS[period %in% c(2005, 2010)]
        am_UCD <- rbind(am_UCD[period == 2005], am_UCD[period == 2005][, period := 2010])
        ## update-join - prefer TRACCS values wherever we have them
        am_UCD[am_TRACCS, value := i.value, on=colnames(am_UCD)[1:13]]
      }
    )

  ## check for missing data
}
