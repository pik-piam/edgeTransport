#' Provide EDGE-Transport input parameters
#' @author Alois Dirnaichner
#' @param subtype one of the parameters required for EDGE-T
#' @param adjustments adjust historical data (boolean, defaults to TRUE)

calcEDGETinputs <- function(subtype, adjustments = TRUE) {

    switch(
      subtype,
      "annualMileage" = {
        am_TRACCS <- toolPrepareData(readSource("TRACCS", subtype="annualMileage"), "TRACCS")
        am_UCD <- NULL

        ## merge
      }
    )

  ## check for missing data
}
