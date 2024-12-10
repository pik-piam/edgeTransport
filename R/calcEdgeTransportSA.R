#' Generate EDGE-Transport Input Data for the REMIND model, madrat interface.
#'
#' `calcEdgeTransportSA()` is a wrapper for `toolEdgeTransportSA()` to make use of
#' madrat caching.
#'
#' @md
#' @param SSPscen SSP or SDP scenario
#' @param transportPolScen EDGE-T transport policy scenario
#' @param isICEban optional enabling of ICE ban
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param outputFolder Path to folder for storing output data
#' @param isStored Optional saving of intermediate RDS files
#' @param isTransportReported Optional transport reporting in MIF format
#' @param isTransportExtendedReported Optional extension of transport reporting providing more detailed variables
#' @param isREMINDinputReported Optional reporting of REMIND input data
#' @param isAnalyticsReported Optional reporting of analytics data (e.g. variables over iterations)
#' @returns Transport input data for REMIND
#' @author Jarusch Muessel, Johanna Hoppe
#' @export
#' @rdname EdgeTransportSA

calcEdgeTransportSA <- function(SSPscen,
                                transportPolScen,
                                isICEban = FALSE,
                                demScen = "default",
                                outputFolder = NULL,
                                isStored = FALSE,
                                isTransportReported = TRUE,
                                isTransportExtendedReported = FALSE,
                                isREMINDinputReported = FALSE,
                                isAnalyticsReported = FALSE) {

  return(list(
    x = toolEdgeTransportSA(SSPscen,
                            transportPolScen,
                            isICEban,
                            demScen,
                            outputFolder,
                            isStored,
                            isTransportReported,
                            isTransportExtendedReported,
                            isREMINDinputReported,
                            isAnalyticsReported),
    class = 'list',
    unit = NA,
    description = NA))
}

