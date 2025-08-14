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
#' @param startyear Year after which policy differentiation sets in
#' @param gdxPath Path to a GDX file to load price signals from a REMIND run
#' @param outputFolder Path to folder for storing output data
#' @param isStored Optional saving of intermediate RDS files
#' @param isTransportReported Optional transport reporting in MIF format
#' @param isTransportExtendedReported Optional extension of transport reporting providing more detailed variables
#' @param isREMINDinputReported Optional reporting of REMIND input data
#' @param isAnalyticsReported Optional reporting of analytics data (e.g. variables over iterations)
#' @param testIterative development setting: make standalone and iterative scripts comparable, sets sectorESdemand = REMINDsectorESdemand and iterations = 1 (cost module)
#' @returns Transport input data for REMIND
#' @author Jarusch Muessel, Johanna Hoppe, Alex K. Hagen
#' @export
#' @rdname EdgeTransportSA

calcEdgeTransportSA <- function(SSPscen,
                                transportPolScen,
                                isICEban = c(FALSE, FALSE),
                                demScen = c("default", "default"),
                                startyear = 2021,
                                gdxPath = NULL,
                                outputFolder = NULL,
                                isStored = FALSE,
                                isTransportReported = TRUE,
                                isTransportExtendedReported = FALSE,
                                isREMINDinputReported = FALSE,
                                isAnalyticsReported = FALSE,
                                testIterative = FALSE) {

  # for backwards compatibility with function calls before startyear implementation
  if (length(SSPscen) == 1){
    SSPscen <- c(SSPscen, SSPscen)
  }
  if (length(transportPolScen) == 1){
    transportPolScen <- c(transportPolScen, transportPolScen)
  }
  if (length(isICEban) == 1){
    isICEban <- c(isICEban, isICEban)
  }
  if (length(demScen) == 1){
    demScen <- c("default", demScen)
  }

  return(list(
    x = toolEdgeTransportSA(SSPscen,
                            transportPolScen,
                            isICEban,
                            demScen,
                            startyear,
                            gdxPath,
                            outputFolder,
                            isStored,
                            isTransportReported,
                            isTransportExtendedReported,
                            isREMINDinputReported,
                            isAnalyticsReported,
                            testIterative),
    class = 'list',
    unit = NA,
    description = NA))
}

