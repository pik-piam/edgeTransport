#' Distribute common parameters to standalone and iterative EDGET script
#'
#' @author Alex K. Hagen
#' @param startyear First time point in which policy differentiation sets in, cm_startyear in REMIND
#' @param ICEbanBefore Boolean to determine if ICEban is in effect before startyear
#' @param ICEbanAfter Boolean to determine if ICEban is in effect from startyear on
#' @export
#' @returns a list of parameters: allEqYear, ICEbanYears, GDPcutoff, baseYear, hybridElecShare
#' @export

toolGetCommonParameters <- function(startyear, ICEbanBefore = FALSE, ICEbanAfter = FALSE) {
  # REMIND startyear is the year in which differences are observed
  # allEqYear in EDGE-T is the last year in which all scenarios are fixed
  # scenario differentiation sets in directly after that, earliest: 2020
  allEqYear <- startyear - 1
  if (allEqYear < 2020) {
    allEqYear <- 2020
  }

  # find years in which ICEban is used
  if (ICEbanBefore & ICEbanAfter) {
    ICEbanYears <- c(seq(2021, 2100, 1), 2110, 2130, 2150)
  } else if (ICEbanBefore & allEqYear > 2020) {
    ICEbanYears  <- seq(2021, allEqYear, 1)
  } else if (ICEbanAfter) {
    ICEbanYears <-  c(seq(allEqYear + 1, 2100, 1), 2110, 2130, 2150)
  } else {
    ICEbanYears <- NULL
  }

  # set GDP cutoff to differentiate between regions
  GDPcutoff <- 30800 # [constant 2017 US$MER]
  # last time step of historical data
  baseYear <- 2010
  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- 0.4

  return(list(
    allEqYear = allEqYear,
    ICEbanYears = ICEbanYears,
    GDPcutoff = GDPcutoff,
    baseYear = baseYear,
    hybridElecShare = hybridElecShare)
    )
}
