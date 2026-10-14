#' Function to load a parameter set of inconvinience start costs for a specific scenario(combination) from the csv file in the package
#' @author Alex K. Hagen
#' @param SSPs SSP scenarios
#' @param transportPolS transport policy scenarios
#' @returns list with different parameter sets, one for each scenario 

toolLoadScenParIncoCost <- function(SSPs, transportPolS) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  SSPscen <- transportPolScen <- startYearCat <- NULL

  # Transport policy scenario inconvenience cost factors
  #
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost[, "startYearCat" := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
  scenParIncoCost <- scenParIncoCost[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]

  return(scenParIncoCost)

}
