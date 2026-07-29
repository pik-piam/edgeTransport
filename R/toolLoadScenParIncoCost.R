#' Function to load a parameter set for a specific scenario(combination) from the csv file in the package
#' @author Alex K. Hagen
#' @param SSPs SSP scenarios
#' @param transportPolS transport policy scenarios
#' @returns list with different input data sets

toolLoadScenParIncoCost <- function(SSPs, transportPolS) {
  # Transport policy scenario inconvenience cost factors
  #
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost[, "startYearCat" := fcase( SSPscen == SSPs[1] & transportPolScen == transportPolS[1], "origin", SSPscen == SSPs[2] & transportPolScen == transportPolS[2], "final")]
  scenParIncoCost <- scenParIncoCost[!is.na(startYearCat)][, c("transportPolScen", "SSPscen") := NULL]

  return(scenParIncoCost)

}
