#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#' Existing files are overwritten silently. Does not return anything.
#'
#' @param inputFiles list of filename strings that should be read in
#' @param edgeTransportFolder directory in the output folder of the REMIND run that stores EDGET RDS files
#' @import data.table
#' @export

toolLoadRDSinputs <- function(edgeTransportFolder, inputFiles) {

  loadRDS <- function(filename, edgeTransportFolder) {
    filePath <- list.files(file.path(".", edgeTransportFolder), paste0(filename, ".RDS"), recursive = TRUE, full.names = TRUE)
    tmp <- readRDS(filePath)
  }

  inputData <- sapply(inputFiles, loadRDS, edgeTransportFolder, simplify = FALSE, USE.NAMES = TRUE)
  return(inputData)
}
