#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#' Existing files are overwritten silently. Does not return anything.
#'
#' @param inputPath the path to the folder containing the input (csv-) files
#' @param filename name of the file
#' @param SSPscenario SSP scenario
#' @param demScenario demand scenario
#' @param transportPolScenario Transport policy scenario
#' @import data.table
#' @export
# Loads the csv input files chooses the correct scenario and
# converts the files into RDS local files
csv2RDS <- function(filename, inputPath, SSPscenario, demScenario, transportPolScenario) {
    # bind variables locally to prevent NSE notes in R CMD CHECK
    SSPscen <- demScen <- transportPolScen <- NULL

    if (filename == "scenSpecPrefTrends") colNames <- c("period", "region", "SSPscen", "demScen", "transportPolScen",
                                                        "sector", "subsectorL1", "subsectorL2", "subsectorL3",
                                                        "vehicleType", "technology", "level", "variable", "unit", "value")
    else if (filename == "initialIncoCosts") colNames <- c("period", "region", "SSPscen", "demScen", "transportPolScen",
                                                           "univocalName", "technology", "variable", "unit", "type", "value")
    else if (filename == "timeValueCosts") colNames <-  c("period", "region", "SSPscen", "demScen", "transportPolScen",
                                                          "univocalName", "variable", "unit", "value")
    else if (filename == "f29_trpdemand") colNames <-  c("period", "region", "SSPscen", "demScen", "transportPolScen", "all_in", "value")
    else colNames <-  c("period", "region", "SSPscen", "demScen", "transportPolScen",
                        "univocalName", "technology", "variable", "unit", "value")
    tmp <- fread(
      file.path(inputPath, paste0(filename, ".cs4r")), skip = 5, col.names = eval(colNames))
    tmp <- tmp[SSPscen == SSPscenario & demScen == demScenario & transportPolScen == transportPolScenario][, c("SSPscen", "demScen", "transportPolScen") := NULL]
    # magclass enforces the same temporal resolution for all vehicletypes -> get rid of the introduced NAs
    assign(filename, tmp[!is.na(tmp$value)])
    tmp <- stats::setNames(list(get(eval(filename))), filename)
    return(tmp)
  }

toolLoadRDSinputs <- function(edgeTransportFolder, inputFiles) {

  loadRDS <- function(filename, edgeTransportFolder) {
    filePath <- list.files(file.path(".", edgeTransportFolder), paste0(filename, ".RDS"), recursive = TRUE, full.names = TRUE)
    tmp <- readRDS(filePath)
  }

  inputData <- sapply(inputFiles, loadRDS, edgeTransportFolder, simplify = FALSE, USE.NAMES = TRUE)
  return(inputData)
}
