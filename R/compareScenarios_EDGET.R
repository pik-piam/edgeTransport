#' Render CompareScenarios EDGE Transport
#'
#' A wrapper for piamPlotComparison::compareScenarios
#'
#' @param mifScen \code{character(n)}, optionally named. Paths to scenario mifs.
#'   If the vector has names, those are used to refer to the scenarios in the
#'   output file.
#' @param mifHist \code{character(1)}. Path to historical mif.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFormat \code{character(1)}, not case-sensitive. \code{"html"},
#'   \code{"pdf"}, or \code{"rmd"}.
#' @return The value returned by \code{\link[rmarkdown:render]{render()}}.
#' @export
compareScenarios_EDGET <- function(
    mifScen, mifHist,
    outputDir = getwd(),
    outputFile = "CompareScenarios_EDGE-Transport",
    outputFormat = "PDF") {

  piamPlotComparison::compareScenarios(
    projectLibrary = "edgeTransport",
    mifScen = mifScen,
    mifHist = mifHist,
    outputFormat = outputFormat,
    outputFile = outputFile,
    sections = "all",
    docTitle = "Edge Transport Compare Scenarios",
    outputDir = outputDir,
    reg = c("OAS", "MEA", "SSA", "LAM", "REF", "CAZ", "CHA", "IND", "JPN", "USA", "NEU", "EUR", "World"),
    yearsHist = c(seq(2010, 2020, 1), seq(2025, 2100, 5))
  )
}
