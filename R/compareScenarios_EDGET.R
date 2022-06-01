#' Render CompareScenarios EDGE Transport
#'
#' Renders the *.Rmd-files associated to CompareScenarios EDGE TRansport. In the Rmds,
#' scenario- and historical .mif-files are loaded. Then plots are created from
#' this data. The result may be rendered to PDF or HTML. Alternatively one can
#' choose Rmd as output format and obtain a copy of the *.Rmd-files.
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
#' @param ... YAML parameters, see below.
#' @importFrom yaml yaml.load
#' @importFrom rlang parse_expr
#' @importFrom ymlthis yml_params_code yml_replace as_yml use_rmarkdown
#' @importFrom rmarkdown render
#' @return The value returned by \code{\link[rmarkdown:render]{render()}}.
#' @section YAML Parameters:
#' \describe{
#'   \item{\code{yearsScen}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(2005, 2060, 5), seq(2070, 2100, 10))}.
#'     Years to show for scenario data.}
#'   \item{\code{yearsHist}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(1990, 2020, 1), seq(2025, 2100, 5))}.
#'     Years to show for historical data.}
#'   \item{\code{yearsBarPlot}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(2010, 2030, 2050, 2100)}.
#'     Years to show in bar plots of scenario data.}
#'   \item{\code{reg}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Regions to show. \code{NULL} means all.}
#'   \item{\code{sections}}{
#'     \code{character(n)}.
#'     Default: \code{"all"}.
#'     Names of sections to include. A subset of
#'     \code{c("01_energy_demand", "02_energy_services", "03_stock_and_sales", "04_costs_and_shareweight_trends")}
#'     or \code{"all"} for all available sections.}
#'   \item{\code{userSectionPath}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Path to a *.Rmd-file that may be included as additional section.}
#'   \item{\code{mainReg}}{
#'     \code{character(1)}.
#'     Default: \code{"World"}.
#'     A region for which larger plots are shown.}
#'   \item{\code{figWidth, figHeight}}{
#'     \code{numeric(1)}.
#'     Default: \code{15} and \code{10}, respectively.
#'     Size of plots in inches.}
#'   \item{\code{warning}}{
#'     \code{logical(1)}.
#'     Default: \code{TRUE}.
#'     Show warnings in output?}
#' }
#' @author Christof Schoetz, Johanna Hoppe
#' @examples
#' \dontrun{
#' compareScenarios2(
#'   mifScen = c("path/to/Base.mif", "path/to/NDC.mif"),
#'   mifHist = "path/to/historical.mif",
#'   outputFile = "CompareScenarios2Example1",
#'   userSectionPath = "path/to/myPlots.Rmd")
#' compareScenarios2(
#'   mifScen = c(ScenarioName1 = "path/to/scen1.mif", ScenarioName2 = "path/to/scen2.mif"),
#'   mifHist = "path/to/historical.mif",
#'   outputFile = "CompareScenarios2Example2",
#'   figWidth = 18, figHeight = 10)
#' }
#' @export
compareScenarios_EDGET <- function(
  mifScen, mifHist,
  outputDir = getwd(),
  outputFile = "CompareScenarios_EDGE-Transport",
  outputFormat = "PDF",
  ...
) {
  yamlParams <- c(
    list(
      mifScen = normalizePath(mifScen, mustWork = TRUE),
      mifScenNames = names(mifScen),
      mifHist = normalizePath(mifHist, mustWork = TRUE)),
    list(...))

  # convert relative to absolute paths
  if ("userSectionPath" %in% names(yamlParams)) {
    yamlParams$userSectionPath <- normalizePath(yamlParams$userSectionPath,
                                                mustWork = TRUE)
  }

  outputFormat <- tolower(outputFormat)
  if (outputFormat == "pdf") outputFormat <- "pdf_document"
  if (outputFormat == "html") outputFormat <- "html_document"
  if (identical(tolower(outputFormat), "rmd")) {
    return(.compareScenarios2Rmd(yamlParams, outputDir, outputFile))
  }
  render(
    system.file("rmd/compareScenarios_Transport/csEDGET_main.Rmd", package = "edgeTrpLib"),
    intermediates_dir = outputDir,
    output_dir = outputDir,
    output_file = outputFile,
    output_format = outputFormat,
    params = yamlParams,
    envir = new.env())
}

# Copies the CompareScenarios2-Rmds to the specified location and modifies
# their YAML header according to \code{yamlParams}.
.compareScenarios2Rmd <- function(yamlParams, outputDir, outputFile) {
  pathMain <- system.file("rmd/compareScenarios_Transport/csEDGET_main.Rmd", package = "edgeTrpLib")
  linesMain <- readLines(pathMain)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", linesMain)
  headerMain <- linesMain[(delimiters[1]):(delimiters[2])]
  yml <- yaml.load(
    headerMain,
    handlers = list(r = function(x) yml_params_code(!!parse_expr(x))))
  baseYaml <- as_yml(yml)
  newYamlParams <- baseYaml$params
  newYamlParams[names(yamlParams)] <- yamlParams
  if (!is.null(names(yamlParams$mifScen))) {
    newYamlParams$mifScenNames <- names(yamlParams$mifScen)
  }
  newYaml <- yml_replace(
    baseYaml,
    params = newYamlParams,
    date = format(Sys.Date()))
  pathDir <- file.path(outputDir, paste0(outputFile, "_Rmd"))
  if (!dir.exists(pathDir)) dir.create(pathDir)
  dirFiles <- dir(
    system.file("rmd/compareScenarios_Transport", package = "edgeTrpLib"),
    full.names = TRUE)
  rmdDirFiles <- grep(
    dirFiles,
    pattern = "csEDGET_main\\.Rmd$",
    invert = TRUE, value = TRUE)
  file.copy(rmdDirFiles, pathDir)
  use_rmarkdown(
    newYaml,
    path = file.path(pathDir, "cs2_main.Rmd"),
    template = system.file(
      "rmd/compareScenarios_Transport/csEDGET_main.Rmd",
      package = "edgeTrpLib"),
    include_yaml = FALSE)
}
