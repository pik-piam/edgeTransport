#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

dummyFolder <- args[1]
refFolder <- NULL
if (length(args) == 2) {
  refFolder <- args[2]
}

renv::status()
renv::restore()
library(edgeTransport)
library(reporttransport)

setwd(dummyFolder)
iterativeEdgeTransport()

# Generate transport extended mif
# This is usually called in the REMIND reporting
edgetOutputDir <- file.path(getwd(), "EDGE-T")
reportEdgeTransport(edgetOutputDir,
                    isTransportExtendedReported = TRUE,
                    isStored = TRUE)


## generate comparison plots

PRchangeLogPath <- dirname(dirname(dummyFolder))

testDirs <- sort(list.dirs(path = PRchangeLogPath, recursive = FALSE))

#Only use folders that comply with naming convention
testDirs <- testDirs[grepl(".*\\d{8}_.*", testDirs)]

if (is.character(refFolder)) {
  testDirs <- testDirs[c(grep(refFolder, testDirs), grep(dirname(dummyFolder), testDirs))]
 } else {
  testDirs <- testDirs[c(grep(dirname(dummyFolder), testDirs)-1, grep(dirname(dummyFolder), testDirs))]
 }

mifHist <- "/p/projects/edget/PRchangeLog/historical21.mif"

mifs <- list.files(testDirs, "Transport.mif", full.names = TRUE, recursive = TRUE)

mifs <- mifs[grep("SSP2-NPi2025-dummy", mifs)]
if (2 != length(mifs)) {
  cat("ERROR: Wrong number of mifs selected.")
  cat(mifs)
  cat(" ")
  quit(save = 'no', status = 1)
}

scenNames <- c("SSP2-NPi2025-dummy_before", "SSP2-NPi2025-dummy_after")
filename <- "ChangeLogIterative"
outDir <- file.path(dirname(dummyFolder), filename)
dir.create(outDir, showWarnings = FALSE)

sec <- c("00_info", "01_energy_demand","02_energy_services","03_energy_intensity", "04_stock_and_sales", "05_emissions", "06_input_parameters", "08_transportRemindInputfiles")

#EU21-short
piamPlotComparison::compareScenarios(
  projectLibrary = "reporttransport",
  mifs,
  mifHist,
  outDir,
  outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filename, "EUR-short.pdf"),
  outputFormat = "pdf",
  mifScenNames = scenNames,
  yearsScen = c(seq(2005, 2050, 5)),
  yearsHist = c(seq(1960, 2020, 1), seq(2025, 2050, 5)),
  yearsBarPlot = c(2020, 2030, 2040, 2050),
  sections = sec,
  reg = c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","UKI","ESW","EUR"),
  mainReg = "EUR"
)

#H12-short
piamPlotComparison::compareScenarios(
  projectLibrary = "reporttransport",
  mifs,
  mifHist,
  outDir,
  outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filename, "H12-short.pdf"),
  outputFormat = "pdf",
  mifScenNames = scenNames,
  yearsScen = c(seq(2005, 2050, 5)),
  yearsHist = c(seq(1960, 2020, 1), seq(2025, 2050, 5)),
  yearsBarPlot = c(2020, 2030, 2040, 2050),
  sections = sec,
  reg = c("OAS","MEA","SSA","LAM","REF","CAZ","CHA","IND","JPN","USA","NEU","EUR","World"),
  mainReg = "World"
)

#EU21-short
piamPlotComparison::compareScenarios(
  projectLibrary = "reporttransport",
  mifs,
  mifHist,
  outDir,
  outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filename, "EUR.pdf"),
  outputFormat = "pdf",
  mifScenNames = scenNames,
  sections = sec,
  reg = c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","UKI","ESW","EUR"),
  mainReg = "EUR"
)

#H12-short
piamPlotComparison::compareScenarios(
  projectLibrary = "reporttransport",
  mifs,
  mifHist,
  outDir,
  outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filename, "H12.pdf"),
  outputFormat = "pdf",
  mifScenNames = scenNames,
  sections = sec,
  reg = c("OAS","MEA","SSA","LAM","REF","CAZ","CHA","IND","JPN","USA","NEU","EUR","World"),
  mainReg = "World"
)

