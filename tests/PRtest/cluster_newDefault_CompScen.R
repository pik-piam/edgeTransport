#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

filename <- args[1]

library(devtools)
library(mrremind)
library(quitte)
library(tidyverse)
library(reporttransport)
library(piamPlotComparison)

plotStandardScenarios <- function(filenameD) {

  current_path <- getwd()
  mifs <- list.files(file.path(current_path), "*Transport.mif", full.names = TRUE, recursive = TRUE)
  # sort mifs
  mifs <- mifs[c(grep("SSP1", mifs), grep("SSP1|SSP3|demRedStrong", mifs, invert = TRUE),
                 grep("demRedStrong", mifs), grep("SSP3", mifs))]
  scenNames <- unlist(lapply(mifs, function(x) {
substring(basename(dirname(x)), 26)
}))

  mifHist <- "/p/projects/edget/PRchangeLog/historical21.mif"

  outDir <- file.path(current_path)
  dir.create(outDir, showWarnings = FALSE)

  sec <- c("00_info", "01_energy_demand", "02_energy_services", "03_energy_intensity", "04_stock_and_sales",
           "05_emissions", "06_input_parameters", "08_transportRemindInputfiles")

  # EU21
        piamPlotComparison::compareScenarios(
        projectLibrary = "reporttransport",
        mifs,
        mifHist,
        outDir,
        outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filenameD, "EUR21.pdf"),
        outputFormat = "pdf",
        mifScenNames = scenNames,
        sections = sec,
        reg = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "UKI", "ESW", "EUR"),
        mainReg = "EUR"
        )

        # H12
        piamPlotComparison::compareScenarios(
        projectLibrary = "reporttransport",
        mifs,
        mifHist,
        outDir,
        outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filenameD, "H12.pdf"),
        outputFormat = "pdf",
        mifScenNames = scenNames,
        sections = sec,
        reg = c("OAS", "MEA", "SSA", "LAM", "REF", "CAZ", "CHA", "IND", "JPN", "USA", "NEU", "EUR", "World"),
        mainReg = "World"
        )
}

plotStandardScenarios(filename)
