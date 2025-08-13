#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

folderName <- args[1]
defScen <- args[2]
refFolder <- NULL
if (length(args) == 3) {
  refFolder <- args[3]
}

# maybe a check for possible argument errors here? only needed if script is used individually

renv::status()
renv::restore()

library(madrat)
library(mrdrivers)
library(mrremind)
library(dplyr)
library(tidyr)
library(edgeTransport)
library(reporttransport)
library(piamPlotComparison)

testStandardScenarios <- function(folderNameD, defScenN){

  allScens <-  tribble(
    ~SSPscen,         ~transportPolScen,        ~isICEban,    ~demScen,
    'SSP2',        'Mix1',                 FALSE,       'default',
    'SSP2',        'Mix2',                 TRUE,        'default',
    'SSP2',        'Mix3',                 TRUE,        'default',
    'SSP2',        'Mix4',                 TRUE,        'default'
  )

  # generate list from data frame rows
  allScens <- split(allScens, seq(nrow(allScens)))
  defScen <- allScens[[defScenN]]

  EdgeTransportSAdata <- calcEdgeTransportSA(SSPscen = defScen[["SSPscen"]],
                                             transportPolScen = defScen[["transportPolScen"]],
                                             isICEban = defScen[["isICEban"]],
                                             demScen = defScen[["demScen"]],
                                             isTransportReported = TRUE,
                                             isTransportExtendedReported = TRUE,
                                             isREMINDinputReported = TRUE,
                                             isAnalyticsReported = TRUE,
                                             isStored = TRUE,
                                             outputFolder = folderNameD)

  renv::snapshot(type = "all")
}

plotStandardScenarios <- function(folderNameD, defScenN, refFolderD = NULL){

  n <- as.numeric(defScenN)
  refFolder <- refFolderD
  current_path <- getwd()
  PRchangeLogPath <- dirname(current_path)

  testDirs <- sort(list.dirs(path = PRchangeLogPath, recursive = FALSE))
  #Only use folders that comply with naming convention
  testDirs <- testDirs[grepl(".*\\d{8}_.*", testDirs)]

  if (is.character(refFolder)) {
    testDirs <- testDirs[c(grep(refFolderD, testDirs), grep(current_path, testDirs))]
  } else {
    testDirs <- testDirs[c(grep(current_path, testDirs)-1, grep(current_path, testDirs))]
  }

  mifHist <- "/p/projects/edget/PRchangeLog/historical21.mif"

  allScens <-  tribble(
    ~transportPolScen,        ~scenName1,    	~scenName2,	~filename,
    'Mix1',                   'Mix1before',     'Mix1after',	"ChangeLogMix1",
    'Mix2',                   'Mix2before',     'Mix2after',	"ChangeLogMix2",
    'Mix3',                   'Mix3before',     'Mix3after',	"ChangeLogMix3",
    'Mix4',                   'Mix4before',     'Mix4after',	"ChangeLogMix4"
  )

  mifs <- list.files(testDirs, "Transport.mif", full.names = TRUE, recursive = TRUE)

  mifs <- mifs[grep(allScens$transportPolScen[n], mifs)]
  mifs <- mifs[grep("SSP2", mifs)]
  mifs <- mifs[grep("demRed", mifs, invert = TRUE)]
  if (2 != length(mifs)) {
    cat("ERROR: Wrong number of mifs selected.")
    cat(mifs)
    cat(" ")
    quit(save = 'no', status = 1)
  }
  scenNames <- c(allScens$scenName1[n], allScens$scenName2[n])
  filename <- allScens$filename[n]
  outDir <- file.path(folderNameD, filename)
  dir.create(outDir, showWarnings = FALSE)

  sec <- c("00_info", "01_energy_demand","02_energy_services","03_energy_intensity", "04_stock_and_sales", "05_emissions", "06_input_parameters", "08_transportRemindInputfiles")

  #EU21
  piamPlotComparison::compareScenarios(
    projectLibrary = "reporttransport",
    mifs,
    mifHist,
    outDir,
    outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filename, "2050_EUR.pdf"),
    outputFormat = "pdf",
    mifScenNames = scenNames,
    yearsScen = c(seq(2005, 2050, 5)),
    yearsHist = c(seq(1960, 2020, 1), seq(2025, 2050, 5)),
    yearsBarPlot = c(2020, 2030, 2040, 2050),
    sections = sec,
    reg = c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","UKI","ESW","EUR"),
    mainReg = "EUR"
  )

  #H12
  piamPlotComparison::compareScenarios(
    projectLibrary = "reporttransport",
    mifs,
    mifHist,
    outDir,
    outputFile = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", filename, "2050_H12.pdf"),
    outputFormat = "pdf",
    mifScenNames = scenNames,
    yearsScen = c(seq(2005, 2050, 5)),
    yearsHist = c(seq(1960, 2020, 1), seq(2025, 2050, 5)),
    yearsBarPlot = c(2020, 2030, 2040, 2050),
    sections = sec,
    reg = c("OAS","MEA","SSA","LAM","REF","CAZ","CHA","IND","JPN","USA","NEU","EUR","World"),
    mainReg = "World"
  )

  #EU21
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

  #H12
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
}


testStandardScenarios(folderName, defScen)
plotStandardScenarios(folderName, defScen, refFolder)
