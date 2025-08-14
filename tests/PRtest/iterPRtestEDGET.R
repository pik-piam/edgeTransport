#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

dummyFolder <- args[1]

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

##
