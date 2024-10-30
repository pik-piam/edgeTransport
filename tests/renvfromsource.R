#!/usr/bin/env Rscript

sourceRepos <- commandArgs(trailingOnly = TRUE)

renv::load()

renv::install(sourceRepos, type = source)

renv::snapshot(type = "all")
