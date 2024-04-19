toolReportEdgeTransport <- function(folderPath = file.path(".","EDGE-T"), data = NULL, reportTransportData = TRUE, reportREMINDinputData = FALSE,
                                    reportExtendedTransportData = FALSE, reportAnalytics = FALSE, storeData = TRUE) {

  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an interpolation step in toolCalculateOutputVariables()
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  #########################################################################
  ## Load data for reporting if data is not supplied in function call
  #########################################################################

  if (is.null(data)) {
    data <- list()
    # load files needed for all
    cfg <- readRDS(file.path(folderPath, "cfg.RDS"))
    data <- append(data, cfg[names(cfg) %in% c("SSPscen", "transportPolScen", "demScen")])
    data$hybridElecShare <- readRDS(file.path(folderPath, "1_InputDataRaw", "hybridElecShare.RDS"))
    data$helpers <- readRDS(file.path(folderPath, "1_InputDataRaw", "helpers.RDS"))
    data$GDPMER <- readRDS(file.path(folderPath, "1_InputDataRaw", "GDPMER.RDS"))
    data$combinedCAPEXandOPEX <- readRDS(file.path(folderPath, "2_InputDataPolicy", "combinedCAPEXandOPEX.RDS"))
    data$enIntensity <- readRDS(file.path(folderPath, "2_InputDataPolicy", "enIntensity.RDS"))
    data$loadFactor <- readRDS(file.path(folderPath, "2_InputDataPolicy", "loadFactor.RDS"))
    data$fleetSizeAndComposition <- readRDS(file.path(folderPath, "4_Output", "fleetSizeAndComposition.RDS"))
    data$ESdemandFVsalesLevel <- readRDS(file.path(folderPath, "4_Output", "ESdemandFVsalesLevel.RDS"))

    # load files for standard transport reporting
    if (reportTransportData) {
      data$upfrontCAPEXtrackedFleet <- readRDS(file.path(folderPath, "2_InputDataPolicy", "upfrontCAPEXtrackedFleet.RDS"))
      gdxPath <- list.files(path = folderPath, pattern = "\\.gdx$", full.names = TRUE)
      # Check if any files were found
      if (length(gdxPath) > 1) {
        gdxPath <- gdxPath[1]
        cat("More than one gdx file found. The following one was chosen\n")
        cat(gdxPath, sep = "\n")
      } else if (length(gdxPath) == 0) {
        stop("No gdx files found in the specified directory.\n")
      }
      data$gdxPath <- gdxPath
    }
    if (reportAnalytics) {
      # load files for analytic purposes
      fleetFilesIterations <- list.files(path = folderPath, pattern = ".*fleetVehNumbersIteration.*", full.names = TRUE)
      data$fleetFilesIterations <- lapply(fleetFilesIterations, readRDS)
      endogenousCostFilesIterations <- list.files(path = folderPath, pattern = ".*endogenousCostsIterations.*", full.names = TRUE)
      data$endogenousCostFilesIterations <- lapply(endogenousCostFilesIterations, readRDS)
    }
    if (reportREMINDinputData) {
      # load files for REMIND input data only reporting
      data$annualMileage <- readRDS(file.path(folderPath, "1_InputDataRaw", "annualMileage.RDS"))
      data$timeValueCosts <- readRDS(file.path(folderPath, "1_InputDataRaw", "timeValueCosts.RDS"))
      data$prefTrends <- readRDS(file.path(folderPath, "2_InputDataPolicy", "prefTrends.RDS"))
      data$initialIncoCosts <- readRDS(file.path(folderPath, "2_InputDataPolicy", "initialIncoCosts.RDS"))
    }
  } else {
      data <- data
  }
  #########################################################################
  ## Calculate output variables
  #########################################################################
  outputVars <- toolCalculateOutputVariables(data,
                                             timeResReporting = timeResReporting,
                                             reportTransportData = reportTransportData,
                                             reportExtendedTransportData = reportExtendedTransportData,
                                             reportAnalytics = reportAnalytics)

  #########################################################################
  ## Transfer output variables to MIF format
  #########################################################################
  if (reportTransportData) {
    reporting <- toolReportAndAggregateMIF(vars = outputVars,
                                           GDPMER = data$GDPMER,
                                           helpers = data$helpers,
                                           scenario = paste0(data$transportPolScen, " ", data$SSPscen),
                                           model = "EDGE-T",
                                           gdx = data$gdxPath,
                                           reportExtendedTransportData = reportExtendedTransportData)

    if (storeData) write.mif(reporting, file.path(folderPath, "Transport.mif"))
  }

  #########################################################################
  ## Report REMIND input data
  #########################################################################
  if (reportREMINDinputData) {
    REMINDinputData <- toolReportREMINDinputData(outputVars$ext$fleetESdemand,
                                                 outputVars$ext$fleetFEdemand,
                                                 outputVars$int$fleetEnergyIntensity,
                                                 outputVars$int$fleetCapCosts,
                                                 data$combinedCAPEXandOPEX,
                                                 data$prefTrends,
                                                 data$enIntensity,
                                                 data$initialIncoCosts,
                                                 data$annualMileage,
                                                 data$timeValueCosts,
                                                 data$demScen,
                                                 data$SSPscen,
                                                 data$transportPolScen,
                                                 data$helpers)
    reporting <- REMINDinputData
    if (storeData) toolStoreData(outputFolder = folderPath, REMINDinputData = REMINDinputData)
  }

  return(reporting)
}
