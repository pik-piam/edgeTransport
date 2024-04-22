#'Report EDGE-Transport Model results
#'
#'This function reports the transport model results of an iterative or standalone run.
#'If not handed over in the function call, it first loads the transport model results from the stored RDS files.
#'Then it calculates the output variables and brings the data into the right format.
#'A basic output variables set is always calculated that is needed for all reporting packages.
#'With the help of switches, different reporting packages can be generated:
#'- reportTransportData activates the reduced reporting of transport variables in MIF format to be attached to a REMIND.mif.
#'  It includes the variables needed to create REMIND compareScenarios2 and report results for projects
#'- reportTransportData + reportExtendedTransportData activates further the extended reporting of transport variables
#'  and if storeData is activated as well, triggers the generation of a seperate transport.MIF.
#'  It includes the reduced reporting and additional transport variables for a detailed analysis of the transport sector
#'  using transportCompareScenarios provided in the edgeTransport package
#'- reportTransportData + reportExtendedTransportData + reportAnalytics activates further the generation of additional variables
#'  for the analysis of the model behavior such as the inconvenience costs over iterations. They can be analyzed in the analytics
#'  sheet in compareScenariosTransport. It can be used in combination or without reportExtendedTransportData.
#'- reportREMINDinputData activates the reporting of REMIND input data from a standalone run. This mode is used in the REMIND input data
#'  generation with all other switches turned off. It can be also used in combination with the other switches.
#'
#' @param folderPath Path to the EDGE-Transport output folder of an iterative or standalone run
#' @param data List of model results. If not handed over, the data is loaded from the RDS files in the output folder
#' @param reportTransportData Switch for activating the reporting of transport data in MIF format
#' @param reportExtendedTransportData Switch for activating the reporting of detailed transport data im MIF format
#'                                    needed to create transportCompareScenarios
#' @param reportAnalytics Switch for activating reporting of model analytics data
#' @param reportREMINDinputdata Switch for activating reporting of REMIND input data
#' @param storeData Switch for activating data storage and creating the transport.MIF file
#'
#' @returns The function either returns the REMINDinputData if reportREMINDinputdata is enabled or the transport data in MIF format
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolReportEdgeTransport <- function(folderPath = file.path(".","EDGE-T"), data = NULL, reportTransportData = TRUE,
                                    reportExtendedTransportData = FALSE, reportAnalytics = FALSE,
                                    reportREMINDinputData = FALSE, storeData = TRUE) {

  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an interpolation step in
  # toolCalculateOutputVariables()
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

    # load files for standard and extended transport reporting
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

  # Base variable set that is needed to report REMIND input data and additional detailed transport data
  baseVarSet <- toolReportBaseVarSet(data = data, timeResReporting = timeResReporting)
  outputVars <- baseVarSet
  browser()
  if (reportTransportData) {
    transportVarSet <- reportTransportVarSet(data = data,
                                        baseVarSet = outputVars,
                                        timeResReporting = timeResReporting)
    # New memory adress to modify output vars
    outputVars <- copy(outputVars)
    # Prevent double accounting for liquids and gases, where the split in different production routes is reported in addition
    outputVars[["ext"]][["fleetFEdemand"]] <- NULL
    outputVars$ext <- append(outputVars$ext, transportVarSet$ext)
    outputVars$int <- append(outputVars$int, transportVarSet$int)
  } else if (reportExtendedTransportData) {
    extendedTransportVarSet <- append(outputVars, reportExtendedTransportVarSet(data = data,
                                                                   baseVarSet = outputVars,
                                                                   timeResReporting = timeResReporting))
    outputVars$ext <- append(extendedTransportVarSet$ext)
    outputVars$int <- append(extendedTransportVarSet$int)
  } else if (reportAnalytics) {
    analyticsVarSet <- append(outputVars, reportAnalyticsVarSet(data = data,timeResReporting = timeResReporting))
    outputVars$ext <- append(analyticsVarSet$ext)
    outputVars$int <- append(analyticsVarSet$int)
  }

  #########################################################################
  ## Transfer output variables to MIF format
  #########################################################################
  if (reportTransportData) {
    reporting <- toolReportMIF(vars = outputVars,
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
    REMINDinputData <- toolReportREMINDinputData(baseVarSet$ext$fleetESdemand,
                                                 baseVarSet$ext$fleetFEdemand,
                                                 baseVarSet$int$fleetEnergyIntensity,
                                                 baseVarSet$int$fleetCost[variable == "Capital costs"],
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
