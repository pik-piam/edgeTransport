toolReportEdgeTransport <- function(..., analytics = FALSE, REMINDinputOnly = FALSE, extendedReporting = FALSE) {

  args <- list(...)

  if (is.null(args$folderPath)) folderPath <- file.path(".","EDGE-T")
  else folderPath <- args$folderPath
  if (is.null(args$gdxPath)) {
    files <- list.files(path = folderPath, pattern = "\\.gdx$", full.names = TRUE)
     # Check if any files were found
    if (length(files) > 1) {
      files <- files[1]
      cat("More than one gdx file found. The following one was chosen\n")
      cat(files, sep = "\n")
    } else {
      stop("No gdx files found in the specified directory.\n")
    }
  }

  if (is.null(args$inputDataRaw)) {
    inputDataRaw <- readRDS(file.path(folderPath, "1_InputDataRaw", "inputDataRaw.RDS"))
  } else {inputDataRaw <- args$inputDataRaw}
  if (is.null(args$hybridElecShare)) {
    hybridElecShare <- readRDS(file.path(folderPath, "1_InputDataRaw", "hybridElecShare.RDS"))
  } else {hybridElecShare <- args$hybridElecShare}
  if (is.null(args$inputData)) {
    inputData <- readRDS(file.path(folderPath, "2_InputDataPolicy", "inputData.RDS"))
    } else {inputData <- args$inputData}

  if (is.null(args$ESdemandFuelVehicle)) {
    ESdemandFuelVehicle <- readRDS(file.path(folderPath, "4_Output", "ESdemandFuelVehicle.RDS"))
    } else {ESdemandFuelVehicle <- args$ESdemandFuelVehicle}
  if (is.null(args$fleetSizeAndComposition)) {
    fleetSizeAndCompositionIterations <- readRDS(file.path(folderPath, "4_Output", "fleetSizeAndCompositionIterations.RDS"))
    } else {fleetSizeAndComp <- args$fleetSizeAndComp}
  if (is.null(args$fleetSizeAndComp)) {
    endogenousCostsIterations <- readRDS(file.path(folderPath, "4_Output", "endogenousCostsIterations.RDS"))
  } else {endogenousCostsIterations <- args$endogenousCostsIterations}

  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an interpolation step in toolCalculateOutputVariables()
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  outputVars <- toolCalculateOutputVariables(inputDataRaw = inputDataRaw,
                                             hybridElecShare = hybridElecShare,
                                             inputData = inputData,
                                             ESdemandFuelVehicle = ESdemandFuelVehicle,
                                             fleetSizeAndCompositionIterations = fleetSizeAndCompositionIterations,
                                             endogenousCostsIterations = endogenousCostsIterations,
                                             gdx = gdxPath,
                                             timeResReporting =timeResReporting,
                                             REMINDinputOnly = REMINDinputOnly,
                                             extendedReporting = extendedReporting,
                                             analytics = analytics)

  toMIF <- toolReportAndAggregateMIF(outputVars, helpers, paste0(transportPolScen, " ", SSPscen), "EDGE-T", gdx, GDPMER)
  write.mif(toMIF, file.path(folderPath, "Transport.mif"))

  # save output vars
  lapply(vars, storeRDS, "4_Output")
}
