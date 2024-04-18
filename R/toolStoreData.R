toolStoreData <- function(outputFolder, outputRaw = NULL, ...){

  allocateFile <- function(varName) {
    subfolder <- NULL
    if (varName %in% c("hybridElecShare",
                       "histESdemand",
                       "energyIntensityRaw",
                       "loadFactorRaw",
                       "annualMileage",
                       "CAPEXtrackedFleet",
                       "nonFuelOPEXtrackedFleet",
                       "CAPEXother",
                       "nonFuelOPEXother",
                       "fuelCosts",
                       "timeValueCosts",
                       "subsidies",
                       "GDPMER",
                       "helpers")) subfolder <- "1_InputDataRaw"
    if (varName %in% c("prefTrends",
                       "loadFactor",
                       "enIntensity",
                       "combinedCAPEXandOPEX",
                       "upfrontCAPEXtrackedFleet",
                       "initialIncoCosts")) subfolder <- "2_InputDataPolicy"
    if (varName %in% c("histPrefs")) subfolder <- "3_Calibration"
    if (varName %in% c("fleetSizeAndComposition",
                       "vehSalesAndModeShares",
                       "fleetVehNumbersIterations",
                       "endogenousCosts",
                       "ESdemandFVsalesLevel")) subfolder <- "4_Output"
    if (varName %in% c("REMINDinputData")) subfolder <- "5_REMINDinputData"
    if (is.null(subfolder)) stop(paste0("No subfolder assigned to ", varName))

    return(subfolder)
  }

  storeRDS <- function(varName, vars, outputFolder, subfolder = NULL) {
      if (is.null(subfolder)) subfolder <- allocateFile(varName)
      saveRDS(vars[[varName]], file.path(outputFolder, subfolder, paste0(varName, ".RDS")))
  }

  storeCSV <- function(varName, vars, outputFolder, subfolder = NULL) {
    if (is.null(subfolder)) subfolder <- allocateFile(varName)
    write.csv(vars[[varName]], file.path(outputFolder, subfolder, paste0(varName, ".csv")))
  }

  vars <- list()
  if (!is.null(outputRaw)) vars <- outputRaw
  addVars <- list(...)
  vars <- append(vars, addVars)

  #########################################################################
  ## Create output folder and subfolders
  #########################################################################

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
    dir.create(file.path(outputFolder, "1_InputDataRaw"))
    dir.create(file.path(outputFolder, "2_InputDataPolicy"))
    dir.create(file.path(outputFolder, "3_Calibration"))
    dir.create(file.path(outputFolder, "4_Output"))
  }

  #########################################################################
  ## Store special cases (files that should be stored in a different way
  ## than transferred in vars)
  #########################################################################
  # general data
  if (!(is.null(vars$SSPscen) & is.null(vars$transportPolScen) & is.null(vars$demScen))) {
    cfg <- list(
      packageVersionEdgeTransport = packageVersion("edgeTransport"),
      packageVersionMrTransport = packageVersion("mrtransport"),
      SSPscen = vars$SSPscen,
      transportPolScen = vars$transportPolScen,
      demScen = vars$demScen,
      timeStamp = format(Sys.time(), "%Y-%m-%d_%H.%M")
    )
    saveRDS(cfg, file.path(outputFolder, "cfg.RDS"))
    vars <- vars[!names(vars) %in% c("SSPscen", "transportPolScen", "demScen")]
  }
  if (!is.null(vars$gdxPath)) {
    file.copy(gdxPath, file.path(outputFolder))
    vars <- vars[!names(vars) %in% c("gdxPath")]}

  # store calibration data if provided
  if (!is.null(vars$histPrefs)){
    lapply(names(vars$histPrefs), storeRDS, vars$histPrefs, outputFolder, "3_Calibration")
    vars <- vars[!names(vars) %in% c("histPrefs")]
  }

  # store output data if provided
  if (!is.null(vars$fleetVehNumbersIterations)){
    for (i in 1:length(fleetVehNumbersIterations)) {
      # store fleetVehNumbers over iterations
      iteration <- unique(fleetSizeAndCompositionIterations[[i]][1]$iteration)
      fleetVehNumbersIterations[[i]][, variable := paste0(variable, "|Iteration ", iteration)][, iteration := NULL]
      saveRDS(vars$fleetSizeAndCompositionIterations[[i]], file.path(outputFolder, "4_Output", paste("fleetVehNumbersIteration", "", iteration, ".RDS")))
      vars <- vars[!names(vars) %in% c("fleetVehNumbersIterations")]
    }
  }
  if (!is.null(vars$endogenousCostsIterations)){
    for (i in 1:length(endogenousCostsIterations)) {
      iteration <- unique(endogenousCostsIterations[[i]][1]$iteration)
      endogenousCostsIterations[[i]][, variable := paste0(variable, "|Iteration ", iteration)][, iteration := NULL]
      saveRDS(vars$endogenousCostsIterations[[i]], file.path(outputFolder, "4_Output", paste("endogenousCostsIterations", "", iteration, ".RDS")))
      vars <- vars[!names(vars) %in% c("endogenousCostsIterations")]
    }
  }

  # store REMIND inputdata if provided
  if (!is.null(vars$REMINDinputData)) {
    if (!dir.exists(file.path(outputFolder, "5_REMINDinputData"))) {
      dir.create(file.path(outputFolder, "5_REMINDinputData"))
    }
    lapply(names(vars$REMINDinputData), storeCSV, vars$REMINDinputData, outputFolder, "5_REMINDinputData")
    vars <- vars[!names(vars) %in% c("REMINDinputData")]
  }

  ###########################################################################
  ## Remaining variables are allocated to a subfolder and stored as RDS files
  ###########################################################################
  if (!is.null(vars)) invisible(lapply(names(vars), storeRDS, vars, outputFolder))

}


