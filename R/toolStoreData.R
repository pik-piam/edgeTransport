toolStoreData <- function(...){

  vars <- list(...)
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
    dir.create(file.path(outputFolder, "1_InputDataRaw"))
    dir.create(file.path(outputFolder, "2_InputDataPolicy"))
    dir.create(file.path(outputFolder, "3_Calibration"))
    dir.create(file.path(outputFolder, "4_Output"))
  }

  # store general data if provided
  if (!(is.null(vars$SSPscen) & is.null(vars$transportPolScen) & is.null(vars$demScen))) {
    cfg <- list(
      packageVersionEdgeTransport = packageVersion("edgeTransport"),
      packageVersionMrTransport = packageVersion("mrtransport"),
      SSPscen = SSPscen,
      transportPolScen = transportPolScen,
      demScen = demScen,
      timeStamp = format(Sys.time(), "%Y-%m-%d_%H.%M")
    )
    saveRDS(cfg, file.path(outputFolder, "cfg.RDS"))
  }

  storeRDS <- function(varName, vars, outputfolder, subfolder) {
    saveRDS(vars[[varName]], file.path(outputFolder, subfolder, paste0(varName, ".RDS")))
  }


  if (!is.null(vars$gdxPath)) {file.copy(gdxPath, file.path(outputFolder))}

  # store raw input data
  if (!is.null(vars$inputDataRaw)) {
    varsToExclude <- c("GDPpcMER", "GDPpcPPP", "population")
    vars$inputDataRaw <- vars$inputDataRaw[!names(vars$inputDataRaw) %in% varsToExclude]
    lapply(names(vars$inputDataRaw), storeRDS, vars$inputDataRaw, outputfolder, "1_InputDataRaw")
  }
  if (!is.null(vars$hybridElecShare)){
    saveRDS(vars$hybridElecShare, file.path(outputFolder, "1_InputDataRaw", "hybridElecShare.RDS"))
  }
  if (!is.null(vars$helpers)){
    saveRDS(vars$helpers, file.path(outputFolder, "1_InputDataRaw", "helpers.RDS"))
  }

  # store transport policy specific Input data if provided
  # Some variables do not get policy specific changes
  if (!is.null(vars$inputData)){
    varsToExclude <- c("annualMileage", "timeValueCosts", "histESdemand", "GDPMER", "GDPpcMER", "population")
    vars$inputData <- vars$inputData[!names(vars$inputData) %in% varsToExclude]
    lapply(names(vars$inputData), storeRDS, vars$inputData, outputfolder, "2_InputDataPolicy")
  }

  # store calibration data if provided
  if (!is.null(vars$histPrefs)){
    lapply(names(vars$histPrefs), storeRDS, vars$histPrefs, outputfolder, "3_Calibration")
  }

  # store output data if provided
  if (!is.null(vars$fleetVehNumbersIterations)){
    for (i in 1:length(fleetVehNumbersIterations)) {
      # store fleetVehNumbers over iterations
      iteration <- unique(fleetSizeAndCompositionIterations[[i]][1]$iteration)
      fleetVehNumbersIterations[[i]][, variable := paste0(variable, "|Iteration ", iteration)][, iteration := NULL]
      saveRDS(vars$fleetSizeAndCompositionIterations[[i]], file.path(outputFolder, "4_Output", paste("fleetVehNumbersIteration", "", iteration, ".RDS")))
    }
  }
  if (!is.null(vars$endogenousCostsIterations)){
    for (i in 1:length(endogenousCostsIterations)) {
      iteration <- unique(endogenousCostsIterations[[i]][1]$iteration)
      endogenousCostsIterations[[i]][, variable := paste0(variable, "|Iteration ", iteration)][, iteration := NULL]
      saveRDS(vars$endogenousCostsIterations[[i]], file.path(outputFolder, "4_Output", paste("endogenousCostsIterations", "", iteration, ".RDS")))
    }
  }
  if (!is.null(vars$ESdemandFuelVehicle)){
    saveRDS(vars$ESdemandFuelVehicle, file.path(outputFolder, "4_Output", "ESdemandFuelVehicle.RDS"))
  }
  if (!is.null(vars$vehSalesAndModeShares)){
    saveRDS(vars$vehSalesAndModeShares, file.path(outputFolder, "4_Output", "vehSalesAndModeShares.RDS"))
  }
  if (!is.null(vars$fleetSizeAndComposition)){
    saveRDS(vars$fleetSizeAndComposition, file.path(outputFolder, "4_Output", "fleetSizeAndComposition.RDS"))
  }

  # store REMIND inputdata if provided
  if (!is.null(vars$REMINDinputData)) {
    if (!dir.exists(file.path(outputFolder, "5_REMINDinputData"))) {
      dir.create(file.path(outputFolder, "5_REMINDinputData"))
    }
    apply(names(vars$REMINDinputData), storeRDS, vars$REMINDinputData, outputFolder, "5_REMINDinputData")
  }
}


