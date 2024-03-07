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
      timeStamp = format(Sys.time(), "%Y-%m-%d_%H.%M.%S")
    )
    saveRDS(cfg, file.path(outputFolder, "cfg.RDS"))
  }

  if (!is.null(vars$gdxPath)) {file.copy(gdxPath, file.path(outputFolder))}

  # store raw input data
  if (!is.null(vars$inputDataRaw)) {
    saveRDS(vars$inputDataRaw, file.path(outputFolder, "1_InputDataRaw", "inputDataRaw.RDS"))
  }
  if (!is.null(vars$hybridElecShare)){
    saveRDS(vars$hybridElecShare, file.path(outputFolder, "1_InputDataRaw", "hybridElecShare.RDS"))
  }

  # store transport policy specific Input data if provided
  # Some variables do not get policy specific changes
  if (!is.null(vars$inputData)){
    saveRDS(vars$inputData, file.path(outputFolder, "2_InputDataPolicy", "inputData.RDS"))
  }

  # store calibration data if provided
  if (!is.null(vars$histPrefs)){
    saveRDS(vars$histPrefs, file.path(outputFolder, "3_Calibration", "histPrefs.RDS"))
  }

  # store output data if provided
  if (!is.null(vars$fleetSizeAndCompositionIterations)){
    saveRDS(vars$fleetSizeAndCompositionIterations, file.path(outputFolder, "4_Output", "fleetSizeAndCompositionIterations.RDS"))
  }
  if (!is.null(vars$endogenousCostsIterations)){
    saveRDS(vars$endogenousCostsIterations, file.path(outputFolder, "4_Output", "endogenousCostsIterations.RDS"))
  }
  if (!is.null(vars$ESdemandFuelVehicle)){
    saveRDS(vars$ESdemandFuelVehicle, file.path(outputFolder, "4_Output", "ESdemandFuelVehicle.RDS"))
  }
  if (!is.null(vars$vehSalesAndModeShares)){
    saveRDS(vars$vehSalesAndModeShares, file.path(outputFolder, "4_Output", "vehSalesAndModeShares.RDS"))
  }

  # store REMIND inputdata if provided
  if (!is.null(vars$REMINDinputData)) {
    if (!dir.exists(file.path(outputFolder, "5_REMINDinputData"))) {
      dir.create(file.path(outputFolder, "5_REMINDinputData"))
    }
    apply(vars$REMINDinputData,  function(x) saveRDS(x, file.path(outputFolder, "5_REMINDinputData", paste0(data.table.name(x), ".RDS"))))
  }
}


