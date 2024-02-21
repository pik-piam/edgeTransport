

storeRDS <- function(SSPscen, transportPolScen, demScen, inputDataRaw, inputData, histPrefs,
                    storeFleetSizeAndCompositionIterations, storeEndogenousCostsIterations, fleetVariables) {
  # Save data
  folderName <- paste0(format(Sys.time(), "%Y-%m-%d_%H.%M"), SSPscen, "-", transportPolScen, "-", demScen)
  file.copy(gdxPath, file.path(outputFolder, folderName))
  cfg <- list(
    packageVersionEdgeTransport = packageVersion("edgeTransport"),
    packageVersionMrTransport = packageVersion("mrtransport"),
    SSPscen = SSPscen,
    transportPolScen = transportPolScen,
    demScen = demScen,
    timeStamp = format(Sys.time(), "%Y-%m-%d_%H.%M.%S"),
  )
  saveRDS(cfg, file.path(outputFolder, folderName, "cfg.RDS"))
  lapply(inputDataRaw,  function(x) saveRDS(x, file.path(outputFolder, folderName, "1_InputDataRaw", paste0(data.table.name(x), ".RDS"))))
  lapply(inputData,  function(x) saveRDS(x, file.path(outputFolder, folderName, "2_InputDataPolicy", paste0(data.table.name(x), ".RDS"))))
  saveRDS(histPrefs, file.path(outputFolder, folderName, "3_Calibration", "histPrefs.RDS"))
  for (i in seq(1, iterations, 1)) {
    saveRDS(storeFleetSizeAndCompositionIterations[[i]], file.path(outputFolder, folderName, "4_Output", paste0("Iteration", i), "fleetSizeAndComposition.RDS"))
    saveRDS(storeEndogenousCostsIterations[[i]], file.path(outputFolder, folderName, "4_Output", paste0("Iteration", i), "endogenousCosts.RDS"))
  }
  lapply(fleetVariables,  function(x) saveRDS(x, file.path(outputFolder, folderName, "4_Output", paste0("Iteration", iterations), paste0(data.table.name(x), ".RDS"))))

}
