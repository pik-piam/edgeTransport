toolCalculateFVdemand <- function(sectorESdemand, salesAndModeShares, helpers, histESdemand = NULL,  baseYear = NULL){

  toolCalcFVSshares <- function(sharesAllLevels) {

    selectLevels <- function(selectedLevel, dt) {
      dt <- dt[level == selectedLevel][, level := NULL]
      setnames(dt, "share", paste0("share", selectedLevel))
      emptyColumns <- names(dt)[colSums(dt == "") == nrow(dt)]
      if (length(emptyColumns) > 0) dt[, c(emptyColumns) := NULL]
      return(dt)
    }
    levels <- unique(sharesAllLevels$level)
    shares <- lapply(levels, selectLevels, copy(sharesAllLevels))

    FVSshares <- shares[[1]]
    for (i in seq(1, (length(shares) - 1), 1)) {
      FVSshares <- merge(FVSshares, shares[[i+1]], by = intersect(names(FVSshares), names(shares[[i+1]])))
    }
    FVSshares[, FVSshare := shareS1S * shareS2S1 * shareS3S2 * shareVS3 * shareFV]
    FVSshares <- FVSshares[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "period", "FVSshare")]
    return(FVSshares)
  }

  FVSshares <- toolCalcFVSshares(salesAndModeShares)
  fuelVehicleESdemand <- merge(sectorESdemand, FVSshares, by = intersect(names(sectorESdemand), names(FVSshares)), all = TRUE)
  fuelVehicleESdemand[, value := value * FVSshare][, FVSshare := NULL]

  if (!is.null(histESdemand)){
    fuelVehicleESdemand <- fuelVehicleESdemand[period > baseYear, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "unit", "period", "value")]
    fuelVehicleESdemand[, variable := "ES"]
    fuelVehicleESdemand <- merge(fuelVehicleESdemand, helpers$decisionTree, by = intersect(names(fuelVehicleESdemand), names(helpers$decisionTree)))
    histESdemand <- merge(histESdemand, helpers$decisionTree, by = intersect(names(histESdemand), names(helpers$decisionTree)))
    fuelVehicleESdemand <- rbind(fuelVehicleESdemand, histESdemand)
  }

  fuelVehicleESdemand <- toolOrderandCheck(fuelVehicleESdemand, helpers$decisionTree)

  return(fuelVehicleESdemand)
}



