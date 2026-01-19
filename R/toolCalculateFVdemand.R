#' Calculate eneryg service demand on fuel vehicle level
#'
#' @author Johanna Hoppe
#' @param sectorESdemand energy service demand on CES node level
#' @param salesAndModeShares vehicle sales and mode shares
#' @param helpers List with helpers
#' @param histESdemand historcial energy service demand for iterative EDGE-T
#' @param baseYear ned year of historical energy service demand
#' @returns data.table
#' @import data.table
#' @export

toolCalculateFVdemand <- function(sectorESdemand, salesAndModeShares, helpers, histESdemand = NULL,  baseYear = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- variable <- level <- FVSshare <- shareS1S <- shareS2S1 <- shareS3S2 <- shareVS3 <- shareFV <- NULL

# Calculate FV to sector shares --------------------------------------------
  selectLevels <- function(selectedLevel, dt) {
    dt <- dt[level == selectedLevel][, level := NULL]
    setnames(dt, "share", paste0("share", selectedLevel))
    emptyColumns <- names(dt)[colSums(dt == "") == nrow(dt)]
    if (length(emptyColumns) > 0) dt[, c(emptyColumns) := NULL]
    return(dt)
  }
  levels <- unique(salesAndModeShares$level)
  shares <- lapply(levels, selectLevels, copy(salesAndModeShares))

  FVSshares <- shares[[1]]
  for (i in seq(1, (length(shares) - 1), 1)) {
    FVSshares <- merge(FVSshares, shares[[i + 1]], by = intersect(names(FVSshares), names(shares[[i + 1]])))
  }

  FVSshares[, FVSshare := shareS1S * shareS2S1 * shareS3S2 * shareVS3 * shareFV]
  FVSshares <- FVSshares[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "period", "FVSshare")]
  # Apply shares on demand --------------------------------------------
  sectorESdemand[, variable := "ES"]
  if (!is.null(histESdemand)) {
    # REMIND energy service demand starts in 2005 -> get 1990 value from historical demand
    # Check whether this is really necessary
    histESdemand <- merge(histESdemand, helpers$decisionTree, by = intersect(names(histESdemand), names(helpers$decisionTree)))
    histESdemand <- histESdemand[, .(value = sum(value)), by = c("region", "period", "sector", "variable", "unit")]
    histESdemand <- histESdemand[period == 1990]
    sectorESdemand <- rbind(sectorESdemand, histESdemand[!period %in% unique(sectorESdemand$period)])
  }
  fuelVehicleESdemand <- merge(sectorESdemand, FVSshares, by = intersect(names(sectorESdemand), names(FVSshares)), all.y = TRUE)
  fuelVehicleESdemand[, value := value * FVSshare][, FVSshare := NULL]
  fuelVehicleESdemand <- toolOrderandCheck(fuelVehicleESdemand, helpers$decisionTree)

  return(fuelVehicleESdemand)
}
