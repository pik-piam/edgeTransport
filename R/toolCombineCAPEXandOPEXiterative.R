#' Function that converts CAPEX and OPEX into US$/(p|t)km and provides them combined in a structured format
#'
#' @param CAPEXandNonFuelOPEX non-fuel OPEX and CAPEX data as combined in the previous EDGET run and loaded from RDS file
#' @param fuelCosts updated fuel cost from REMIND
#' @param energyIntensity energy intensity data
#' @param loadFactor load factor data
#' @param annualMileage annual mileage data
#' @param helpers list with helpers
#' @import data.table
#' @returns data.table including total costs of ownership in US$/(p|t)km


toolCombineCAPEXandOPEXiterative <- function(CAPEXandNonFuelOPEX,
                                    fuelCosts,
                                    energyIntensity,
                                    loadFactor,
                                    annualMileage,
                                    helpers){


  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- variable <- . <- unit <- univocalName <- technology <- NULL

  # Convert fuel costs from US$/MJ to US$/vehkm
  # Merge with energy intensity
  energyIntensity <- copy(energyIntensity)
  energyIntensity[, c("variable", "unit") := NULL]
  setnames(energyIntensity, "value", "energyIntensity")
  fuelCosts <- merge(fuelCosts, energyIntensity, by = c("region", "univocalName", "technology", "period"))
  fuelCosts[, value := value * energyIntensity][, unit := gsub("MJ", "vehkm", unit)][, energyIntensity := NULL]
  combinedCAPEXandOPEX <- rbind(CAPEXandNonFuelOPEX, fuelCosts)

  # Convert all cost components from US$/vehkm to US$/(p|t)km
  loadFactor <- copy(loadFactor)
  loadFactor[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  combinedCAPEXandOPEX <- merge(combinedCAPEXandOPEX, loadFactor, by = c("region", "univocalName", "technology", "period"))
  combinedCAPEXandOPEX[, value := value / loadFactor][, loadFactor := NULL]
  combinedCAPEXandOPEX[, unit := ifelse(univocalName %in% c(helpers$filterEntries$trn_pass, "International Aviation"), gsub("vehkm", "pkm", unit), gsub("vehkm", "tkm", unit))]

  # add zeros for active modes (time value costs are treated seperately)
  # use dummy that does not feature fleet tracking
  dummy <- unique(combinedCAPEXandOPEX$univocalName)
  dummy <- dummy[!dummy %in% helpers$filterEntries$trackedFleet & dummy %in% helpers$filterEntries$trn_pass][1]
  walk <- combinedCAPEXandOPEX[univocalName == dummy & technology == "Liquids"][, technology := NULL]
  walk <- unique(walk)[, value := 0]
  walk[, univocalName := "Walk"][, technology := "Walk_tmp_technology"]
  cycle <- copy(walk)[, univocalName := "Cycle"][, technology := "Cycle_tmp_technology"]
  combinedCAPEXandOPEX <- rbind(combinedCAPEXandOPEX, walk, cycle)

  if (anyNA(combinedCAPEXandOPEX) == TRUE | anyDuplicated(combinedCAPEXandOPEX) == TRUE) {
    stop("combinedCAPEXandOPEX contain NAs or duplicates")
  }

 return(combinedCAPEXandOPEX)
}
