#' Function that converts CAPEX and OPEX into US$2017/(p|t)km and provides them combined in a structured format
#'
#' @param CAPEXtrackedFleet CAPEX data for vehicle types that feature fleet tracking: Cars, trucks, busses
#' @param nonFuelOPEXtrackedFleet non-fuel OPEX data for vehicle types that feature fleet tracking: Cars, trucks, busses
#' @param CAPEXother CAPEX data for other vehicle types
#' @param nonFuelOPEXother non-fuel OPEX data for other vehicle types
#' @param fuelCosts fuel cost data
#' @param subsidies purchase price subsidy data
#' @param energyIntensity energy intensity data
#' @param loadFactor load factor data
#' @param annualMileage annual mileage data
#' @param annuity calculated annuity for different vehicle types
#' @param helpers list with helpers
#' @import data.table
#' @returns data.table including total costs of ownership in US$2017/(p|t)km


toolCombineCAPEXandOPEX <- function(CAPEXtrackedFleet,
                                    nonFuelOPEXtrackedFleet,
                                    CAPEXother,
                                    nonFuelOPEXother,
                                    fuelCosts,
                                    subsidies,
                                    energyIntensity,
                                    loadFactor,
                                    annualMileage,
                                    annuity,
                                    helpers){

  # Apply very random subsidy phase out from the old EDGE-T version -> Remove after reproduktion of old values
  subsidies[, value := ifelse(period >= 2020 & period <= 2030,
                              -(value * 1.14 * 0.78 * (1/15 * (period - 2020) - 1)),
                              value), by = c("region", "period", "value")]

  # Tracked fleet (LDV 4W, Trucks, Busses)
  # Annualize and discount CAPEX to convert to US$2017/veh/yr
  # Include subsidies on LDV 4 Wheelers
  upfrontCAPEXtrackedFleet <- rbind(CAPEXtrackedFleet, subsidies) # in US$2017/veh
  cols <- names(upfrontCAPEXtrackedFleet)
  cols <- cols[!cols %in% c("value", "variable")]
  upfrontCAPEXtrackedFleet[, .(value = sum(value)), by = cols][, variable := "Upfront capital costs sales"]
  annualizedCapexTrackedFleet <- merge(upfrontCAPEXtrackedFleet, annuity, by = "univocalName", allow.cartesian = TRUE)
  annualizedCapexTrackedFleet[, value := value * annuity][, unit := "US$2017/veh/yr"][, annuity := NULL]
  # Combine with non Fuel OPEX
  CAPEXandNonFuelOPEXtrackedFleet <- rbind(annualizedCapexTrackedFleet, nonFuelOPEXtrackedFleet)
  # Merge with annual mileage to convert to US$2017/vehkm
  annualMileage <- copy(annualMileage)
  annualMileage[, c("variable", "unit") := NULL]
  setnames(annualMileage, "value", "annualMileage")
  CAPEXandNonFuelOPEXtrackedFleet <- merge(CAPEXandNonFuelOPEXtrackedFleet, annualMileage, by = c("region", "univocalName", "technology", "period"))
  CAPEXandNonFuelOPEXtrackedFleet[, value := value / annualMileage][, unit := "US$2017/vehkm"][, annualMileage := NULL]

  # Combine with other modes of transport provided in US$2017/vehkm
  CAPEXandNonFuelOPEX <- rbind(CAPEXandNonFuelOPEXtrackedFleet, CAPEXother, nonFuelOPEXother)

  # Convert fuel costs from US$2017/MJ to US$2017/vehkm
  # Merge with energy intensity
  energyIntensity <- copy(energyIntensity)
  energyIntensity[, c("variable", "unit") := NULL]
  setnames(energyIntensity, "value", "energyIntensity")
  fuelCosts <- merge(fuelCosts, energyIntensity, by = c("region", "univocalName", "technology", "period"))
  fuelCosts[, value := value * energyIntensity][, unit := "US$2017/vehkm"][, energyIntensity := NULL]
  combinedCAPEXandOPEX <- rbind(CAPEXandNonFuelOPEX, fuelCosts)

  # Convert all cost components from US$2017/vehkm to US$2017/(p|t)km
  loadFactor <- copy(loadFactor)
  loadFactor[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  combinedCAPEXandOPEX <- merge(combinedCAPEXandOPEX, loadFactor, by = c("region", "univocalName", "technology", "period"))
  combinedCAPEXandOPEX[, value := value / loadFactor][, loadFactor := NULL]
  combinedCAPEXandOPEX[, unit := ifelse(univocalName %in% c(helpers$filter$trn_pass, "International Aviation"), "US$2017/pkm", "US$2017/tkm")]

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

  transportCosts <- list(
    combinedCAPEXandOPEX = combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = upfrontCAPEXtrackedFleet
  )

  return(transportCosts)
}
