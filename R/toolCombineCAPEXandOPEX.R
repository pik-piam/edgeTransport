#' Function that converts CAPEX and OPEX into US$2005/(p|t)km and provides them combined in a structured format
#' @param mrtransportInput list containing all mrtransport input data
#' @param annuity calculated annuity for different vehicle types
#' @param fuelCosts fuel costs from REMIND
#' @param subsidies subsides for alternative cars from mrremind
#' @param decisionTree edgeTransport decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated univocalNames
#' @import data.table
#' @returns data.table including total costs of ownership in US$2005/(p|t)km


toolCombineCAPEXandOPEX <- function(CAPEXtrackedFleet, nonFuelOPEXtrackedFleet, CAPEXother, nonFuelOPEXother, fuelCosts, subsidies, energyIntensity, loadFactor, annualMileage, annuity, helpers){

  # Tracked fleet (LDV 4W, Trucks, Busses)
    # Annualize and discount CAPEX to convert to US$2005/veh/yr
    # Include subsidies on LDV 4 Wheelers
    upfrontCAPEXtrackedFleet <- rbind(CAPEXtrackedFleet, subsidies) # in US$2005/veh
    cols <- names(upfrontCAPEXtrackedFleet)
    cols <- cols[!cols %in% c("value", "variable")]
    upfrontCAPEXtrackedFleet[, .(value = sum(value)), by = cols][, variable := "Upfront capital costs sales"]
    annualizedCapexTrackedFleet <- merge(upfrontCAPEXtrackedFleet, annuity, by = "univocalName", allow.cartesian = TRUE)
    annualizedCapexTrackedFleet[, value := value * annuity][, unit := "US$2005/veh/yr"][, annuity := NULL]
    # Combine with non Fuel OPEX
    CAPEXandNonFuelOPEXtrackedFleet <- rbind(annualizedCapexTrackedFleet, nonFuelOPEXtrackedFleet)
    # Merge with annual mileage to convert to US$2005/vehkm
    annualMileage <- copy(annualMileage)
    annualMileage[, c("variable", "unit") := NULL]
    setnames(annualMileage, "value", "annualMileage")
    CAPEXandNonFuelOPEXtrackedFleet <- merge(CAPEXandNonFuelOPEXtrackedFleet, annualMileage, by = c("region", "univocalName", "technology", "period"))
    CAPEXandNonFuelOPEXtrackedFleet[, value := value / annualMileage][, unit := "US$2005/vehkm"][, annualMileage := NULL]

  # Combine with other modes of transport provided in US$2005/vehkm
    CAPEXandNonFuelOPEX <- rbind(CAPEXandNonFuelOPEXtrackedFleet, CAPEXother, nonFuelOPEXother)

  # Convert fuel costs from US$2005/MJ to US$2005/vehkm
    # Merge with energy intensity
    energyIntensity <- copy(energyIntensity)
    energyIntensity[, c("variable", "unit") := NULL]
    setnames(energyIntensity, "value", "energyIntensity")
    fuelCosts <- merge(fuelCosts, energyIntensity, by = c("region", "univocalName", "technology", "period"))
    fuelCosts[, value := value * energyIntensity][, unit := "US$2005/vehkm"][, energyIntensity := NULL]
    combinedCAPEXandOPEX <- rbind(CAPEXandNonFuelOPEX, fuelCosts)

  # Convert all cost components from US$2005/vehkm to US$2005/(p|t)km
    loadFactor <- copy(loadFactor)
    loadFactor[, c("variable", "unit") := NULL]
    setnames(loadFactor, "value", "loadFactor")
    combinedCAPEXandOPEX <- merge(combinedCAPEXandOPEX, loadFactor, by = c("region", "univocalName", "technology", "period"))
    combinedCAPEXandOPEX[, value := value / loadFactor][, loadFactor := NULL]
    combinedCAPEXandOPEX[, unit := ifelse(univocalName %in% c(helpers$filter$trn_pass, "International Aviation"), "US$2005/pkm", "US$2005/tkm")]

  # add zeros for active modes (time value costs are treated seperately)
    # use dummy that does not feature fleet tracking
    dummy <- unique(combinedCAPEXandOPEX$univocalName)
    dummy <- dummy[!dummy %in% helpers$filterEntries$trackedFleet & dummy %in% helpers$filterEntries$trn_pass][1]
    walk <- combinedCAPEXandOPEX[univocalName == dummy][, technology := NULL]
    walk <- unique(walk)[, value := 0]
    walk[, univocalName := "Walk"][, technology := "Walk_tmp_technology"]
    cycle <- copy(walk)[, univocalName := "Cycle"][, technology := "Cycle_tmp_technology"]
    combinedCAPEXandOPEX <- rbind(combinedCAPEXandOPEX, walk, cycle)

    if (anyNA(combinedCAPEXandOPEX) == TRUE) {
      stop("combinedCAPEXandOPEX contain NAs")
    }

    transportCosts <- list(
      combinedCAPEXandOPEX = combinedCAPEXandOPEX,
      upfrontCAPEXtrackedFleet = upfrontCAPEXtrackedFleet
    )

    return(transportCosts)
}