#' Function that converts CAPEX and OPEX into US$2005/(p|t)km and provides them combined in a structured format
#' @param mrtransportInput list containing all mrtransport input data
#' @param annuity calculated annuity for different vehicle types
#' @param fuelCosts fuel costs from REMIND
#' @param subsidies subsides for alternative cars from mrremind
#' @param decisionTree edgeTransport decision tree
#' @param years temporal resolution of edgeTransport
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated univocalNames
#' @import data.table
#' @returns data.table including total costs of ownership in US$2005/(p|t)km


toolCombineCAPEXandOPEX <- function(CAPEXtrackedFleet, nonFuelOPEXtrackedFleet, CAPEXother, nonFuelOPEXother, fuelCosts, subsidies, energyIntensity, loadFactor, annualMileage, annuity, years, helpers){

  # Tracked fleet (LDV 4W, Trucks, Busses)
    # Annualize and discount CAPEX to convert to US$2005/veh/yr
    # Include subsidies on LDV 4 Wheelers
    annualizedCapexTrackedFleet <- rbind(CAPEXtrackedFleet, subsidies)
    annualizedCapexTrackedFleet <- merge(annualizedCapexTrackedFleet, annuity, by = "univocalName", allow.cartesian = TRUE)
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
    nonMotorized <- unique(helpers$decisionTree[univocalName %in% c("Cycle", "Walk"), c("region", "univocalName", "technology")])
    nonMotorized[, value := 0][, variable := "Price non motorized"][, unit := "US$2005/pkm"][, temporal := "All"]
    temporal <- data.table(period = years)[, temporal := "All"]
    nonMotorized <- merge(nonMotorized,  temporal, by = "temporal", allow.cartesian = TRUE)[, temporal := NULL]

    combinedCAPEXandOPEX <- rbind(combinedCAPEXandOPEX, nonMotorized)

    if (anyNA(combinedCAPEXandOPEX) == TRUE) {
      stop("combinedCAPEXandOPEX contain NAs")
    }

    return(combinedCAPEXandOPEX)
}
