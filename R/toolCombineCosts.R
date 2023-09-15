#' Function that converts CAPEX and OPEX into US$2005/(p|t)km and provides them combined in a structured format
#'
#' @import data.table


toolCombineCosts <- function(mrtransportInput, annuity, fuelCosts){
  
  # Tracked fleet (LDV 4W, Trucks, Busses)
    # Annualize and discount CAPEX to convert to US$2005/veh/yr
    annualizedCapexTrackedFleet <- merge(mrtransportInput$CAPEXtrackedFleet, annuity, by = vehicleType, allow.cartesian = TRUE)
    annualizedCapexTrackedFleet[, value := value * annuity][, unit := "US$2005/veh/yr"]
    # Combine with non Fuel OPEX
    APEXandNonFuelOPEXtrackedFleet <- rbind(annualizedCapexTrackedFleet, mrtransportInput$nonFuelOPEXtrackedFleet)
    # Merge with annual mileage to convert to US$2005/vehkm
    annualMileage <- mrtransportInput$annualMileage
    annualMileage[, c("variable", "unit") := NULL]
    setnames(annualMileage, "value", "annualMileage")
    APEXandNonFuelOPEXtrackedFleet <- merge(APEXandNonFuelOPEXtrackedFleet, annualMileage, by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                                                                      "technology", "univocalName", "period"))
    CAPEXandNonFuelOPEXtrackedFleet[, value := value / annualMileage][, unit := "US$2005/vehkm"]
  
  # Combine with other modes of transport provided in US$2005/vehkm
    CAPEXandNonFuelOPEX <- rbind(CAPEXandNonFuelOPEXtrackedFleet, mrtransportInput$CAPEXother, mrtransportInput$nonFuelOPEXother)
    
  # Convert fuel costs from US$2005/MJ to US$2005/vehkm 
    # Merge with energy intensity
    energyIntensity <- mrtransportInput$energyIntensity
    energyIntensity[, c("variable", "unit") := NULL]
    setnames(energyIntensity, "value", "energyIntensity")
    fuelCosts <- merge(fuelCosts, energyIntensity, by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                         "technology", "univocalName", "period"))
    fuelCosts[, value := value * energyIntensity][, unit := "US$2005/vehkm"][, energyIntensity := NULL]
    
    combinedCAPEXandOPEX <- rbind(CAPEXandNonFuelOPEX, fuelCosts)
    
  # Convert all cost components from US$2005/vehkm to US$2005/(p|t)km
    loadFactor <- mrtransportInput$loadFactor
    loadFactor[, c("variable", "unit") := NULL]
    setnames(loadFactor, "value", "loadFactor")
    combinedCAPEXandOPEX <- merge(combinedCAPEXandOPEX, loadFactor, by = c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                          "technology", "univocalName", "period"))
    combinedCAPEXandOPEX[, value := value / loadFactor][, loadFactor := NULL]
    combinedCAPEXandOPEX[, unit := ifelse(sector %in% c("trn_pass", "trn_aviation_intl", "US$2005/pkm", "US$2005/tkm"))]
    
    
    if (anyNA(combinedCAPEXandOPEX) == TRUE) {
      stop("combinedCAPEXandOPEX contain NAs")
    }
    
    return(combinedCAPEXandOPEX)
}