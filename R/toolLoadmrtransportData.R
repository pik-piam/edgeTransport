#' Load input data from the mrtransport package
#'
#' @importFrom rmndt magpie2dt

toolLoadmrtransportData <- function(SSPscen, filterEntries, decisionTree) {

  # Energy Service demand [billion (p|t)km/yr]
  histESdemandMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                      regionmapping = "regionmapping_21_EU11.csv", subtype = "histESdemand", SSPscen = SSPscen)
  histESdemand <- magpie2dt(histESdemandMagpieobj)

  # Energy Intensity after IEA harmonization [MJ/vehkm]
  energyIntensityMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                         regionmapping = "regionmapping_21_EU11.csv", subtype = c("energyIntensity"), SSPscen = SSPscen)
  energyIntensity <- magpie2dt(energyIntensityMagpieobj)[, variable := paste0(variable, " (raw)")]

  # Load Factor [(p|t)/veh]
  loadFactorMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                    regionmapping = "regionmapping_21_EU11.csv", subtype = "loadFactor", SSPscen = SSPscen)
  loadFactor <- magpie2dt(loadFactorMagpieobj)[, variable := paste0(variable, " (raw)")]

  # CAPEX for the tracked fleet (cars, trucks, busses) [US$2005/veh]
  CAPEXtrackedFleetMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                           regionmapping = "regionmapping_21_EU11.csv", subtype = "CAPEXtrackedFleet", SSPscen = SSPscen)
  CAPEXtrackedFleet <- magpie2dt(CAPEXtrackedFleetMagpieobj)

  # non-fuel OPEX for the tracked fleet (cars, trucks, busses) [US$2005/veh/yr]
  nonFuelOPEXtrackedFleetMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                                 regionmapping = "regionmapping_21_EU11.csv", subtype = "nonFuelOPEXtrackedFleet", SSPscen = SSPscen)
  nonFuelOPEXtrackedFleet <- magpie2dt(nonFuelOPEXtrackedFleetMagpieobj)

  # CAPEX other [US$2005/vehkm]
  CAPEXotherMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                    regionmapping = "regionmapping_21_EU11.csv", subtype = "CAPEXother", SSPscen = SSPscen)
  CAPEXother <- magpie2dt(CAPEXotherMagpieobj)

  # non-fuel OPEX other [US$2005/vehkm]
  nonFuelOPEXotherMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                          regionmapping = "regionmapping_21_EU11.csv", subtype = "nonFuelOPEXother", SSPscen = SSPscen)
  nonFuelOPEXother <- magpie2dt(nonFuelOPEXotherMagpieobj)

  # Annual Mileage (currently only for the tracked fleet) [vehkm/veh/yr]
  annualMileageMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                       regionmapping = "regionmapping_21_EU11.csv", subtype = "annualMileage", SSPscen = SSPscen)
  annualMileage <- magpie2dt(annualMileageMagpieobj)

  # Time value costs [US$2005/pkm]
  timeValueCostsMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                      regionmapping = "regionmapping_21_EU11.csv", subtype = "timeValueCosts", SSPscen = SSPscen)
  timeValueCosts <- magpie2dt(timeValueCostsMagpieobj)
  timeValueCosts <- unique(timeValueCosts[, technology := NULL])

  mrtransportdata <- list(histESdemand = histESdemand,
                           energyIntensity = energyIntensity,
                           loadFactor = loadFactor,
                           CAPEXtrackedFleet = CAPEXtrackedFleet,
                           nonFuelOPEXtrackedFleet = nonFuelOPEXtrackedFleet,
                           CAPEXother = CAPEXother,
                           nonFuelOPEXother = nonFuelOPEXother,
                           annualMileage = annualMileage,
                           timeValueCosts = timeValueCosts
                           )
  #Übergangslösung bis Daten direkt in hoher auflösung aus mrtransport kommen

  highRes <- c(1990, seq(2005, 2100, by = 1), 2110, 2130, 2150)

  applytimeres <- function(dt, highRes, filter){
    cols <- names(dt)
    dthighRes <- dt[univocalName %in% filter]
    if (nrow(dthighRes) > 0) {
      dthighRes <- approx_dt(dthighRes, highRes, "period", "value", idxcols = cols[!cols %in% c("value", "period")], extrapolate = TRUE)
      }
    dt <- rbind(dt[!univocalName %in% filter], dthighRes)
  }

  mrtransportdata <- lapply(mrtransportdata, applytimeres, highRes, filterEntries$trackedFleet)
    browser()
  mrtransportdata[["histESdemand"]] <-  mrtransportdata[["histESdemand"]][period <= 2010]

  return(mrtransportdata)

}
