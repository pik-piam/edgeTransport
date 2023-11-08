#' Load input data from the mrtransport package
#'
#' @importFrom rmndt magpie2dt

toolLoadmrtransportData <- function(SSPscen) {

  replaceMadratSnakeInUnivocalName <- function(dt){
    dt <- dt[grepl("Truck.*", univocalName), univocalName := gsub("_", ".", univocalName)]
  }

  # Energy Service demand [billion (p|t)km/yr]
  histESdemandMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                      regionmapping = "regionmapping_21_EU11.csv", subtype = "histESdemand", SSPscen = SSPscen)
  histESdemand <- magpie2dt(histESdemandMagpieobj)

  # Energy Intensity after IEA harmonization [MJ/vehkm]
  energyIntensityMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                         regionmapping = "regionmapping_21_EU11.csv", subtype = c("energyIntensity"), SSPscen = SSPscen)
  energyIntensity <- magpie2dt(energyIntensityMagpieobj)

  # Load Factor [(p|t)/veh]
  loadFactorMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                    regionmapping = "regionmapping_21_EU11.csv", subtype = "loadFactor", SSPscen = SSPscen)
  loadFactor <- magpie2dt(loadFactorMagpieobj)

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

  mrtransportdata <- sapply(mrtransportdata, replaceMadratSnakeInUnivocalName, USE.NAMES = TRUE)

  return(mrtransportdata)

}
