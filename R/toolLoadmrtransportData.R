#' Load input data from the mrtransport package
#'
#' @importFrom rmndt magpie2dt

toolLoadmrtransportData <- function() {

  # Energy Service demand [billion (p|t)km/yr]
  histESdemandMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                      regionmapping = "regionmapping_21_EU11.csv", subtype = "histESdemand")
  histESdemand <- magpie2dt(histESdemandMagpieobj)

  # Energy Intensity after IEA harmonization [MJ/vehkm]
  energyIntensityMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                         regionmapping = "regionmapping_21_EU11.csv", subtype = c("energyIntensity"))
  energyIntensity <- magpie2dt(energyIntensityMagpieobj)

  # Load Factor [(p|t)/veh]
  loadFactorMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                    regionmapping = "regionmapping_21_EU11.csv", subtype = "loadFactor")
  loadFactor <- magpie2dt(loadFactorMagpieobj)

  # CAPEX for the tracked fleet (cars, trucks, busses) [US$2005/veh]
  CAPEXtrackedFleetMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                           regionmapping = "regionmapping_21_EU11.csv", subtype = "CAPEXtrackedFleet")
  CAPEXtrackedFleet <- magpie2dt(CAPEXtrackedFleetMagpieobj)

  # non-fuel OPEX for the tracked fleet (cars, trucks, busses) [US$2005/veh/yr]
  nonFuelOPEXtrackedFleetMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                                 regionmapping = "regionmapping_21_EU11.csv", subtype = "nonFuelOPEXtrackedFleet")
  nonFuelOPEXtrackedFleet <- magpie2dt(nonFuelOPEXtrackedFleetMagpieobj)

  # CAPEX other [US$2005/vehkm]
  CAPEXotherMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                    regionmapping = "regionmapping_21_EU11.csv", subtype = "CAPEXother")
  CAPEXother <- magpie2dt(CAPEXotherMagpieobj)

  # non-fuel OPEX other [US$2005/vehkm]
  nonFuelOPEXotherMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                          regionmapping = "regionmapping_21_EU11.csv", subtype = "nonFuelOPEXother")
  nonFuelOPEXother <- magpie2dt(nonFuelOPEXotherMagpieobj)

  # Annual Mileage (currently only for the tracked fleet) [vehkm/veh/yr]
  annualMileageMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                       regionmapping = "regionmapping_21_EU11.csv", subtype = "annualMileage")
  annualMileage <- magpie2dt(annualMileageMagpieobj)

  # Speed of modes [km/h]
  speedOfModesMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                      regionmapping = "regionmapping_21_EU11.csv", subtype = "speedOfModes")
  speedOfModesother <- magpie2dt(speedOfModesMagpieobj)

  # Value of Time multiplier [-]
  valueOfTimeMultiplierMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = TRUE, warnNA = FALSE,
                                               regionmapping = "regionmapping_21_EU11.csv", subtype = "valueOfTimeMultiplier")
  valueOfTimeMultiplier <- magpie2dt(speedOfModesMagpieobj)

}
