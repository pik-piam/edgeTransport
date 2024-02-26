#' Load GDP data from mrdrivers
#'
#' @importFrom rmndt magpie2dt

toolLoadmrdriversData <- function(SSPscenario, helpers) {

  #choose highest resolution for GDP
  years <- unique(helpers$dtTimeRes$period)

  GDPMERmag <- calcOutput("GDP", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                            unit = "constant 2005 US$MER", average2020 = FALSE)[, , paste0("gdp_", SSPscenario)]|> time_interpolate(years)
  GDPMER <- magpie2dt(GDPMERmag, yearcol = "period", regioncol = "region")[, variable := NULL]
  GDPpcMERmag <- calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                         unit = "constant 2005 US$MER", average2020 = FALSE)[, , paste0("gdppc_", SSPscenario)]|> time_interpolate(years)
  GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  GDPpcPPPmag <- calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv", average2020 = FALSE)[, , paste0("gdppc_", SSPscenario)]|> time_interpolate(years)
  GDPpcPPP <- magpie2dt(GDPpcPPPmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  POPmag <- calcOutput("Population", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv")[, , paste0("pop_", SSPscenario)]|> time_interpolate(years)
  POP <- magpie2dt(POPmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  return(list(
    GDPMER = GDPMER,
    GDPpcMER = GDPpcMER,
    GDPpcPPP = GDPpcPPP,
    population = POP
  ))

}
