#' Load GDP and Population data from mrdrivers
#' @param SSPscen SSP scenario for which the mrdrivers input data shall be loaded
#' @param helpers list containg several helpers used throughout the model.
#'          It includes dtTimeRes, a data.table containing the temporal
#'          resolution for different univocalNames
#' @returns list of data.tables containing mrdrivers input data
#' @importFrom rmndt magpie2dt

toolLoadmrdriversData <- function(SSPscen, helpers) {

  #choose highest resolution for GDP
  years <- unique(helpers$dtTimeRes$period)

  GDPMERmag <- calcOutput("GDP", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                            unit = "constant 2017 US$MER")[, , paste0("gdp_", SSPscen)]|> time_interpolate(years)
  GDPMER <- magpie2dt(GDPMERmag, yearcol = "period", regioncol = "region")[, variable := NULL]
  GDPpcMERmag <- calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                         unit = "constant 2017 US$MER")[, , paste0("gdppc_", SSPscen)]|> time_interpolate(years)
  GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  GDPpppMag <- calcOutput("GDP", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv")
  GDPpppMag <- GDPpppMag[, , paste0("gdp_", SSPscen)]|> time_interpolate(years)
  GDPppp <- magpie2dt(GDPpppMag, yearcol = "period", regioncol = "region")[, variable := NULL]

  GDPpcPPPmag <- calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv")
  GDPpcPPPmag <- GDPpcPPPmag[, , paste0("gdppc_", SSPscen)]|> time_interpolate(years)
  GDPpcPPP <- magpie2dt(GDPpcPPPmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  POPmag <- calcOutput("Population", aggregate = TRUE,
                       regionmapping = "regionmapping_21_EU11.csv")[, , paste0("pop_", SSPscen)]|> time_interpolate(years)
  POP <- magpie2dt(POPmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  return(
   list(
    GDPMER = GDPMER,
    GDPpcMER = GDPpcMER,
    GDPppp = GDPppp,
    GDPpcPPP = GDPpcPPP,
    population = POP
   )
  )

}
