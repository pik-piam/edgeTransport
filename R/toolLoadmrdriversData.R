#' Load GDP data from mrdrivers
#'
#' @importFrom rmndt magpie2dt

toolLoadmrdriversData <- function(SSPscenario, yrs) {

  GDPpcMERmag <- calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                         unit = "constant 2005 US$MER")[, yrs, paste0("gdppc_", SSPscenario)]
  GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region")[, variable := NULL]
  GDPpcPPPmag <- calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv")[, yrs, paste0("gdppc_", SSPscenario)]
  GDPpcPPP <- magpie2dt(GDPpcPPPmag, yearcol = "period", regioncol = "region")[, variable := NULL]

  return(list(
    GDPpcMER = GDPpcMER,
    GDPpcPPP = GDPpcPPP
  ))

}
