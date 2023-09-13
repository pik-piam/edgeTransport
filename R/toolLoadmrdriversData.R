#' Load GDP data from mrdrivers
#'
#' @importFrom rmndt magpie2dt

toolLoadmrdriversData <- function(SSPscenario) {

  GDPpcMER <- magpie2dt(calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                         unit = "constant 2005 Int$MER", scenario = SSPscenario))
  GDPpcPPP <- magpie2dt(calcOutput("GDPpc", aggregate = TRUE, regionmapping = "regionmapping_21_EU11.csv",
                                   unit = "constant 2005 Int$PPP", scenario = SSPscenario))

  return(list(
    GDPpcMER = GDPpcMER,
    GDPpcPPP = GDPpcPPP
  ))

}
