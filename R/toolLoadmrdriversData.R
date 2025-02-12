#' Load GDP and Population data from mrdrivers
#' @param SSPscen SSP scenario for which the mrdrivers input data shall be loaded
#' @param helpers list containg several helpers used throughout the model.
#'          It includes dtTimeRes, a data.table containing the temporal
#'          resolution for different univocalNames
#' @returns list of data.tables containing mrdrivers input data
#' @importFrom rmndt magpie2dt
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getComment time_interpolate
#'
toolLoadmrdriversData <- function(SSPscen, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  variable <- unit <- . <- value <- period <- subsectorL3 <- NULL

  # choose highest resolution for GDP
  years <- unique(helpers$dtTimeRes$period)

  GDPMERmag <- calcOutput("GDP",
                          scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                          naming = "scenario",
                          regionmapping = "regionmapping_21_EU11.csv",
                          unit = mrdrivers::toolGetUnitDollar())
  unitGDP <- gsub(" unit: ", "", getComment(GDPMERmag)[2])
  GDPMERmag <- GDPMERmag[, , SSPscen] |> time_interpolate(years)
  GDPMER <- magpie2dt(GDPMERmag, yearcol = "period", regioncol = "region")[, variable := "GDP|MER"][, unit := unitGDP]

  GDPpcMERmag <- calcOutput("GDPpc",
                            scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                            naming = "scenario",
                            regionmapping = "regionmapping_21_EU11.csv",
                            unit = mrdrivers::toolGetUnitDollar())
  unitGDP <- gsub(" unit: ", "", getComment(GDPpcMERmag)[2])
  GDPpcMERmag <- GDPpcMERmag[, , SSPscen] |> time_interpolate(years)
  GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region")[, variable := "GDPpc|MER"][, unit := unitGDP]

  GDPpppMag <- calcOutput("GDP",
                          scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                          naming = "scenario",
                          regionmapping = "regionmapping_21_EU11.csv")
  unitGDP <- gsub(" unit: ", "", getComment(GDPpppMag)[2])
  GDPpppMag <- GDPpppMag[, , SSPscen] |> time_interpolate(years)
  GDPppp <- magpie2dt(GDPpppMag, yearcol = "period", regioncol = "region")[, variable := "GDP|PPP"][, unit := unitGDP]

  GDPpcPPPmag <- calcOutput("GDPpc",
                            scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                            naming = "scenario",
                            regionmapping = "regionmapping_21_EU11.csv")
  unitGDP <- gsub(" unit: ", "", getComment(GDPpcPPPmag)[2])
  GDPpcPPPmag <- GDPpcPPPmag[, , SSPscen] |> time_interpolate(years)
  GDPpcPPP <- magpie2dt(GDPpcPPPmag, yearcol = "period", regioncol = "region")[, variable := "GDPpc|PPP"][, unit := unitGDP]

  POPmag <- calcOutput("Population",
                       scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                       naming = "scenario",
                       regionmapping = "regionmapping_21_EU11.csv")[, , SSPscen] |> time_interpolate(years)
  POP <- magpie2dt(POPmag, yearcol = "period", regioncol = "region")[, variable := "Population"][, unit := "million"]

  list(GDPMER = GDPMER,
       GDPpcMER = GDPpcMER,
       GDPppp = GDPppp,
       GDPpcPPP = GDPpcPPP,
       population = POP)
}
