#' Load GDP and Population data from mrdrivers
#' @param SSPscen SSP scenarios for which the mrdrivers input data shall be loaded
#' @param helpers list containg several helpers used throughout the model.
#'          It includes dtTimeRes, a data.table containing the temporal
#'          resolution for different univocalNames
#' @param allEqYear Year after which policy differentiation sets in
#' @returns list of data.tables containing mrdrivers input data
#' @importFrom rmndt magpie2dt
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getComment time_interpolate
#'
toolLoadmrdriversData <- function(SSPscen, helpers, allEqYear) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  variable <- unit <- . <- value <- period <- subsectorL3 <- NULL

  # choose highest resolution for GDP
  years <- unique(helpers$dtTimeRes$period)

  getUnitGDP <- function(magObject) {
    # extracts the GDP unit from the comment of a magclass object
    unitGDP <- gsub(" unit: ", "", getComment(magObject)[2])
    return(unitGDP)
  }

  # calculate output for magclass objects: GDPMER, GDPpcMER, GDPppp, GDPpcPPP, POP
  GDPMERmag <- calcOutput("GDP",
                          scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                          naming = "scenario",
                          regionmapping = "regionmapping_21_EU11.csv",
                          unit = mrdrivers::toolGetUnitDollar())
  unitGDPMER <- getUnitGDP(GDPMERmag)

  GDPpcMERmag <- calcOutput("GDPpc",
                            scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                            naming = "scenario",
                            regionmapping = "regionmapping_21_EU11.csv",
                            unit = mrdrivers::toolGetUnitDollar())
  unitGDPpcMER <- getUnitGDP(GDPpcMERmag)

  GDPpppMag <- calcOutput("GDP",
                          scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                          naming = "scenario",
                          regionmapping = "regionmapping_21_EU11.csv")
  unitGDPppp <- getUnitGDP(GDPpppMag)

  GDPpcPPPmag <- calcOutput("GDPpc",
                            scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                            naming = "scenario",
                            regionmapping = "regionmapping_21_EU11.csv")
  unitGDPpcPPP <- getUnitGDP(GDPpcPPPmag)

  POPmag <- calcOutput("Population",
                       scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                       naming = "scenario",
                       regionmapping = "regionmapping_21_EU11.csv")

  # Filter GDP and Pop objects for SSPscen, interpolate timesteps and transform to data.tables for further processing
  GDPMERmagF <- GDPMERmag[, , SSPscen[2]] |> time_interpolate(years)
  GDPMER <- magpie2dt(GDPMERmagF, yearcol = "period", regioncol = "region")[, variable := "GDP|MER"][, unit := unitGDPMER]

  GDPpcMERmagF <- GDPpcMERmag[, , SSPscen[2]] |> time_interpolate(years)
  GDPpcMER <- magpie2dt(GDPpcMERmagF, yearcol = "period", regioncol = "region")[, variable := "GDPpc|MER"][, unit := unitGDPpcMER]

  GDPpppMagF <- GDPpppMag[, , SSPscen[2]] |> time_interpolate(years)
  GDPppp <- magpie2dt(GDPpppMagF, yearcol = "period", regioncol = "region")[, variable := "GDP|PPP"][, unit := unitGDPppp]

  GDPpcPPPmagF <- GDPpcPPPmag[, , SSPscen[2]] |> time_interpolate(years)
  GDPpcPPP <- magpie2dt(GDPpcPPPmagF, yearcol = "period", regioncol = "region")[, variable := "GDPpc|PPP"][, unit := unitGDPpcPPP]

  POPmagF <- POPmag[, , SSPscen[2]] |> time_interpolate(years)
  POP <- magpie2dt(POPmagF, yearcol = "period", regioncol = "region")[, variable := "Population"][, unit := "million"]

  # Check if a change of SSPscen with allEqYear is performed
  # If so, assemble data from two different scenario sets
  if (SSPscen[1] != SSPscen[2]){
    GDPMERmagO <- GDPMERmag[, , SSPscen[1]] |> time_interpolate(years[years <= allEqYear])
    GDPMERO <- magpie2dt(GDPMERmagO, yearcol = "period", regioncol = "region")[, variable := "GDP|MER"][, unit := unitGDPMER]
    GDPMER <- rbind(GDPMERO, GDPMER[period > allEqYear])

    GDPpcMERmagO <- GDPpcMERmag[, , SSPscen[1]] |> time_interpolate(years[years <= allEqYear])
    GDPpcMERO <- magpie2dt(GDPpcMERmagO, yearcol = "period", regioncol = "region")[, variable := "GDPpc|MER"][, unit := unitGDPpcMER]
    GDPpcMER <- rbind(GDPpcMERO, GDPpcMER[period > allEqYear])

    GDPpppMagO <- GDPpppMag[, , SSPscen[1]] |> time_interpolate(years[years <= allEqYear])
    GDPpppO <- magpie2dt(GDPpppMagO, yearcol = "period", regioncol = "region")[, variable := "GDP|PPP"][, unit := unitGDPppp]
    GDPppp <- rbind(GDPpppO, GDPppp[period > allEqYear])

    GDPpcPPPmagO <- GDPpcPPPmag[, , SSPscen[1]] |> time_interpolate(years[years <= allEqYear])
    GDPpcPPPO <- magpie2dt(GDPpcPPPmagO, yearcol = "period", regioncol = "region")[, variable := "GDPpc|PPP"][, unit := unitGDPpcPPP]
    GDPpcPPP <- rbind(GDPpcPPPO, GDPpcPPP[period > allEqYear])

    POPmagO <- POPmag[, , SSPscen[1]] |> time_interpolate(years[years <= allEqYear])
    PopO <- magpie2dt(POPmagO, yearcol = "period", regioncol = "region")[, variable := "Population"][, unit := "million"]
    POP <- rbind(PopO, POP[period > allEqYear])
  }

  list(GDPMER = GDPMER,
       GDPpcMER = GDPpcMER,
       GDPppp = GDPppp,
       GDPpcPPP = GDPpcPPP,
       population = POP)
}
