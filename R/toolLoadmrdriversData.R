#' Load GDP and Population data from mrdrivers
#' @param SSPscen SSP scenarios for which the mrdrivers input data shall be loaded
#' @param helpers list containg several helpers used throughout the model.
#'          It includes dtTimeRes, a data.table containing the temporal
#'          resolution for different univocalNames
#' @returns list of data.tables containing mrdrivers input data
#' @importFrom rmndt magpie2dt
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getComment time_interpolate
#'
toolLoadmrdriversData <- function(SSPscen, helpers, cm_startYear = 2025) {
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
                          scenario = c("SSPs", "SDPs"),
                          naming = "scenario",
                          regionmapping = "regionmapping_21_EU11.csv",
                          unit = mrdrivers::toolGetUnitDollar())
  unitGDPMER <- getUnitGDP(GDPMERmag)

  GDPpcMERmag <- calcOutput("GDPpc",
                            scenario = c("SSPs", "SDPs"),
                            naming = "scenario",
                            regionmapping = "regionmapping_21_EU11.csv",
                            unit = mrdrivers::toolGetUnitDollar())
  unitGDPpcMER <- getUnitGDP(GDPpcMERmag)

  GDPpppMag <- calcOutput("GDP",
                          scenario = c("SSPs", "SDPs"),
                          naming = "scenario",
                          regionmapping = "regionmapping_21_EU11.csv")
  unitGDPppp <- getUnitGDP(GDPpppMag)

  GDPpcPPPmag <- calcOutput("GDPpc",
                            scenario = c("SSPs", "SDPs"),
                            naming = "scenario",
                            regionmapping = "regionmapping_21_EU11.csv")
  unitGDPpcPPP <- getUnitGDP(GDPpcPPPmag)

  POPmag <- calcOutput("Population",
                       scenario = c("SSPs", "SDPs"),
                       naming = "scenario",
                       regionmapping = "regionmapping_21_EU11.csv")

  # Filter GDP and Pop objects for SSPscen, interpolate timesteps and transform to data.tables for further processing
  GDPMERmag <- GDPMERmag[, , SSPscen[2]] |> time_interpolate(years)
  GDPMER <- magpie2dt(GDPMERmag, yearcol = "period", regioncol = "region")[, variable := "GDP|MER"][, unit := unitGDPMER]

  GDPpcMERmag <- GDPpcMERmag[, , SSPscen[2]] |> time_interpolate(years)
  GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region")[, variable := "GDPpc|MER"][, unit := unitGDPpcMER]

  GDPpppMag <- GDPpppMag[, , SSPscen[2]] |> time_interpolate(years)
  GDPppp <- magpie2dt(GDPpppMag, yearcol = "period", regioncol = "region")[, variable := "GDP|PPP"][, unit := unitGDPppp]

  GDPpcPPPmag <- GDPpcPPPmag[, , SSPscen[2]] |> time_interpolate(years)
  GDPpcPPP <- magpie2dt(GDPpcPPPmag, yearcol = "period", regioncol = "region")[, variable := "GDPpc|PPP"][, unit := unitGDPpcPPP]

  POPmag <- POPmag[, , SSPscen[2]] |> time_interpolate(years)
  POP <- magpie2dt(POPmag, yearcol = "period", regioncol = "region")[, variable := "Population"][, unit := "million"]

  # Check if a change of SSPscen with cm_startYear is performed
  # If so, assemble data from two different scenario sets
  if (SSPscen[1] != SSPscen[2]){
    GDPMERmagO <- GDPMERmag[, , c(SSPscen[1])] |> time_interpolate(years[years <= cm_startYear])
    GDPMERO <- magpie2dt(GDPMERmagO, yearcol = "period", regioncol = "region")[, variable := "GDP|MER"][, unit := unitGDPMER]
    GDPMER <- rbind(GDPMERO, GDPMER[period > cm_startYear])

    GDPpcMERmagO <- GDPpcMERmag[, , c(SSPscen[1])] |> time_interpolate(years[years <= cm_startYear])
    GDPpcMERO <- magpie2dt(GDPpcMERmagO, yearcol = "period", regioncol = "region")[, variable := "GDPpc|MER"][, unit := unitGDPpcMER]
    GDPpcMER <- rbind(GDPpcMERO, GDPpcMER[period > cm_startYear])

    GDPpppMagO <- GDPpppMag[, , c(SSPscen[1])] |> time_interpolate(years[years <= cm_startYear])
    GDPpppO <- magpie2dt(GDPpppMag, yearcol = "period", regioncol = "region")[, variable := "GDP|PPP"][, unit := unitGDPppp]
    GDPppp <- rbind(GDPpppO, GDPppp[period > cm_startYear])

    GDPpcPPPmagO <- GDPpcPPPmag[, , c(SSPscen[1])] |> time_interpolate(years[years <= cm_startYear])
    GDPpcPPPO <- magpie2dt(GDPpcPPPmag, yearcol = "period", regioncol = "region")[, variable := "GDPpc|PPP"][, unit := unitGDPpcPPP]
    GDPpcPPP <- rbind(GDPpcPPPO, GDPpcPPP[period > cm_startYear])

    POPmagO <- POPmag[, , SSPscen[1]] |> time_interpolate(years[years <= cm_startYear])
    PopO <- magpie2dt(POPmagO, yearcol = "period", regioncol = "region")[, variable := "Population"][, unit := "million"]
    POP <- rbind(PopO, POP[period > cm_startYear])
  }

  list(GDPMER = GDPMER,
       GDPpcMER = GDPpcMER,
       GDPppp = GDPppp,
       GDPpcPPP = GDPpcPPP,
       population = POP)
}
