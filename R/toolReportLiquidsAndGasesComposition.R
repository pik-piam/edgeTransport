#'Report the split of liquids and gases into fossil|bio|hydrogen
#'
#' @param dtFE Final energy data for liquids and gases
#' @param gdxPath Path to REMIND gdx, which contains the share of the various production routes for liquid and gaseous energy carriers
#' @param helpers List of helpers
#'
#' @returns Final energy for liquids and gases split into fossil|bio|hydrogen
#' @author Johanna Hoppe
#' @importFrom gdxdt readGDX
#' @importFrom rmndt magpie2dt
#' @import data.table
#' @export

toolReportLiquidsAndGasesComposition <- function(dtFE, gdxPath, timeResReporting, helpers) {

  calcSplit <- function(REMINDsegment, dataREMIND, splitOverall, timeResReporting) {

    # Final energy carrier types liquids and gases consist of the following secondary energy carrier types in REMIND
    mixedCarrierTypes <- c("seliqfos", "seliqbio", "seliqsyn", "segafos", "segabio", "segasyn")
    # Final energy carrier type liquids used in LDVs is listed under "fepet" in REMIND and liquids used for other modes (except for bunkers)
    # are listed under "fedie". The emissions from these energy carrier types fall under the effort sharing regulation(ES).
    # For gases there is no differentiation between LDVs and other modes + there are not used in bunkers at all
    dataREMIND <- switch(
      REMINDsegment,
      LDVs = dataREMIND[to %in% c("fepet", "fegat") & type == "ES"],
      nonLDVs = dataREMIND[to %in% c("fedie", "fegat") & type == "ES"],
      bunker = dataREMIND[to == "fedie" & type == "other"]
    )

    # some regions do not have all entrys -> add zeros to fill the gaps
    dummy <- unique(dataREMIND[, c("region", "period")])[, all := "All"]
    carrier <- unique(dataREMIND[, c("from", "to", "emiSectors", "type")])[, all := "All"]
    dummy <- merge(dummy, carrier, by = intersect(names(carrier), names(dummy)), allow.cartesian = TRUE)
    dataREMIND <- merge(dummy, dataREMIND, by = intersect(names(dataREMIND), names(dummy)), all = TRUE)
    dataREMIND[is.na(value), value := 0]

    dataLiquids <- merge(dataREMIND[from %in% mixedCarrierTypes[grepl(".*liq.*", mixedCarrierTypes)]], splitOverall$liqBioToSyn, by = c("region", "period"))
    dataGases <- merge(dataREMIND[from %in% mixedCarrierTypes[grepl(".*ga.*", mixedCarrierTypes)]], splitOverall$gasesBioToSyn, by = c("region", "period"))

    # calc shares liquids (synfuels -> Liquids|Hydrogen)
    # the fossil share is kept constant, the remaining amount is distributed according to the biotosynshareoverall and the synToBioShareOverall
    # e.g. 500 -> 300 is disrtributed to fossil (in accordance to fossil share)
    # remaining 200 is distributed liked this: bio -> 200 * biotosynshareoverall, syn <- 200 * synToBioShareOverall
    dataLiquids[, Fossil := value[from == "seliqfos"] / sum(value), by = c("region", "period")]
    dataLiquids[, Biomass := sum(value[from %in% c("seliqbio", "seliqsyn")]) /
                  sum(value) * bioToSynShareOverall, by = c("region", "period")]
    dataLiquids[, Hydrogen := sum(value[from %in% c("seliqbio", "seliqsyn")]) / sum(value)
                * synToBioShareOverall, by = c("region", "period")]
    cols <- c("region", "period", "Fossil", "Hydrogen", "Biomass")
    sharesLiquids <- unique(dataLiquids[, ..cols])
    sharesLiquids <- melt(sharesLiquids, id.vars = c("region", "period"), variable.name = "fuel")[, variable := paste0("Share|Liquids|", fuel)][, technology := "Liquids"]

    # calc shares gases i.a. (syngases -> Gases|Hydrogen)
    if (nrow(dataGases) > 0) {
      dataGases[, Fossil := ifelse(!sum(value) == 0, value[from == "segafos"] / sum(value), 0), by = c("region", "period")]
      dataGases[, Biomass := ifelse(!sum(value) == 0, sum(value[from %in% c("segabio", "segasyn")]) / sum(value)
                                    *  bioToSynShareOverall, 0), by = c("region", "period")]
      dataGases[, Hydrogen := ifelse(!sum(value) == 0, sum(value[from %in% c("segabio", "segasyn")]) / sum(value)
                                     * synToBioShareOverall, 0), by = c("region", "period")]
      sharesGases <- unique(dataGases[, ..cols])
      sharesGases <- melt(sharesGases, id.vars = c("region", "period"), variable.name = "fuel")[, variable := paste0("Share|Gases|", fuel)][, technology := "Gases"]
      shares <- rbind(sharesLiquids, sharesGases)

    } else {shares <- sharesLiquids}

    # apply low time resolution
    shares <- approx_dt(shares, timeResReporting, "period", "value", extrapolate = TRUE)
    shares[, sum := sum(value), by = c("region", "period", "technology")]

    if (anyNA(shares) | nrow(shares[(sum < 0.9999 | sum > 1.0001) & sum != 0])) stop("Something went wrong with the mixed carrier splitting. Please check calcSplit()")
    shares[, c("sum") := NULL]
    return(shares)
  }

  applySplit <- function(REMINDsegment, FEdata, mixedCarrierSplits, helpers) {

    # map EDGE-T categories on segements
    modes <- list(
      LDVs = c(helpers$filterEntries$trn_pass_road_LDV_4W,
               helpers$filterEntries$trn_pass_road_LDV_2W),
      nonLDVs = c(helpers$filterEntries$trn_freight_road, "Domestic Ship",
                  "Freight Rail", "Domestic Aviation", "Passenger Rail", "HSR", "Bus"),
      bunker = c("International Ship", "International Aviation")
    )
    splittedFEdata <- FEdata[univocalName %in% modes[[REMINDsegment]]]
    mixedCarrierSplits <- copy(mixedCarrierSplits[[REMINDsegment]])
    mixedCarrierSplits[, variable := NULL]
    setnames(mixedCarrierSplits, "value", "share")

    splittedFEdata <- merge(splittedFEdata, mixedCarrierSplits, by = c("region", "period", "technology"), allow.cartesian = TRUE, all.x = TRUE)
    splittedFEdata[, value := value * share]
    splittedFEdata[, c("share") := NULL]

    if (anyNA(splittedFEdata)) stop("Something went wrong with the mixed carrier splitting. Please check applySplit()")

    return(splittedFEdata)
  }

  # for reading a variable from a gdxPath some transformation steps are necessary to get a data.table
  # therefore readgdx from gdxdt is used
  demFeSector <- magpie2dt(readGDX(gdxPath, "vm_demFeSector", field = "l", restore_zeros = FALSE))
  setnames(demFeSector, c("all_regi", "all_enty", "all_enty1", "emi_sectors", "all_emiMkt", "ttot"), c("region", "from", "to", "emiSectors", "type", "period"))
  # Select transport sector
  demFeSector <- demFeSector[emiSectors == "trans"]
  # "vm_demFeSector" contains reasonable data from 2005 onward (before REMIND is not running the optimization)
  demFeSector <-  demFeSector[period >= 2005]

  # For now, REMIND cannot really differentiate which segment in transport is getting how much bio/syn fuels.
  # Therefore we keep the share of bio to synfuels constant in each segment and equal to to the share in transport overall.
  # The fossil share is kept for each segment and the remaining FE is splitted accordingly.
  # Both syn und bio share needs to be calculated, as both can be 0 or either one of them (soo 1 - the other does not work)
  liqBioToSyn <- demFeSector[to %in% c("fepet", "fedie") & from %in% c("seliqsyn", "seliqbio")]
  liqBioToSyn <- liqBioToSyn[, sumbio := sum(value[from == "seliqbio"]), by = c("region", "period")]
  liqBioToSyn <- liqBioToSyn[, sumsyn := sum(value[from == "seliqsyn"]), by = c("region", "period")]
  liqBioToSyn <- liqBioToSyn[, sum := sum(value), by = c("region", "period")]
  liqBioToSyn <- liqBioToSyn[, bioToSynShareOverall := ifelse(!sum(value) == 0, sumbio / sum, 0), by = c("region", "period")]
  liqBioToSyn <- liqBioToSyn[, synToBioShareOverall := ifelse(!sum(value) == 0, sumsyn / sum, 0), by = c("region", "period")]
  liqBioToSyn <- unique(liqBioToSyn[, .(region, period, bioToSynShareOverall, synToBioShareOverall)])
  gasesBioToSyn <- demFeSector[to == "fegat" & from %in% c("segasyn", "segabio")]
  gasesBioToSyn <- gasesBioToSyn[, sumbio := sum(value[from == "segabio"]), by = c("region", "period")]
  gasesBioToSyn <- gasesBioToSyn[, sumsyn := sum(value[from == "segasyn"]), by = c("region", "period")]
  gasesBioToSyn <- gasesBioToSyn[, sum := sum(value), by = c("region", "period")]
  gasesBioToSyn <- gasesBioToSyn[, bioToSynShareOverall := ifelse(!sum(value) == 0, sumbio / sum, 0), by = c("region", "period")]
  gasesBioToSyn <- gasesBioToSyn[, synToBioShareOverall := ifelse(!sum(value) == 0, sumsyn / sum, 0), by = c("region", "period")]
  gasesBioToSyn <- unique(gasesBioToSyn[, .(region, period, bioToSynShareOverall, synToBioShareOverall)])

  splitTransportOverall <- list(liqBioToSyn = liqBioToSyn, gasesBioToSyn = gasesBioToSyn)

  REMINDsegments <- c("LDVs", "nonLDVs", "bunker")
  splitShares <- sapply(REMINDsegments, calcSplit, demFeSector, splitTransportOverall, timeResReporting, simplify = FALSE, USE.NAMES = TRUE)

  # Make sure that only Liquids are supplied
  dtFE <- copy(dtFE)
  dtFE <- dtFE[technology %in% c("Liquids", "Gases")]
  splittedCarriers <- rbindlist(lapply(REMINDsegments, applySplit, dtFE, splitShares, helpers))
  splitShares[["LDVs"]][, variable := paste0(variable, "Transport|LDV")]
  splitShares[["nonLDVs"]][, variable := paste0(variable, "Transport|Other")]
  splitShares[["bunker"]][, variable := paste0(variable, "Transport|Bunkers")]
  splitShares <- rbindlist(splitShares)[, unit := "-"][, c("fuel", "technology") := NULL]
  carrierSplit <- list(splitShares = splitShares,
                       splittedCarriers = splittedCarriers)
  return(carrierSplit)
}
