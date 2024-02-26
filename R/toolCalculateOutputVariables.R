

toolCalculateOutputVariables <- function(ESdemand, enIntensity, loadFactor, fleetSizeAndComp, CAPEXtrackedFleet, subsides, combinedCAPEXandOPEX, gdx, timeResReporting, hybridElecShare, extendedReporting = FALSE){

  toolCalculateFleetVariables <- function(salesData, vehiclesConstrYears, helpers, timeResReporting) {

    vehiclesConstrYears <- copy(vehiclesConstrYears)
    vehiclesConstrYears <- vehiclesConstrYears[, sum := sum(value), by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]
    vehiclesConstrYears <- vehiclesConstrYears[, share := value / sum, by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "constrYear")]
    # No weights needed to aggregate over construction years when there are no vehicles (e.g. the energy intensity for BEVs on fleet level should be kept, even if there is no demand)
    vehiclesConstrYears[is.nan(share) & period == constrYear, share := 1]
    vehiclesConstrYears[is.nan(share) & !period == constrYear, share := 0]
    vehiclesConstrYears[, c("value", "sum", "unit", "variable") := NULL]
    salesData <- copy(salesData)
    salesData <- merge(salesData, helpers$decisionTree, by = c(intersect(names(salesData), names(helpers$decisionTree))))
    salesDataTrackedVeh <- salesData[grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)]
    #Interpolate the missing timesteps between 1990 and 2005
    salesDataTrackedVeh <- approx_dt(salesDataTrackedVeh, unique(vehiclesConstrYears$constrYear), "period", "value", extrapolate = TRUE)
    setnames(salesDataTrackedVeh, "period", "constrYear")
    salesDataTrackedVeh <- merge(salesDataTrackedVeh, vehiclesConstrYears, by = intersect(names(salesDataTrackedVeh), names(vehiclesConstrYears)), all.x = TRUE, allow.cartesian = TRUE)
    #Timesteps after 2100 need to be interpolated
    fleetData <- salesDataTrackedVeh[, .(value = sum(value * share)), by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "unit", "variable")]
    fleetData <- approx_dt(fleetData, unique(salesData$period), "period", "value", extrapolate = TRUE)
    fleetData <- rbind(fleetData, salesData[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)])
    fleetData[, variable := gsub("sales", "", variable)]
    fleetData <- toolOrderandCheck(fleetData, helpers$decisionTree)

    return(fleetData)
  }

  toolCalculateFE <- function(energyIntensity, ESdemand, loadFactor, helpers){

    energyIntensity <- copy(energyIntensity)[, c("variable", "unit") := NULL]
    setnames(energyIntensity, "value", "energyIntensity")
    ESdemand <- copy(ESdemand)[, c("variable", "unit") := NULL]
    setnames(ESdemand, "value", "ESdemand")
    loadFactor <- copy(loadFactor)[, c("variable", "unit") := NULL]
    setnames(loadFactor, "value", "loadFactor")
    fleetFEDemand <- merge(ESdemand, energyIntensity, by = intersect(names(ESdemand), names(energyIntensity)))
    fleetFEDemand <- merge(fleetFEDemand, loadFactor, by = intersect(names(fleetFEDemand), names(loadFactor)))
    fleetFEDemand[, value := (energyIntensity / loadFactor) * ESdemand][, unit := "EJ/yr"][, variable := "FE"]
    # There is no final energy type "hybrid electric". So hybrids need to be split in liquids and electricity
    fleetFEDemand <- rbind(fleetFEDemand, copy(fleetFEDemand[technology == "Hybrid electric"])[, value := value * hybridElecShare][, technology := "BEV"])
    fleetFEDemand[technology == "Hybrid electric", value := value * (1 - hybridElecShare)]
    fleetFEDemand[technology == "Hybrid electric", technology := "Liquids"]
    fleetFEDemand[univocalName %in% c("Cycle", "Walk"), value := 0]
    fleetFEDemand[, c("loadFactor", "energyIntensity", "ESdemand") := NULL]
    fleetFEDemand <-  toolOrderandCheck(fleetFEDemand, helpers$decisionTree)

    return(fleetFEDemand)
  }

  toolCalculateUE <- function(FEdemand){

    # Note that this is a really rough assumptions (as the aircarft burning hydrogen is getting the same efficiency than
    # a fuel cell electric truck)
    MappUE <- data.table(
      technology = c("Electric", "BEV", "Hydrogen", "FCEV", "Liquids", "Hybrid electric"),
      UEefficiency = c(0.64, 0.64, 0.25, 0.25, 0.23))

    UEdemand <- copy(FEdemand)
    UEdemand <- merge(UEdemand, UEefficiency, by = "technology")
    UEdemand[univocalName %in% c("Cycle", "Walk"), UEefficiency := 1]
    UEdemand[, value := value * UEefficency]
    UEdemand[, variable := "Useful energy"]

    UEdemand <-  toolOrderandCheck(fleetFEDemand, helpers$decisionTree)

    return(fleetFEDemand)
  }

  toolSplitMixedCarrier <- function(dtFE, gdx) {

    calcSplit <- function(REMINDsegment, dataREMIND, splitOverall) {
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
      dataLiquids[, Fossil := value[from == "seliqfos"] / sum(value), by = c("region", "period")]
      dataLiquids[, Biomass := sum(value[from %in% c("seliqbio", "seliqsyn")]) /
                    sum(value) * bioToSynShareOverall, by = c("region", "period")]
      dataLiquids[, Hydrogen := sum(value[from %in% c("seliqbio", "seliqsyn")]) / sum(value)
                  * synToBioShareOverall, by = c("region", "period")]
      cols <- c("region", "period", "Fossil", "Hydrogen", "Biomass")
      sharesLiquids <- unique(dataLiquids[, ..cols])
      sharesLiquids <- melt(sharesLiquids, id.vars = c("region", "period"))[, variable := paste0("Share|Liquids|", variable)][, type := "Liquids"]

      # calc shares gases i.a. (syngases -> Gases|Hydrogen)
      if (nrow(dataGases) > 0) {
        dataGases[, Fossil := ifelse(!sum(value) == 0, value[from == "segafos"] / sum(value), 0), by = c("region", "period")]
        dataGases[, Biomass := ifelse(!sum(value) == 0, sum(value[from %in% c("segabio", "segasyn")]) / sum(value)
                  *  bioToSynShareOverall, 0), by = c("region", "period")]
        dataGases[, Hydrogen := ifelse(!sum(value) == 0, sum(value[from %in% c("segabio", "segasyn")]) / sum(value)
                  * synToBioShareOverall, 0), by = c("region", "period")]
        sharesGases <- unique(dataGases[, ..cols])
        sharesGases <- melt(sharesGases, id.vars = c("region", "period"))[, variable := paste0("Share|Gases|", variable)][, type := "Gases"]
        shares <- rbind(sharesLiquids, sharesGases)

      } else {shares <- sharesLiquids}

      # apply low time resolution
      shares <- approx_dt(shares, timeResReporting, "period", "value", extrapolate = TRUE)
      shares[, sum := sum(value), by = c("region", "period", "type")]

      if (anyNA(shares) | nrow(shares[(sum < 0.9999 | sum > 1.0001) & sum != 0])) stop("Something went wrong with the mixed carrier splitting. Please check calcSplit()")
      shares[, c("sum", "type") := NULL]
      return(shares)
    }

    applySplit <- function(REMINDsegment, FEdata, mixedCarrierSplits) {

      # map EDGE-T categories on segements
      modes <- list(
        LDVs = c(helpers$filterEntries$trn_pass_road_LDV_4W,
                                        helpers$filterEntries$trn_pass_road_LDV_2W),
        nonLDVs = c(helpers$filterEntries$trn_freight_road, "Domestic Ship",
                                           "Freight Rail", "Domestic Aviation", "Passenger Rail", "HSR", "Bus"),
        bunker = c("International Ship", "International Aviation")
      )
      splittedFEdata <- FEdata[univocalName %in% modes[[REMINDsegment]]]
      splittedFEdata[, c("variable") := NULL]
      mixedCarrierSplits <- copy(mixedCarrierSplits[[REMINDsegment]])[, technology := ifelse(grepl(".*Liquids.*", variable), "Liquids", "Gases")]
      setnames(mixedCarrierSplits, "value", "share")
      splittedFEdata <- merge(splittedFEdata, mixedCarrierSplits, by = c("region", "period", "technology"), allow.cartesian = TRUE, all.x = TRUE)
      splittedFEdata[, value := value * share][, technology := gsub("Share\\|", "", variable)][, variable := "FE"]
      splittedFEdata[, c("share") := NULL]

      if (anyNA(splittedFEdata)) stop("Something went wrong with the mixed carrier splitting. Please check applySplit()")

      return(splittedFEdata)
    }

    # for reading a variable from a gdx some transformation steps are necessary to get a data.table
    # therefore readGDX from gdxdt is used
    demFeSector <- magpie2dt(readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = FALSE))
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
    splitShares <- sapply(REMINDsegments, calcSplit, demFeSector, splitTransportOverall, simplify = FALSE, USE.NAMES = TRUE)

    # Make sure that only Liquids are supplied
    dtFE <- copy(dtFE)
    dtFE <- dtFE[technology %in% c("Liquids", "Gases")]
    splittedCarriers <- rbindlist(lapply(REMINDsegments, applySplit, dtFE, splitShares))
    splitShares[["LDVs"]][, variable := paste0(variable, "Transport|LDV")]
    splitShares[["nonLDVs"]][, variable := paste0(variable, "Transport|Other")]
    splitShares[["bunker"]][, variable := paste0(variable, "Transport|Bunkers")]
    splitShares <- rbindlist(splitShares)[, unit := "-"]
    carrierSplit <- list(splitShares = splitShares,
                         splittedCarriers = splittedCarriers)
    return(carrierSplit)
  }

  toolCalculateEmissions <- function(dtFE, gdx, prefix) {

    # Get emission factors from REMIND gdx
    GtCtoGtCO2 <- rgdx.scalar(gdx, "sm_c_2_co2", ts = FALSE)
    EJ2TWa <- rgdx.scalar(gdx, "sm_EJ_2_TWa", ts = FALSE)
    gdxColNames <- c("period", "region", "from", "to", "conversionTechnology", "emissionType", "value")
    emissionFactors <- as.data.table(rgdx.param(gdx, "pm_emifac", names = gdxColNames))
    # liquid fuels
    emissionFactors[from == "seliqfos" & to ==  "fedie" & emissionType == "co2",  emissionFactor := value * GtCtoGtCO2 *1e3 * EJ2TWa]
    emissionFactors[from == "seliqfos" & to ==  "fepet" & emissionType == "co2",  emissionFactor := value * GtCtoGtCO2 *1e3 * EJ2TWa]
    # gaseous fuels
    emissionFactors[from == "segafos" & to ==  "fegas" & emissionType == "co2",  emissionFactor := value * GtCtoGtCO2 *1e3 * EJ2TWa]
    # unit MtCO2/EJ
    emissionFactors <- emissionFactors[!is.na(emissionFactor)]
    emissionFactors <- emissionFactors[, c("region", "period", "to", "emissionFactor")]

    ## attribute explicitly fuel used to the FE values
    dtFE <- copy(dtFE)
    dtFE[univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                             helpers$filterEntries$trn_pass_road_LDV_2W) & technology %in% c("Liquids", "Hybrid electric"), to := "fepet"]
    dtFE[!(univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                             helpers$filterEntries$trn_pass_road_LDV_2W)) & technology == "Liquids", to := "fedie"]
    dtFE[technology == "Gases", to := "fegas"]

    emissionFactors[, period := as.double(as.character(period))]
    ## merge with emission factors
    emi <- merge(dtFE, emissionFactors, by = c("to", "region", "period"), all.x = TRUE)
    emi[is.na(emissionFactor), emissionFactor := 0]
    ## calculate emissions and attribute variable and unit names
    emi[, value := value * emissionFactor][, variable := paste0("Emi|CO2|", prefix)][, unit := "Mt CO2/yr"]
    emi[, c("to", "emissionFactor") := NULL]
    return(emi)
  }

  toolAggregateCosts <- function (combinedCAPEXandOPEX){

    combinedCAPEXandOPEX <- copy(combinedCAPEXandOPEX)
    combinedCAPEXandOPEX[grepl("Capital.*", variable), type := "Capital costs sales"]
    combinedCAPEXandOPEX[grepl("Operating.*", variable), type := "Operating costs (total non-fuel)"]
    combinedCAPEXandOPEX[grepl("Fuel.*", variable), type := "Fuel costs"]
    if (anyNA(combinedCAPEXandOPEX) == TRUE) stop("Some cost mixedCarrierTypes did not receive an aggregated level and would get lost in the aggregation. Please check toolAggregateCosts()")
    combinedCAPEXandOPEX[, variable := NULL]
    setnames(combinedCAPEXandOPEX, "type", "variable")
    cols <- names(combinedCAPEXandOPEX)
    combinedCAPEXandOPEX <- combinedCAPEXandOPEX[, .(value = sum(value)), by = eval(cols[cols != "value"])]

    return(combinedCAPEXandOPEX)
  }

  # Move from sales to fleet reporting for *affected* variables (in the variables named fleet other modes are still included)
  # aggregate costs
  aggregatedCosts <- toolAggregateCosts(combinedCAPEXandOPEX)
  aggregatedCosts <- merge(aggregatedCosts, helpers$decisionTree, by = intersect(names(aggregatedCosts), names(helpers$decisionTree)))
  # Energy service demand on fleet level deviates from the sales level regarding the share that each technology gets
  fleetESdemand <- rbind(ESdemand[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)], fleetSizeAndComp$fleetESdemand)
  # Energy intensity and Capital costs are tied to the construction year and have to be recalculated to reflect the value for each year referring to the vehicle stock
  fleetVariables <- list(fleetEnergyIntensity = enIntensity,
                         fleetCapCosts = copy(aggregatedCosts[variable == "Capital costs sales"]))
  fleetData <- lapply(fleetVariables, toolCalculateFleetVariables, fleetSizeAndComp$fleetVehNumbersConstrYears, helpers, timeResReporting)

  # Switch from mixed time resolution to the reporting time resolution for all vars
  ESdemand <- ESdemand[period %in% timeResReporting]
  fleetESdemand <- fleetESdemand[period %in% timeResReporting]
  fleetData <- lapply(fleetData, FUN = function(x) x <- x[period %in% timeResReporting])
  fleetSizeAndComp <- lapply(fleetSizeAndComp, FUN = function(x) x <- x[period %in% timeResReporting])
  enIntensity <- enIntensity[period %in% timeResReporting]
  loadFactor <- loadFactor[period %in% timeResReporting]
  CAPEXtrackedFleet <- CAPEXtrackedFleet[period %in% timeResReporting]
  aggregatedCosts <- aggregatedCosts[period %in% timeResReporting]
  subsides <- subsides[period %in% timeResReporting]
  combinedCAPEXandOPEX <- combinedCAPEXandOPEX[period %in% timeResReporting]

  # Calculate final energy
  fleetFEdemand <- toolCalculateFE(fleetData$fleetEnergyIntensity, fleetESdemand, loadFactor, helpers)

  # Calculate liquids and gases split
  mixedCarrierSplit <- toolSplitMixedCarrier(fleetFEdemand[technology %in% c("Liquids", "Gases")], gdx)

  # Calculate emissions
  fleetEmissionsTailpipe <- toolCalculateEmissions(fleetFEdemand, gdx, "Tailpipe")
  FEwithoutBioSyn <- rbind(mixedCarrierSplit$splittedCarriers[grepl(".*Fossil", technology)][, technology := gsub("|.*", "", technology)], fleetFEdemand[!technology %in% c("Liquids", "Gases")])
  fleetEmissionsDemand <- toolCalculateEmissions(fleetFEdemand, gdx, "Demand")
  fleetEmissions <- rbind(fleetEmissionsTailpipe, fleetEmissionsDemand)

  # Calculate vehicle sales
  sales <- copy(fleetSizeAndComp$fleetVehNumbersConstrYears[period == constrYear])
  sales[, variable := "Sales"][, constrYear := NULL]
  sales <- approx_dt(sales, timeResReporting, "period", "value", extrapolate = TRUE)

  # Calculate yearly costs
  fleetES <- copy(fleetESdemand)
  fleetES[, c("variable", "unit") := NULL]
  setnames(fleetES, "value", "ESdemand")
  fleetCost <- rbind(fleetData$fleetCapCosts, aggregatedCosts[!variable == "Capital costs sales"])
  fleetYrlCosts <- merge(fleetCost, fleetES, by = intersect(names(fleetCost), names(fleetES)))
  fleetYrlCosts[, value := value * ESdemand][, unit := "billion US$2005/yr"][, ESdemand := NULL]

  # Calculate upfront capital cost for vehicle sales
  upfrontCAPEX <- rbind(CAPEXtrackedFleet, subsides)
  cols <- names(upfrontCAPEX)
  cols <- cols[!cols %in% c("value", "variable")]
  upfrontCAPEX <-  upfrontCAPEX[, .(value = sum(value)), by = cols][, variable := "Upfront capital costs sales"]
  upfrontCAPEX <- merge(upfrontCAPEX, helpers$decisionTree, by = intersect(names(upfrontCAPEX), names(helpers$decisionTree)))

  # Differentiate between intensive and extensive variables (those that can be aggregated without a weight)
  outputVarsExt <- list(
    fleetESdemand = fleetESdemand,
    fleetFEdemand = fleetFEdemand,
    FEsplittedCarriers = mixedCarrierSplit$splittedCarriers,
    fleetEmissions = fleetEmissions,
    sales = sales,
    stock = fleetSizeAndComp$fleetVehNumbers,
    fleetYrlCosts = fleetYrlCosts
  )
  outputVarsInt <- list(
    fleetEnergyIntensity = fleetData$fleetEnergyIntensity,
    upfrontCAPEX = upfrontCAPEX
  )

  # Extended reporting
  if (extendedReporting == TRUE) {

     # Calculate useful energy
    fleetUEdemand <- toolCalculateUE(fleetFEdemand)

    # Calculate vintages (stock without sales)
    vintages <- copy(fleetSizeAndComp$fleetVehNumbersConstrYears[!period == constrYear])
    vintages[, variable := "Vintages"][, constrYear := NULL]
    cols <- names(vintages)
    vintages <- vintages[, .(value = sum(value)), by = eval(cols[!cols %in% c("value", "constrYear")])]
    vintages <- approx_dt(vintages, timeResReporting, "period", "value", extrapolate = TRUE)


    extendedExt <- list(fleetUEdemand = fleetUEdemand,
                        vintages = vintages,
                        )

    extendedInt <- list(loadFactor = loadFactor,
                        fleetCost = fleetCost,
                        FEsplitShares = mixedCarrierSplit$splitShares
                        )

    outputVarsExt <- append(outputVars, extendedExt)
    outputVarsInt <- append(outputVars, extendedInt)
  }

  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)

  return(outputVars)
}
