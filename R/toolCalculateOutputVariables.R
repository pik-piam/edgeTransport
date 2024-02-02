

toolCalculateOutputVariables <- function(ESdemand, enIntensity, loadFactor, fleetSizeAndComp, CAPEXtrackedFleet, subsides, combinedCAPEXandOPEX, gdx){

  toolCalculateFleetVariables <- function(salesData, vehiclesConstrYears, helpers) {
    browser()
    salesData <- merge(salesData, helpers$decisionTree, by = c(intersect(names(salesData), names(helpers$decisionTree))))
    vehiclesConstrYears <- copy(vehiclesConstrYears)
    vehiclesConstrYears <- vehiclesConstrYears[, sum := sum(value), by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName")]
    vehiclesConstrYears <- vehiclesConstrYears[, share := value / sum, by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "constrYear")]
    # No weights needed to aggregate over construction years when there are no vehicles (e.g. the energy intensity for BEVs on fleet level should be kept, even if there is no demand)
    vehiclesConstrYears[is.nan(share), share := 1]
    vehiclesConstrYears[, c("value", "sum", "unit", "variable") := NULL]
    setnames(salesData, "period", "constrYear")
    salesDataTrackedVeh <- merge(salesData, vehiclesConstrYears, by = intersect(names(salesData), names(vehiclesConstrYears)))
    fleetData <- salesDataTrackedVeh[, .(value = sum(value * share)), by = c("region", "period", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "unit", "variable")]
    setnames(salesData, "constrYear", "period")
    fleetData <- rbind(fleetData, salesData[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)])
    fleetData[, variable := gsub("sales", "fleet", variable)]
    fleetData <- toolOrderandCheck(fleetData, helpers$decisionTree)


    return(fleetData)
  }

  toolCalculateFE <- function(energyIntensity, ESdemand, loadFactor, helpers){
    browser()
    energyIntensity <- copy(energyIntensity)[, c("variable", "unit") := NULL]
    setnames(energyIntensity, "value", "energyIntensity")
    ESdemand <- copy(ESdemand)[, c("variable", "unit") := NULL]
    setnames(ESdemand, "value", "ESdemand")
    loadFactor <- copy(loadFactor)[, c("variable", "unit") := NULL]
    setnames(loadFactor, "value", "loadFactor")
    fleetFEDemand <- merge(ESdemand, energyIntensity, by = intersect(names(ESdemand), names(energyIntensity)))
    fleetFEDemand <- merge(fleetFEDemand, loadFactor, by = intersect(names(fleetFEDemand), names(loadFactor)))
    fleetFEDemand[, value := (energyIntensity / loadFactor) * ESdemand][, unit := "EJ/yr"][, variable := "FE"]
    fleetFEDemand[, c("loadFactor", "energyIntensity", "ESdemand") := NULL]
    fleetFEDemand <-  toolOrderandCheck(fleetFEDemand, helpers$decisionTree)
    return(fleetFEDemand)
  }

  toolSplitLiquids <- function(dtFE, gdx) {
  browser()
    calcShares <- function(data, types){
      data <- data[all_enty %in% types]
      for (i in types){
        data[, paste0(i, "Share") := value[all_enty == i] / sum(value), by = c("region", "period")]
        data[is.na(get(paste0(i, "Share"))), paste0(i, "Share") := 0]
      }
      cols <- c("region", "period", paste0(types, "Share"))
      data <- unique(data[, ..cols])
      data <- melt(data, id.vars = c("region", "period"), variable.name = "split", value.name = "share")
      data[, sum := sum(share), c("region", "period")]
      if (nrow(data[sum < 0.9999 | sum > 1.0001])) stop("Something went wrong with the liquid splitting. Please check toolSplitLiquids()")
      data[, sum := NULL]
    }

    applySplit <- function(x, mode, split) {
      browser()
      mode <- mode[[x]]
      split <- split[[x]]
      dataTable <- merge(dataTable, split, by = intersect(names(dataTable), names(split)))
      dataTable[grepl(".*fos.*", split), split := "Fossil"]
      dataTable[grepl(".*bio.*", split), split := "Biomass"]
      dataTable[grepl(".*syn.*", split), split := "Hydrogen"]
      dataTable[, value := value * share][, technology := paste0(technology, "|", split)]
    }

    demFeSector <- magpie2dt(readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = F))
    # Select transport sector
    demFeSector <- demFeSector[emi_sectors == "trans"]
    setnames(demFeSector, c("all_regi", "ttot"), c("region", "period"))
    # Fuel types
    LiquidsTypes = c("seliqfos", "seliqbio", "seliqsyn")
    # Splits for different transport modes
    splits <- list(
      LDVs = demFeSector[all_enty1 == "fepet" & all_emiMkt == "ES"],
      nonLDVs = demFeSector[all_enty1 == "fedie" & all_emiMkt == "ES"],
      bunker = demFeSector[all_enty1 == "fedie" & all_emiMkt == "other"]
    )
    Liquidsplits <- lapply(modes, calcShares, LiquidsTypes)
    # Make sure that only Liquids are supplied
    dtFE <- dtFE[technology == "Liquids"]
    modes <- list(
      LDVs = dtFE[univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                                      helpers$filterEntries$trn_pass_road_LDV_2W)],
      nonLDVs = dtFE[univocalName %in% c(helpers$filterEntries$trn_freight_road, "Domestic Ship",
                                         "Freight Rail", "Domestic Aviation", "Passenger Rail", "HSR", "Bus")],
      bunker = dtFE[univocalName %in% c("International Ship", "International Aviation")]
    )
    splittedLiquids <- rbindlist(sapply(c(LDVs, nonLDVs, bunker), applySplit, modes, splits))
    return(splittedLiquids)
  }

  toolCalculateEmissions <- function(dtFE, gdx) {
    browser()
    # Tailpipe emissions
    ## load emission factors for fossil fuels
    emissionFactors <- readgdx(gdx, "pm_emifac")[all_enty %in% c("fepet", "fedie", "fegas")]  ## MtCO2/EJ
    Pm_emifac

    emissionFactors[all_enty == "fegat", all_enty := "fegat"]
    setnames(emissionFactors, old = c("value", "all_regi"), new = c("emissionFactor", "region"))
    ## attribute explicitly fuel used to the FE values
    dtFE[univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                             helpers$filterEntries$trn_pass_road_LDV_2W) & technology == "Liquids", all_enty := "fepet"]
    dtFE[!(univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                             helpers$filterEntries$trn_pass_road_LDV_2W)) & technology == "Liquids", all_enty := "fedie"]
    dtFE[technology == "Gases", all_enty := "fegat"]
    dtFE[technology == "Electricity", all_enty := "feelt"]
    dtFE[technology == "Hydrogen", all_enty := "feh2t"]
    ## merge with emission factors
    emiTailpipe <- merge(dtFE, emissionFactors, by = c("all_enty", "region"))
    ## calculate emissions and attribute variable and unit names
    emiTailpipe[, value := value * emissionFactor][, c("variable", "unit") := c("Emi|CO2|Tailpipe", "Mt CO2/yr")]

    # Demand emissions (bio and synfuels counted as zero)
    emiDemand <- copy(emiTailpipe)
    prodFe <- readgdx(gdx, "vm_prodFE")[, ttot := as.numeric(ttot)]
    setnames(prodFe,
             c("period", "region", "se", "all_enty", "te", "feDemand"))
    # Calculate secondary energy share
    prodFe[, seShare := feDemand / sum(feDemand), by = c("period", "region", "all_enty")]
    prodFe <- prodFe[all_enty %in% c("fedie", "fepet", "fegat") & se %in% c("segafos", "seliqfos")][, c("se", "te", "feDemand") := NULL]

    emiDemand <- merge(emiDemand, prodFe, by = c("period", "region", "all_enty"))
    ## in case no fossil fuels are used (e.g. 100% biodiesel), the value in seShare results NA. set the NA value to 0
    emiDemand[is.na(seShare), seShare := 0]
    emiDemand <- emiDemand[, value := value * seShare][, c("seShare", "type", "emissionFactor", "all_enty") := NULL]
    emiDemand[, variable := "Emi|CO2|Demand"]

    emi <- rbind(emiDemand, emiTailpipe)

    return(emi)

  }

  toolAggregateCosts <- function (combinedCAPEXandOPEX){

    combinedCAPEXandOPEX <- copy(inputData$combinedCAPEXandOPEX)
    combinedCAPEXandOPEX[grepl("Capital.*", variable), type := "Capital costs sales"]
    combinedCAPEXandOPEX[grepl("Operating.*", variable), type := "Operating costs (total non-fuel)"]
    combinedCAPEXandOPEX[grepl("Fuel.*", variable), type := "Fuel costs"]
    activeModes <- combinedCAPEXandOPEX[univocalName %in% c("Walk", "Cycle")][, type := "Capital costs sales"]
    #Insert placeholders for active modes
    combinedCAPEXandOPEX <- rbind(combinedCAPEXandOPEX[!univocalName %in% c("Walk", "Cycle")],
                                  activeModes,
                                  copy(activeModes)[, type := "Operational costs (non fuel)"],
                                  copy(activeModes)[, type := "Fuel costs"])[, variable := NULL]
    if (anyNA(combinedCAPEXandOPEX$type) == TRUE) stop("Some cost types did not receive an aggregaten level and would get lost in the aggregation. Please check toolAggregateCosts()")
    setnames(combinedCAPEXandOPEX, "type", "variable")
    cols <- names(combinedCAPEXandOPEX)
    combinedCAPEXandOPEX <- combinedCAPEXandOPEX[, .(value = sum(value)), by = eval(cols[cols != "value"])]

    return(combinedCAPEXandOPEX)
  }

  ESdemand <- copy(fuelVehicleESdemand)
  enIntensity <- copy(inputData$enIntensity)
  loadFactor <- copy(inputData$loadFactor)
  fleetSizeAndComp <- copy(fleetSizeAndComposition)
  CAPEXtrackedFleet <- copy(inputDataRaw$CAPEXtrackedFleet)
  subsides <- copy(inputDataRaw$subsidies)
  combinedCAPEXandOPEX <- copy(inputData$combinedCAPEXandOPEX)

  # Aggregate costs
  aggregatedCosts <- toolAggregateCosts(combinedCAPEXandOPEX)

  fleetESdemand <- rbind(ESdemand[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)], fleetSizeAndComp$fleetESdemand)
  fleetVariables <- list(fleetEnergyIntensity = enIntensity,
                         fleetCAPEX = copy(aggregatedCosts[grepl("Capital.*", variable)]))
  fleetData <- lapply(fleetVariables, toolCalculateFleetVariables, fleetSizeAndComp$fleetVehNumbersConstrYears, helpers)

  # Calculate fleet FE demand
  fleetFEdemand <- toolCalculateFE(fleetData$fleetEnergyIntensity, fleetESdemand, loadFactor, helpers)

  # Calculate emissions
  fleetEmissions <- toolCalculateEmissions(fleetFEdemand, gdx)

  # Calculate vehicle sales and vintages
  sales <- fleetVehNumbersConstrYears[period == constrYear]
  sales[, variable := "Sales"][, constrYear := NULL]
  vintages <- fleetVehNumbersConstrYears[!period == constrYear]
  cols <- names(vintages)
  vintages <- vintages[, .(value = sum(value)), by = eval(cols[cols %in% c("value", "constrYear")])][, variable := "Vintages"]

  upfrontCAPEX <- rbind(CAPEXtrackedFleet, subsides)
  cols <- names(upfrontCAPEX)
  cols <- cols[!cols %in% c("value", "variable")]
  upfrontCAPEX <-  upfrontCAPEX[, .(value = sum(value)), ..cols][, variable := "Upfront capital costs sales"]

  outputVars <- list(
    aggregatedCosts = aggregatedCosts,
    fleetEnergyIntensity = fleetData$fleetEnergyIntensity,
    fleetCAPEX = fleetData$fleetCAPEX,
    fleetFEDemand = fleetFEDemand,
    fleetEmissions = fleetEmissions,
    sales = sales,
    vintages = vintages,
    upfrontCAPEX = upfrontCAPEX
  )

  return(outputVars)
}
