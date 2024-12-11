#'Calculate composition of the fleet
#'
#'This function calculates the fleet composition based on the energy service demand, the composition of new sales
#'each year and the service life of vehicles. With the help of the annual mileage, the absolute numbers of vehicles is calculated.
#'If the remaining stock is sufficient to meet the energy service demand, 10% of the fleet are retired early to ensure a minimum sales volume each year.
#'
#' @param ESdemandFVsalesLevel Energy service demand on fuel vehicle level for new sales
#' @param vehDepreciationFactors Factors for the depreciation of vehicles in each year of the maximum service Life
#' @param vehSalesAndModeShares Vehicle sales and mode shares for the decision tree
#' @param annualMileage Annual kilometers driven by a vehicle
#' @param loadFactor Persons or tons loaded on each vehicle
#' @param helpers List of helpers
#'
#' @returns list of fleet composition data
#' @author Johanna Hoppe
#' @importFrom rmndt approx_dt
#' @import data.table
#' @export

toolCalculateFleetComposition <- function(ESdemandFVsalesLevel,
                                          vehDepreciationFactors,
                                          vehSalesAndModeShares,
                                          annualMileage,
                                          loadFactor,
                                          helpers) {

  # bind variables locally to prevent NSE notes in R CMD CHECK
 # period <- value <- share <- level <- variable <- unit <- univocalName <- subsectorL3 <- testSum <- NULL
  #allYears <- depreciationFactor <- indexUsagePeriod <- demandNewAdditions <- totalESdemand <- NULL
 # vintagesDemand <- earlyRetirement <- earlyRetirementRate <- contribConstrYear <- constrYear <- NULL

  highTimeRes <- unique(helpers$dtTimeRes$period)
  #start fleet calc
  startYear <- 2005

  # calculate total energy service demand for modes with tracked fleets ------------------------------------------------------------
  fleetESdemand <- copy(ESdemandFVsalesLevel)
  fleetESdemand <- fleetESdemand[grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)]
  fleetESdemand <- fleetESdemand[, .(totalESdemand = sum(value)), by = c("region", "period", "subsectorL3")]

  # calculate distribution of total demand on construction years ---------------------------------------------------------------------
  # change to yearly resolution
  timesteps <- seq(1990, 2100, by = 1)
  fleetESdemand <- approx_dt(fleetESdemand, timesteps, "period", "totalESdemand",
                             c("region", "subsectorL3"),
                             extrapolate = TRUE)
  vehDepreciationFactors <- copy(vehDepreciationFactors)
  vehDepreciationFactors <- merge(vehDepreciationFactors, unique(helpers$decisionTree[, c("univocalName", "subsectorL3")]), by = "univocalName")
  vehDepreciationFactors <- unique(vehDepreciationFactors[, c("indexUsagePeriod", "depreciationFactor", "subsectorL3")])
  # to update all sectors in the same loop the years need to be unified and get the depreciation factor of zero (no contribution)
  unifyDt <- unique(vehDepreciationFactors[, c("subsectorL3")])[, allYears := "all"]
  allTimeSteps <- unique(vehDepreciationFactors$indexUsagePeriod)
  unifyDt <- merge(data.table(indexUsagePeriod = allTimeSteps)[, allYears := "all"], unifyDt, by = "allYears", allow.cartesian = TRUE)[, allYears := NULL]
  vehDepreciationFactors <- merge(unifyDt, vehDepreciationFactors, by = c("indexUsagePeriod", "subsectorL3"), all = TRUE)
  vehDepreciationFactors[is.na(depreciationFactor), depreciationFactor := 0]

  #1. The startYear demand that needs to be covered by new additions is calculated------------------------------------------------------------
  # In order to obtain a starting point, it is assumed that the absolute number of new additions (equivalent to the contributed energy service demand,
  # when annual mileage stays constant as well) is constant back to the last year that contributes to the fleet in the base year.
  # hence absDemand = sum(depreciationFactors) * newDemand
  contributionsTostartYear <- copy(vehDepreciationFactors)
  contributionsTostartYear[, period := startYear - indexUsagePeriod][, indexUsagePeriod := NULL]
  fleetESdemand <- merge(fleetESdemand, contributionsTostartYear, by = c("period", "subsectorL3"), all.x = TRUE)
  fleetESdemand[!is.na(depreciationFactor), demandNewAdditions := ifelse(period == startYear,
                                                                         totalESdemand[period == startYear] / sum(depreciationFactor),
                                                                         0),
                by = c("region", "subsectorL3")][, depreciationFactor := NULL]
  fleetESdemand <- fleetESdemand[period >= startYear]

  constructionYears <- seq(startYear - max(vehDepreciationFactors$indexUsagePeriod), startYear, 1)
  contributionYears <- seq(1, max(vehDepreciationFactors$indexUsagePeriod), 1)

  # Initialize columns for fleet tracking (to have them in the data.table before the construction year columns)
  fleetESdemand[, c("vintagesDemand", "earlyRetirement", "earlyRetirementRate") := 0]

  for (i in constructionYears)  {
    for (j in contributionYears) {
      vehDepreciation <- copy(vehDepreciationFactors)[, period := i + indexUsagePeriod][, indexUsagePeriod := NULL]
      fleetESdemand <- merge(fleetESdemand, vehDepreciation, by = c("period", "subsectorL3"), all.x = TRUE)
      fleetESdemand[, eval(paste0("C", i)) := demandNewAdditions[period == startYear] * depreciationFactor, by = c("region", "subsectorL3")]
      fleetESdemand[, depreciationFactor := NULL]
    }
  }

  vintCols <- paste0("C", constructionYears)[1:length(constructionYears) - 1]
  fleetESdemand[period == startYear, vintagesDemand := rowSums(.SD, na.rm = TRUE), .SDcols = vintCols]
  setkey(fleetESdemand, "period")

  for (i in timesteps[timesteps > startYear]) {
    #1. Look only on affected years to lower runtime
    tmp <-  fleetESdemand[period %in% c(i, i + contributionYears)]
    #2. Calculate the demand covered by vintages
    constructionYears <- seq(i - max(vehDepreciationFactors$indexUsagePeriod), i, 1)
    vintCols <- paste0("C", constructionYears)[1:length(constructionYears) - 1]
    tmp[period == i, vintagesDemand := rowSums(.SD, na.rm = TRUE), .SDcols = vintCols]
    #3. Check whether the demand for new additions is larger than 10% of the total energy service demand in that year.
    #   Otherwise we assume an early retirement case so that at least 10% of the fleet are new vehicles
    tmp[period == i, demandNewAdditions := totalESdemand - vintagesDemand]
    tmp[period == i, earlyRetirement := ifelse(demandNewAdditions < 0, TRUE, FALSE)]
    tmp[period == i & earlyRetirement == TRUE, earlyRetirementRate := (vintagesDemand - (totalESdemand * 0.1 - demandNewAdditions)) / vintagesDemand]
    tmp[period == i & earlyRetirement == TRUE, (vintCols) := lapply(.SD, function(x) x * earlyRetirementRate), .SDcols = vintCols]
    tmp[period == i & earlyRetirement == TRUE, vintagesDemand := rowSums(.SD, na.rm = TRUE), .SDcols = vintCols]
    tmp[period == i & earlyRetirement == TRUE, demandNewAdditions := totalESdemand - vintagesDemand]

    #4. Depreciate the demand for new additions for future timesteps
    vehDepreciation <- copy(vehDepreciationFactors)
    vehDepreciation[, period := i + indexUsagePeriod][, indexUsagePeriod := NULL]
    tmp <- merge(tmp, vehDepreciation, by = c("period", "subsectorL3"), all.x = TRUE)
    tmp[, eval(paste0("C", i)) := demandNewAdditions[period == i] * depreciationFactor, by = c("region", "subsectorL3")][, depreciationFactor := NULL]
    #5. Merge back to the demand for all years
    fleetESdemand[, paste0("C", i) := 0]
    fleetESdemand <- rbind(tmp, fleetESdemand[!(period %in% c(i, i + contributionYears))])
  }

  reportEarlyRetirement <- fleetESdemand[, c("period", "subsectorL3", "region", "earlyRetirement", "earlyRetirementRate")]
  fleetESdemand[, c("earlyRetirement", "earlyRetirementRate") := NULL]
  fleetESdemand <- melt(fleetESdemand, id.vars = c("period", "subsectorL3", "region", "totalESdemand", "demandNewAdditions", "vintagesDemand"),
                        value.name = "contribConstrYear", variable.name = "constrYear")
  #filter out all the NAs and zero contributions from the construction years that are no longer needed in long-format
  fleetESdemand <- fleetESdemand[!is.na(contribConstrYear) & !contribConstrYear == 0]

  #Calculate fleet composition on FV level----------------------------------------------------------------------------------------------------------
  VS3shares <- vehSalesAndModeShares[level == "VS3"]
  VS3shares <- VS3shares[, c("period", "region", "subsectorL3", "vehicleType", "share")]
  VS3shares <- approx_dt(VS3shares, timesteps, "period", "share",
              c("region", "subsectorL3", "vehicleType"),
              extrapolate = TRUE)
  VS3shares[, constrYear := paste0("C", period)][, period := NULL]
  fleetESdemand <- merge(fleetESdemand, VS3shares, by = c(intersect(names(fleetESdemand), names(VS3shares))), all.x = TRUE, allow.cartesian = TRUE)
  fleetESdemand[, contribConstrYear := contribConstrYear * share][, share := NULL]
  FVshares <- vehSalesAndModeShares[level == "FV"]
  FVshares <- FVshares[, c("period", "region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "share")]
  FVshares <- approx_dt(FVshares, timesteps, "period", "share",
                         c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology"),
                         extrapolate = TRUE)
  FVshares[, constrYear := paste0("C", period)][, period := NULL]
  fleetESdemand <- merge(fleetESdemand, FVshares, by = c(intersect(names(fleetESdemand), names(FVshares))), all.x = TRUE, allow.cartesian = TRUE)
  fleetESdemand[, contribConstrYear := contribConstrYear * share][, share := NULL]

  fleetESdemand[, test := sum(contribConstrYear) - totalESdemand, by = c("region", "period", "subsectorL3")]
  if (max(fleetESdemand$test) > 10^-5) {stop("The energy service demand of the different construction years does not add up to the total
                                          energy service demand. Please check the fleet calculation in toolCalculateFleetComposition()")}
  # Delete data on subsectorL3 level to prevent confusion
  fleetESdemand[, c("totalESdemand", "demandNewAdditions", "vintagesDemand", "test") := NULL]

  #Calculate vehicles----------------------------------------------------------------------------------------------------------------------------
  annualMileage <- copy(annualMileage)
  annualMileage <- merge(annualMileage, helpers$decisionTree, by = c(intersect(names(annualMileage), names(helpers$decisionTree))))
  annualMileage <- annualMileage[, c("region", "period", "vehicleType", "technology", "value")]
  annualMileage <- approx_dt(annualMileage, timesteps, "period", "value",
                        c("region", "vehicleType", "technology"),
                        extrapolate = TRUE)
  setnames(annualMileage, "value", "annualMileage")
  loadFactor <- copy(loadFactor)
  loadFactor <- merge(loadFactor, helpers$decisionTree, by = c(intersect(names(loadFactor), names(helpers$decisionTree))))
  loadFactor <- loadFactor[, c("region", "period", "vehicleType", "technology", "univocalName", "value")]
  loadFactor <- approx_dt(loadFactor, timesteps, "period", "value",
                             c("region", "vehicleType", "technology", "univocalName"),
                             extrapolate = TRUE)
  setnames(loadFactor, "value", "loadFactor")
  fleetESdemand[, constrYear := as.numeric(gsub("C", "", constrYear))]
  fleetVehNumbers <- merge(fleetESdemand, annualMileage, by = c("period", "region", "vehicleType", "technology"), all.x = TRUE)
  fleetVehNumbers <- merge(fleetVehNumbers, loadFactor, by = c("period", "region", "vehicleType", "technology"), all.x = TRUE)
  #Calculate number of vehicles and convert from billion to million vehicles
  fleetVehNumbers <- fleetVehNumbers[, contribConstrYear := (contribConstrYear / (annualMileage * loadFactor)) * 10^3]
  fleetVehNumbers[, c("annualMileage", "loadFactor") := NULL]

  # Prepare output data---------------------------------------------------------------------------------------------------------------------------
  fleetESdemand <- merge(fleetESdemand, helpers$decisionTree, by = c(intersect(names(fleetESdemand), names(helpers$decisionTree))))
  fleetESdemand[univocalName %in% c(helpers$filterEntries$trn_pass, "International Aviation"), unit := "billion pkm/yr"]
  fleetESdemand[univocalName %in% c(helpers$filterEntries$trn_freight, "International Ship"), unit := "billion tkm/yr"]
  fleetESdemandConstrYears <- copy(fleetESdemand)
  fleetESdemand <- fleetESdemand[, .(value = sum(contribConstrYear)),
                                 by = c("period", "region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                        "technology", "univocalName", "unit")]
  fleetESdemand[, variable := "ES"]
  cols <- names(fleetESdemand)
  cols <- cols[!cols %in% c("period", "value")]
  fleetESdemand <- approx_dt(fleetESdemand, highTimeRes, "period", "value",
                               eval(cols), extrapolate = TRUE)
  fleetESdemandConstrYears[, variable := paste0("Energy service demand construction year ", constrYear)]
  setnames(fleetESdemandConstrYears, "contribConstrYear", "value")

  fleetVehNumbers[, unit := "million veh"]
  fleetVehNumbersConstrYears <- copy(fleetVehNumbers)
  fleetVehNumbers <- fleetVehNumbers[, .(value = sum(contribConstrYear)),
                                     by = c("period", "region", "sector", "subsectorL1", "subsectorL2", "subsectorL3",
                                            "vehicleType", "technology", "univocalName", "unit")]
  fleetVehNumbers[, variable := "Stock"]
  fleetVehNumbers <- approx_dt(fleetVehNumbers, highTimeRes, "period", "value",
                                    eval(cols), extrapolate = TRUE)
  fleetVehNumbersConstrYears[, variable := paste0("Stock construction year ", constrYear)]
  setnames(fleetVehNumbersConstrYears, "contribConstrYear", "value")

  #Aggregate to technology specific total numbers of vehicles
  fleetVehiclesPerTech <- fleetVehNumbers[, .(totVeh = sum(value)), by = c("period", "region", "sector", "subsectorL2", "subsectorL3", "technology")]

  fleetSizeAndComposition <- list(
    fleetESdemand = fleetESdemand,
    fleetESdemandConstrYears = fleetESdemandConstrYears,
    fleetVehNumbers = fleetVehNumbers,
    fleetVehNumbersConstrYears = fleetVehNumbersConstrYears,
    fleetVehiclesPerTech = fleetVehiclesPerTech,
    reportEarlyRetirement = reportEarlyRetirement
  )

  return(fleetSizeAndComposition)
}
