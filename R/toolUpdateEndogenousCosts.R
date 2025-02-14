#' @title toolUpdateEndogenousCosts
#' @description Provides updates for endogenous cost components e.g. inconvenience costs for cars
#'
#' @param dataEndoCosts data.table containing all cost components for cars over the full range
#'                      of policy years in a yearly resolution. Exogenous CAPEX and OPEX are provided over the full range.
#'                      Endogenous cost components and FS3 shares are provided until 2020. Rest is filled with NA.
#' @param depreciationFactors data.table containing vehicle depreciation factor for each year of service Life
#' @param scenParIncoCost data.table containing scenario specific parameters for inconvenience costs policy mask
#' @param policyStartYear year from which scenario-specific differentiation begins
#' @param timeValue data.table containing mode specific time value costs based on speed and gdp
#' @param preferences preference factor trends
#' @param lambdas data.table containing exponents for discrete choice calculation
#' @param helpers list containing helpers like mappings, decisionTree etc.
#' @param isICEban optional enabling of ICE ban
#' @param ICEbanYears sequence of years in which ICEban is applied
#' @param vehiclesPerTech data.table containing total number of vehicles for all years and regions
#' @return list containing data.table with endogenous cost components over the full time span and additional data.tables
#'         for model behavior analysis
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolUpdateEndogenousCosts <- function(dataEndoCosts,
                                      depreciationFactors,
                                      scenParIncoCost,
                                      policyStartYear,
                                      timeValue,
                                      preferences,
                                      lambdas,
                                      helpers,
                                      isICEban,
                                      ICEbanYears,
                                      vehiclesPerTech = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  totVeh <- technology <- startValue <- period <- startYear <- targetYear <- targetValue <- NULL
  FVvehvar <- regionCode12 <- region <- type <- endoCostRaw <- value <- indexUsagePeriod <- NULL
  depreciationFactor <- FS3share <- variable <- FS3shareUpdate <- unit <- NULL

  # parameters of endogenous cost trends
  bfuelav <- -20    ## value based on Greene 2001
  bmodelav <- -12   ## value based on Greene 2001
  coeffrisk <- 3800 ## value based on Pettifor 2017

  policyYears <- seq(2021, 2100, 1)
  # preventing dataEndoCosts to be updated outside of the function
  dataEndoCosts <- copy(dataEndoCosts)

  if (is.null(vehiclesPerTech)) {
    dataEndoCosts[, totVeh := 1]
  } else {
    dataEndoCosts <- merge(dataEndoCosts, vehiclesPerTech,
                           by = c("region", "period", "sector", "subsectorL2", "subsectorL3", "technology"))
  }

  # calculate policy mask from scenParIncoCost ---------------------------------------------------
  linFunc <- function(year, startYear, startValue, targetYear, targetValue) {
    if (year < startYear) {
      return(startValue)
    } else if (year < targetYear) {
      return(startValue + ((targetValue - startValue) / (targetYear - startYear)) * (year - startYear))
    } else {
      return(targetValue)
    }
  }

  ## Check if transportPol or SSPscen change is introduced with policyStartYear
  # If both stay the same, set policyStartYear out of bounds such that it does not affect the calculation here
  if (!"final" %in% scenParIncoCost$startYearCat){
    policyStartYear <- 2200
  }

  ## the policymaker bans ICEs increasingly more strictly
  strangeICEbanFunction <- function(x, x0, y0, x1, y1) {
    return(min(y1, max(y0, (y1 - y0) / (x1 - x0) * (x - x0) + y0)))
  }

  applyICEban <- function(year, currentMask) {
    if (year < 2021) {
      floorCosts <- currentMask
    } else if (year >= 2021 && year <= 2030) {
      floorCosts <- strangeICEbanFunction(year, 2025, 0.08, 2030, 0.19)
    } else if (year > 2030 && year < 2035) {
      floorCosts <- strangeICEbanFunction(year, 2031, 0.2, 2034, 0.6)
    } else if (year == 2035) {
      floorCosts <- 1
    } else if  (year > 2035) {
      floorCosts <- 2
    }
    return(floorCosts)
  }
  policyMask <- copy(scenParIncoCost)
  # Expand regional and temporal resolution
  regions <- unique(dataEndoCosts$region)
  tempAndregions <- CJ(region = regions, period = policyYears)
  tempAndregions[, all := "All"]
  policyMask[, all := "All"]
  policyMaskO <- merge(policyMask[startYearCat == "origin"], tempAndregions[period <= policyStartYear], by = "all", allow.cartesian = TRUE)[, all := NULL]
  policyMaskF <- merge(policyMask[startYearCat == "final"], tempAndregions[period > policyStartYear], by = "all", allow.cartesian = TRUE)[, all := NULL]
  policyMask <- rbind(policyMaskO, policyMaskF)
  policyMask[, "startYearCat" := NULL]
  # Hybrid electric vehicles get a different policy parameter than BEV and ICE
  policyMaskPHEV <- policyMask[technology == "Hybrid electric"]
  setnames(policyMaskPHEV, "value", "policyMask")
  policyMaskPHEV <- policyMaskPHEV[, c("region", "period", "FVvehvar", "technology", "policyMask")]
  policyMask <- policyMask[!technology == "Hybrid electric"]
  policyMask <- dcast(policyMask, region + period + FVvehvar + technology ~ param, value.var = "value")
  # At the start of the policy intervention, the inconvenience costs for ICEs are zero, as they are the predominant and well-established technology.
  policyMask[technology == "Liquids", startValue := 0]
  policyMask[, policyMask := linFunc(period, startYear, startValue, targetYear, targetValue), by = c("region", "period", "technology")]
  policyMask <- policyMask[, c("region", "period", "FVvehvar", "technology", "policyMask")]
  policyMask <- rbind(policyMask, policyMaskPHEV)
  policyMask <- merge(policyMask, helpers$mitigationTechMap[, c("univocalName", "FVvehvar")], all.x = TRUE, allow.cartesian = TRUE)[, FVvehvar := NULL]


  # Change policy mask for ICEs when ban is activated
  if (isICEban) {
    # Ban is applied to EU28 or EUR in case of REMIND running on 12 regions
    affectedRegions <- unique(helpers$regionmappingISOto21to12[regionCode12 == "EUR"]$regionCode21)
    affectedRegions <- c(affectedRegions, "EUR")
    # affectedRegions <- affectedRegions[!affectedRegions == "UKI"]
    # After 2030 Gases and Hybrid Electric get the policy mask of liquids
    policyMaskICEban <- policyMask[technology %in% c("Liquids") & region %in% affectedRegions]
    policyMask <- rbind(policyMask[!(technology %in% c("Gases", "Hybrid electric") & region %in% affectedRegions)],
                        copy(policyMaskICEban)[, technology := "Hybrid electric"], copy(policyMaskICEban)[, technology := "Gases"])
    setkey(policyMask, region, period, technology)
    dt <- policyMask
    after <- policyMask

    policyMask[technology %in% c("Liquids", "Gases", "Hybrid electric") & region %in% affectedRegions & period %in% ICEbanYears,
               policyMask := max(policyMask, applyICEban(period, policyMask)), by = c("period")]
  }

  # check whether policy mask is calculated correctly for respective technologys
  if (anyNA(policyMask)) {
    stop("Something went wrong with the calculation of the policyMask in toolUpdateEndogenousCosts() ")
  }
  dataEndoCosts <- merge(dataEndoCosts, policyMask, by = c("region", "period", "univocalName", "technology"), all.x = TRUE)
  dataEndoCosts[type == "Inconvenience costs", endoCostRaw := value]

  for (t in policyYears) {
    # calculate proxy for total vehicles of one technology in the fleet ----------------------------

    vehDepreciation <- copy(depreciationFactors)
    vehDepreciation <- vehDepreciation[!indexUsagePeriod == 0]
    vehDepreciation[, period := t - indexUsagePeriod]
    dataEndoCosts <- merge(dataEndoCosts, vehDepreciation[, c("period", "univocalName", "depreciationFactor")], by = c("period", "univocalName"), all.x = TRUE)
    # calculate weighted average of the market sales multiplied with total vehicle number depreciating in time
    # to get a proxy for total vehicles of one technology in the fleet
    dataEndoCosts[!is.na(depreciationFactor), techFleetProxy := sum(FS3share * totVeh * depreciationFactor) / sum(totVeh * depreciationFactor),
                  by = c("region", "univocalName", "technology", "variable")]

    # update raw endogenous costs-------------------------------------------------------------------
    ## Stations availability featured by BEV, FCEV, Hybrid electric, Gases
    dataEndoCosts[variable == "Stations availability" & technology %in% c("Gases"), endoCostRaw := ifelse(period == t,
                                                                                                          pmax(value[period == 2020], value[period == 2020] * exp(techFleetProxy[period == (t - 1)] * bfuelav)),
                                                                                                          endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]

    dataEndoCosts[variable == "Stations availability" & technology %in% c("FCEV", "Gases"), endoCostRaw := ifelse(period == t,
                                                                                                         value[period == 2020] * exp(techFleetProxy[period == (t - 1)] * bfuelav),
                                                                                                         endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]

    dataEndoCosts[variable == "Stations availability" & technology %in% c("BEV", "Hybrid electric"), endoCostRaw := ifelse(period == t,
                                                                                                                  value[period == t - 1],
                                                                                                                  endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]



    ## Risk aversion featured by BEV, FCEV, Hybrid electric, Gases
    # HOW IT SHOULD BE (check in Pettifor 2017)
    # dataEndoCosts[variable == "Risk aversion", endoCostRaw := ifelse(period == t,
                #      pmax(value[period == 2020] - coeffrisk * techFleetProxy[period == (t - 1)], 0),
                #       endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]
    # HOW ACTUALLY IS -> Risk aversion stays constant on 2020 value. Change may make it necessary to rework the scenarios parameters.
    dataEndoCosts[variable == "Risk aversion", endoCostRaw := ifelse(period == t,
                    value[period == 2020],
                      endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]


    ## Model availability featured by BEV, FCEV, Hybrid electric, Gases
    dataEndoCosts[variable == "Model availability"  &  technology == "Hybrid electric", endoCostRaw := ifelse(period == t,
                   value[period == 2020],
                    endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]

    # Model availability of BEV, FCEV, Gases is not updated
    dataEndoCosts[variable == "Model availability"  &  !technology == "Hybrid electric", endoCostRaw := ifelse(period == t,
                                                                                                               value[period == t - 1], endoCostRaw),
                  by = c("region", "technology", "vehicleType", "univocalName")]

    # Range anxiety featured by BEV (Does it make sense, that Range anxiety behaves exactly like stations availability?)
    dataEndoCosts[variable == "Range anxiety", endoCostRaw := ifelse(period == t,
                    value[period == 2020] * exp(techFleetProxy[period == (t - 1)] * bfuelav),
                      endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]

    # ICE inconvenience featured by ICE (Why 0.5?)
    dataEndoCosts[variable == "ICE inconvenience", endoCostRaw := ifelse(period == t,
                    0.5 * exp(techFleetProxy[period == (t - 1)] * bmodelav),
                      endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]
    # check whether all inconvenience cost types were updated
    if (anyNA(dataEndoCosts[period == t & type == "Inconvenience costs"]$endoCostRaw)) {
      stop(paste0("Something went wrong with the calculation of the raw endogenous costs in toolUpdateEndogenousCosts() ", t))
    }

    # calculate resulting endogenous costs ---------------------------------------------------
    # The policy mask affects only certain types of inconvenience costs. Others are affected indirectly by the increasing/decreasing market share of the technologies.
    # There is no lever for FCEVs. This should be reworked.

    # ICE inconvenience featured by ICE
    dataEndoCosts[variable == "ICE inconvenience" & period == t,
                    value := pmax(policyMask, endoCostRaw),
                      by = c("region", "technology", "vehicleType", "univocalName")]

    # Range anxiety featured by BEV (right now, the policy masks ensures that BEVs are phased-in not too quickly, therefore pmax is used. This is a result from poor parameterization
    # and should be reworked)
    dataEndoCosts[variable == "Range anxiety" & period == t,
                   value := pmax(value[period == 2020] * policyMask, endoCostRaw),
                     by = c("region", "technology", "vehicleType", "univocalName")]

    ratioPhev <- unique(policyMaskPHEV$policyMask)
    # Model availability for Hybrid electric
    if (isICEban) dataEndoCosts[variable == "Model availability" & technology == "Hybrid electric" & period == t & period >= 2030 & period %in% ICEbanYears &
                      region %in% affectedRegions, endoCostRaw := pmax(policyMask, endoCostRaw),
                                by = c("region", "technology", "vehicleType", "univocalName")]

    dataEndoCosts[variable == "Model availability" & technology == "Hybrid electric" & period == t,
                  value := pmax(value[period == 2020] * ratioPhev, endoCostRaw),
                    by = c("region", "technology", "vehicleType", "univocalName")]


    # Stations availability for NG vehicles is fixed to 2020 values (due to very low costs for NG in CHA). Maybe this fix can be removed nowadays?
    # For PhOP scenario NG and Hybrid electrics needs to be phased out like ICEs: Not implemented yet
    dataEndoCosts[variable == "Stations availability" & technology == "Gases" & period == t,
                    value := pmax(value[period == 2020], endoCostRaw),
                      by = c("region", "technology", "vehicleType", "univocalName")]
    if (isICEban) dataEndoCosts[variable == "Stations availability" & technology == "Gases" & period == t & period %in% ICEbanYears,
                                value := pmax(policyMask, endoCostRaw),
                                by = c("region", "technology", "vehicleType", "univocalName")]

    dataEndoCosts[period == t & is.na(value), value := endoCostRaw]

    # check whether all resulting inconvenience costs were calculated
    if (anyNA(dataEndoCosts[period == t & type == "Inconvenience costs"]$value)) {
      stop(paste0("Something went wrong with the endogenous cost update in toolUpdateEndogenousCosts() in timestep ", t))
      }

    # calculate FS3 share --------------------------------------------------------------------
    FS3shares <- toolCalculateFS3share(dataEndoCosts, t, timeValue, preferences, lambdas, helpers)
    setnames(FS3shares, "FS3share", "FS3shareUpdate")
    dataEndoCosts <- merge(dataEndoCosts, FS3shares, by = intersect(names(dataEndoCosts), names(FS3shares)), all.x = TRUE)
    dataEndoCosts[period == t, FS3share := FS3shareUpdate][, c("FS3shareUpdate", "depreciationFactor") := NULL]

  }

  # Monetary costs and time value costs were needed to calculate the technology fleet proxy
  # They are stored already and should not be stored again together with the inconvenience costs
  dataEndoCosts <- dataEndoCosts[!type == "Monetary costs"]
  dataEndoCosts[, c("FS3share", "type") := NULL]

  # For model behavior analysis all data is stored
  updatedEndogenousCosts <- copy(dataEndoCosts)[, variable := paste0("Inconvenience costs|", variable)]
  updatedEndogenousCosts <- updatedEndogenousCosts[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                       "technology", "univocalName", "variable", "unit", "period", "value")]
  rawEndogenousCosts <- copy(dataEndoCosts)
  rawEndogenousCosts[, value := endoCostRaw][, variable := paste0("Inconvenience costs (raw)|", variable)]
  rawEndogenousCosts <- rawEndogenousCosts[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")]
  policyMask <- copy(dataEndoCosts)
  policyMask[, value := policyMask][, variable := paste0("Policy mask|", variable)]
  policyMask <- policyMask[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")]
  techFleetProxy <- copy(dataEndoCosts)
  techFleetProxy[, value := techFleetProxy]
  techFleetProxy[, variable := "Technology fleet proxy"][, unit := "-"]
  techFleetProxy <- unique(techFleetProxy[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")])

  endogenousCosts <- list(updatedEndogenousCosts = updatedEndogenousCosts, policyMask = policyMask, rawEndogenousCosts = rawEndogenousCosts, techFleetProxy = techFleetProxy)

  outputYears <- c(1990, seq(2005, 2100, by = 1), 2110, 2130, 2150)
  endogenousCosts <- lapply(endogenousCosts, approx_dt, outputYears, "period", "value",
                       setdiff(names(updatedEndogenousCosts), c("period", "value")), extrapolate = TRUE)


  return(endogenousCosts)
}
