


toolUpdateEndogenousCosts <- function(dataEndoCosts, depreciationFactors, scenParIncoCost, policyStartYear, timeValue, lambdas, helpers, numberOfvehicles = NULL) {

  dataEndoCosts <- copy(dataEndogenousCosts)
  depreciationFactors <- copy(vehicleDepreciationFactors)
  scenParIncoCost <- copy(scenModelPar$scenParIncoCost)
  # parameters of endogenous cost trends
  bfuelav = -20    ## value based on Greene 2001
  bmodelav = -12   ## value based on Greene 2001
  coeffrisk = 3800 ## value based on Pettifor 2017

  policyYears <- seq(policyStartYear, max(unique(dataEndoCosts$period)), 1)
  if (is.null(numberOfvehicles)){
    dataEndoCosts[, totVeh := 1]
  } else {
    dataEndoCosts <- merge(dataEndoCosts, numberOfvehicles, by = c("region", "period"))
  }


  for (t in policyYears) {

    # calculate proxy for total vehicles of one technology in the fleet----------------------------

    vehDepreciation <- copy(depreciationFactors)
    vehDepreciation[, period := t - indexUsagePeriod]
    dataEndoCosts <- merge(dataEndoCosts, vehDepreciation[, c("period", "univocalName", "depreciationFactor")], by = c("period", "univocalName"), all.x = TRUE)
    # calculate weighted average of the market sales multiplied with total vehicle number depreciating in time
    # to get a proxy for total vehicles of one technology in the fleet
    dataEndoCosts[!is.na(depreciationFactors), techFleetProxy := sum(FS3share * totVeh * depreciationFactor) / sum(totVeh * depreciationFactor),
                  by = c("region", "univocalName", "technology", "variable")]

    # update raw endogenous costs-------------------------------------------------------------------

    ## Stations availability featured by BEV, FCEV, Hybrid electric, NG
    dataEndoCosts[variable == "Stations availability" & period == t,
                    endoCostRaw := value[year == 2020] * exp(techFleetProxy[year == (t-1)] * bfuelav),
                        by = c("region", "technology", "vehicleType", "univocalName")]

    ## Risk aversion featured by BEV, FCEV, Hybrid electric, NG
    # HOW IT SHOULD BE (check in Pettifor 2017)
    # dataEndoCosts[variable == "Risk aversion" & period == t,
                #     value := pmax(value[year == 2020] - coeffrisk * techFleetProxy[year == (t-1)], 0),
                #       by = c("region", "technology", "vehicleType", "univocalName")]
    # HOW ACTUALLY IS
    dataEndoCosts[variable == "Risk aversion" & period == t,
                    endoCostRaw := value[year == 2020],
                      by = c("region", "technology", "vehicleType", "univocalName")]


    ## Model availability featured by BEV, FCEV, Hybrid electric, NG
    dataEndoCosts[variable == "Model availability" & period == t,
                   endoCostRaw := value[year == 2020] * exp(techFleetProxy[year == (t-1)] * bmodelav),
                      by = c("region", "technology", "vehicleType", "univocalName")]

    # Range anxiety featured by BEV (Does it make sense, that Range anxiety behaves exactly like stations availability?)
    dataEndoCosts[variable == "Range anxiety" & period == t,
                    endoCostRaw := value[year == 2020] * exp(techFleetProxy[year == (t-1)] * bfuelav),
                      by = c("region", "technology", "vehicleType", "univocalName")]

    # ICE inconvenience featured by ICE
    dataEndoCosts[variable == "ICE inconvenience" & period == t,
                    endoCostRaw := value[year == 2020] * exp(techFleetProxy[year == (t-1)] * bfuelav),
                      by = c("region", "technology", "vehicleType", "univocalName")]

    # calculate policy mask from scenParIncoCost ---------------------------------------------------

    linFunc <- function(year, startYear, startValue, targetYear, targetValue) {
      if (year < startYear) {
        return(startValue)
      } else if (year < targetYear) {
        return(startValue + ((targetValue - startValue) / (targetYear - startYear)) * (year - startYear))
      } else{
        return(targetValue)
      }
    }

    policyMask <- copy(scenParIncoCost)
    # Hybrid electric vehicles get a different policy parameter than BEV and ICE
    policyMaskPHEV <- policyMask[technology == "Hybrid electric"]
    setnames(policyMaskPHEV, "value", "policyMask")
    policyMaskPHEV <- policyMaskPHEV[, c("FVvehvar", "technology", "policyMask")]
    policyMask <- policyMask[!technology == "Hybrid electric"]
    policyMask <- dcast(policyMask, FVvehvar + technology ~ param, value.var = "value")
    # At the start of the policy intervention, the inconvenience costs for ICEs are zero, as they are the predominant and well-established technology.
    policyMask[technology == "Liquids", startValue := 0]
    policyMask[, policyMask := linFunc(t, startYear, startValue, targetYear, targetValue), by = c("technology")]
    policyMask <- policyMask[, c("FVvehvar", "technology", "policyMask")]
    policyMask <- rbind(policyMask, policyMaskPHEV)
    policyMask[, period := t]
    policyMask <- merge(policyMask, helpers$mitigationTechMap[, c("univocalName", "FVvehvar")], all.x = TRUE)[, FVvehvar := NULL]
    dataEndoCosts <- merge(dataEndoCosts, policyMask, by = c("univocalName", "technology", "period"), all.x = TRUE)

    # calculate resulting endogenous costs ---------------------------------------------------

    # ICE inconvenience featured by ICE
    dataEndoCosts[variable == "ICE inconvenience" & period == t,
                    value := pmax(policyMask, endoCostRaw),
                      by = c("region", "technology", "vehicleType", "univocalName")]

    # Range anxiety featured by BEV
    dataEndoCosts[variable == "Range anxiety" & period == t,
                   value := pmax(value[year == 2020] * policyMask, endoCostRaw),
                     by = c("region", "technology", "vehicleType", "univocalName")]

    # Model availability for hybrid electric
    dataEndoCosts[variable == "Model availability" & technology == "Hybrid electric" & period == t,
                  value := pmax(value[year == 2020] * policyMask, endoCostRaw),
                    by = c("region", "technology", "vehicleType", "univocalName")]

    dataEndoCosts[period == t & is.na(value), value := endoCostRaw]

    # calculate FS3 share --------------------------------------------------------------------
    FS3shares <- toolCalculateFS3share(dataEndoCosts, t, timeValue, lambdas, helpers)
    setnames(FS3shares, "FS3share", "FS3shareUpdate")
    dataEndoCosts <- merge(dataEndoCosts, FS3shares, by = intersect(names(dataEndoCosts), names(FS3shares)))
    dataEndoCosts[period == t, FS3share := FS3shareUpdate][, FS3shareUpdate := NULL]

  }

  dataEndoCosts[, FS3share := NULL]

  # For model behavior analysis all data is stored
  updatedEndogenousCosts <- copy(dataEndoCosts)
  updatedEndogenousCosts <- updatedEndogenousCosts[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                       "technology", "univocalName", "variable", "unit", "period", "value")]
  rawEndogenousCosts <- copy(dataEndoCosts)
  rawEndogenousCosts[, value := endoCostRaw]
  rawEndogenousCosts <- rawEndogenousCosts[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")]
  policyMask <- copy(dataEndoCosts)
  policyMask[, value := endoCostRaw]
  policyMask <- policyMask[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")]
  techFleetProxy <- copy(dataEndoCosts)
  techFleetProxy[, value := techFleetProxy]
  techFleetProxy[, variable := "Technology fleet proxy"]
  techFleetProxy <- unique(techFleetProxy[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")])

  endogenousCosts <- list(updatedEndogenousCosts = updatedEndogenousCosts, policyMask = policyMask, rawEndogenousCosts = rawEndogenousCosts, techFleetProxy = techFleetProxy)
  return(endogenousCosts)
}
