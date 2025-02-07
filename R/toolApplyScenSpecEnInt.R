#' Apply demand scenario specific adjustments to the energy intensity
#' @author Johanna Hoppe
#' @param enInt Energy intensity input data supplied by mrtransport
#' @param scenParEnergyIntensity Transport policy scenario specific energy intensity improvement factors
#' @param policyStartYear Year from which scenario specific transport policies are applied
#' @param helpers List with helpers
#' @returns data.table with scenario specific energy intensity input data
#' @import data.table


toolApplyScenSpecEnInt <- function(enInt, scenParEnergyIntensity, policyStartYear, helpers, cm_startYear = 2025) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- region <- univocalName <- technology <- FVvehvar <- startYear <- startFade <- endFade <- endYear <- NULL
  value.x <- value.y <- annualFactor <- annualImprovementRate <- NULL

  # get yearly resolution
  enIntYearly <- copy(enInt)
  enIntYearly <- enIntYearly[period >= policyStartYear]
  enIntYearly <- approx_dt(enIntYearly,
                             xdata = unique(enIntYearly$period),
                             xcol = "period",
                             ycol = "value",
                             extrapolate = TRUE)

  enIntNew <- merge(helpers$mitigationTechMap[, c("FVvehvar", "univocalName")], enIntYearly, by = "univocalName", all.y = TRUE)
  enIntNew <- merge(enIntNew, scenParEnergyIntensity, by = c("FVvehvar", "technology"))[, FVvehvar := NULL]

  # ToDo: apply cm_startYear switch, either update startYear here and start later phase-in, or only apply factor after startYear in ln 62

  # Apply efficiency improvement factors provided in scenParEnergyIntensity only after year 2020
  enIntNew[, startYear := ifelse(startYear < policyStartYear, policyStartYear, startYear)]
  # fade in and fade out time period
  fadeInOutPeriod <- 15
  # Define start of fade in and end of fade out period, fade in period delays the improvement from the defined start year
  enIntNew[, startFade := startYear]
  enIntNew[, startYear := startFade + fadeInOutPeriod]
  enIntNew[, endFade := endYear + fadeInOutPeriod]

  # Delete rows that are out of time scope
  enIntNew <- enIntNew[period <= endFade]
  enIntNew[period < startFade, annualFactor := 0,
                by = c("region", "univocalName", "technology")]
  enIntNew[period >= startFade & period < startYear, annualFactor := (100 - (annualImprovementRate *
                                                                 (period - startFade) / fadeInOutPeriod)) / 100,
                by = c("region", "univocalName", "technology")]
  enIntNew[period >= startYear & period <= endYear, annualFactor := (100 - annualImprovementRate) / 100,
                by = c("region", "univocalName", "technology")]
  enIntNew[period > endYear & period <= endFade, annualFactor := (100 - (annualImprovementRate * (endFade - period)
                                                                        / fadeInOutPeriod)) / 100,
                by = c("region", "univocalName", "technology")]

  # sort data.table
  setkey(enIntNew, region, univocalName, technology, period)

  # Calculate cumulated efficiency factors
  enIntNew[, factor := cumprod(annualFactor),
                by = c("region", "univocalName", "technology")]

  # Apply factors only for period > cm_startYear for fix on reference run in REMIND
  # this equals a 'hot-start' of the EnInt adjustments with cm_startYear,
  # i.e. no phase-in from that point but a jump to the cumulative factor in cm_startYear
  # assume startYearCat is 'final', i.e. cm_startYear enables a later switch-on of the mask
  if (unique(enIntNew$startYearCat) == "final"){
    enIntNew <- enIntNew[ period > cm_startYear]
  } else {
    stop("Error in application of cm_startYear. Please check toolApplyScenSpecEnInt()")
    }

  enIntNew[period >= startFade & period <= endFade, value := value * factor,
           by = c("region", "univocalName", "technology")][, c("factor", "startYear", "startFade", "endYear", "endFade", "annualImprovementRate", "annualFactor", "startYearCat") := NULL]
  # Remove yearly resolution for vehicle types that do not feature fleet tracking and extrapolate the timeSteps after the fadeout year to be constant
  enIntNew <- toolApplyMixedTimeRes(enIntNew, helpers)
  enIntNew <- enIntNew[period >= policyStartYear]

  # Merge with unaffected enInt data
  cols <- intersect(names(enInt), names(enIntNew))
  cols <- cols[!cols == "value"]
  enInt <- merge(enInt, enIntNew, by = cols, all.x = TRUE)
  enInt[, value := ifelse(is.na(value.y), value.x, value.y)]
  # If Baseline efficiency improvements outperform transportPolScen specific improvements after fadeOut year (when the adjusted ones stay contstant), keep the Baseline
  enInt[!is.na(value.y), value := ifelse(value.x < value.y, value.x, value.y)][, c("value.x", "value.y") := NULL]

  if (anyNA(enInt) == TRUE) {
stop("NAs were introduced whilst applying the scenario specific energy intensity improvements. Please check toolApplyScenSpecEnInt()")
}

  return(enInt)
  }
