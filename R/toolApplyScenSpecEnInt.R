#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param enInt energy intensity input data supplied by mrtransport
#' @param intImprovementFactors transport policy scenario specific energy intensity improvement factors
#' @param mitigationTechMap aggregated vehicle map for policy parameters
#' @param yrs temporal resolution of edgeTransport
#' @import data.table


toolApplyScenSpecEnInt <- function(enInt, intImprovementFactors, mitigationTechMap, yrs) {

  #get yearly resolution
  enIntYearly <- copy(enInt)
  enIntYearly <- enIntYearly[period >= 2020]
  enIntYearly <- approx_dt(enIntYearly,
                             xdata = seq(2020, period[length(period)]),
                             xcol = "period",
                             ycol = "value",
                             idxcols = c("region", "univocalName", "technology"),
                             extrapolate = T)

  enIntNew <- merge(mitigationTechMap[, c("FVvehvar", "univocalName")], enIntYearly, by = "FVvehvar", all.y = TRUE)

  intImproTab <- intImproTab[level == "FV"]
  enIntNew <- merge(enIntNew, intImprovementFactors, by = c("FVvehvar", "technology"))[, FVvehvar := NULL]

  #Apply efficiency improvements only after year 2020
  enIntNew[, startYear := ifelse(startYear < 2020, 2020, startYear)]
  #fade in and fade out time period
  fadeInOutPeriod <- 15
  #Define start of fade in and end of fade out period, fade in period delays the improvement from the defined start year
  enIntNew[, startFade := startYear]
  enIntNew[, startYear := startFade + fadeInOutPeriod]
  enIntNew[, endFade := endYear + fadeInOutPeriod]

  #Delete rows that are out of time scope
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

  #Calculate cumulated efficiency factors
  enIntNew[, factor := cumprod(annualFactor),
                by = c("region", "univocalName", "technology")]

  #Remove yearly resolution
  enIntNew <- enIntNew[period %in% yrs]

  #Apply factors
  enIntNew[period >= startFade & period <= endFade, value := value * factor,
                by = c("region", "univocalName", "technology")]
  #Keep the enInt from endYear onward constant
  enIntNew <- enIntNew[period <= endFade, c("region", "univocalName", "technology", "period", "value")]
  enIntNew <- approx_dt(enIntNew,
                             xdata = years[periods >= 2020],
                             xcol = "period",
                             ycol = "value",
                             idxcols = c("region", "univocalName", "technology"),
                             extrapolate = T)
  #Merge with unaffected enInt data
  enInt <- merge(enInt, enIntNew, by = c("region", "technology", "univocalName", "period"), all = TRUE)
  enInt[, value := ifelse(is.na(value.y), value.x, value.y)]
  #If Baseline efficiency improvements outperform transportPolScen specific improvements after fadeOut year (when the adjusted ones stay contstant), keep the Baseline
  enInt[!is.na(value.y), value := ifelse(value.x < value.y, value.x, value.y)][, c("value.x", "value.y") := NULL]

  return(enInt)
  }
