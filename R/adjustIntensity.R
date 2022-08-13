#' Adjust energy intensity on technology level based on techSen specific annual improvement rates and given start and end year
#'
#' @param intensity energy intensity raw data in EJ/pkm or EJ/tkm
#' @param intImproTab table that provides techScen specific annual improvement rates, start year and end year
#' @param years time horizon of EDGE-T
#' @return Adjusted energy intensities in EJ/pkm or EJ/tkm


toolAdjustIntensity <- function(intensity, intImproTab, years){

  FV_techvar <- technology <- level <- start_year <- start_fade <- end_year <- end_fade <- annual_factor <- annual_improvement_rate <- factor <- EJ_Mpkm_final <-
    EJ_Mpkm_final.y <- EJ_Mpkm_final.x <-  NULL

    #get yearly resolution
    intensity_yrl <- copy(intensity)
    intensity_yrl[, c("subsector_L1", "subsector_L2", "subsector_L3", "sector") := NULL]
    intensity_yrl <- intensity_yrl[year >= 2020]
    intensity_yrl <- approx_dt(intensity_yrl,
                             xdata = seq(2020,years[length(years)]),
                             xcol = "year",
                             ycol = "EJ_Mpkm_final",
                             idxcols = c("region", "vehicle_type", "technology", "sector_fuel"),
                             extrapolate = T)
    mimap <- system.file("extdata", "mitigation-techmap.csv", package = "edgeTransport")
    #Treat NG as liquids for simplification: Both get the same mitigation factors
    techmap <- fread(text="technology,FV_techvar
                             FCEV,Hydrogen
                             BEV,Electric
                             NG,Liquids
                             Hybrid Electric,Liquids")
    mimap <- fread(system.file("extdata", "mitigation-techmap.csv", package = "edgeTransport"))
    intensity_new <- merge(mimap, intensity_yrl, by = "vehicle_type", all.y = TRUE)
    intensity_new <- merge(techmap, intensity_new, by = "technology", all.y = TRUE)
    intensity_new[is.na(FV_techvar), FV_techvar := technology]
    intImproTab <- intImproTab[level == "FV"]
    intensity_new <- merge(intensity_new, intImproTab, by = c("FV_vehvar", "FV_techvar"))

    #Apply efficiency improvements only after year 2020
    intensity_new[, start_year := ifelse(start_year <2020, 2020, start_year)]
    #fade in and fade out time period
    fade_inout_period <- 15
    #Define start of fade in and end of fade out period, fade in period delays the improvement from the defined start year
    intensity_new[, start_fade := start_year]
    intensity_new[, start_year := start_fade + fade_inout_period]
    intensity_new[, end_fade := end_year + fade_inout_period]

    #Delete rows that are out of time scope
    intensity_new <- intensity_new[year <= end_fade]
    intensity_new[year < start_fade, annual_factor := 0,
                  by = c("region", "vehicle_type", "technology")]
    intensity_new[year >= start_fade & year < start_year, annual_factor := (100-(annual_improvement_rate*(year-start_fade)/fade_inout_period))/100,
                  by = c("region", "vehicle_type", "technology")]
    intensity_new[year >= start_year & year <= end_year, annual_factor := (100-annual_improvement_rate)/100,
                  by = c("region", "vehicle_type", "technology")]
    intensity_new[year > end_year & year <= end_fade, annual_factor := (100-(annual_improvement_rate*(end_fade-year)/fade_inout_period))/100,
                  by = c("region", "vehicle_type", "technology")]

    #Calculate cumulated efficiency factors
    intensity_new[, factor := cumprod(annual_factor),
                  by = c("region", "vehicle_type", "technology")]

    #Remove yearly resolution
    intensity_new <- intensity_new[year %in% years]

    #Apply factors
    intensity_new[year >= start_fade & year <= end_fade, EJ_Mpkm_final := EJ_Mpkm_final* factor,
                      by = c("region", "vehicle_type", "technology")]
    #Keep the intensity from end_year onward constant
    intensity_new <- intensity_new[year <= end_fade, c("region", "vehicle_type", "technology", "sector_fuel", "year", "EJ_Mpkm_final")]
    intensity_new <- approx_dt(intensity_new,
                      xdata = years[years>=2020],
                      xcol = "year",
                      ycol = "EJ_Mpkm_final",
                      idxcols = c("region", "vehicle_type", "technology", "sector_fuel"),
                      extrapolate = T)
    #Merge with unaffected intensity data
    intensity <- merge(intensity, intensity_new, by = c("region", "technology", "vehicle_type", "year", "sector_fuel"), all = TRUE)
    intensity[, EJ_Mpkm_final := ifelse(is.na(EJ_Mpkm_final.y), EJ_Mpkm_final.x, EJ_Mpkm_final.y)]
    #If Baseline efficiency improvements outperform techScen specific improvements after fade_out year (when the adjusted ones stay contstant), keep the Baseline
    intensity[!is.na(EJ_Mpkm_final.y), EJ_Mpkm_final := ifelse(EJ_Mpkm_final.x < EJ_Mpkm_final.y, EJ_Mpkm_final.x, EJ_Mpkm_final.y)][, c("EJ_Mpkm_final.x", "EJ_Mpkm_final.y") := NULL]

    return(intensity)


}
