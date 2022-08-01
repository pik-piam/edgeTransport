toolAdjust_intensity <- function(intensity, int_impro, years){

    mimap <- system.file("extdata", "mitigation-techmap.csv", package = "edgeTransport")
    techmap <- fread(text="technology,FV_techvar
                             FCEV,Hydrogen
                             BEV,Electric
                             NG,Liquids
                             Hybrid Electric,Liquids")
    mimap <- fread(system.file("extdata", "mitigation-techmap.csv", package = "edgeTransport"))
    intensity_new <- merge(mimap, intensity, by = "vehicle_type", all.y = TRUE)
    intensity_new <- merge(techmap, intensity_new, by = "technology", all.y = TRUE)
    intensity_new[is.na(FV_techvar), FV_techvar := technology]
    int_impro <- int_impro[level == "FV"]
    intensity_new <- merge(intensity_new, int_impro, by = c("FV_vehvar", "FV_techvar"))

    #Apply efficiency improvements only after year 2020
    intensity_new[, start_year := ifelse(start_year <2020, 2020, start_year)]
    intensity_new[year >= start_year & year <= end_year, EJ_Mpkm_final := EJ_Mpkm_final* ((100-annual_improvement_rate)/100)^(year-start_year),
                      by = c("region", "vehicle_type", "technology")]

    #Keep the intensity from end_year onward constant
    intensity_new <- intensity_new[year <= end_year, c("region", "vehicle_type", "technology", "sector_fuel", "year", "EJ_Mpkm_final")]
    intensity_new <- approx_dt(intensity_new,
                      xdata = years,
                      xcol = "year",
                      ycol = "EJ_Mpkm_final",
                      idxcols = c("region", "vehicle_type", "technology", "sector_fuel"),
                      extrapolate = T)
    intensity <- intensity[year %in% unique(intensity_new$year)]

    #Merge with unaffected intensity data
    intensity <- merge(intensity, intensity_new, by = c("region", "technology", "vehicle_type", "year", "sector_fuel"), all = TRUE)
    intensity[, EJ_Mpkm_final := ifelse(is.na(EJ_Mpkm_final.y), EJ_Mpkm_final.x, EJ_Mpkm_final.y)][, c("EJ_Mpkm_final.x", "EJ_Mpkm_final.y") := NULL]

    return(intensity)


}
