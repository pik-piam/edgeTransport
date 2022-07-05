adjust_intensity <- function(intensity, int_impro){
    
    ## Adjust tech mixes
    apply_logistic_trends <- function(yrs, final, ysymm, speed, initial = 1){
      fct <- exp((yrs - ysymm)/speed)/(exp((yrs - ysymm)/speed) + 1)
      initial + fct * (final - initial)
    }
   
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
      int_impro <- int_impro[level == "FV"][, c("subsector_L2", "subsector_L3") := NULL] 
      intensity_new <- merge(intensity_new, int_impro, by = c("FV_vehvar", "FV_techvar"))
      #Apply efficiency improvements 
      intensity_new[year >= 2020, EJ_Mpkm_final := apply_logistic_trends(year, target, symmyr, speed) * EJ_Mpkm_final,
        by = c("region", "vehicle_type", "technology")]
      intensity_new[, c("FV_vehvar", "FV_techvar", "level", "target", "symmyr", "speed", "SSP_scen", "tech_scen") := NULL]
      
      #Merge with unaffected intensity data 
      intensity <- merge(intensity, intensity_new, by = c("region", "technology", "vehicle_type", "year", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel"), all = TRUE)
      intensity[, EJ_Mpkm_final := ifelse(is.na(EJ_Mpkm_final.y), EJ_Mpkm_final.x, EJ_Mpkm_final.y)][, c("EJ_Mpkm_final.x", "EJ_Mpkm_final.y") := NULL]
      
    return(intensity)

    } 
    
}