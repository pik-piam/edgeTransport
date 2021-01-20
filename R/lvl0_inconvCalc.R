#' Calculates the inconvenience costs for 4wheelers
#'
#' @param annual_mileage annual mileage of vehicles
#' @param load_factor load factor vehicles
#' @param fcr_veh depreciation rate

lvl0_incocost <- function(annual_mileage, load_factor, fcr_veh){
  subsector_L1 <- prange <- pref <- pchar <- value <- loadFactor <- technology <- NULL

  pmod_av = 100000 ## $/veh
  prefuelFCEV = 100000 ## $/veh
  prefuelNG = 100000 ## $/veh
  prangeBEV = 100000 ## $/veh
  prisk = 3800 ## $/veh
  prefElRech = 1000 ## $/veh

  pmod_av = pmod_av*fcr_veh ## disc$/veh
  prefuelFCEV = prefuelFCEV*fcr_veh ## disc$/veh
  prefuelNG = prefuelNG*fcr_veh ## disc$/veh
  prangeBEV = prangeBEV*fcr_veh ## disc$/veh
  prisk = prisk*fcr_veh ## disc$/veh
  prefElRech = prefElRech*fcr_veh ## disc$/veh

  pinco = merge(annual_mileage, load_factor, by = c("region", "year", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type"))[year <= 2020]
  pinco = pinco[subsector_L1 == "trn_pass_road_LDV_4W" & technology %in% c("BEV", "FCEV", "Hybrid Electric","NG")]
  ## infrastructure lack contains range anxiety and refueling availability, and is 0 in the case of Hybrids
  pinco[, prange := ifelse(technology == "BEV", prangeBEV, 0)]
  pinco[, pref := ifelse(technology == "FCEV", prefuelFCEV, 0)]
  pinco[, pref := ifelse(technology == "NG", prefuelNG, pref)]
  pinco[, pchar := ifelse(technology %in% c("Hybrid Electric", "BEV"), prefElRech, 0)]
  ## model availability is the same for all technologies
  pinco[, pmod_av := pmod_av]
  ## risk premium is the same for all technologies
  pinco[, prisk := prisk]

  pinco[year >= 2010 & technology == "BEV", pmod_av := (pmod_av[year==2010]-10000*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "Hybrid Electric", pmod_av := (pmod_av[year==2010]-69300*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "NG", pmod_av := (pmod_av[year==2010]-69300*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "FCEV", pmod_av := (pmod_av[year==2010]-69300*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "BEV", prange := (prange[year==2010]-65000*fcr_veh)/(2010-2020)*(year-2010)+prange[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]

  pinco = melt(pinco, id.vars = c("region", "year", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "annual_mileage", "loadFactor", "mode"))
  setnames(pinco, old = "variable", new = "logit_type")
  pinco[, value:= value/(annual_mileage*loadFactor)]
  pinco[, c("annual_mileage", "loadFactor", "mode"):= NULL]

  return(pinco)
}
