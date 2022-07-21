#' Calculates the inconvenience costs for 4wheelers
#'
#' @param annual_mileage annual mileage of vehicles
#' @param load_factor load factor vehicles
#' @param fcr_veh depreciation rate
#' @param REMINDp REMIND prices

toolIncocost <- function(annual_mileage, load_factor, fcr_veh, REMINDp) {
  subsector_L1 <- prange <- pref <- pchar <- value <- loadFactor <- technology <- vkm.veh <- `.` <-
    meanp <- non_fuel_price <- pmod_av_startv <- prange_startv <- prisk <- region <- resc <-
      tot_price <- NULL

  ## very high values attributed to years before 2010, as almost 0 alternative cars are in the mix
  pmod_av = 100000 ## $/veh
  prefuelFCEV = 100000 ## $/veh
  prefuelNG = 100000 ## $/veh
  prangeBEV = 100000 ## $/veh

  ## discount the starting values
  pmod_av = pmod_av*fcr_veh ## disc$/veh
  prefuelFCEV = prefuelFCEV*fcr_veh ## disc$/veh
  prefuelNG = prefuelNG*fcr_veh ## disc$/veh
  prangeBEV = prangeBEV*fcr_veh ## disc$/veh

  pinco = merge(annual_mileage, load_factor, by = c("region", "year", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type"))[year <= 2020]
  pinco = pinco[subsector_L1 == "trn_pass_road_LDV_4W" & technology %in% c("BEV", "FCEV", "Hybrid Electric","NG")]


  ## infrastructure lack contains range anxiety and refueling availability, and is 0 in the case of Hybrids
  pinco[, prange := ifelse(technology == "BEV", prangeBEV, 0)]
  pinco[, pref := ifelse(technology == "FCEV", prefuelFCEV, 0)]
  pinco[, pref := ifelse(technology == "NG", prefuelNG, pref)]

  ## model availability is the same for all technologies
  pinco[, pmod_av := pmod_av]

  ## EU values for model availability and range anxiety in 2020
  EU_val = data.table(technology = c("BEV", "BEV", "NG", "FCEV", "Hybrid Electric", "BEV", "NG", "FCEV", "Hybrid Electric", "BEV", "Hybrid Electric"),
                      cost_comp = c("pmod_av_startv", "prange_startv", "pmod_av_startv", "pmod_av_startv", "pmod_av_startv", "prisk", "prisk", "prisk", "prisk", "pchar", "pchar"),
                      value = c(10000, 65000, 69300, 69300, 69300, 3800, 3800, 3800, 3800, 1000, 1000))
  ## find the average price wrt
  REMINDp = REMINDp[subsector_L1 == "trn_pass_road_LDV_4W" & !is.na(tot_price) & year==2020]
  REMINDp = REMINDp[,.(meanp = mean(non_fuel_price)), by = c("region", "technology")]

  ## use DEU as a comparison (all EU regions will not be overwritten anyways)
  REMINDp[, resc := meanp/meanp[technology=="Liquids" & region =="DEU"]]
  allreg = merge(EU_val, REMINDp, by = "technology", allow.cartesian = T)
  allreg[!region %in% c("DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI"), value := value*((resc-1)/2+1)]
  allreg = allreg[, c("meanp", "resc") := NULL]
  allreg = dcast(allreg,... ~ cost_comp, value.var = "value")

  pinco = merge(pinco, allreg, by = c("region", "technology"))

  pinco[year >= 2010 & technology == "BEV", pmod_av := (pmod_av[year==2010]-pmod_av_startv*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "Hybrid Electric", pmod_av := (pmod_av[year==2010]-pmod_av_startv*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "NG", pmod_av := (pmod_av[year==2010]-pmod_av_startv*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "FCEV", pmod_av := (pmod_av[year==2010]-pmod_av_startv*fcr_veh)/(2010-2020)*(year-2010)+pmod_av[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[year >= 2010 & technology == "BEV", prange := (prange[year==2010]-prange_startv*fcr_veh)/(2010-2020)*(year-2010)+prange[year==2010], by = c("region", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type")]
  pinco[, prisk := fcr_veh*prisk]
  pinco[, pchar := fcr_veh*pchar]

  pinco[, c("prange_startv", "pmod_av_startv") := NULL]
  pinco = melt(pinco, id.vars = c("region", "year", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "vkm.veh", "loadFactor"))
  setnames(pinco, old = "variable", new = "logit_type")
  pinco[, value:= value/(vkm.veh*loadFactor)]
  pinco[, c("vkm.veh", "loadFactor"):= NULL]

  return(pinco)
}
