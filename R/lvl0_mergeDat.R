#' Merge input data
#'
#' Final values:
#'
#'
#' @param UCD_output UCD data
#' @param EU_data EU data
#' @param PSI_costs PSI-based costs
#' @param altCosts alternative trucks cost
#' @param CHN_trucks CHN trucks costs
#' @param GCAM_data GCAM data
#' @param PSI_int PSI-based intensity
#' @param trsp_incent transport incentives on capital costs
#' @param fcr_veh annualization factor for LDVs
#' @param nper_amort_veh years of amortization which a LDV
#' @param smartlifestyle switch activatinf sustainable lifestyles
#' @param SSP_scen REMIND SSP scenario
#' @param REMIND2ISO_MAPPING REMIND regional mapping
#' @param years time steps
#' @return costs, intensity, LF, AM, demand
#' @author Marianna Rottoli, Alois Dirnaichner

lvl0_mergeDat = function(UCD_output, EU_data, PSI_costs, altCosts, CHN_trucks, GCAM_data,
                         PSI_int, trsp_incent, fcr_veh, nper_amort_veh, smartlifestyle,
                         SSP_scen, years, REMIND2ISO_MAPPING){
  vkm.veh <- value <- variable <- conv_pkm_MJ <- conv_vkm_MJ <- ratio <- MJ_km <- sector_fuel <- subsector_L3 <- `.` <- NULL
  k <- subsector_L2 <- tech_output <- MJ <- region <- loadFactor <- vehicle_type <- iso <- univocal_name <- technology <- NULL
  val <- markup <- UCD_technology <- valUCD <- NULL
  subsector_L1 <- vkm.veh <- tot_purchasecost <- aveval <- incentive_val <- unit <- demldv <- NULL
  logit_cat = copy(GCAM_data[["logit_category"]])
  logit_cat = rbind(logit_cat,
                    logit_cat[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "BEV"][, technology := "Hybrid Electric"])
  logit_cat = logit_cat[technology != "Hybrid Liquids"]
  ## merge LF
  LF_EU = approx_dt(EU_data$LF_countries_EU,
                    xdata = unique(GCAM_data$load_factor$year),
                    xcol = "year",
                    ycol = "loadFactor",
                    idxcols = c("iso", "vehicle_type"),
                    extrapolate = T)

  LF_EU = merge(LF_EU, logit_cat, by = "vehicle_type", allow.cartesian = T, all.x = TRUE)[, univocal_name:= NULL]


  LF = rbind(LF_EU, GCAM_data$load_factor[!(iso %in% unique(LF_EU$iso) & vehicle_type %in% unique(LF_EU$vehicle_type))])

  ## set target for LF for LDVs
  target_LF = if(smartlifestyle) 1.8 else 1.7
  target_year = if(smartlifestyle) 2060 else 2080

  if(SSP_scen == "SDP_RC"){
    target_LF = 1.9
    target_year = 2060
  }

  LF[
    subsector_L1 == "trn_pass_road_LDV_4W" &
      year >= 2020 & year <= target_year,
    loadFactor := loadFactor + (year - 2020)/(target_year - 2020) * (target_LF - loadFactor)]

  LF[
    subsector_L1 == "trn_pass_road_LDV_4W" &
      year >= target_year,
    loadFactor := target_LF]
  ## LF for electric and h2 trucks/buses assumed ot be the same as liquids
  LF = rbind(LF,
             LF[subsector_L1 %in% c("trn_freight_road_tmp_subsector_L1", "Bus_tmp_subsector_L1") & technology == "Liquids"][, technology := "FCEV"],
             LF[subsector_L1 %in% c("trn_freight_road_tmp_subsector_L1", "Bus_tmp_subsector_L1") & technology == "Liquids"][, technology := "Electric"])

  ## LF for H2 aviation is the same as Liquids aviation
  LF = rbind(LF,
             LF[subsector_L3 == "Domestic Aviation" & technology == "Liquids"][, technology := "Hydrogen"])

  ## merge annual mileage
  AM_EU = approx_dt(EU_data$am_countries_EU,
                    xdata = unique(GCAM_data$load_factor$year),
                    xcol = "year",
                    ycol = "annual_mileage",
                    idxcols = c("iso", "vehicle_type"),
                    extrapolate = T)

  setnames(AM_EU, old = "annual_mileage", new = "vkm.veh")

  AM = rbind(AM_EU, UCD_output$UCD_mileage[!(iso %in% unique(AM_EU$iso) & vehicle_type %in% unique(AM_EU$vehicle_type))])
  AM = merge(AM, logit_cat, by = "vehicle_type", allow.cartesian = T, all.x = TRUE)[, univocal_name:= NULL]
  AM = AM[year >= 1990]
  eu_iso = unique(REMIND2ISO_MAPPING[region %in% c("DEU", "FRA", "UKI", "ECS", "ENC", "ESW", "EWN", "ESC", "ECE", "NEN", "NES"), iso])
  ## calculate PSI costs in terms of 2005$/pkm annualized
  PSI_c = copy(PSI_costs)
  ## define markups on alternative techs based on the percentage difference we find in EU countries
  PSI_c = rbind(PSI_c, PSI_c[year == 2040][, year := 2100])
  ## in 2100, purchase price for BEVs is 0.8*purchase price, for Hybrid Electric is 0.7, for FCEVs is 0.9
  decr = data.table(technology = c("BEV", "Hybrid Electric", "FCEV", "Liquids", "NG"), val = c(0.8, 0.7, 0.9, 1, 1))
  PSI_c = merge(PSI_c, decr, by = "technology")
  PSI_c[year == 2100, tot_purchasecost := tot_purchasecost[technology== "Liquids"]*val, by = "vehicle_type"]


  ## add "Large Car"and "Light Truck and SUV" taking the same values as for "Large Car and SUV"
  PSI_c = rbind(PSI_c,
                PSI_c[vehicle_type == "Large Car and SUV",][, vehicle_type := "Light Truck and SUV"],
                PSI_c[vehicle_type == "Large Car and SUV",][, vehicle_type := "Large Car"])

  PSI_c = approx_dt(PSI_c,
                       xdata = years,
                       xcol = "year",
                       ycol = "tot_purchasecost",
                       idxcols = c("technology", "vehicle_type"),
                       extrapolate = TRUE)

  PSI_c[, val := NULL]
  PSI_c[, markup := ifelse(technology %in% c("BEV", "Hybrid Electric", "FCEV"),
                           tot_purchasecost[technology %in% c("FCEV", "BEV", "Hybrid Electric")]/
                             tot_purchasecost[technology == "Liquids"],
                           0), by = c("year")]

  PSI_c[, c("variable", "unit") := list("Capital costs (purchase)", "2005$/veh/yr")]
  PSI_c = merge(PSI_c, unique(AM[, c("iso", "year")]), by = "year", allow.cartesian=TRUE)

  ## load the UCD based purchase costs
  UCD_c = copy(UCD_output$UCD_cost)
  ## find the purchase costs of liquid and NG technologies for non-EU countries (for EU, data comes from PSI)
  purchCost = UCD_c[!iso %in% eu_iso & variable %in% "Capital costs (purchase)" & UCD_technology %in% c("Liquids", "NG")][, c("iso", "UCD_technology", "year", "value", "vehicle_type")]
  setnames(purchCost, old="value", new="valUCD")
  setnames(purchCost, old="UCD_technology", new="technology")
  ## merge the data from UCD with the PSI values as they contain the corresponding markups for each tech/veh type
  purchCost=merge(PSI_c, purchCost, by = c("year", "technology", "iso", "vehicle_type"))
  purchCost[, tot_purchasecost := valUCD]
  purchCost[, valUCD := NULL]

  ## remove the "extra vehicle types" that have purchase cost associated but not all other costs - they are all in non_eu countries- as they are not in the original demand trends
  tokeep = merge(PSI_c, unique(purchCost[,c("iso", "vehicle_type")]), by = c("iso", "vehicle_type"), all.y=TRUE)

  ## use the PSI database for eu countries, and the filtered-out version for non-eu contries
  PSI_c =rbind(PSI_c[iso %in% eu_iso], tokeep)
  ## apply the markup to the respective technologies
  PSI_c[!iso %in% eu_iso & year >= 2015, tot_purchasecost := ifelse(technology %in% c("BEV", "FCEV", "Hybrid Electric"),
                                                                    tot_purchasecost[technology=="Liquids"]*markup,
                                                                    tot_purchasecost), by = c("iso", "year", "vehicle_type")][, markup := NULL]

  setnames(PSI_c, old ="tot_purchasecost", new = "value")
  PSI_c = merge(PSI_c, logit_cat, by = c("vehicle_type", "technology"), all.x = T)[, univocal_name := NULL]
  ## calculate UCD costs for LDVs in terms of 2005$/pkm annualized
  UCD_c = rbind(UCD_c,
                altCosts)

  logit_cat = rbind(logit_cat,
                    logit_cat[vehicle_type %in% unique(altCosts$vehicle_type) & vehicle_type != "Domestic Aviation_tmp_vehicletype"][, technology := "Electric"],
                    logit_cat[vehicle_type %in% unique(altCosts$vehicle_type) & vehicle_type != "Domestic Aviation_tmp_vehicletype"][, technology := "FCEV"],
                    logit_cat[vehicle_type == "Domestic Aviation_tmp_vehicletype"][, technology := "Hydrogen"])
  logit_cat = unique(logit_cat[, c("sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "univocal_name")])

  setnames(UCD_c, old = "UCD_technology", new = "technology")

  UCD_c = merge(UCD_c, logit_cat, by = c("vehicle_type", "technology"), all.x = T, allow.cartesian = T)[, univocal_name := NULL]

  ## CHA costs
  CHN_c = merge(CHN_trucks, logit_cat, by = c("vehicle_type", "technology"), all.x = T)[, univocal_name := NULL]
  CHN_c[, unit := "2005$/vkt"]

  other_costs = UCD_c[!(iso %in% "CHN" & vehicle_type %in% unique(CHN_c$vehicle_type) & technology %in% c("Liquids", "NG") & variable %in% unique(CHN_c$variable))]
  other_costs = other_costs[!(subsector_L1 == "trn_pass_road_LDV_4W" &
                                variable %in% unique(PSI_c$variable))]


  costs = rbind(CHN_c,
                PSI_c,
                other_costs)

  ## hybrid electric costs, other than Purchase, are assumed to be equal to BEVs
  costs = rbind(costs,
                costs[technology == "BEV" & subsector_L1 == "trn_pass_road_LDV_4W" & !variable %in% unique(PSI_c$variable)][, technology := "Hybrid Electric"])

  ## a set of EU countries do not have Midsize Cars costs other than Purchase: assumed to be equal the average vehicle costs
  eu_noMid = setdiff(unique(costs[vehicle_type == "Midsize Car" & variable == "Capital costs (purchase)", iso]), unique(costs[vehicle_type == "Midsize Car" & variable == "Operating costs (maintenance)", iso]))
  costs = rbind(costs,
                unique(costs[iso %in% eu_noMid & variable != "Capital costs (purchase)" & subsector_L1 == "trn_pass_road_LDV_4W"][, value := mean(value), by = c("year", "variable", "technology")][, vehicle_type := "Midsize Car"]))
  ## apply transport incentives to EU countries
  trsp_inc = copy(trsp_incent)
  trsp_inc = magpie2dt(trsp_inc)
  trsp_inc[, aveval := mean(value), by = c("region", "period", "variable")]
  trsp_inc = unique(trsp_inc[,c("region", "period", "variable", "aveval")])
  # exchange rate 2020: 1 euro = 1.12 dollar
  # conversion from US$2005 to EUR2020: inflation/exchange rate = 1.3504/1.12 = 1.205714286
  trsp_inc[, aveval := aveval/1.205714286] ## in 2005 USD
  setnames(trsp_inc, old = c("region", "period", "aveval", "variable"), new = c("iso", "year", "incentive_val", "technology"))
  trsp_inc[, variable := "Capital costs (purchase)"]
  costs = merge(costs, trsp_inc, by = c("iso", "technology", "variable", "year"), all.x = TRUE)
  costs[is.na(incentive_val), incentive_val := 0]

  ## year where complete phase-out incentives occurs
  year_out = 2030
  ## attribute first (to the countries that have them) the same incentives value until the phase out year
  costs[variable == "Capital costs (purchase)",  incentive_val := ifelse(year>=2020 & year<=year_out,
                                                                         incentive_val[year==2020],
                                                                 0),
        by = c("iso", "vehicle_type", "technology", "variable")]
  ## incentives are actually phasing out, so the annualized value of the incentives have to be included
  costs[variable == "Capital costs (purchase)",  value := ifelse(year>=2020 & year<=year_out,
                                                                value + incentive_val*1.14*0.78*fcr_veh*(1/nper_amort_veh*(year-2020)-1),
                                                                value),
                  by = c("iso", "vehicle_type", "technology", "variable")]
  costs[, incentive_val := NULL]
  costs = merge(costs,
                unique(AM[,c("iso", "vkm.veh", "year", "vehicle_type")]),
                all = TRUE,
                by = c("vehicle_type", "year", "iso"))

  ## for the countries that do not feature mileage for specific vehicle, assumed average mileage
  costs[subsector_L1 == "trn_pass_road_LDV_4W", vkm.veh := ifelse(is.na(vkm.veh), mean(vkm.veh, na.rm = TRUE), vkm.veh), by = c("year", "vehicle_type")]

  costs[!is.na(vkm.veh) & unit == "2005$/veh/yr", value := value/vkm.veh]
  costs[!is.na(vkm.veh) & unit == "2005$/veh/yr", unit := "2005$/vkt"]
  costs = costs[!is.na(unit)]
  costs = merge(costs,
                unique(LF[,c("iso", "loadFactor", "year", "vehicle_type", "technology")]),
                by = c("vehicle_type", "year", "iso", "technology"))

  costs[, value := value/loadFactor]

  costs[, c("vkm.veh", "unit", "loadFactor") := NULL]

  ## merge PSI, UCD and CHN costs

  ## some costs are missing from the database: integrate them (for the moment using the costs of the lower category TODO)
  costs = rbind(costs,
                costs[iso %in% unique(EU_data$dem_eurostat$iso) & vehicle_type == "Truck (0-3.5t)"][, vehicle_type := "Truck (7.5t)"],
                costs[iso %in% unique(EU_data$dem_eurostat$iso) & vehicle_type == "Truck (26t)"][, vehicle_type := "Truck (18t)"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region=="REF", iso]) & vehicle_type == "Truck (26t)"][, vehicle_type := "Truck (18t)"],      ## REF has missing 18t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region=="REF", iso]) & vehicle_type == "Truck (0-3.5t)"][, vehicle_type := "Truck (7.5t)"],  ## REF has missing 7.5t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region=="CAZ", iso]) & vehicle_type == "Truck (26t)"][, vehicle_type := "Truck (40t)"],      ## CAZ has missing 40t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("OAS", "SSA"), iso]) & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (26t)"],  ## OAS and SSAhas missing 26t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("OAS", "SSA"), iso]) & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (40t)"],  ## OAS has missing 40t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("CAZ"), iso]) & vehicle_type == "Large Car"][, vehicle_type := "Large Car and SUV"],  ## OAS has missing 40t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("OAS"), iso]) & vehicle_type == "Large Car and SUV"][, vehicle_type := "Van"],  ## OAS has missing 40t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("CAZ"), iso]) & vehicle_type == "Motorcycle (>250cc)"][, vehicle_type := "Motorcycle (50-250cc)"],  ## OAS has missing 40t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("CAZ"), iso]) & vehicle_type == "Motorcycle (>250cc)"][, vehicle_type := "Moped"],  ## OAS has missing 40t
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("CAZ"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("CAZ"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Subcompact Car"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("SSA"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Large Car"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("SSA"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Large Car and SUV"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("SSA"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Van"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("REF"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("SSA"), iso]) & vehicle_type == "Motorcycle (50-250cc)"][, vehicle_type := "Moped"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("SSA"), iso]) & vehicle_type == "Motorcycle (50-250cc)"][, vehicle_type := "Motorcycle (>250cc)"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("ENC", "NEN", "NES", "UKI"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"])

  ## extend costs data to 2150
  costs <- rbind(costs,
                costs[year==2100][, year := 2110],
                costs[year==2100][, year := 2130],
                costs[year==2100][, year := 2150])


  ## merge PSI intensity, GCAM intensity and TRACCS intensity
  LDV_PSI_i = merge(PSI_int$LDV_PSI_int, unique(LF[, c("iso", "year", "vehicle_type", "loadFactor")]), by = c("year", "vehicle_type"))

  ## convert into MJ/pkm
  LDV_PSI_i[, conv_pkm_MJ := conv_vkm_MJ/ ## MJ/vkm
              loadFactor] ## in MJ/pkm
  LDV_PSI_i[, c("loadFactor", "conv_vkm_MJ") := NULL] ##  remove load factor
  LDV_PSI_i = approx_dt(LDV_PSI_i,
                        xdata = unique(years),
                        xcol = "year",
                        ycol = "conv_pkm_MJ",
                        idxcols = c("technology", "vehicle_type", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "sector_fuel", "iso"),
                        extrapolate = TRUE)

  ## rescale the energy intensity according to the single countries for ICEs
  LDV_PSI_i_rescale = merge(LDV_PSI_i[technology %in% c("Liquids", "NG") &iso %in% unique(EU_data$energy_intensity_EU$iso) &
                                        vehicle_type %in% c("Large Car and SUV", "Compact Car", "Subcompact Car","Midsize Car"), c("vehicle_type", "year", "technology", "iso",  "conv_pkm_MJ", "sector_fuel")],
                            EU_data$energy_intensity_EU[year %in% c(2005, 2010) & vehicle_type %in% c("Large Car and SUV", "Compact Car", "Subcompact Car","Midsize Car")], by = c("vehicle_type", "year", "technology", "iso", "sector_fuel"), all = T)

  LDV_PSI_i_rescale[, ratio := conv_pkm_MJ/conv_pkm_MJ[year == 2010], by = c("iso", "technology", "vehicle_type")]
  LDV_PSI_i_rescale[year > 1990 & year <= 2010, conv_pkm_MJ := MJ_km]

  LDV_PSI_i_rescale[, conv_pkm_MJ := ifelse(year > 2010, conv_pkm_MJ[year == 2010]*ratio, conv_pkm_MJ), by = c("iso", "technology", "vehicle_type")]

  LDV_PSI_i_rescale = merge(LDV_PSI_i_rescale, logit_cat, all.x = TRUE, by = c("vehicle_type", "technology"))
  LDV_PSI_i_rescale[, c("ratio", "MJ_km", "univocal_name"):= NULL]
  ## merge the LDVs

  int_GCAM = GCAM_data[["conv_pkm_mj"]][!(subsector_L1=="trn_pass_road_LDV_4W" &
                                            technology %in% c("BEV", "FCEV", "Hybrid Electric")) &
                                          !(iso %in% eu_iso)]
  int_GCAM = rbind(int_GCAM,
                   int_GCAM[year==2100][, year := 2110],
                   int_GCAM[year==2100][, year := 2130],
                   int_GCAM[year==2100][, year := 2150])

  LDV_PSI_i = rbind(LDV_PSI_i[iso %in% eu_iso & technology %in% c("BEV", "FCEV", "Hybrid Electric")],
                    LDV_PSI_i_rescale,
                    LDV_PSI_i[!iso %in% eu_iso & technology %in% c("BEV", "FCEV", "Hybrid Electric")],
                    int_GCAM)

  LDV_PSI_i[, sector_fuel := NULL]



  Truck_PSI_i = merge(PSI_int$Truck_PSI_int, LF[year %in% unique(PSI_int$Truck_PSI_int$year)], by = c("year", "vehicle_type", "technology"), all.x = T)
  Truck_PSI_i[, conv_pkm_MJ := conv_vkm_MJ/loadFactor]
  Truck_PSI_i = Truck_PSI_i[!is.na(conv_pkm_MJ)]
  Truck_PSI_i[, c("loadFactor", "conv_vkm_MJ") := NULL]
  ## approx to the whole time range
  Truck_PSI_i = approx_dt(Truck_PSI_i,
                          xdata = years,
                          xcol = "year",
                          ycol = "conv_pkm_MJ",
                          idxcols = c("iso", "technology", "vehicle_type", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                          extrapolate = TRUE)

  ## missing Trucks for EU countries are averaged on the other countries (e.g. Truck (18t) is present in terms of demand, but only the alternative trucks intensity is provided)
  aveMissingTruck = GCAM_data[["conv_pkm_mj"]][subsector_L3 == "trn_freight_road" & vehicle_type %in% c("Truck (18t)","Truck (7.5t)") & technology %in% c("Liquids", "NG")]
  aveMissingTruck = aveMissingTruck[,.(conv_pkm_MJ = mean(conv_pkm_MJ)), by = c("year", "technology", "subsector_L1", "subsector_L2",
                                                                                "subsector_L3", "sector", "vehicle_type")]
  ## approx to the whole time range
  aveMissingTruck = approx_dt(aveMissingTruck,
                              xdata = unique(Truck_PSI_i$year),
                              xcol = "year",
                              ycol = "conv_pkm_MJ",
                              idxcols = c("technology", "vehicle_type", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                              extrapolate = TRUE)

  aveMissingTruck = merge(aveMissingTruck[, k := 1], data.table(iso = eu_iso, k = 1), all = TRUE, allow.cartesian=TRUE, by = c("k"))[, k := NULL]
  Truck_PSI_i = rbind(
    Truck_PSI_i,
    GCAM_data[["conv_pkm_mj"]][(subsector_L3 == "trn_freight_road"|subsector_L2 == "Bus") &
                                 technology %in% c("Liquids", "NG"),
                               c("conv_pkm_MJ", "iso","year", "technology", "subsector_L1", "subsector_L2",
                                 "subsector_L3", "sector", "vehicle_type")])

  Truck_PSI_i = rbind(Truck_PSI_i,
                      aveMissingTruck)

  ##merge with GCAM intensities and substitute all LDVs for EU and only alternative LDVs for other regions
  int_GCAM = GCAM_data[["conv_pkm_mj"]][!subsector_L1 %in% c("trn_pass_road_LDV_4W", "trn_freight_road_tmp_subsector_L1","Bus_tmp_subsector_L1")][, sector_fuel := NULL]
  int_GCAM = rbind(int_GCAM,
                   int_GCAM[year==2100][, year := 2110],
                   int_GCAM[year==2100][, year := 2130],
                   int_GCAM[year==2100][, year := 2150])

  int = rbind(Truck_PSI_i,
              LDV_PSI_i,
              int_GCAM)


  ## include hydrogen airplanes intensity
  ## based on "A review on potential use of hydrogen in aviation applications", Dincer, 2016: the energy intensity of a hydrogen airplane is around 1MJ/pkm. The range of energy intensity of a fossil-based airplane is here around 3-2 MJ/pkm->a factor of 0.5 is assumed
  int = rbind(int, int[subsector_L3 %in% c("Domestic Aviation"),][,c("conv_pkm_MJ", "technology") := list(0.5*conv_pkm_MJ, "Hydrogen")])

  dem = copy(GCAM_data$tech_output)
  ## include 0 demand for electric+FCEV buses and trucks, H2 airplanes
  dem = rbind(dem,
              dem[subsector_L3 %in% c("Domestic Aviation"),][, c("technology", "tech_output") := list("Hydrogen", 0)],
              dem[subsector_L1 %in% c("trn_freight_road_tmp_subsector_L1", "Bus_tmp_subsector_L1") & technology == "Liquids"][, c("technology", "tech_output") := list("Electric", 0)],
              dem[subsector_L1 %in% c("trn_freight_road_tmp_subsector_L1", "Bus_tmp_subsector_L1") & technology == "Liquids"][, c("technology", "tech_output") := list("FCEV", 0)],
              dem[subsector_L1 %in% c("trn_pass_road_LDV_4W") & technology == "Liquids"][, c("technology", "tech_output") := list("Hybrid Electric", 0)])

  demNM = dem[vehicle_type %in% c("Walk_tmp_vehicletype", "Cycle_tmp_vehicletype")]
  ## substitute the EU based demand
  demBunk = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("International Aviation_tmp_vehicletype", "International Ship_tmp_vehicletype"),iso])) &
                  vehicle_type %in% c("International Aviation_tmp_vehicletype", "International Ship_tmp_vehicletype")]  ## International aviation and shipping come from Eurostat -> not the same ISO as in TRACCS are provided
  demRail = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("Freight Rail_tmp_vehicletype", "Passenger Rail_tmp_vehicletype"),iso])) &
                  vehicle_type %in% c("Freight Rail_tmp_vehicletype", "Passenger Rail_tmp_vehicletype")]  ## Rail come from Eurostat -> not the same ISO as in TRACCS are provided
  demDomAv = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("Domestic Aviation_tmp_vehicletype"),iso])) &
                   vehicle_type %in% c("Domestic Aviation_tmp_vehicletype")]  ## Domestic aviation come from Eurostat -> not the same ISO as in TRACCS are provided
  demDomSh = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("Domestic Ship_tmp_vehicletype"),iso])) &
                   vehicle_type %in% c("Domestic Ship_tmp_vehicletype")]  ## Domestic shipping comes from Eurostat -> not the same ISO as in TRACCS are provided
  demRoad = dem[!(iso %in% unique(EU_data$dem_eurostat$iso) & subsector_L3 %in% c("trn_pass_road", "trn_freight_road")) &
                  subsector_L3 %in% c("trn_pass_road", "trn_freight_road")]
  ## demand for HSR has to be included separately for all ISO
  demHSR = dem[subsector_L3 %in% "HSR"]

  demEU = merge(EU_data$dem_eurostat, int, by = c("vehicle_type", "technology", "iso", "year"))
  demEU[, tech_output := MJ/  ## in MJ
          conv_pkm_MJ*    ## in km
          1e-6][, c("MJ", "conv_pkm_MJ") := NULL] ## in million km

  dem = rbind(demBunk,
              demRail,
              demDomAv,
              demDomSh,
              demRoad,
              demHSR,
              demEU,
              demEU[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][, c("tech_output", "technology"):= list(0, "BEV")],
              demEU[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][, c("tech_output", "technology"):= list(0, "FCEV")],
              demEU[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][, c("tech_output", "technology"):= list(0, "Hybrid Electric")],
              demNM)

  ## temporary data tables to allow for covering missing categories, iso and years
  tmp1 = CJ(vehicle_type = unique(dem$vehicle_type), iso = unique(dem$iso))
  tmp2 = unique(dem[,c("vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector")])
  tmp3 = CJ(vehicle_type = unique(dem$vehicle_type),year = years)
  tmp4 = merge(tmp1, tmp2, by = "vehicle_type", allow.cartesian = T)
  tmp4 = merge(tmp3, tmp4, by = "vehicle_type", allow.cartesian = T)

  ## fill in missing categories and add a 0
  dem = merge(dem, tmp4, all = TRUE, by = c("vehicle_type", "technology", "iso", "year", "subsector_L1", "subsector_L2", "subsector_L3", "sector"))
  dem[is.na(tech_output), tech_output := 0]

  ## fill in missing categories for int and LF
  int = merge(int, unique(dem[!vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year")]), all.y = TRUE, by = c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year"))
  int[, conv_pkm_MJ := ifelse(is.na(conv_pkm_MJ), mean(conv_pkm_MJ, na.rm = TRUE), conv_pkm_MJ), by = c("year", "technology", "vehicle_type")]

  LF = merge(LF, unique(dem[!vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year")]), all.y = TRUE, by = c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year"))
  LF[, loadFactor := ifelse(is.na(loadFactor), mean(loadFactor, na.rm = TRUE), loadFactor), by = c("year", "vehicle_type")]
  LF[year > 2100, loadFactor := rep(LF[year == 2100]$loadFactor, 3)]
  if(nrow(LF[is.na(loadFactor) | loadFactor == 0]) > 0){
    stop("Zero load factor provided.")
  }


  AM = merge(AM, unique(dem[unique(AM$vehicle_type),c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year")]), all.y = TRUE, by = c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year"))
  AM[, vkm.veh := ifelse(is.na(vkm.veh), mean(vkm.veh, na.rm = TRUE), vkm.veh), by = c("year", "technology", "vehicle_type")]
  AM=AM[!is.nan(vkm.veh)]

  dem = dem[year <= 2010]

  ## add some base demand for Cycle & Walk (2%)
  dem[, demldv := sum(tech_output), by=c("year", "iso")]
  dem[subsector_L3 == "Cycle" & tech_output == 0 & iso %in% c("USA", "AUS", "CAN"), tech_output := demldv*0.006]
  dem[subsector_L3 == "Cycle" & tech_output == 0 & iso %in% c("IND", "CHN"), tech_output := demldv*0.02]
  dem[subsector_L3 == "Cycle" & tech_output == 0, tech_output := demldv*0.01]
  dem[subsector_L3 == "Walk" & tech_output == 0, tech_output := demldv*0.002]
  ## dem[subsector_L3 == "Cycle" & , tech_output := demldv*0.02]
  ## dem[subsector_L3 == "Walk" & tech_output == 0, tech_output := demldv*0.002]
  dem[, demldv := NULL]

  ## from https://www.iea.org/reports/tracking-rail-2020-2
  dem[year <= 2010 & iso == "CHN" & subsector_L3 == "HSR", tech_output := 70000]
  ## from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  ## total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dem[year <= 2010 & iso == "CHN" & subsector_L3 == "trn_freight_road", tech_output := tech_output * 3]
  ## missing cost estimates for heavy trucks (use lighter trucks estimates)
  costs <- rbindlist(list(
    costs,
    costs[iso == "CHN" & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (26t)"],
    costs[iso == "CHN" & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (40t)"],
    costs[iso == "USA" & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (26t)"],
    costs[iso == "USA" & vehicle_type == "Truck (18t)"][, vehicle_type := "Truck (40t)"]
  ))

  return(list(costs = costs, int = int, LF = LF, AM = AM, dem = dem))


}
