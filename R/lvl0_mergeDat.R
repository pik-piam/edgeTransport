#' Merge PSI Energy Intensity for EU with GCAM Data
#'
#' Final values: MJ/km (pkm and tkm)
#'
#' @param GCAM_data GCAM based data
#' @param load_factor TRACCS based load factors for EU-15
#' @param GCAM2ISO_MAPPING the mapping of GCAM regions to ISO countries
#' @param input_folder folder hosting raw data
#' @param PSI_dir  directory with PSI data
#' @param enhancedtech switch activating optimistic development of alternative technologies
#' @param techswitch technology at the center of the policy packages
#' @importFrom rmndt approx_dt
#' @importFrom readxl read_excel


lvl0_mergePSIintensity <- function(GCAM_data, load_factor, GCAM2ISO_MAPPING,
                                   input_folder, PSI_dir="PSI",
                                   enhancedtech, techswitch){
  
  subsector_L1 <- technology <- univocal_name <- iso <- vehicle_type <- loadFactor <- region <- tot_purchasecost <- NULL
  powertrain <- kJ.per.vkm <- i.loadFactor <- ttw_energy <- subsector_L2 <- subsector_L3 <- conv_pkm_MJ <- `.` <- sector_fuel <- NULL

  psi_file <- function(fname){
    file.path(input_folder, PSI_dir, fname)
  }
  
  ##load vehicle intensity from GCAM
  conv_pkm_mj=GCAM_data[["conv_pkm_mj"]]
  #load the logit mapping so that I can attribute the full logit tree to each level
  logit_category=GCAM_data[["logit_category"]]
  
  ## alternative trucks intensity
  LDV_PSI_int = data.table(read.csv(psi_file("ttw-efficiencies.csv")))
  ## rename entries according to the rest of the databases
  setnames(LDV_PSI_int, old = c("size"), new = c("vehicle_type_PSI"))
  ##load mapping that matches PSI vehicle types with EDGE types
  mapping=fread(psi_file("mapping_PSI_EDGE.csv"), na.strings=c("","NA"))
  ##filter out the NA cells in the mapping-> PSI has more alternatives than EDGE
  mapping=mapping[!is.na(vehicle_type),]
  ## merge mapping and PSI database
  LDV_PSI_int=merge(LDV_PSI_int,mapping,all=FALSE,by="vehicle_type_PSI")[,-"vehicle_type_PSI"]
  
  LDV_PSI_int[,technology:=ifelse(powertrain=="ICEV-g","NG",NA)]
  LDV_PSI_int[,technology:=ifelse(powertrain %in% c("ICEV-p","ICEV-d"),"Liquids",technology)]
  LDV_PSI_int[,technology:=ifelse(grepl("PHEV",powertrain),"Hybrid Electric",technology)]
  LDV_PSI_int[,technology:=ifelse(powertrain%in%c("FCEV"),"FCEV",technology)]
  LDV_PSI_int[,technology:=ifelse(powertrain%in%c("BEV"),"BEV",technology)]
  LDV_PSI_int[,sector_fuel:=ifelse(technology=="BEV","elect_td_trn",NA)]
  LDV_PSI_int[,sector_fuel:=ifelse(technology=="FCEV","H2 enduse",sector_fuel)]
  LDV_PSI_int[,sector_fuel:=ifelse(technology=="NG","delivered gas",sector_fuel)]
  LDV_PSI_int[,sector_fuel:=ifelse(technology=="Hybrid Electric","Liquids-Electricity",sector_fuel)]
  LDV_PSI_int[,sector_fuel:=ifelse(is.na(sector_fuel),"refined liquids enduse",sector_fuel)]
  ## remove mild/full hybrids
  LDV_PSI_int = LDV_PSI_int[!powertrain %in% c("HEV-d", "HEV-p")]
  ## average on the EDGE category
  LDV_PSI_int = LDV_PSI_int[, .(kJ.per.vkm=mean(kJ.per.vkm)), by = c("technology","vehicle_type","year","sector_fuel")]
  
  LDV_PSI_int[, conv_pkm_MJ := kJ.per.vkm*  ## kj/vkm
                1e-3] ## MJ/vkm]
  
  LDV_PSI_int[, c("kJ.per.vkm") := NULL]
  ## approx to the whole time range
  LDV_PSI_int = approx_dt(LDV_PSI_int,
                          xdata = unique(conv_pkm_mj$year),
                          xcol = "year",
                          ycol = "conv_pkm_MJ",
                          idxcols = c("technology", "vehicle_type", "sector_fuel"),
                          extrapolate = TRUE)
  
  tsteps=unique(conv_pkm_mj$year)
  
  ## add logit_category for Hybrid Electric
  logit_category = rbind(logit_category, logit_category[technology == "BEV"][, technology := "Hybrid Electric"])
  
  ##merge with logit_category
  LDV_PSI_int=merge(logit_category,LDV_PSI_int,by=c("technology","vehicle_type"))[,-"univocal_name"]
  ## add missing 4Ws
  LDV_PSI_int = rbind(LDV_PSI_int,
                      LDV_PSI_int[vehicle_type == "Large Car and SUV"][, vehicle_type := "Large Car"],
                      LDV_PSI_int[vehicle_type == "Large Car and SUV"][, vehicle_type := "Light Truck and SUV"]
  )
  
  LDV_PSI_int = merge(LDV_PSI_int, unique(conv_pkm_mj[, c("iso", "year")]), by = "year", allow.cartesian = TRUE)
  LDV_PSI_int = merge(LDV_PSI_int, unique(conv_pkm_mj[, c("iso", "vehicle_type")]), by = c("iso", "vehicle_type"), all = FALSE)
  eu_15 <- GCAM2ISO_MAPPING[region == "EU-15", iso]
  LDV_PSI_int = rbind(LDV_PSI_int[iso %in% eu_15],
                      LDV_PSI_int[!(iso %in% eu_15) &
                                    technology %in% c("BEV", "FCEV", "Hybrid Electric") &
                                    subsector_L1 == "trn_pass_road_LDV_4W"])
  
  ## GCAM load factors for the resto of the world
  LDV_PSI_int = merge(LDV_PSI_int, GCAM_data$load_factor, by = c("iso", "year", "vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"))
  
  
  lf2010 = load_factor[year == 2010 & iso %in% eu_15][, year := NULL]
  
  LDV_PSI_int[lf2010, loadFactor := i.loadFactor, on=c("iso", "vehicle_type")]
  
  ## convert into MJ/pkm
  LDV_PSI_int[, conv_pkm_MJ := conv_pkm_MJ/loadFactor] ## in MJ/pkm
  LDV_PSI_int[, loadFactor := NULL] ##  remove load factor
  
  
  ## alternative trucks intensity
  Truck_PSI_int = data.table(read.csv(psi_file("Regional delivery_truck_efficiencies.csv")))
  ## use only Electric and FCEV trucks
  Truck_PSI_int = Truck_PSI_int[powertrain %in% c("BEV", "FCEV"),]
  ## rename entries according to the rest of the databases
  setnames(Truck_PSI_int, old = c("powertrain", "size"), new = c("technology", "vehicle_type"))
  
  Truck_PSI_int[technology == "BEV", technology := "Electric"]
  Truck_PSI_int[vehicle_type == "3.5t", vehicle_type := "Truck (0-3.5t)"]
  Truck_PSI_int[vehicle_type != "Truck (0-3.5t)", vehicle_type := paste0("Truck (", vehicle_type, ")")]
  Truck_PSI_int[, conv_pkm_MJ := ttw_energy*  ## kj/km
                  1e-3] ## MJ/km]
  Truck_PSI_int[, c("X", "unit", "ttw_energy") := NULL]
  ## Buses are assumed to be as 18 tons truck
  Bus_PSI_int = Truck_PSI_int[vehicle_type == "Truck (18t)"][, vehicle_type := "Bus_tmp_vehicletype"]
  Truck_PSI_int = rbind(Truck_PSI_int, Bus_PSI_int)
  Truck_PSI_int = merge(Truck_PSI_int, lf2010, by = "vehicle_type", allow.cartesian = T)
  Truck_PSI_int[, conv_pkm_MJ := conv_pkm_MJ/loadFactor]
  Truck_PSI_int[, c("loadFactor", "region") := NULL]
  
  ## approx to the whole time range
  Truck_PSI_int = approx_dt(Truck_PSI_int,
                            xdata = unique(conv_pkm_mj$year),
                            xcol = "year",
                            ycol = "conv_pkm_MJ",
                            idxcols = c("technology", "vehicle_type"),
                            extrapolate = TRUE)
  
  
  Truck_PSI_int = merge(
    Truck_PSI_int,
    unique(conv_pkm_mj[(subsector_L3 == "trn_freight_road"|subsector_L2 == "Bus") &
                         technology %in% c("Liquids"),
                       c("iso","year", "subsector_L1", "subsector_L2",
                         "subsector_L3", "sector", "vehicle_type")]),
    by = c("year", "vehicle_type", "iso"), allow.cartesian=TRUE)
  Truck_PSI_int[, sector_fuel := ifelse(technology == "FCEV","H2 enduse", "elect_td_trn")]
  
  
  ##merge with GCAM intensities and substitute all LDVs for EU and only alternative LDVs for other regions
  conv_pkm_mj_rest = conv_pkm_mj[!(subsector_L1=="trn_pass_road_LDV_4W" &
                                     technology %in% c("BEV", "FCEV", "Hybrid Electric")) &
                                   !(iso %in% eu_15)]
  conv_pkm_mj_rest= rbind(conv_pkm_mj_rest, Truck_PSI_int)
  conv_pkm_mj_EU = conv_pkm_mj[!(subsector_L1=="trn_pass_road_LDV_4W") &
                                 iso %in% eu_15]
  
  conv_pkm_mj=rbind(conv_pkm_mj_rest, conv_pkm_mj_EU, LDV_PSI_int)
  
  return(conv_pkm_mj)
}


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
#' @param smartlifestyle switch activatinf sustainable lifestyles
#' @param REMIND2ISO_MAPPING REMIND regional mapping
#' @param years time steps
#' @return
#' @author Marianna Rottoli, Alois Dirnaichner

lvl0_mergeDat = function(UCD_output, EU_data, PSI_costs, altCosts, CHN_trucks, GCAM_data, PSI_int, smartlifestyle, years, REMIND2ISO_MAPPING){
  vkm.veh <- value <- variable <- conv_pkm_MJ <- conv_vkm_MJ <- ratio <- MJ_km <- sector_fuel <- subsector_L3 <- `.` <- NULL
  k <- subsector_L2 <- tech_output <- MJ <- region <- loadFactor <- vehicle_type <- iso <- univocal_name <- technology <- NULL
  subsector_L1 <- vkm.veh <- tot_purchasecost <- NULL
  
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
  PSI_c = merge(PSI_costs, AM, all.x = TRUE, by = c("vehicle_type", "technology", "year"))
  PSI_c[, c("variable", "value") := list("Capital costs (purchase)", tot_purchasecost/vkm.veh)][, vkm.veh := NULL]
  PSI_c = rbind(PSI_c[iso %in% eu_iso], PSI_c[!iso %in% eu_iso & technology %in% c("BEV", "FCEV", "Hybrid Electric")])
  PSI_c[, tot_purchasecost := NULL]
  ## calculate UCD costs for LDVs in terms of 2005$/pkm annualized
  UCD_c = copy(UCD_output$UCD_cost)
  UCD_c = rbind(UCD_c,
                altCosts)
  
  logit_cat = rbind(logit_cat,
                    logit_cat[vehicle_type %in% unique(altCosts$vehicle_type) & vehicle_type != "Domestic Aviation_tmp_vehicletype"][, technology := "Electric"],
                    logit_cat[vehicle_type %in% unique(altCosts$vehicle_type) & vehicle_type != "Domestic Aviation_tmp_vehicletype"][, technology := "FCEV"],
                    logit_cat[vehicle_type == "Domestic Aviation_tmp_vehicletype"][, technology := "Hydrogen"])
  logit_cat = unique(logit_cat[, c("sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "univocal_name")])
  
  setnames(UCD_c, old = "UCD_technology", new = "technology")
  UCD_c = merge(UCD_c,
                unique(AM[,c("iso", "vkm.veh", "year", "vehicle_type")]),
                all = TRUE,
                by = c("vehicle_type", "year", "iso"))
  UCD_c[!is.na(vkm.veh) & unit == "2005$/veh/yr", value := value/vkm.veh]
  UCD_c[!is.na(vkm.veh) & unit == "2005$/veh/yr", unit := "2005$/vkt"]
  UCD_c = UCD_c[!is.na(unit)]
  UCD_c[, c("vkm.veh", "unit") := NULL]
  UCD_c = merge(UCD_c, logit_cat, by = c("vehicle_type", "technology"), all.x = T, allow.cartesian = T)[, univocal_name := NULL]
  
  ## CHA costs
  CHN_c = merge(CHN_trucks, logit_cat, by = c("vehicle_type", "technology"), all.x = T)[, univocal_name := NULL]
  
  ## merge PSI, UCD and CHN costs
  UCD_c = rbind(UCD_c,
                UCD_c[technology == "BEV" & subsector_L1 == "trn_pass_road_LDV_4W" & !variable %in% unique(PSI_c$variable)][, technology := "Hybrid Electric"])
  UCD_c = UCD_c[!(technology %in% c("BEV", "FCEV", "Hybrid Electric") & subsector_L1 == "trn_pass_road_LDV_4W" & variable %in% unique(PSI_c$variable))]
  UCD_c = UCD_c[!(iso %in% eu_iso & subsector_L1 == "trn_pass_road_LDV_4W" & variable %in% unique(PSI_c$variable))]
  UCD_c = UCD_c[!(iso %in% "CHN" & subsector_L1 == "trn_freight_road_tmp_subsector_L1" & variable %in% unique(CHN_c$variable))]
  
  costs = rbind(UCD_c,
                PSI_c,
                CHN_c)
  
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
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("NEN", "NES"), iso]) & vehicle_type == "Large Car and SUV" & technology %in% c("Liquids", "NG")][, vehicle_type := "Midsize Car"],
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("ENC", "NEN", "NES", "UKI"), iso]) & vehicle_type == "Compact Car"][, vehicle_type := "Mini Car"])  ## OAS has missing 40t
  
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
  # browser()
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
  
  ## substitute the EU based demand
  demNM = dem[iso %in% unique(EU_data$dem_eurostat$iso) & vehicle_type %in% c("Walk_tmp_vehicletype", "Cycle_tmp_vehicletype")]
  demBunk = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("International Aviation_tmp_vehicletype", "International Ship_tmp_vehicletype"),iso])) &
                  vehicle_type %in% c("International Aviation_tmp_vehicletype", "International Ship_tmp_vehicletype")]  ## intenrantional aviation and shipping come from Eurostat -> not the same ISO as in TRACCS are provided
  demRail = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("Freight Rail_tmp_vehicletype", "Passenger Rail_tmp_vehicletype"),iso])) &
                  vehicle_type %in% c("Freight Rail_tmp_vehicletype", "Passenger Rail_tmp_vehicletype")]  ## intenrantional aviation and shipping come from Eurostat -> not the same ISO as in TRACCS are provided
  demDomAv = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("Domestic Aviation_tmp_vehicletype"),iso])) &
                   vehicle_type %in% c("Domestic Aviation_tmp_vehicletype")]  ## intenrantional aviation and shipping come from Eurostat -> not the same ISO as in TRACCS are provided
  demDomSh = dem[!(iso %in% unique(EU_data$dem_eurostat[vehicle_type %in% c("Domestic Ship_tmp_vehicletype"),iso])) &
                   vehicle_type %in% c("Domestic Ship_tmp_vehicletype")]  ## intenrantional aviation and shipping come from Eurostat -> not the same ISO as in TRACCS are provided
  demRoad = dem[!(iso %in% unique(EU_data$dem_eurostat$iso) & subsector_L3 %in% c("trn_pass_road", "trn_freight_road")) &
                  subsector_L3 %in% c("trn_pass_road", "trn_freight_road")]
  
  
  demEU = merge(EU_data$dem_eurostat, int, by = c("vehicle_type", "technology", "iso", "year"))
  demEU[, tech_output := MJ/  ## in MJ
          conv_pkm_MJ*    ## in km
          1e-6][, c("MJ", "conv_pkm_MJ") := NULL] ## in million km
  dem = rbind(demBunk,
              demRail,
              demDomAv,
              demDomSh,
              demRoad,
              demEU,
              demEU[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][, c("tech_output", "technology"):= list(0, "BEV")],
              demEU[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][, c("tech_output", "technology"):= list(0, "FCEV")],
              demEU[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][, c("tech_output", "technology"):= list(0, "Hybrid Electric")],
              demNM)
  bb = CJ(vehicle_type = unique(dem$vehicle_type), iso = unique(dem$iso))
  cc = unique(dem[,c("vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector")])
  ee = CJ(vehicle_type = unique(dem$vehicle_type),year = years)
  dd=merge(bb, cc, by = "vehicle_type", allow.cartesian = T)
  ff = merge(ee, dd, by = "vehicle_type", allow.cartesian = T)
  
  ## fill in missing categories and add a 0
  dem = merge(dem, ff, all = TRUE, by = c("vehicle_type", "technology", "iso", "year", "subsector_L1", "subsector_L2", "subsector_L3", "sector"))
  dem[is.na(tech_output), tech_output := 0]
  
  ## fill in missing categories for int and LF
  int = merge(int, unique(dem[!vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year")]), all.y = TRUE, by = c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year"))
  int[, conv_pkm_MJ := ifelse(is.na(conv_pkm_MJ), mean(conv_pkm_MJ, na.rm = TRUE), conv_pkm_MJ), by = c("year", "technology", "vehicle_type")]
  
  LF = merge(LF, unique(dem[!vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year")]), all.y = TRUE, by = c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year"))
  LF[, loadFactor := ifelse(is.na(loadFactor), mean(loadFactor, na.rm = TRUE), loadFactor), by = c("year", "technology", "vehicle_type")]
  
  AM = merge(AM, unique(dem[unique(AM$vehicle_type),c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year")]), all.y = TRUE, by = c("iso", "vehicle_type", "technology", "subsector_L1", "subsector_L2", "subsector_L3", "sector",  "year"))
  AM[, vkm.veh := ifelse(is.na(vkm.veh), mean(vkm.veh, na.rm = TRUE), vkm.veh), by = c("year", "technology", "vehicle_type")]
  AM=AM[!is.nan(vkm.veh)]
  
  dem = dem[year <= 2010]
  
  return(list(costs = costs, int = int, LF = LF, AM = AM, dem = dem))
  
  
}
