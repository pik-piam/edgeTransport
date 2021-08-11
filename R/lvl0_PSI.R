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
  vehicle_type_PSI <- loadFactor <- scenario <- `.` <- technology <- y2040 <- y2015 <- y2100 <- variable <- region <- EDGE_category <- sector_fuel <- value <- subsector_L1 <- iso <- subsector_L2 <- subsector_L3 <- intensity_KJ_km <- vehicle_type <- i.loadFactor <- NULL
  powertrain <- conv_pkm_MJ <- ttw_energy <- kJ.per.vkm <- NULL
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
  # browser()
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
