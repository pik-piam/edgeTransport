#' Merge PSI Energy Intensity for EU with GCAM Data
#'
#' Final values: MJ/km (pkm and tkm)
#'
#' @param GCAM_data GCAM based data
#' @param input_folder folder hosting raw data
#' @param PSI_dir  directory with PSI data
#' @param enhancedtech switch activating optimistic development of alternative technologies
#' @param techswitch technology at the center of the policy packages
#' @importFrom rmndt approx_dt
#' @importFrom readxl read_excel


lvl0_mergePSIintensity <- function(GCAM_data, input_folder, PSI_dir="PSI", enhancedtech, techswitch){
  vehicle_type_PSI <- scenario <- `.` <- technology <- y2040 <- y2015 <- y2100 <- variable <- region <- EDGE_category <- sector_fuel <- value <- subsector_L1 <- iso <- subsector_L2 <- subsector_L3 <- intensity_KJ_km <- vehicle_type <- NULL
  powertrain <- conv_pkm_MJ <- ttw_energy <- NULL
  psi_file <- function(fname){
    file.path(input_folder, PSI_dir, fname)
  }

  ##load vehicle intensity from GCAM
  conv_pkm_mj=GCAM_data[["conv_pkm_mj"]]
  #load the logit mapping so that I can attribute the full logit tree to each level
  logit_category=GCAM_data[["logit_category"]]

  ##load vehicle intensity from PSI
  LDV_PSI_int=data.table(read_excel(path=psi_file("Car model result_modified.xlsx"), ##file is modified with respect to the original one: names are copy pasted to simplify R data wrangling
                                      sheet="Vehicle","A1:M191"))

  ## select relevant columns and only the "current" values->2015 and the Baseline ones->2040 in a baseline scenario
  LDV_PSI_int=LDV_PSI_int[scenario %in% c("current","Baseline"),c("scenario","technology","vehicle_type_PSI","Tank to wheel energy_(kJ / km)")]
  ## rename columns
  colnames(LDV_PSI_int)=c("scenario","technology","vehicle_type_PSI","intensity_KJ_km")
  ## substitute the "scenario" with the year it stands for
  LDV_PSI_int[,scenario:=ifelse(scenario=="current",2015,2040)]

  LDV_PSI_int=LDV_PSI_int[,.(year=scenario, ## rename col scenario with year
                                 technology,vehicle_type_PSI,
                                 intensity_MJ_km=as.numeric(intensity_KJ_km) ## in KJ/km
                                   *0.001)]                                  ## in MJ/km

  ##load mapping that matches PSI vehicle types with EDGE types
  mapping=fread(psi_file("mapping_PSI_EDGE.csv"), na.strings=c("","NA"))
  ##filter out the NA cells in the mapping-> PSI has more alternatives than EDGE
  mapping=mapping[!is.na(vehicle_type),]
  ## merge mapping and PSI database
  LDV_PSI_int=merge(LDV_PSI_int,mapping,all=FALSE,by="vehicle_type_PSI")[,-"vehicle_type_PSI"]
  ##cast
  LDV_PSI_int=dcast(LDV_PSI_int, technology + vehicle_type ~ year, value.var = "intensity_MJ_km")
  setnames(LDV_PSI_int,old=c("2015","2040"),new=c("y2015","y2040"))
  LDV_PSI_int = LDV_PSI_int[!(technology %in% c("PHEV-c", "PHEV-e")),] ## PSI reports separately the electric and ICE running modes of a plug-in hybrid

  ## in the conservative scenario, conservative assumptions about costs of alternative technologies
  if (!(enhancedtech)) {
    LDV_PSI_int[technology %in% c("BEV", "FCEV", "HEV-p", "PHEV"), y2040 := y2040 + 0.5*(y2015-y2040), by = "technology"]
  }

  ## give alternative pathways depending on the scenario
  if (enhancedtech & techswitch == "FCEV") {
    LDV_PSI_int[technology %in% c("BEV", "HEV-p", "PHEV"), y2040 := (y2015 + y2040)/2, by = "technology"] ## value in 2040 is halfway the loaded value in 2040 and 2015
  }

  LDV_PSI_int[,y2100:=0.1*(y2040-y2015)/(2040-2015)*(2100-2040)+y2040] ## set a value for 2100 that is not present in PSI database. It's set to 1/10 of the decrease between 2015 and 2040 (quite conservative)

  tsteps=unique(conv_pkm_mj$year)

  for (t in tsteps[tsteps<2040 & tsteps > 2015]) {
    LDV_PSI_int[,paste0("y",t):=(y2015-y2040)*(1-(t-2015)/(2040-2015))+y2040]
  }

  for (t in tsteps[tsteps>2040 & tsteps <2100]) {
    LDV_PSI_int[,paste0("y",t):=(y2040-y2100)*(1-(t-2040)/(2100-2040))+y2100]
  }

  for (t in tsteps[tsteps<2015]){
    LDV_PSI_int[,paste0("y",t):=y2015]
  }

  for (t in tsteps[tsteps>2100]){
    LDV_PSI_int[,paste0("y",t):=y2100]
  }

  LDV_PSI_int = melt(LDV_PSI_int, id.vars = c("technology", "vehicle_type"),
                       measure.vars = paste0("y",tsteps))

  LDV_PSI_int[,variable:=as.numeric(gsub("y","",variable))]

  setnames(LDV_PSI_int,old="variable",new="year")
  LDV_PSI_int[,EDGE_category:=ifelse(technology=="ICEV-g","NG",NA)]
  LDV_PSI_int[,EDGE_category:=ifelse(technology %in% c("ICEV-p","ICEV-d"),"Liquids",EDGE_category)]
  LDV_PSI_int[,EDGE_category:=ifelse(grepl("PHEV",technology),"Hybrid Electric",EDGE_category)]
  LDV_PSI_int[,EDGE_category:=ifelse(is.na(EDGE_category),technology,EDGE_category)]
  LDV_PSI_int[,sector_fuel:=ifelse(EDGE_category=="BEV","elect_td_trn",NA)]
  LDV_PSI_int[,sector_fuel:=ifelse(EDGE_category=="FCEV","H2 enduse",sector_fuel)]
  LDV_PSI_int[,sector_fuel:=ifelse(EDGE_category=="NG","delivered gas",sector_fuel)]
  LDV_PSI_int[,sector_fuel:=ifelse(EDGE_category=="Hybrid Electric","Liquids-Electricity",sector_fuel)]
  LDV_PSI_int[,sector_fuel:=ifelse(is.na(sector_fuel),"refined liquids enduse",sector_fuel)]
  ## average on the EDGE category
  LDV_PSI_int = LDV_PSI_int[, .(value=mean(value)), by = c("EDGE_category","vehicle_type","year","sector_fuel")]
  LDV_PSI_int_liq = LDV_PSI_int[EDGE_category == "Liquids"]
  LDV_PSI_int_liq[year > 2010, value := ifelse(year < 2020, value, NA)]
  LDV_PSI_int_liq[, value := ifelse(year == 2020, 0.9*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  LDV_PSI_int_liq[, value := ifelse(year == 2030, 0.8*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  LDV_PSI_int_liq[, value := ifelse(year == 2040, 0.7*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  LDV_PSI_int_liq[, value := ifelse(year == 2050, 0.6*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  LDV_PSI_int_liq[, value := ifelse(year == 2100, 0.6*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  ## approximate the trend of energy intensity to fill in the missing time steps
  LDV_PSI_int_liq = approx_dt(LDV_PSI_int_liq, unique(LDV_PSI_int_liq$year),
                 xcol = "year", ycol = c("value"),
                 idxcols = c("EDGE_category", "vehicle_type"),
                 extrapolate=T)
  LDV_PSI_int = rbind(LDV_PSI_int[EDGE_category != "Liquids"], LDV_PSI_int_liq)
  ## add logit_category for Hybrid Electric
  logit_category = rbind(logit_category, logit_category[technology == "BEV"][, technology := "Hybrid Electric"])

  ##merge with logit_category
  LDV_PSI_int=merge(logit_category,LDV_PSI_int,by.x=c("technology","vehicle_type"),by.y=c("EDGE_category","vehicle_type"))[,-"univocal_name"]
  setnames(LDV_PSI_int,old="value",new="conv_pkm_MJ")
  ## add missing 4Ws
  LDV_PSI_int = rbind(LDV_PSI_int,
                        LDV_PSI_int[vehicle_type == "Large Car and SUV"][, vehicle_type := "Large Car"],
                        LDV_PSI_int[vehicle_type == "Large Car and SUV"][, vehicle_type := "Light Truck and SUV"])
  LDV_PSI_int = merge(LDV_PSI_int, unique(conv_pkm_mj[, c("region", "year")]), by = "year", allow.cartesian = TRUE)
  LDV_PSI_int = merge(LDV_PSI_int, unique(conv_pkm_mj[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all = FALSE)
  LDV_PSI_int = rbind(LDV_PSI_int[region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU")],
                        LDV_PSI_int[!region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU") & technology %in% c("BEV", "FCEV", "Hybrid Electric")])

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
  ## approx to the whole time range
  Truck_PSI_int = approx_dt(Truck_PSI_int,
                            xdata = unique(conv_pkm_mj$year),
                            xcol = "year",
                            ycol = "conv_pkm_MJ",
                            idxcols = c("technology", "vehicle_type"),
                            extrapolate = TRUE)

  ## Buses are assumed to be as 7.5 tons truck
  Bus_PSI_int = Truck_PSI_int[vehicle_type == "Truck (7.5t)"][, vehicle_type := "Bus"]
  Truck_PSI_int = rbind(Truck_PSI_int, Bus_PSI_int)

  Truck_PSI_int = merge(Truck_PSI_int, unique(conv_pkm_mj[(subsector_L3 == "trn_freight_road"|subsector_L2 == "Bus") & technology %in% c("Liquids"),
                                                          c("region","year", "subsector_L1", "subsector_L2",
                                                            "subsector_L3", "sector", "vehicle_type")]), by = c("year", "vehicle_type"), allow.cartesian=TRUE)
  Truck_PSI_int[, sector_fuel := ifelse(technology == "FCEV","H2 enduse", "elect_td_trn")]


  ##merge with GCAM intensities and substitute all LDVs for EU and only alternative LDVs for other regions
  conv_pkm_mj_rest = conv_pkm_mj[!(subsector_L1=="trn_pass_road_LDV_4W" &
                                     technology %in% c("BEV", "FCEV", "Hybrid Electric")) &
                                   !region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU")]
  conv_pkm_mj_rest= rbind(conv_pkm_mj_rest, Truck_PSI_int)
  conv_pkm_mj_EU = conv_pkm_mj[!(subsector_L1=="trn_pass_road_LDV_4W") &
                                 region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU") ]

  conv_pkm_mj=rbind(conv_pkm_mj_rest, conv_pkm_mj_EU, LDV_PSI_int)

  return(conv_pkm_mj)
}
