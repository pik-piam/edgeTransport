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
  psi_file <- function(fname){
    file.path(input_folder, PSI_dir, fname)
  }

  ##load vehicle intensity from GCAM
  conv_pkm_mj=GCAM_data[["conv_pkm_mj"]]
  #load the logit mapping so that I can attribute the full logit tree to each level
  logit_category=GCAM_data[["logit_category"]]

  ##load vehicle intensity from PSI
  psi_intensity=data.table(read_excel(path=psi_file("Car model result_modified.xlsx"), ##file is modified with respect to the original one: names are copy pasted to simplify R data wrangling
                                      sheet="Vehicle","A1:M191"))

  ## select relevant columns and only the "current" values->2015 and the Baseline ones->2040 in a baseline scenario
  psi_intensity=psi_intensity[scenario %in% c("current","Baseline"),c("scenario","technology","vehicle_type_PSI","Tank to wheel energy_(kJ / km)")]
  ## rename columns
  colnames(psi_intensity)=c("scenario","technology","vehicle_type_PSI","intensity_KJ_km")
  ## substitute the "scenario" with the year it stands for
  psi_intensity[,scenario:=ifelse(scenario=="current",2015,2040)]

  psi_intensity=psi_intensity[,.(year=scenario, ## rename col scenario with year
                                 technology,vehicle_type_PSI,
                                 intensity_MJ_km=as.numeric(intensity_KJ_km) ## in KJ/km
                                   *0.001)]                                  ## in MJ/km

  ##load mapping that matches PSI vehicle types with EDGE types
  mapping=fread(psi_file("mapping_PSI_EDGE.csv"), na.strings=c("","NA"))
  ##filter out the NA cells in the mapping-> PSI has more alternatives than EDGE
  mapping=mapping[!is.na(vehicle_type),]
  ## merge mapping and PSI database
  psi_intensity=merge(psi_intensity,mapping,all=FALSE,by="vehicle_type_PSI")[,-"vehicle_type_PSI"]
  ##cast
  psi_intensity=dcast(psi_intensity, technology + vehicle_type ~ year, value.var = "intensity_MJ_km")
  setnames(psi_intensity,old=c("2015","2040"),new=c("y2015","y2040"))
  psi_intensity = psi_intensity[!(technology %in% c("PHEV-c", "PHEV-e")),] ## PSI reports separately the electric and ICE running modes of a plug-in hybrid

  ## in the conservative scenario, conservative assumptions about costs of alternative technologies
  if (!(enhancedtech)) {
    psi_intensity[technology %in% c("BEV", "FCEV", "HEV-p", "PHEV"), y2040 := y2040 + 0.5*(y2015-y2040), by = "technology"]
  }

  ## give alternative pathways depending on the scenario
  if (enhancedtech & techswitch == "FCEV") {
    psi_intensity[technology %in% c("BEV", "HEV-p", "PHEV"), y2040 := (y2015 + y2040)/2, by = "technology"] ## value in 2040 is halfway the loaded value in 2040 and 2015
  }

  psi_intensity[,y2100:=0.1*(y2040-y2015)/(2040-2015)*(2100-2040)+y2040] ## set a value for 2100 that is not present in PSI database. It's set to 1/10 of the decrease between 2015 and 2040 (quite conservative)

  tsteps=unique(conv_pkm_mj$year)

  for (t in tsteps[tsteps<2040 & tsteps > 2015]) {
    psi_intensity[,paste0("y",t):=(y2015-y2040)*(1-(t-2015)/(2040-2015))+y2040]
  }

  for (t in tsteps[tsteps>2040 & tsteps <2100]) {
    psi_intensity[,paste0("y",t):=(y2040-y2100)*(1-(t-2040)/(2100-2040))+y2100]
  }

  for (t in tsteps[tsteps<2015]){
    psi_intensity[,paste0("y",t):=y2015]
  }

  for (t in tsteps[tsteps>2100]){
    psi_intensity[,paste0("y",t):=y2100]
  }

  psi_intensity = melt(psi_intensity, id.vars = c("technology", "vehicle_type"),
                       measure.vars = paste0("y",tsteps))

  psi_intensity[,variable:=as.numeric(gsub("y","",variable))]

  setnames(psi_intensity,old="variable",new="year")
  psi_intensity[,EDGE_category:=ifelse(technology=="ICEV-g","NG",NA)]
  psi_intensity[,EDGE_category:=ifelse(technology %in% c("ICEV-p","ICEV-d"),"Liquids",EDGE_category)]
  psi_intensity[,EDGE_category:=ifelse(grepl("PHEV",technology),"Hybrid Electric",EDGE_category)]
  psi_intensity[,EDGE_category:=ifelse(is.na(EDGE_category),technology,EDGE_category)]
  psi_intensity[,sector_fuel:=ifelse(EDGE_category=="BEV","elect_td_trn",NA)]
  psi_intensity[,sector_fuel:=ifelse(EDGE_category=="FCEV","H2 enduse",sector_fuel)]
  psi_intensity[,sector_fuel:=ifelse(EDGE_category=="NG","delivered gas",sector_fuel)]
  psi_intensity[,sector_fuel:=ifelse(EDGE_category=="Hybrid Electric","Liquids-Electricity",sector_fuel)]
  psi_intensity[,sector_fuel:=ifelse(is.na(sector_fuel),"refined liquids enduse",sector_fuel)]
  ## average on the EDGE category
  psi_intensity = psi_intensity[, .(value=mean(value)), by = c("EDGE_category","vehicle_type","year","sector_fuel")]
  psi_intensity_liq = psi_intensity[EDGE_category == "Liquids"]
  psi_intensity_liq[year > 2010, value := ifelse(year < 2020, value, NA)]
  psi_intensity_liq[, value := ifelse(year == 2020, 0.9*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  psi_intensity_liq[, value := ifelse(year == 2030, 0.8*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  psi_intensity_liq[, value := ifelse(year == 2040, 0.7*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  psi_intensity_liq[, value := ifelse(year == 2050, 0.6*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  psi_intensity_liq[, value := ifelse(year == 2100, 0.6*value[year == 2010], value), by = c("vehicle_type", "EDGE_category")]
  ## approximate the trend of energy intensity to fill in the missing time steps
  psi_intensity_liq = approx_dt(psi_intensity_liq, unique(psi_intensity_liq$year),
                 xcol = "year", ycol = c("value"),
                 idxcols = c("EDGE_category", "vehicle_type"),
                 extrapolate=T)
  psi_intensity = rbind(psi_intensity[EDGE_category != "Liquids"], psi_intensity_liq)
  ## add logit_category for Hybrid Electric
  logit_category = rbind(logit_category, logit_category[technology == "BEV"][, technology := "Hybrid Electric"])

  ##merge with logit_category
  psi_intensity=merge(logit_category,psi_intensity,by.x=c("technology","vehicle_type"),by.y=c("EDGE_category","vehicle_type"))[,-"univocal_name"]
  setnames(psi_intensity,old="value",new="conv_pkm_MJ")
  ## add missing 4Ws
  psi_intensity = rbind(psi_intensity,
                        psi_intensity[vehicle_type == "Large Car and SUV"][, vehicle_type := "Large Car"],
                        psi_intensity[vehicle_type == "Large Car and SUV"][, vehicle_type := "Light Truck and SUV"])
  psi_intensity = merge(psi_intensity, unique(conv_pkm_mj[, c("region", "year")]), by = "year", allow.cartesian = TRUE)
  psi_intensity = merge(psi_intensity, unique(conv_pkm_mj[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all = FALSE)
  psi_intensity = rbind(psi_intensity[region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU")],
                        psi_intensity[!region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU") & technology %in% c("BEV", "FCEV", "Hybrid Electric")])

  ##merge with GCAM intensities and substitute all LDVs for EU and only alternative LDVs for other regions
  conv_pkm_mj_rest = conv_pkm_mj[!(subsector_L1=="trn_pass_road_LDV_4W" &
                                     technology %in% c("BEV", "FCEV", "Hybrid Electric")) & !region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU")]
  conv_pkm_mj_EU = conv_pkm_mj[!(subsector_L1=="trn_pass_road_LDV_4W") &
                                 region %in% c("EU-12", "EU-15", "European Free Trade Association", "Europe Non EU") ]

  conv_pkm_mj=rbind(conv_pkm_mj_rest, conv_pkm_mj_EU, psi_intensity)

  return(conv_pkm_mj)
}
