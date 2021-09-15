#' Read and prepare GCAM data
#'
#' Demand in million pkm and tmk, EI in MJ/km
#'
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING a mapping between ISO3 codes and GCAM region names
#' @param GDP_country country level GDP PPP (used for disaggregation)
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return transport entries (demand, energy intensity, load factor, value of time, structure of the logit, vehicle speed)


lvl0_GCAMraw <- function(input_folder, GCAM2ISO_MAPPING, GDP_country, GCAM_dir = "GCAM"){
  coefficient <- `.` <- region <- supplysector <- tranSubsector <- minicam.energy.input <- stub.technology <- maxspeed <- sector <- subsector_L1 <- NULL
  conv_pkm_MJ <- MJvkm <- loadFactor <- subsector <- technology <- addTimeValue <- time.value.multiplier <- vehicle_type <- NULL
  GCAM_folder = file.path(input_folder, GCAM_dir)

  ## names of some regions show underscores that have to be substituted with blank spaces
  rename_region = function(df){
    region <- NULL
    df = data.table(df)
    df[, region := gsub("_", " ", region)]
  }
  ## merge data base with full logit structure
  distribute_logit = function(df, colname, extracol){
    df = merge(df, logit_category, by.x = c(colname, "technology"), by.y = c("univocal_name", "technology"), all.x = TRUE)
    df[, c(colname, extracol) := NULL]
    return(df)
  }
  ## function that adds the vehicle types entries that are not in the original GCAM dataframe
  addvehicletypes = function(dt, ## dt that needs to be extended
                             reg,  ## region that needs to be extended
                             vehfrom, ## vehicle type similar to missing vehicle type
                             vehto, ## missing vehicle type to be integrated
                             col2use ## name of the column where vehicle types are
                             ){
    for (r in reg) {
      tmp = dt[get(col2use) == vehfrom & region == r,][, (col2use) := vehto]
      dt = rbind(dt, tmp)
    }
    return(dt)
  }
  ## coal technologies are not included in REMIND
  remove.coal = function(dt, cat_name = "technology"){
    dt[get(cat_name) != "Coal"]
  }
  ## due to a bug freight electric trains are not included in GCAM in some entries
  add.ElTrains = function(dt){
    subsector <- technology <- region <- NULL
    reg = setdiff(dt[subsector == "Freight Rail" & technology == "Liquids", region], dt[subsector == "Freight Rail" & technology == "Electric", region])
    rail = dt[technology == "Liquids" & subsector == "Freight Rail" & region %in% reg,][, c("technology", "tech_output") := list("Electric", 0)]
    dt = rbind(dt, rail)
  }

  #load logit structure
  logit_category = fread(file.path(GCAM_folder, "logit_categories.csv"), na.strings = c("", "NA"))
  ## remove coal
  logit_category = remove.coal(logit_category)

  #energy intensity
  vehicle_intensity = fread(file.path(GCAM_folder, "L254.StubTranTechCoef.csv"), skip=4)

  CONV_MJ_btu = 947.777
  vehicle_intensity = rename_region(vehicle_intensity)
  vehicle_intensity = vehicle_intensity[,.(MJvkm = coefficient/CONV_MJ_btu,  #convert from BTU to MJ
                                         region, supplysector, tranSubsector, year, sector_fuel = minicam.energy.input,
                                         technology = stub.technology)]

  ## remove coal
  vehicle_intensity = remove.coal(vehicle_intensity)

  vehicle_intensity = distribute_logit(vehicle_intensity, colname = "tranSubsector", extracol = "supplysector")

  vehicle_intensity = addvehicletypes(vehicle_intensity,
                                      vehfrom = "Large Car and SUV",
                                      vehto = "Midsize Car",
                                      reg = c("EU-15","European Free Trade Association","Europe Non EU"),
                                      col2use = "vehicle_type")
  ## remove double category of buses and remove three wheelers
  vehicle_intensity = vehicle_intensity[!vehicle_type %in% c("Heavy Bus", "Light Bus", "Three-Wheeler_tmp_vehicletype", "Truck")]
  ## remove the Adv categories, Hybrid Liquids and LA_BEV
  vehicle_intensity = vehicle_intensity[!technology %in% c("Tech-Adv-Electric", "Adv-Electric", "Hybrid Liquids", "Tech-Adv-Liquid", "Adv-Liquid")]
  vehicle_intensity[vehicle_type %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), vehicle_type := "Truck (0-3.5t)"]
  vehicle_intensity[vehicle_type %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), vehicle_type := "Truck (7.5t)"]
  vehicle_intensity[vehicle_type %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)"), vehicle_type := "Truck (18t)"]
  vehicle_intensity[vehicle_type %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), vehicle_type := "Truck (26t)"]
  vehicle_intensity[vehicle_type %in% c("Truck (>32t)"), vehicle_type := "Truck (40t)"]
  vehicle_intensity=vehicle_intensity[,.(MJvkm = mean(MJvkm)), by = c("sector_fuel","region", "year", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2","subsector_L1")]

  #load factor
  load_factor = read.csv(file.path(GCAM_folder, "L254.StubTranTechLoadFactor.csv"), skip=4, header = T,stringsAsFactors = FALSE)
  load_factor = rename_region(load_factor)
  setnames(load_factor, old = "stub.technology", new = "technology")
  ## remove the Adv categories, Hybrid Liquids and LA_BEV
  load_factor = load_factor[!technology %in% c("Tech-Adv-Electric", "Adv-Electric", "Hybrid Liquids", "Tech-Adv-Liquid", "Adv-Liquid")]

  ## remove coal
  load_factor = remove.coal(load_factor)

  load_factor = distribute_logit(load_factor, colname = "tranSubsector", extracol = "supplysector")

  load_factor = addvehicletypes(dt=load_factor,
                                      vehfrom = "Large Car and SUV",
                                      vehto = "Midsize Car",
                                      reg = c("EU-15","European Free Trade Association","Europe Non EU"),
                                      col2use = "vehicle_type")
  ## remove double category of buses and remove three wheelers; substitute
  load_factor = load_factor[!vehicle_type %in% c("Heavy Bus", "Light Bus", "Three-Wheeler_tmp_vehicletype", "Truck")]
  ## add load factor for trucks
  load_factor[vehicle_type %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), vehicle_type := "Truck (0-3.5t)"]
  load_factor[vehicle_type %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), vehicle_type := "Truck (7.5t)"]
  load_factor[vehicle_type %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)", "Truck"), vehicle_type := "Truck (18t)"]
  load_factor[vehicle_type %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), vehicle_type := "Truck (26t)"]
  load_factor[vehicle_type %in% c("Truck (>32t)"), vehicle_type := "Truck (40t)"]

  load_factor=load_factor[,.(loadFactor = mean(loadFactor)), by = c("region", "year", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2","subsector_L1")]

  #calculate MJ/km conversion factor
  conv_pkm_mj = merge(vehicle_intensity,load_factor, all = TRUE)
  conv_pkm_mj = conv_pkm_mj[,conv_pkm_MJ := MJvkm/loadFactor]
  conv_pkm_mj[, c("MJvkm", "loadFactor") := NULL]
  conv_pkm_mj = conv_pkm_mj[technology != "LA-BEV"]
  ## load and change the tech_output file so that it reflects the logit tree
  tech_output = fread(file.path(GCAM_folder, "tech_output.csv"), skip = 1, sep=";", header = T)
  tech_output = melt(tech_output, measure.vars=6:26, value.name="tech_output", variable.name = "year")
  tech_output[, c("Units", "scenario", "year") := list(NULL, NULL, as.numeric(as.character(year)))]
  tech_output = tech_output[year <= 2010 & !subsector %in% c("road","LDV","bus","4W","2W")]
  tech_output[, technology := ifelse(subsector %in% c("Walk","Cycle"), paste0(subsector,"_tmp_technology"),technology)]

  tech_output = remove.coal(tech_output)
  tech_output = add.ElTrains(tech_output)

  setnames(tech_output, old="sector", new="supplysector")

  tech_output = distribute_logit(tech_output,colname = "subsector",extracol = "supplysector")
  ## merge 2wheelers and 3 wheelers and different categories of buses; remove NG motorbikes and merge BEV and LA-BEV
  tech_output[vehicle_type %in% c("Heavy Bus", "Light Bus"), c("vehicle_type","subsector_L1", "subsector_L2", "subsector_L3", "sector") := list("Bus_tmp_vehicletype", "Bus_tmp_subsector_L1", "Bus", "trn_pass_road", "trn_pass")]
  tech_output[vehicle_type %in% c("Three-Wheeler_tmp_vehicletype", "Scooter"), c("vehicle_type","subsector_L1", "subsector_L2", "subsector_L3", "sector") := list("Motorcycle (50-250cc)", "trn_pass_road_LDV_2W", "trn_pass_road_LDV", "trn_pass_road", "trn_pass")]
  tech_output[vehicle_type == "Multipurpose Vehicle", vehicle_type := "Subcompact Car"]
  tech_output[technology == "LA-BEV", technology := "BEV"]
  tech_output[technology %in% c("Tech-Adv-Electric", "Adv-Electric"), technology := "Electric"]
  tech_output[technology %in% c("Hybrid Liquids", "Tech-Adv-Liquid", "Adv-Liquid"), technology := "Liquids"]
  tech_output = tech_output[technology == "NG" & subsector_L1 == "trn_pass_road_LDV_2W", technology := "Liquids"]

  ## merge the truck categories
  tech_output[vehicle_type %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), vehicle_type := "Truck (0-3.5t)"]
  tech_output[vehicle_type %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), vehicle_type := "Truck (7.5t)"]
  tech_output[vehicle_type %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)", "Truck"), vehicle_type := "Truck (18t)"]
  tech_output[vehicle_type %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), vehicle_type := "Truck (26t)"]
  tech_output[vehicle_type %in% c("Truck (>32t)"), vehicle_type := "Truck (40t)"]
  tech_output = tech_output[,.(tech_output = sum(tech_output)), by = c("region","sector","subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type","technology","year")]

  tech_output = rename_region(tech_output)

  ## speed motorized modes
  speed_mot = fread(file.path(GCAM_folder, "L254.tranSubsectorSpeed.csv"), skip=4)
  speed_mot = rename_region(speed_mot)
  speed_mot = unique(speed_mot) ## delete the reduntand rows present in the dataframe
  speed_mot = speed_mot[, .(speed=mean(speed)), by = c("region", "year", "supplysector", "tranSubsector")] ## some entries are repeated otherwise (e.g. Moped, 1990)

  ## speed non-motorized
  speed_not_mot = fread(file.path(GCAM_folder, "A54.globaltech_nonmotor.csv"), skip=1, header = T)
  speed_not_mot = speed_not_mot[, c("supplysector", "tranSubsector", "speed")]

  ## conversion GDP -> PPP-MER coefficient
  PPP_MER = fread(file.path(GCAM_folder, "GCAM_PPP_MER.csv"), header = T)
  PPP_MER = PPP_MER[,.(region,PPP_MER)] ## select only the coefficient to move from PPP to MER GDP
  regions = unique(PPP_MER$region)

  speed_not_mot = speed_not_mot[,.(tmp=paste0(supplysector,"#",tranSubsector,"#",speed))]
  speed_not_mot = CJ(tmp = speed_not_mot$tmp, region = regions, unique = T)
  speed_not_mot = speed_not_mot[,.(tmp = paste0(tmp,"#",region))]
  speed_not_mot = CJ(tmp = speed_not_mot$tmp, year = speed_mot$year, unique = TRUE)
  speed_not_mot[, `:=`(c("supplysector","tranSubsector","speed","region"), tstrsplit(tmp, "#",fixed = TRUE))]
  speed_not_mot[, c("tmp", "speed") := list(NULL, as.numeric(speed))]
  speed_not_mot = rename_region(speed_not_mot)
  speed = merge(speed_not_mot, speed_mot, all=TRUE, by=c("region", "year", "tranSubsector", "supplysector", "speed"))
  speed = addvehicletypes(speed,
                          vehfrom = "Large Car and SUV",
                          vehto = "Midsize Car",
                          reg = c("EU-15","European Free Trade Association","Europe Non EU"),
                          col2use = "tranSubsector")
  ## Apply convergence in time to the fastest vehicle across regions
  speed[, maxspeed := max(speed[year == 2100]), by = .(tranSubsector)]
  speed[year >= 2020, speed := speed[year == 2020]*(2100-year)/(2100-2010) + maxspeed*(year-2020)/(2100-2020), by =c("tranSubsector", "region")]
  speed[, maxspeed := NULL]
  ## rename category following EDGE-T structure
  speed[supplysector == "trn_pass_road_bus", supplysector := "trn_pass_road_bus_tmp_subsector_L1"]
  ## VOT
  vott_all = fread(file.path(GCAM_folder, "A54.tranSubsector_VOTT.csv"), skip = 1)
  vott_all[supplysector == "trn_pass_road_LDV_4W", addTimeValue := 1]
  vott_all = vott_all[!duplicated(vott_all)][addTimeValue==1] #delete the levels that have not time value added
  vott_all = vott_all[tranSubsector != "4W"]  ## this entry has to be removed due to the required "double counting" happening in GCAM for their data structure
  vott_all[, c("speed.source", "wait.walk.vott", "wait.walk.share", "in.vehicle.VOTT", "fuelprefElasticity", "addTimeValue") := list(NULL, NULL, NULL, NULL, NULL, NULL)]

  tmp = CJ(tranSubsector = vott_all$tranSubsector,
               region = regions, unique=TRUE)

  vott_all = merge(tmp, vott_all, all=TRUE, by = "tranSubsector")
  vott_all = merge(vott_all,PPP_MER,all=TRUE,by="region")
  vott_all = vott_all[,.(time.value.multiplier=time.value.multiplier/PPP_MER,region,tranSubsector,supplysector)] #rescale the time value multiplier so that it is in PPP basis and not MER
  vott_all = rename_region(vott_all)
  ## rename category following EDGE-T structure
  vott_all[supplysector == "trn_pass_road_bus", supplysector := "trn_pass_road_bus_tmp_subsector_L1"]

  ## to ISO
  tech_output_iso = disaggregate_dt(
    tech_output, GCAM2ISO_MAPPING,
    fewcol = "region", manycol = "iso", valuecol = "tech_output",
    datacols=c("sector", "subsector_L1", "subsector_L2",
               "subsector_L3", "vehicle_type", "technology"),
    weights = GDP_country, weightcol = "weight"
  )

  int_iso <- disaggregate_dt(conv_pkm_mj, GCAM2ISO_MAPPING)
  load_factor_iso <- disaggregate_dt(load_factor, GCAM2ISO_MAPPING)

  vott_iso <- disaggregate_dt(vott_all, GCAM2ISO_MAPPING)

  speed_iso <- disaggregate_dt(speed, GCAM2ISO_MAPPING)

  GCAM_data = list(tech_output = tech_output_iso,
                 logit_category = logit_category,
                 conv_pkm_mj = int_iso,
                 load_factor = load_factor_iso,
                 vott_all = vott_iso,
                 speed = speed_iso
  )
  return(GCAM_data)
}

#' Load value-of-time and exponents
#'
#' load logit exponents for each level: they are based on GCAM assumptions.
#' They are on csv files that already follow the EDGE structure, created by hand.
#'
#' VOT values in (1990$/pkm)
#' @param GCAM_data GCAM based data
#' @param GDP_MER_country GDP iso level MER
#' @param POP_country population (ISO level)
#' @param input_folder folder hosting raw data
#' @param logitexp_dir directory with logit exponents for GCAM
#' @importFrom rmndt aggregate_dt
#'


lvl0_VOTandExponents <- function(GCAM_data, GDP_MER_country, POP_country, input_folder, logitexp_dir="GCAM_logit_exponents"){
  sector <- logit.exponent <- value <-  region <- ISO3 <- `.` <- time <- Year <- Value <- time_price <- GDP_cap <- time.value.multiplier <- tranSubsector <- supplysector <- univocal_name <- speed_conv <- year_at_yearconv <- yearconv <-weight <- GDP <- speed_trend <- POP_val <- NULL
  loadFactor <- loadFactor_conv <- loadFactor_trend <- subsector_L1 <- subsector_L2 <- speed <- iso <- NULL
  subsector_L3 <- technology <- vehicle_type <- NULL
  exp_folder = function(fname){
    file.path(input_folder, logitexp_dir, fname)
  }

  GDP_POP = merge(GDP_MER_country, POP_country, by = c("iso", "year"))

  GDP_POP_cap = GDP_POP[,GDP_cap := weight/value]

  logit_exponent_FV = fread(exp_folder("FV_logitexponent.csv"))
  logit_exponent_VS1 = fread(exp_folder("VS1_logitexponent.csv"))
  logit_exponent_S1S2 = fread(exp_folder("S1S2_logitexponent.csv"))
  logit_exponent_S2S3 = fread(exp_folder("S2S3_logitexponent.csv"))
  logit_exponent_S3S = fread(exp_folder("S3S_logitexponent.csv"))

  ## load VOT factors, speed and load factor
  vott_all = copy(GCAM_data[["vott_all"]])

  ## calculate VOT
  vott_all = merge(vott_all, GDP_POP_cap, all = TRUE, by = "iso", allow.cartesian = TRUE) #for each time step and each region
  vott_all = merge (vott_all, GCAM_data[["speed"]], all = FALSE, by = c("iso", "year", "tranSubsector", "supplysector"))
  WEEKS_PER_YEAR = 50
  HOURS_PER_WEEK = 40
  vott_all[, time_price := GDP_cap                             ## [2005$/person/year]
                           *time.value.multiplier              ## [2005$/person/year]
                           /(HOURS_PER_WEEK* WEEKS_PER_YEAR)/  ## [2005$/h]
                           speed]                              ## [2005$/km]

  value_of_time = vott_all[,.(iso, year, time_price, tranSubsector, supplysector)]

  ## Ceate level specific VOT data tables

  ## FV

  value_of_time_LDV = value_of_time[supplysector %in% c("trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W"),
                                    .(vehicle_type = tranSubsector, subsector_L1 = supplysector, iso, year, time_price)]

  value_time_FV = value_of_time_LDV[!is.na(time_price),]

  ## VS1

  value_time_VS1 = data.table(subsector_L1 = character(), vehicle_type = character(), iso = character(), year = numeric(), time_price = numeric())

  ## S1S2

  value_time_S1S2 = data.table(subsector_L2 = character(), subsector_L1 = character(), iso = character(), year = numeric(), time_price = numeric())

  ## S2S3

  value_time_S2S3 = value_of_time[tranSubsector == "Bus",
                                 .(subsector_L3 = supplysector, subsector_L2 = tranSubsector, iso, year, time_price)]

  ## S3S

  value_time_S3S = value_of_time[tranSubsector %in% c("International Aviation", "Passenger Rail", "Domestic Aviation", "HSR"),
                                 .(sector=supplysector, subsector_L3 = tranSubsector, iso, year, time_price)]

  price_nonmot = value_of_time[tranSubsector %in% c("Cycle", "Walk"),
                              .(sector = supplysector, subsector_L1 = tranSubsector, iso, year, tot_price = time_price)]

  price_nonmot[, subsector_L2 := paste0(subsector_L3, "_tmp_subsector_L2")]
  price_nonmot[, subsector_L1 := paste0(subsector_L3, "_tmp_subsector_L1")]
  price_nonmot[, vehicle_type := paste0(subsector_L3, "_tmp_vehicletype")]
  price_nonmot[, technology := paste0(subsector_L3, "_tmp_technology")]

  VOT_output = list(value_time_FV = value_time_FV,
                    value_time_VS1 = value_time_VS1,
                    value_time_S1S2 = value_time_S1S2,
                    value_time_S2S3 = value_time_S2S3,
                    value_time_S3S = value_time_S3S)

  logit_output = list(logit_exponent_FV = logit_exponent_FV,
                      logit_exponent_VS1 = logit_exponent_VS1,
                      logit_exponent_S1S2 = logit_exponent_S1S2,
                      logit_exponent_S2S3 = logit_exponent_S2S3,
                      logit_exponent_S3S = logit_exponent_S3S)

  result = list(VOT_output = VOT_output,
                logit_output = logit_output,
                price_nonmot = price_nonmot)

  return(result)

}

