#' Read and prepare GCAM data
#'
#' Demand in million pkm and tmk, EI in MJ/km
#'
#' @param input_folder folder hosting raw data
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return transport entries (demand, energy intensity, load factor, value of time, structure of the logit, vehicle speed)


lvl0_GCAMraw <- function(input_folder, GCAM_dir = "GCAM"){
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


  GCAM_data = list(tech_output = tech_output,
                 logit_category = logit_category,
                 conv_pkm_mj = conv_pkm_mj,
                 load_factor = load_factor,
                 vott_all = vott_all,
                 speed = speed
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
#' @param GDP_country GDP ISO level PPP
#' @param GDP_POP GDP per capita PPP
#' @param GDP_MER_country GDP iso level MER
#' @param POP_country population (ISO level)
#' @param REMIND_scenario SSP scenario
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param logitexp_dir directory with logit exponents for GCAM
#' @importFrom rmndt aggregate_dt
#'


lvl0_VOTandExponents <- function(GCAM_data, GDP_country, GDP_POP, GDP_MER_country, POP_country, REMIND_scenario, input_folder, GCAM2ISO_MAPPING, logitexp_dir="GCAM_logit_exponents"){
  sector <- logit.exponent <- value <-  region <- ISO3 <- `.` <- time <- Year <- Value <- time_price <- GDP_cap <- time.value.multiplier <- tranSubsector <- supplysector <- univocal_name <- speed_conv <- year_at_yearconv <- yearconv <-weight <- GDP <- speed_trend <- POP_val <- NULL
  loadFactor <- loadFactor_conv <- loadFactor_trend <- subsector_L1 <- subsector_L2 <- NULL
  subsector_L3 <- technology <- vehicle_type <- NULL
  CONV_2005USD_1990USD = 0.67
  exp_folder = function(fname){
    file.path(input_folder, logitexp_dir, fname)
  }

  logit_exponent_FV = fread(exp_folder("FV_logitexponent.csv"))
  logit_exponent_VS1 = fread(exp_folder("VS1_logitexponent.csv"))
  logit_exponent_S1S2 = fread(exp_folder("S1S2_logitexponent.csv"))
  logit_exponent_S2S3 = fread(exp_folder("S2S3_logitexponent.csv"))
  logit_exponent_S3S = fread(exp_folder("S3S_logitexponent.csv"))

  ## load VOT factors, speed and load factor
  vott_all = GCAM_data[["vott_all"]]
  speed = GCAM_data[["speed"]]
  load_factor = GCAM_data[["load_factor"]]

  ## speed converges
  gdp_country = copy(GDP_MER_country)
  gdp <- aggregate_dt(gdp_country, GCAM2ISO_MAPPING,
                      valuecol="weight",
                      datacols=c("variable"))

  pop <- aggregate_dt(POP_country, GCAM2ISO_MAPPING,
                      valuecol="value",
                      datacols=c("POP"))

  GDP_POP = merge(gdp, pop, by = c("region", "year"))

  GDP_POP_cap = GDP_POP[,GDP_cap := weight/value]

  tmp = merge(speed, GDP_POP_cap, by = c("region", "year"))
  ## define rich regions
  richregions = unique(unique(tmp[year == 2010 & GDP_cap > 25000, region]))
  ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
  richave = tmp[region %in% richregions & speed > 0,]
  richave = richave[, .(speed = sum(speed*weight)/sum(weight)), by = c("tranSubsector", "supplysector", "year")]
  GDP_POP = GDP_POP_cap[region %in% richregions,]
  GDP_POP = GDP_POP[, .(GDP = sum(weight), POP_val = sum(value)), by = c("year")]
  richave = merge(richave, GDP_POP, by = "year")
  ## average gdp per capita of the rich countries
  richave[, GDP_cap := GDP/POP_val]
  ## dt on which the GDPcap is checked
  tmp1 = tmp[!region %in% richregions, c("region", "year",
                                         "GDP_cap", "supplysector", "tranSubsector",
                                         "speed")]
  ## dt contaning the gdp towards which to converge
  tmp2 = richave[, c("year", "GDP_cap")]
  ## dt containing the non fuel price for rich countries
  tmp3 = richave[, c("year", "supplysector", "tranSubsector")]
  ## names has to be different across dts for roll join
  setnames(tmp2, old = c("year"), new = c("time"))
  setnames(tmp3, old = c("year"), new = c("time"))

  setkey(tmp1,GDP_cap)
  setkey(tmp2,GDP_cap)
  ## find the time step at which the GDPcap matches the GDPcap of the rich countries
  tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

  ## merge with non fuel price of corresponding values
  tmp2 = merge(tmp2, tmp3, by = c("time", "supplysector", "tranSubsector"))

  ## find year closest to 2010 for each region, this is the year at which is going to converge
  tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("region")]

  ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
  tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("region", "time")]
  tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("region", "time")]
  ## year at which the convergence happens
  tmp2[, year_at_yearconv := year[time == yearconv], by = c("region","supplysector", "tranSubsector")]

  ## value of speed after the convergence
  tmp3 = richave[, c("year", "speed", "supplysector", "tranSubsector")]
  setnames(tmp3, old = c("speed", "year"), new = c("speed_trend", "year_at_yearconv"))
  tmp2 = merge(tmp2,tmp3,by=c("year_at_yearconv", "supplysector", "tranSubsector"), all.x=TRUE)

  ## after the year of convergence, the values are the "average" developed countries values
  tmp2[year >= year_at_yearconv & year > 2010, speed := speed_trend, by = c("region","supplysector", "tranSubsector")]

  ## value of yearconv represents the convergence value
  tmp2[, speed_conv := speed_trend[time==yearconv], by = c("region","supplysector", "tranSubsector")]
  ## convergence is linear until the value corresponding to 2010 is reached
  tmp2[year <= year_at_yearconv & year >= 2010, speed := speed[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(speed_conv-speed[year == 2010]), by =c("supplysector", "tranSubsector", "region")]
  tmp2[is.na(speed), speed := speed_trend]
  ## select only useful columns
  tmp2 = tmp2[,.(region, year, speed, supplysector, tranSubsector)]
  ## rich countries need to be reintegrated
  speed = rbind(tmp2, speed[region %in% richregions])
  ## load logit categories table
  logit_category = GCAM_data[["logit_category"]]
  ## calculate VOT
  gdp_country = copy(GDP_country)
  gdp <- aggregate_dt(gdp_country, GCAM2ISO_MAPPING,
                      valuecol="weight",
                      datacols=c("variable"))

  pop <- aggregate_dt(POP_country, GCAM2ISO_MAPPING,
                      valuecol="value",
                      datacols=c("POP"))

  GDP_POP = merge(gdp, pop, by = c("region", "year"))

  GDP_POP_cap = GDP_POP[,GDP_cap := weight/value]
  vott_all = merge(vott_all, GDP_POP_cap, all = TRUE, by = "region", allow.cartesian = TRUE) #for each time step and each region
  vott_all = merge (vott_all, speed, all = FALSE, by = c("region", "year", "tranSubsector", "supplysector"))
  WEEKS_PER_YEAR = 50
  HOURS_PER_WEEK = 40
  vott_all[, time_price := GDP_cap                             ## [1990$/person/year]
                           *time.value.multiplier              ## [1990$/person/year] CHECK!
                           /(HOURS_PER_WEEK* WEEKS_PER_YEAR)/  ## [1990$/h]
                           speed]                              ## [1990$/km]

  value_of_time = vott_all[,.(region, year, time_price, tranSubsector, supplysector)]

  ## load factor convergence
  GDP_POP = merge(gdp, pop, by = c("region", "year"))
  GDP_POP_cap = GDP_POP[,GDP_cap := weight/value]
  tmp = merge(load_factor, GDP_POP_cap, by = c("region", "year"))
  ## define rich regions
  richregions = unique(unique(tmp[year == 2010 & GDP_cap > 25000, region]))
  ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
  richave = tmp[region %in% richregions & loadFactor > 0,]
  richave = richave[, .(loadFactor = sum(loadFactor*weight)/sum(weight)), by = c("vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "year")]
  GDP_POP = GDP_POP_cap[region %in% richregions,]
  GDP_POP = GDP_POP[, .(GDP = sum(weight), POP_val = sum(value)), by = c("year")]
  richave = merge(richave, GDP_POP, by = "year")
  ## average gdp per capita of the rich countries
  richave[, GDP_cap := GDP/POP_val]
  ## dt on which the GDPcap is checked
  tmp1 = tmp[!region %in% richregions, c("region", "year",
                                         "GDP_cap", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1",
                                         "loadFactor")]
  ## dt contaning the gdp towards which to converge
  tmp2 = richave[, c("year", "GDP_cap")]
  ## dt containing the non fuel price for rich countries
  tmp3 = richave[, c("year", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]
  ## names has to be different across dts for roll join
  setnames(tmp2, old = c("year"), new = c("time"))
  setnames(tmp3, old = c("year"), new = c("time"))

  setkey(tmp1,GDP_cap)
  setkey(tmp2,GDP_cap)
  ## find the time step at which the GDPcap matches the GDPcap of the rich countries
  tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

  ## merge with non fuel price of corresponding values
  tmp2 = merge(tmp2, tmp3, by = c("time", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1"))

  ## find year closest to 2010 for each region, this is the year at which is going to converge
  tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("region")]

  ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
  tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("region", "time")]
  tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("region", "time")]
  ## year at which the convergence happens
  tmp2[, year_at_yearconv := year[time == yearconv], by = c("region","vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]

  ## value of speed after the convergence
  tmp3 = richave[, c("year", "loadFactor", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]
  setnames(tmp3, old = c("loadFactor", "year"), new = c("loadFactor_trend", "year_at_yearconv"))
  tmp2 = merge(tmp2,tmp3,by=c("year_at_yearconv", "vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1"))

  ## after the year of convergence, the values are the "average" developed countries values
  tmp2[year >= year_at_yearconv & year > 2010, loadFactor := loadFactor_trend, by = c("region","vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]

  ## value of yearconv represents the convergence value
  tmp2[, loadFactor_conv := loadFactor_trend[time==yearconv], by = c("region","vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1")]
  ## convergence is linear until the value corresponding to 2010 is reached
  tmp2[year <= year_at_yearconv & year >= 2010, loadFactor := loadFactor[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(loadFactor_conv-loadFactor[year == 2010]), by =c("vehicle_type", "technology", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "region")]
  tmp2[is.na(loadFactor), loadFactor := loadFactor_trend]
  ## select only useful columns
  tmp2 = tmp2[,.(region, year, loadFactor, vehicle_type, technology, sector, subsector_L3, subsector_L2, subsector_L1)]
  ## rich countries need to be reintegrated
  load_factor = rbind(tmp2[subsector_L1 != "trn_pass_road_LDV_4W"], load_factor[(region %in% richregions)|(subsector_L1 == "trn_pass_road_LDV_4W")])



  ## Ceate level specific VOT data tables

  ## FV

  value_of_time_LDV = value_of_time[supplysector %in% c("trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W"),
                                    .(vehicle_type = tranSubsector, subsector_L1 = supplysector, region, year, time_price)]

  value_time_FV = value_of_time_LDV[!is.na(time_price),]

  ## VS1

  value_time_VS1 = data.table(subsector_L1 = character(), vehicle_type = character(), region = character(), year = numeric(), time_price = numeric())

  ## S1S2

  value_time_S1S2 = data.table(subsector_L2 = character(), subsector_L1 = character(), region = character(), year = numeric(), time_price = numeric())

  ## S2S3

  value_time_S2S3 = value_of_time[tranSubsector == "Bus",
                                 .(subsector_L3 = supplysector, subsector_L2 = tranSubsector, region, year, time_price)]

  ## S3S

  value_time_S3S = value_of_time[tranSubsector %in% c("International Aviation", "Passenger Rail", "Domestic Aviation", "HSR"),
                                 .(sector=supplysector, subsector_L3 = tranSubsector, region, year, time_price)]

  price_nonmot = value_of_time[tranSubsector %in% c("Cycle", "Walk"),
                              .(sector = supplysector, subsector_L3 = tranSubsector, region, year, tot_price = time_price)]
  price_nonmot = merge(price_nonmot, logit_category, all.x = TRUE)
  price_nonmot[, univocal_name := NULL]

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
                price_nonmot = price_nonmot,
                load_factor = load_factor,
                speed = speed)

  return(result)

}

#' Fix issues with the GCAM data
#'
#' Applies corrections to GCAM outdated data. No conversion of units happening.
#'
#' @param GCAM_output output from GCAM raw data wrangling
#' @param NEcost non energy costs
#' @param logitexp logit exponents

lvl0_correctTechOutput <- function(GCAM_output, NEcost, logitexp){
  region <- sector <- `.` <- technology <- tech_output <- vehicle_type <- subsector_L3 <- subsector_L2 <- subsector_L1 <- value <- ratio <- type <- non_fuel_price <- cost <- tot_nonfuelprice <- conv_pkm_MJ <- logit.exponent <- NULL
  ##=== Correct & Integrate demand ===##
  ##apply the value from 2005 to 1990 in International Shipping in regions Europe Eastern and Central Asia
  GCAM_output$tech_output = GCAM_output$tech_output[!(region %in% c("Europe Eastern", "Central Asia") & sector =="trn_shipping_intl" & year == 1990),]
  GCAM_output$tech_output = rbind(GCAM_output$tech_output, GCAM_output$tech_output[region %in% c("Europe Eastern", "Central Asia") & sector == "trn_shipping_intl" & year == 2005, .(year = 1990, technology, region, tech_output, vehicle_type, subsector_L3, subsector_L2, subsector_L1, sector)])

  ##rescale China road transport to the values that are compatible with "TIMES modelling of transport sector in China and USA: Comparison from a decarbonization perspective" Hongjun Zhang, 2016

  ## load data from the paper (NB they are roughly taken from a graph)
  dt = data.table(year = c(1990, 2005, 2010), LDV = c(500000, 1500000, 2000000), bus = c(1000000, 2000000, 3000000))

  dt = melt(dt, id.vars = c("year"),
               measure.vars = c("LDV", "bus"))
  ## create a temporary dt with only LDV and bus broad categories, with the "bus" category explicitly included
  tmp1 = GCAM_output$tech_output[region == "China" & subsector_L3 == "trn_pass_road", .(variable = ifelse(grepl("bus", subsector_L2, ignore.case = TRUE), "bus", "LDV")), by = c("region", "year", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "tech_output", "technology")]
  ## sum up to the "LDV" and "bus" level in another temporary dt
  tmp = tmp1[,.(tech_output=sum(tech_output)),by=c("region","year","variable")]
  ## merge and rescale the values based on the paper
  tmp = merge(tmp, dt, all = TRUE)
  tmp = tmp[,.(ratio = value/tech_output), by = c("region", "year", "variable")]
  tmp = merge(tmp, tmp1)
  tmp[, tech_output := tech_output*ratio]
  tmp = tmp[, -c("ratio", "variable")]
  ## include the values again in the original dt
  GCAM_output$tech_output = GCAM_output$tech_output[!(region == "China" & subsector_L3 == "trn_pass_road"),]
  GCAM_output$tech_output = rbind(tmp, GCAM_output$tech_output)
  ## include Electric buses, electric trucks, h2 buses, h2 trucks
  electric = GCAM_output$tech_output[(subsector_L3 %in% c("trn_freight_road")|
                                                     subsector_L2 %in% c("Bus")) & technology == "Liquids"][, c("technology", "tech_output") := list("Electric", 0)]
  FCEV = GCAM_output$tech_output[(subsector_L3 %in% c("trn_freight_road")|
                                                     subsector_L2 %in% c("Bus")) & technology == "Liquids"][, c("technology", "tech_output") := list("FCEV", 0)]

  hydrogen = GCAM_output$tech_output[subsector_L3 %in% c("Domestic Aviation") & technology == "Liquids"][, c("technology", "tech_output") := list("Hydrogen", 0)]

  newtech = rbind(electric, FCEV, hydrogen)

  GCAM_output$tech_output = rbind(GCAM_output$tech_output, newtech)

  ## add 0 plug-in hybrids for years 1990-2010
  GCAM_output$tech_output = rbind(GCAM_output$tech_output, GCAM_output$tech_output[technology == "BEV" & subsector_L1=="trn_pass_road_LDV_4W"][, c("technology", "tech_output") := list("Hybrid Electric", 0)])


  ## IND buses costs http://www.asrtu.org/wp-content/uploads/2018/09/LBNL-Electric-Buses-in-India_BusWorld-v6.pdf
  ## capita costs
  ## Diesel = 9 Rs = 0.12 2020$ = 0.09 2005$
  ## NG = 8 Rs = 0.11 2020$ = 0.08 2005$
  ## Electric = 17 Rs = 0.23 2020$ = 0.13 2005$
  ## variable costs
  ## Diesel = 8 Rs = 0.11 2020$ = 0.08 2005$
  ## NG = 7 Rs = 0.1 2020$ = 0.08 2005$
  ## Electric = 7 Rs = 0.1 2020$ = 0.08 2005$

  ## china has 20% EBuses https://www.bloomberg.com/news/articles/2019-05-15/in-shift-to-electric-bus-it-s-china-ahead-of-u-s-421-000-to-300

    ## === Correct & Integrate energy intensity == ##
  ## intensity for electric buses/trucks, FCEV buses/trucks, hydrogen aviation

  hydrogen = GCAM_output$conv_pkm_mj[subsector_L3 == "Domestic Aviation" & technology == "Liquids"][, c("technology", "conv_pkm_MJ", "sector_fuel") := list("Hydrogen", 0, "H2 enduse")]
  liquids = GCAM_output$conv_pkm_mj[(subsector_L3 %in% c("Domestic Aviation")) & technology == "Liquids"]
  newtech = rbind(liquids, hydrogen)

  ## according to "A review on potential use of hydrogen in aviation applications", Dincer, 2016: the energy intensity of a hydrogen airplane is around 1MJ/pkm. The range of energy intensity of a fossil-based airplane is here around 3-2 MJ/pkm->a factor of 0.5 is assumed
  newtech[subsector_L3 %in% c("Domestic Aviation"), conv_pkm_MJ := ifelse(technology == "Hydrogen", 0.5*conv_pkm_MJ[technology=="Liquids"], conv_pkm_MJ), by = c("region", "year", "vehicle_type")]
  GCAM_output$conv_pkm_mj = rbind(GCAM_output$conv_pkm_mj, newtech[technology!="Liquids"])
  ## add plug-in hybrids to all countries, using the same ratio as BEVs for each country (ref: EU-15)
  inspect_ratios_eff = GCAM_output$conv_pkm_mj[technology %in% c("BEV") & subsector_L1 == "trn_pass_road_LDV_4W", ]
  inspect_ratios_eff[, ratio := conv_pkm_MJ/conv_pkm_MJ[region == "EU-15"], by = c("year", "technology", "vehicle_type")]
  inspect_ratios_eff = inspect_ratios_eff[, ratio := ifelse(is.na(ratio), mean(ratio, na.rm = TRUE), ratio), by = c("region", "technology", "year")] ## if vehicle type is not originally in EU-15

  inspect_ratios_eff[, c("sector_fuel", "technology", "conv_pkm_MJ") := NULL]

  inspect_ratios_eff = merge(inspect_ratios_eff, GCAM_output$conv_pkm_mj[technology %in% c("Hybrid Electric") & subsector_L1 == "trn_pass_road_LDV_4W" & region == "EU-15", ][, c("region", "technology", "sector_fuel") := NULL], all = TRUE)
  inspect_ratios_eff = inspect_ratios_eff[, conv_pkm_MJ := ifelse(is.na(conv_pkm_MJ), mean(conv_pkm_MJ, na.rm = TRUE), conv_pkm_MJ), by = c("region", "year")] ## if vehicle type is not originally in EU-15
  inspect_ratios_eff[, conv_pkm_MJ := ratio*conv_pkm_MJ]
  inspect_ratios_eff[, c("technology", "sector_fuel", "ratio") := list("Hybrid Electric", "Liquids-Electricity", NULL)]

  GCAM_output$conv_pkm_mj = rbind(GCAM_output$conv_pkm_mj, inspect_ratios_eff[!region %in% c("EU-15", "EU-12", "Europe Non EU", "European Free Trade Association")])

  ## public buses in china are very cheap: http://www.expatfocus.com/taking-public-transport-in-china
  # Public buses in cities are the most common and popular form of public transport. Public bus fares in China are extremely cheap and usually cost a flat RMB1 or RMB 2 (US$0.15 to US$0.25). You pay the same price regardless of the distance you travel.
  ## same holds for India
  #http://www.mytravelcost.com/India/prices-public_transport/

  ## === Substitute lambda === ##
  ## logit exponent is based on Givord et al (see paper)
  logitexp$logit_exponent_FV[, logit.exponent := ifelse(logit.exponent==-8,-4,logit.exponent)]
  ## make freight less price sensitive
  logitexp$logit_exponent_S3S[sector == "trn_freight", logit.exponent := -1]

  return(list(GCAM_output = GCAM_output,
              NEcost = NEcost,
              logitexp = logitexp))
}

