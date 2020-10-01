#' Read and prepare GCAM data
#'
#' Demand in million pkm and tmk, EI in MJ/km
#'
#' @param input_folder folder hosting raw data
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return transport entries (demand, energy intensity, load factor, value of time, structure of the logit, vehicle speed)


lvl0_GCAMraw <- function(input_folder, GCAM_dir = "GCAM"){
  coefficient <- `.` <- region <- supplysector <- tranSubsector <- minicam.energy.input <- stub.technology <- maxspeed <- NULL
    conv_pkm_MJ <- MJvkm <- loadFactor <- subsector <- technology <- addTimeValue <- time.value.multiplier <- NULL
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
  #load factor
  load_factor = read.csv(file.path(GCAM_folder, "L254.StubTranTechLoadFactor.csv"), skip=4, header = T,stringsAsFactors = FALSE)
  load_factor = rename_region(load_factor)
  setnames(load_factor, old = "stub.technology", new = "technology")
  ## remove coal
  load_factor = remove.coal(load_factor)

  load_factor = distribute_logit(load_factor, colname = "tranSubsector", extracol = "supplysector")

  load_factor = addvehicletypes(dt=load_factor,
                                      vehfrom = "Large Car and SUV",
                                      vehto = "Midsize Car",
                                      reg = c("EU-15","European Free Trade Association","Europe Non EU"),
                                      col2use = "vehicle_type")

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
#' @param REMIND_scenario SSP scenario
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param logitexp_dir directory with logit exponents for GCAM
#'
#' @importFrom madrat calcOutput
#' @importFrom rmndt aggregate_dt


lvl0_VOTandExponents <- function(GCAM_data, REMIND_scenario, input_folder, GCAM2ISO_MAPPING, logitexp_dir="GCAM_logit_exponents"){
  value <-  region <- ISO3 <- `.` <- Year <- Value <- time_price <- GDP_cap <- time.value.multiplier <- tranSubsector <- supplysector <- univocal_name <- NULL
  CONV_2005USD_1990USD = 0.67

  exp_folder = function(fname){
    file.path(input_folder, logitexp_dir, fname)
  }

  logit_exponent_FV = fread(exp_folder("FV_logitexponent.csv"))
  logit_exponent_VS1 = fread(exp_folder("VS1_logitexponent.csv"))
  logit_exponent_S1S2 = fread(exp_folder("S1S2_logitexponent.csv"))
  logit_exponent_S2S3 = fread(exp_folder("S2S3_logitexponent.csv"))
  logit_exponent_S3S = fread(exp_folder("S3S_logitexponent.csv"))
  ## load VOT factors and speed
  vott_all = GCAM_data[["vott_all"]]
  speed = GCAM_data[["speed"]]

  ## load logit categories table
  logit_category = GCAM_data[["logit_category"]]
  ## calculate VOT
  GDPppp_country = calcOutput("GDPppp", aggregate = F)[,, REMIND_scenario, pmatch=TRUE]
  POP_country = calcOutput("Population", aggregate = F)[,, as.numeric(gsub("\\D", "", REMIND_scenario)),pmatch=TRUE]
  GDPcap = as.data.table(GDPppp_country*CONV_2005USD_1990USD/POP_country) ## GDP_cap in [1990$/person/year]
  GDPcap = GDPcap[,.(entry_name="GDP_cap", iso = ISO3, year = as.numeric(as.character(gsub("y", "", Year))), GDP_cap = value)]

  gdpdt = as.data.table(GDPppp_country[,,REMIND_scenario, pmatch=TRUE])[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(gdpdt, c("ISO3", "value"), c("iso", "weight"))

  GDPcap=aggregate_dt(GDPcap,
                      mapping = GCAM2ISO_MAPPING,
                      valuecol = "GDP_cap",
                      datacols = "entry_name",
                      weights = gdpdt)

  vott_all = merge(vott_all, GDPcap, all = TRUE, by = "region", allow.cartesian = TRUE) #for each time step and each region
  vott_all = merge (vott_all, speed, all = FALSE, by = c("region", "year", "tranSubsector", "supplysector"))
  WEEKS_PER_YEAR = 50
  HOURS_PER_WEEK = 40
  vott_all[, time_price := GDP_cap                             ## [1990$/person/year]
                           *time.value.multiplier              ## [1990$/person/year] CHECK!
                           /(HOURS_PER_WEEK* WEEKS_PER_YEAR)/  ## [1990$/h]
                           speed]                              ## [1990$/km]

  value_of_time = vott_all[,.(region, year, time_price, tranSubsector, supplysector)]


  ## Ceate level specific VOT data tables

  ## FV

  value_of_time_LDV = value_of_time[supplysector %in% c("trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W"),
                                    .(vehicle_type = tranSubsector, subsector_L1 = supplysector, region, year, time_price)]

  value_of_time_buses_lower = value_of_time[supplysector == "trn_pass_road_bus_tmp_subsector_L1",
                                            .(vehicle_type = tranSubsector, subsector_L1 = supplysector, region, year, time_price)]

  value_time_FV = merge(value_of_time_LDV,value_of_time_buses_lower,all=TRUE)
  value_time_FV = value_time_FV[!is.na(time_price),]

  ## VS1

  value_time_VS1 = data.table(subsector_L1 = character(), vehicle_type = character(), region = character(), year = numeric(), time_price = numeric())

  ## S1S2

  value_time_S1S2 = value_of_time[tranSubsector %in% c("Three-Wheeler"),
                                  .(subsector_L2 = supplysector, subsector_L1 = tranSubsector, region, year, time_price)]

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
                price_nonmot = price_nonmot)

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
  newtech = NULL
  for (i in unique(GCAM_output$tech_output$region)) {
    electric = GCAM_output$tech_output[(subsector_L3 %in% c("trn_freight_road")|
                                                     subsector_L2 %in% c("Bus", "trn_pass_road_bus")) & technology == "Liquids" & region == i][, c("technology", "tech_output") := list("Electric", 0)]
    hydrogen = GCAM_output$tech_output[(subsector_L3 %in% c("trn_freight_road")|
                                                     subsector_L2 %in% c("Bus", "trn_pass_road_bus")) & technology == "Liquids" & region == i][, c("technology", "tech_output") := list("FCEV", 0)]
    newtech = rbind(newtech, electric, hydrogen)
  }

  GCAM_output$tech_output = rbind(GCAM_output$tech_output, newtech)

  ## add 0 plug-in hybrids for years 1990-2010
  GCAM_output$tech_output = rbind(GCAM_output$tech_output, GCAM_output$tech_output[technology == "Hybrid Liquids"][, c("technology", "tech_output") := list("Hybrid Electric", 0)])


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

  ## === Correct & Integrate costs === ##
  ## costs of non fuel price LDV have to be scaled as well
  ## costs in 2010
  ## 1.12 [2017USD/pkm] = 0.59 [1990USD/pkm] BEV car
  ## 0.78 [2017USD/pkm] = 0.42 [1990USD/pkm] HV and ICE car
  ## 2.04 [2017USD/pkm] = 1.08 [1990USD/pkm] FCEV car
  ## costs in 2030
  ## 0.58 [2017USD/pkm] = 0.31 [1990USD/pkm] BEV car
  ## 0.58 [2017USD/pkm] = 0.31 [1990USD/pkm] HV and ICE car
  ## 0.89 [2017USD/pkm] = 0.47 [1990USD/pkm] FCEV car
  ## costs in 2050
  ## 0.52 [2017USD/pkm] = 0.28 [1990USD/pkm] BEV car
  ## 0.54 [2017USD/pkm] = 0.29 [1990USD/pkm] HV and ICE car
  ## 0.54 [2017USD/pkm] = 0.29 [1990USD/pkm] FCEV car
  ## value in 2100 is assumed equal to 2050
  BEVs = data.table(year = c(2010, 2030, 2050, 2100), cost = c(1.53, 0.60, 0.56, 0.56), technology = rep("BEV", 4))
  FCEVs = data.table(year = c(2010, 2030, 2050, 2100), cost = c(2.10, 0.91, 0.56, 0.54), technology = rep("FCEV", 4))
  PHEVs = data.table(year = c(2010, 2030, 2050, 2100), cost = c(0.80, 0.59, 0.54, 0.54), technology = rep("Hybrid Electric", 4))
  ## values for Hybrid Liquids, Liquids and NG are estimated
  ## liquids are ~ 40% of a BEV car today, see https://www.sciencedirect.com/science/article/abs/pii/S0306261916307206?via%3Dihub
  ## hybrid liquids are 0.7 always
  ## NG are 0.7 always
  ICEs = data.table(year = c(2010, 2030, 2050, 2100), cost = rep(0.6, 4), technology = rep("Liquids", 4))
  NGVs = data.table(year = c(2010, 2030, 2050, 2100), cost = rep(0.7, 4), technology = rep("NG", 4))
  HLVs = data.table(year = c(2010, 2030, 2050, 2100), cost = rep(0.7, 4), technology = rep("Hybrid Liquids", 4))

  costs_zhang = rbind(BEVs, FCEVs, PHEVs, ICEs, NGVs, HLVs)
  ## assume linear decrease in non-fuel price between the temporal time steps
  costs_zhang = approx_dt(costs_zhang, unique(NEcost$non_energy_cost$year),
                         xcol = "year",
                         ycol = "cost",
                         idxcols = c("technology"),
                         extrapolate = TRUE)


  ## find the ratio in prices between a compact car and any other LDV (e.g. ICE: CompactCar/MidsizeCar)
  inspect_ratios = NEcost$non_energy_cost[region %in% c("China") & subsector_L1 %in% c("trn_pass_road_LDV_4W") & type =="normal", ]
  inspect_ratios[, ratio := non_fuel_price/ non_fuel_price[vehicle_type == "Compact Car" & year == 2015], by = c("technology")]


  ## use Zhang for a Midsize Car, and scale up/down for any other category
  for (tech in unique(costs_zhang[,technology])) {
    inspect_ratios[technology == tech, non_fuel_price := ratio*costs_zhang[technology == tech, cost]]
  }

  inspect_ratios[, ratio:= NULL]

  ## costs for the adnvanced and middle models assumed to be 20% more and 40% more
  inspect_ratios = rbind(inspect_ratios,
                         inspect_ratios[technology == "Liquids", ][, non_fuel_price := 1.4*non_fuel_price, by = c("vehicle_type")][, type:= "advanced"],
                         inspect_ratios[technology == "Liquids", ][, non_fuel_price := 1.2*non_fuel_price, by = c("vehicle_type")][, type:= "middle"])

  NEcost$non_energy_cost = rbind(NEcost$non_energy_cost[!(region == "China" & subsector_L1 == "trn_pass_road_LDV_4W")], inspect_ratios)

  setnames(inspect_ratios, old = "non_fuel_price", new = "tot_nonfuelprice")
  ## scale up of the same values the costs split out TODO
  costsSplit = NEcost$non_energy_cost_split[region =="China" & subsector_L2 == "trn_pass_road_LDV"]
  costsSplit = merge(costsSplit, inspect_ratios, by = c("region", "year", "vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "type"))
  costsSplit[, non_fuel_price := non_fuel_price*tot_nonfuelprice/sum(non_fuel_price), by=c("year","vehicle_type","technology", "type")]
  costsSplit[, tot_nonfuelprice := NULL]
  NEcost$non_energy_cost_split = rbind(NEcost$non_energy_cost_split[!(region == "China" & subsector_L1 == "trn_pass_road_LDV_4W")],
                                       costsSplit)

  # costs for LDV in USA and EU:
  # https://www.quora.com/Why-are-cars-considerably-cheaper-in-the-US-than-in-Europe-for-the-same-level-of-functionality

  ## Create values for plug-in hybrids costs: add plug-in hybrids to all countries, using the same ratio as Hybrid Liquids for each country (ref: EU-15)

  ## costs
  inspect_ratios_costs = NEcost$non_energy_cost[technology %in% c("Hybrid Liquids"),]
  inspect_ratios_costs[, ratio := non_fuel_price/non_fuel_price[region == "EU-15"], by = c("year","technology", "vehicle_type", "type")]
  inspect_ratios_costs = inspect_ratios_costs[, ratio := ifelse(is.na(ratio), mean(ratio, na.rm = TRUE), ratio), by = c("region", "technology", "year", "type")] ## if vehicle type is not originally in EU-15

  inspect_ratios_costs[, c("technology", "non_fuel_price") := NULL]

  inspect_ratios_costs = merge(inspect_ratios_costs, NEcost$non_energy_cost[technology %in% c("Hybrid Electric") & region == "EU-15", ][, c("region", "technology") := NULL], all = TRUE)
  inspect_ratios_costs = inspect_ratios_costs[, non_fuel_price := ifelse(is.na(non_fuel_price), mean(non_fuel_price, na.rm = TRUE), non_fuel_price), by = c("region", "year")] ## if vehicle type is not originally in EU-15
  inspect_ratios_costs[, non_fuel_price := ratio*non_fuel_price]
  inspect_ratios_costs[, c("technology",  "ratio") := list("Hybrid Electric", NULL)]

  NEcost$non_energy_cost = rbind(NEcost$non_energy_cost, inspect_ratios_costs[!(region %in% c("EU-15", "EU-12"))|(region %in% c("EU-15", "EU-12") & vehicle_type == "Mini Car")])


  ## costs split
  inspect_ratios_costs = NEcost$non_energy_cost_split[technology %in% c("Hybrid Liquids") & region != "EU-12", ] ## EU-12 is already present, so we exclude it
  inspect_ratios_costs[, ratio := non_fuel_price/non_fuel_price[region == "EU-15"], by=c("year","technology", "vehicle_type", "price_component")]
  inspect_ratios_costs[, ratio := ifelse(is.na(ratio), mean(ratio, na.rm = TRUE), ratio), by = c("region", "technology", "year")] ## if vehicle type is not originally in EU-15

  inspect_ratios_costs[, c("technology", "non_fuel_price") := NULL]

  inspect_ratios_costs = merge(inspect_ratios_costs, NEcost$non_energy_cost_split[technology %in% c("Hybrid Electric") & region == "EU-15", ][, c("region", "technology") := NULL], all = TRUE)
  inspect_ratios_costs = inspect_ratios_costs[, non_fuel_price := ifelse(is.na(non_fuel_price), mean(non_fuel_price, na.rm = TRUE), non_fuel_price), by = c("region", "year", "price_component")] ## if vehicle type is not originally in EU-15
  inspect_ratios_costs[, non_fuel_price := ratio*non_fuel_price]
  inspect_ratios_costs[, c("technology",  "ratio") := list("Hybrid Electric", NULL)]

  NEcost$non_energy_cost_split = rbind(NEcost$non_energy_cost_split, inspect_ratios_costs[!(region %in% c("EU-15", "EU-12"))|(region %in% c("EU-15", "EU-12") & vehicle_type == "Mini Car")])


  ## === Correct & Integrate energy intensity == ##
  ## intensity for electric buses, electric trucks, electric aviation, electric shipping
  newtech = NULL

  for (i in unique(GCAM_output$conv_pkm_mj$region)) {
    electric = GCAM_output$conv_pkm_mj[(subsector_L3 %in% c("trn_freight_road")|
                                           subsector_L2 %in% c("Bus", "trn_pass_road_bus")) & technology == "Liquids" & region == i][, c("technology", "conv_pkm_MJ", "sector_fuel") := list("Electric", 0, "elect_td_trn")]
    hydrogen = GCAM_output$conv_pkm_mj[(subsector_L3 %in% c("trn_freight_road")|
                                           subsector_L2 %in% c("Bus", "trn_pass_road_bus")) & technology == "Liquids" & region == i][, c("technology", "conv_pkm_MJ", "sector_fuel") := list("FCEV", 0, "H2 enduse")]
    liquids = GCAM_output$conv_pkm_mj[(subsector_L3 %in% c("trn_freight_road")|
                                          subsector_L2 %in% c("Bus", "trn_pass_road_bus")) & technology == "Liquids" & region == i]
    newtech = rbind(newtech,electric, liquids, hydrogen)
  }

  newtech[subsector_L1 =="trn_freight_road_tmp_subsector_L1", conv_pkm_MJ := ifelse(technology == "Electric", 0.3*conv_pkm_MJ[technology=="Liquids"], conv_pkm_MJ), by = c("region", "year", "vehicle_type")]
  newtech[subsector_L2 %in% c("Bus", "trn_pass_road_bus"), conv_pkm_MJ := ifelse(technology == "Electric", 0.3*conv_pkm_MJ[technology=="Liquids"], conv_pkm_MJ), by = c("region", "year", "vehicle_type")]
  newtech[subsector_L1 =="trn_freight_road_tmp_subsector_L1", conv_pkm_MJ := ifelse(technology == "FCEV", 3*conv_pkm_MJ[technology=="Liquids"], conv_pkm_MJ), by = c("region", "year", "vehicle_type")]
  newtech[subsector_L2 %in% c("Bus", "trn_pass_road_bus"), conv_pkm_MJ := ifelse(technology == "FCEV", 3*conv_pkm_MJ[technology=="Liquids"], conv_pkm_MJ), by = c("region", "year", "vehicle_type")]

  GCAM_output$conv_pkm_mj = rbind(GCAM_output$conv_pkm_mj, newtech[technology!="Liquids"])

  ## add plug-in hybrids to all countries, using the same ratio as Hybrid Liquids for each country (ref: EU-15)
  inspect_ratios_eff = GCAM_output$conv_pkm_mj[technology %in% c("Hybrid Liquids"), ]
  inspect_ratios_eff[, ratio := conv_pkm_MJ/conv_pkm_MJ[region == "EU-15"], by = c("year", "technology", "vehicle_type")]
  inspect_ratios_eff = inspect_ratios_eff[, ratio := ifelse(is.na(ratio), mean(ratio, na.rm = TRUE), ratio), by = c("region", "technology", "year")] ## if vehicle type is not originally in EU-15

  inspect_ratios_eff[, c("sector_fuel", "technology", "conv_pkm_MJ") := NULL]

  inspect_ratios_eff = merge(inspect_ratios_eff, GCAM_output$conv_pkm_mj[technology %in% c("Hybrid Electric") & region == "EU-15", ][, c("region", "technology", "sector_fuel") := NULL], all = TRUE)
  inspect_ratios_eff = inspect_ratios_eff[, conv_pkm_MJ := ifelse(is.na(conv_pkm_MJ), mean(conv_pkm_MJ, na.rm = TRUE), conv_pkm_MJ), by = c("region", "year")] ## if vehicle type is not originally in EU-15
  inspect_ratios_eff[, conv_pkm_MJ := ratio*conv_pkm_MJ]
  inspect_ratios_eff[, c("technology", "sector_fuel", "ratio") := list("Hybrid Electric", "Liquids-Electricity", NULL)]

  GCAM_output$conv_pkm_mj = rbind(GCAM_output$conv_pkm_mj, inspect_ratios_eff[!region %in% c("EU-15", "EU-12", "Europe Non EU", "European Free Trade Association")])


  ## costs are set to constant for Liq and NG ICE
  # NEcost$non_energy_cost[subsector_L2 == "trn_pass_road_LDV" & technology %in% c("BEV"), ratio := non_fuel_price/non_fuel_price[year == 2100], by = c("region", "technology", "vehicle_type")]
  # NEcost$non_energy_cost_split[subsector_L2 == "trn_pass_road_LDV" & technology %in% c("Liquids", "NG"), non_fuel_price := non_fuel_price[year == 2015], by = c("region", "technology", "vehicle_type", "cost_component")]
  # already more or less in the input data: OK


  ## public buses in china are very cheap: http://www.expatfocus.com/taking-public-transport-in-china
  # Public buses in cities are the most common and popular form of public transport. Public bus fares in China are extremely cheap and usually cost a flat RMB1 or RMB 2 (US$0.15 to US$0.25). You pay the same price regardless of the distance you travel.
  ## same holds for India
  #http://www.mytravelcost.com/India/prices-public_transport/

  ## === Substitute lambda === ##
  ## logit exponent is based on Givord et al (see paper)
  logitexp$logit_exponent_FV[, logit.exponent := ifelse(logit.exponent==-8,-4,logit.exponent)]

  return(list(GCAM_output = GCAM_output,
              NEcost = NEcost,
              logitexp = logitexp))
}

