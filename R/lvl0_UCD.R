#' Load the UCD transportation database
#'
#' Final values:
#'   non fuel price in 2005$/vkm
#'   annual mileage in vkt/veh/yr (vehicle km traveled per year),
#'
#' @param input_folder folder hosting raw data
#' @param fcr_veh annualization factor for LDVs
#' @param years time steps
#' @param UCD_dir directory with UCD data
#' @return non fuel price and annual mileage of vehicles
#' @author Marianna Rottoli
#'
#' @importFrom utils read.csv
#' @importFrom rmndt approx_dt disaggregate_dt aggregate_dt


lvl0_loadUCD <- function(input_folder, fcr_veh, years, UCD_dir="UCD"){
  subsector_L1 <- vehicle_type <- size.class <- UCD_region <- scenario <- `.` <- technology <- vehicle_type_PSI <- tot_purchase_euro2017 <- y2040 <- y2015 <- y2100 <- variable <- region <- EDGE_category <- value <- Xyear <- UCD_region <- size.class <- mileage <- UCD_technology <- Capital_costs_purchase <- non_fuel_cost <- non_fuel_OPEX <- Operating_subsidy <- Operating_costs_tolls <- Capital_costs_total <- Capital_costs_other <- Capital_costs_infrastructure <- CAPEX_and_non_fuel_OPEX <- CAPEX <- Operating_costs_maintenance <- Operating_costs_registration_and_insurance <- NULL
  ## load database UCD transportation
  UCD2iso=fread(file.path(input_folder, UCD_dir, "mapping_UCDdb_ISO.csv"))
  logit_category=GCAM_data[["logit_category"]]
  UCD_transportation_database = fread(file.path(input_folder, UCD_dir, "UCD_transportation_database.csv"), stringsAsFactors=F, header = TRUE)
  ## simplify costs removing non used data
  UCD_transportation_database = UCD_transportation_database[!size.class %in% c("Three-Wheeler", "Heavy Bus", "Light Bus")]
  ## read load factor from the previous step, FIXME improper nmes again, not clear who should contain
  load_factor=GCAM_data[["load_factor"]]

  ## list all 4wheelers vehicle types
  list4w = unique(GCAM_data$conv_pkm_mj[subsector_L1 == "trn_pass_road_LDV_4W", vehicle_type])
  ## define years on which the approximation has to work
  yrs = c(1990, seq(2010, 2100, 5))

  UCD_cost <- UCD_transportation_database[unit %in% c("2005$/veh/yr", "2005$/veh", "2005$/vkt"),]
  UCD_cost <- melt(UCD_cost, measure.vars = c("2005", "2020", "2035", "2050", "2065", "2080", "2095"), variable.name = "year" )

  #Where the unit is $/vkt, the various cost components are ready for aggregation
  #Where the unit is $/veh, convert to $/veh/yr using an exogenous fixed charge rate
  UCD_cost[unit == "2005$/veh", value := value*fcr_veh ]
  UCD_cost[unit == "2005$/veh", unit := "2005$/veh/yr"]

  #Next, match in the number of km per vehicle per year in order to calculate a levelized cost (per vkm)
  UCD_vkm_veh <- UCD_transportation_database[variable == "annual travel per vehicle",][, c("variable", "unit", "UCD_technology", "UCD_fuel") := NULL]
  UCD_vkm_veh <- melt(UCD_vkm_veh, measure.vars = c("2005", "2020", "2035", "2050", "2065", "2080", "2095"), variable.name = "year" )
  setnames(UCD_vkm_veh, old = "value", new = "vkm.veh")

  ## on ISO level
  setnames(UCD_cost, old="UCD_region", new="region")
  UCD_cost[region =="Australia_NZ", region := "Australia NZ"]
  UCD_cost[, year := as.numeric(as.character(year))]
  UCD_cost = UCD_cost[!UCD_technology %in% c("LA-BEV", "Hybrid Liquids", "Tech-Adv-Liquid", "Tech-Adv-Electric")]
  UCD_cost[, c("UCD_fuel"):=NULL]

  UCD_cost[size.class %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), size.class := "Truck (0-3.5t)"]
  UCD_cost[size.class %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), size.class := "Truck (7.5t)"]
  UCD_cost[size.class %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)"), size.class := "Truck (18t)"]
  UCD_cost[size.class %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), size.class := "Truck (26t)"]
  UCD_cost[size.class %in% c("Truck (>32t)"), size.class := "Truck (40t)"]
  UCD_cost=UCD_cost[,.(value = mean(value)), by = c("region", "UCD_technology", "UCD_sector", "mode", "size.class", "variable", "unit", "year")]
  UCD_cost[, vehicle_type := size.class]
  UCD_cost[mode == "Rail", vehicle_type := paste0(UCD_sector, " ", mode, "_tmp_vehicletype")]
  UCD_cost[mode == "Air International", vehicle_type := paste0("International Aviation", "_tmp_vehicletype")]
  UCD_cost[mode == "Air Domestic", vehicle_type := paste0("Domestic Aviation", "_tmp_vehicletype")]
  UCD_cost[mode == "Ship International", vehicle_type := paste0("International Ship", "_tmp_vehicletype")]
  UCD_cost[mode == "Ship Domestic", vehicle_type := paste0("Domestic Ship", "_tmp_vehicletype")]
  UCD_cost[mode %in% c("Bus", "HSR"), vehicle_type := paste0(mode, "_tmp_vehicletype")]
  UCD_cost[,c("UCD_sector", "mode", "size.class") := NULL]

  UCD_cost = approx_dt(UCD_cost,
                       xdata = years,
                       xcol = "year",
                       ycol = "value",
                       extrapolate = TRUE,
                       idxcols = c("region", "UCD_technology", "vehicle_type","variable", "unit"))
  setnames(UCD2iso, old="UCD_region", new="region")

  UCD_cost = disaggregate_dt(UCD_cost,
                             UCD2iso,
                             valuecol = "value",
                             weights = NULL,
                             datacols = c("UCD_technology", "vehicle_type", "variable", "year", "unit"))

  setnames(UCD_vkm_veh, old="UCD_region", new="region")
  UCD_vkm_veh[region =="Australia_NZ", region := "Australia NZ"]
  UCD_vkm_veh[, year := as.numeric(as.character(year))]

  UCD_vkm_veh[size.class %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), size.class := "Truck (0-3.5t)"]
  UCD_vkm_veh[size.class %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), size.class := "Truck (7.5t)"]
  UCD_vkm_veh[size.class %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)"), size.class := "Truck (18t)"]
  UCD_vkm_veh[size.class %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), size.class := "Truck (26t)"]
  UCD_vkm_veh[size.class %in% c("Truck (>32t)"), size.class := "Truck (40t)"]
  UCD_vkm_veh=UCD_vkm_veh[,.(vkm.veh = mean(vkm.veh)), by = c("region", "UCD_sector", "mode", "size.class", "year")]

  UCD_vkm_veh[, vehicle_type := size.class]
  UCD_vkm_veh[mode == "Rail", vehicle_type := paste0(mode, UCD_sector, "_tmp_vehicletype")]
  UCD_vkm_veh[mode == "Air International", vehicle_type := paste0("International Aviation", "_tmp_vehicletype")]
  UCD_vkm_veh[mode == "Air Domestic", vehicle_type := paste0("Domestic Aviation", "_tmp_vehicletype")]
  UCD_vkm_veh[mode == "Ship International", vehicle_type := paste0("International Ship", "_tmp_vehicletype")]
  UCD_vkm_veh[mode %in% c("Bus", "HSR"), vehicle_type := paste0(mode, "_tmp_vehicletype")]
  UCD_vkm_veh[,c("UCD_sector", "mode", "size.class") := NULL]

  UCD_vkm_veh = approx_dt(UCD_vkm_veh,
                       xdata = years,
                       xcol = "year",
                       ycol = "vkm.veh",
                       extrapolate = TRUE,
                       idxcols = c("region", "vehicle_type"))

  UCD_vkm_veh = disaggregate_dt(UCD_vkm_veh,
                                UCD2iso,
                                valuecol = "vkm.veh",
                                weights = NULL,
                                datacols = c("vehicle_type","year"))

  return(list(UCD_cost = UCD_cost, UCD_mileage = UCD_vkm_veh))

}

#' Load the PSI costs
#'
#' Final values:
#'   non fuel price in 2005USD annualized
#'
#' @param input_folder folder hosting raw data
#' @param fcr_veh annualization factor for LDVs
#' @param years time steps
#' @return purchase cost LDVs
#' @author Marianna Rottoli


lvl0_PSI_costs=function(input_folder, years, fcr_veh){
  ##load costs from PSI
  PSI_folder <- file.path(input_folder, "PSI")
  psi_costs=data.table(read_excel(file.path(PSI_folder,"Car model result_modified.xlsx"), ##file is mdified from the original one: names are copy pasted to simplify R data wrangling
                                  sheet="Vehicle","A1:X191"))

  ## select relevant columns and only the "current" values->2015 and the Baseline ones->2040 in a baseline scenario
  psi_costs = psi_costs[scenario %in% c("current","Baseline"),c("scenario", "technology", "vehicle_type_PSI", "Total1_(Euro)")]
  ## rename columns
  colnames(psi_costs)=c("scenario", "technology", "vehicle_type_PSI", "tot_purchase_euro2017")
  ## substitute the "scenario" with the year it stands for
  psi_costs[,scenario:=ifelse(scenario=="current", 2015, 2040)]

  psi_costs=psi_costs[,.(year=scenario, ## rename col scenario with year
                         technology,vehicle_type_PSI,
                         tot_purchasecost=as.numeric(tot_purchase_euro2017)   ## in 2017euro
                         *1.14                                                ## in 2017USD
                         *0.78                                                ## in 2005USD
                         *fcr_veh)]                                           ## in annualized 2005USD


  ##load mapping that matches PSI vehicle types with EDGE types
  mapping <- fread(file.path(PSI_folder, "mapping_PSI_EDGE.csv"), na.strings=c("","NA"))
  ##filter out the NA cells in the mapping-> PSI has more alternatives than EDGE
  mapping=mapping[!is.na(vehicle_type),]
  ## merge mapping and PSI database
  psi_costs=merge(psi_costs,mapping,all=FALSE,by="vehicle_type_PSI")[,-"vehicle_type_PSI"]
  psi_costs = psi_costs[!(technology %in% c("PHEV-c", "PHEV-e"))] ## PSI reports separately the electric and ICE running modes of a plug-in hybrid
  psi_costs[technology =="HEV-p", technology := "PHEV"]

  psi_costs = approx_dt(psi_costs,
                        xdata = years,
                        xcol = "year",
                        ycol = "tot_purchasecost",
                        idxcols = c("technology", "vehicle_type"),
                        extrapolate = TRUE)

  psi_costs[, technology:=ifelse(technology=="ICEV-g","NG",technology)]
  psi_costs[, technology:=ifelse(technology %in% c("ICEV-p","ICEV-d"),"Liquids",technology)]
  psi_costs[, technology:=ifelse(grepl("PHEV",technology),"Hybrid Electric",technology)]

  ## average on the EDGE category
  psi_costs=psi_costs[,.(tot_purchasecost=mean(tot_purchasecost)),by=c("technology","vehicle_type","year")]

  return(psi_costs = psi_costs)

}

#' Load the China truck costs
#'
#' Final values:
#'   non fuel price in 2005USD/vkm
#'
#' @param input_folder folder hosting raw data
#' @param years time steps
#' @return CAPEX and non-fuel OPEX conventional trucks
#' @author Marianna Rottoli


## function that loads and applies CAPEX costs for CHA conventional trucks. Final values in 2005USD/vkm
lvl0_CHNTrucksCosts=function(input_folder, years){
  ## load costs from CHA Trucks source file
  CHA_folder <- file.path(input_folder, "CHA_data")
  costs=data.table(fread(file.path(CHA_folder,"CHA_trucks.csv"), header = T))
  costs = melt(costs,
               id.vars = c("region", "sector", "mode", "size.class", "technology", "fuel", "variable", "unit"),
               measure.vars = c("2005", "2020", "2035", "2050", "2065", "2080", "2095"))
  costs[, region := "CHN"]
  setnames(costs, old = c("region", "variable.1", "size.class"), new = c("iso", "year", "vehicle_type"))
  costs = costs[,c("iso", "year", "technology", "variable", "vehicle_type", "value")]

  ## attribute right veh types
  costs[vehicle_type %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), vehicle_type := "Truck (0-3.5t)"]
  costs[vehicle_type %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), vehicle_type := "Truck (7.5t)"]
  costs[vehicle_type %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)"), vehicle_type := "Truck (18t)"]
  costs[vehicle_type %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), vehicle_type := "Truck (26t)"]
  costs[vehicle_type %in% c("Truck (>32t)"), vehicle_type := "Truck (40t)"]
  costs=costs[,.(value = mean(value)), by = c("iso", "year", "technology", "variable", "vehicle_type")]
  costs[, year := as.numeric(as.character(year))]

  costs = approx_dt(costs,
                    xdata = years,
                    xcol = "year",
                    ycol = "value",
                    idxcols = c("technology", "vehicle_type", "variable", "iso"),
                    extrapolate = TRUE)

  return(CHN_costs = costs)
}


#' Load the PSI energy intensity
#'
#' Final values:
#'
#'
#' @param input_folder folder hosting raw data
#' @param years time steps
#' @return CAPEX and non-fuel OPEX conventional trucks
#' @author Marianna Rottoli

## function that loads and applies CAPEX costs for CHA conventional trucks. Final values in 2005USD/vkm
lvl0_PSIint=function(input_folder, PSI_dir="PSI", years){
  psi_file <- function(fname){
    file.path(input_folder, PSI_dir, fname)
  }

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
                          xdata = years,
                          xcol = "year",
                          ycol = "conv_pkm_MJ",
                          idxcols = c("technology", "vehicle_type", "sector_fuel"),
                          extrapolate = TRUE)

  ## add logit_category for Hybrid Electric
  logit_category = rbind(logit_category, logit_category[technology == "BEV"][, technology := "Hybrid Electric"])

  ##merge with logit_category
  LDV_PSI_int=merge(logit_category,LDV_PSI_int,by=c("technology","vehicle_type"))[,-"univocal_name"]
  ## add missing 4Ws
  LDV_PSI_int = rbind(LDV_PSI_int,
                      LDV_PSI_int[vehicle_type == "Large Car and SUV"][, vehicle_type := "Large Car"],
                      LDV_PSI_int[vehicle_type == "Large Car and SUV"][, vehicle_type := "Light Truck and SUV"]
  )


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

  ## approx to the whole time range
  Truck_PSI_int = approx_dt(Truck_PSI_int,
                            xdata = years,
                            xcol = "year",
                            ycol = "conv_pkm_MJ",
                            idxcols = c("technology", "vehicle_type"),
                            extrapolate = TRUE)


  return(list(LDV_PSI_int = LDV_PSI_int, Truck_PSI_int = Truck_PSI_int))

}



lvl0_AltHDV=function(UCD_output){

  costs = copy(UCD_output$UCD_cost)
  trucks = unique(costs[grepl("Truck", vehicle_type) & !grepl("Light", vehicle_type), vehicle_type])
  buses = unique(costs[grepl("Bus", vehicle_type), vehicle_type])

  electric = costs[vehicle_type %in% c(trucks, buses) & UCD_technology == "Liquids" & variable == "CAPEX and non-fuel OPEX"][, "UCD_technology" := "Electric"]
  hydrogen = costs[vehicle_type %in% c(trucks, buses) & UCD_technology == "Liquids" & variable == "CAPEX and non-fuel OPEX"][, "UCD_technology" := "FCEV"]
  altcost = rbind(electric, hydrogen)

  yeartarget_early = 2030  ## target year for electric trucks and electric and hydrogen buses
  yeartarget_late = 2035  ## target year for FCEV trucks

  ## cost of electric truck is 60% more than a as conventional truck today
  altcost[vehicle_type %in% trucks & year <= 2020 & UCD_technology == "Electric", value := 1.6*value, by = c("iso", "year", "vehicle_type")]
  ## cost of a FCEV truck is 80% more than a as conventional truck today
  altcost[vehicle_type %in% trucks & year <= 2020 & UCD_technology == "FCEV", value := 1.8*value, by = c("iso", "year", "vehicle_type")]

  ## for electric and FCEV trucks, in between 2020 and the target year, the cost follows a linear trend
  altcost[vehicle_type %in% trucks & year <= yeartarget_early & year >= 2020 & UCD_technology %in% c("Electric"),
          value := (value[year == 2020] - value[year == yeartarget_early])*(1 - (year - 2020)/(yeartarget_early - 2020)) + value[year == yeartarget_early],
          by = c("iso", "UCD_technology", "vehicle_type")]

  altcost[vehicle_type %in% trucks & year <= yeartarget_late & year >= 2020 & UCD_technology %in% c("FCEV"),
          value := (value[year == 2020] - value[year == yeartarget_late])*(1 - (year - 2020)/(yeartarget_late - 2020)) + value[year == yeartarget_late],
          by = c("iso", "UCD_technology", "vehicle_type")]

  ## cost of electric and H2 buses is 40% more of a conventional bus today
  altcost[vehicle_type %in% buses & year <= 2020, value := 1.4*value, by = c("iso", "year", "vehicle_type")]

  ## for electric and FCEV buses, in between 2020 and target year the cost follows a linear trend
  altcost[vehicle_type %in% buses & year <= yeartarget_early & year >= 2020,
          value := (value[year == 2020]-value[year == yeartarget_early])*(1 - (year - 2020)/(yeartarget_early - 2020)) + value[year == yeartarget_early],
          by = c("iso", "UCD_technology", "vehicle_type")]

  ## assumed same other costs components to be equal to ICE trucks/buses
  tmp = rbind(costs[vehicle_type %in% c(trucks, buses) & variable != "CAPEX and non-fuel OPEX" & UCD_technology == "Liquids"][, "UCD_technology" := "Electric"],
              costs[vehicle_type %in% c(trucks, buses) & variable != "CAPEX and non-fuel OPEX" & UCD_technology == "Liquids"][, "UCD_technology" := "FCEV"])


  ## add hydrogen airplanes
  h2air = costs[vehicle_type == "Domestic Aviation_tmp_vehicletype" & UCD_technology == "Liquids"][, UCD_technology := "Hydrogen"]
  ## CAPEX of hydrogen airplanes is assumed today 5 times more expensive than a conventional airplane (i.e. not present in the market)
  h2air[vehicle_type %in% "Domestic Aviation_tmp_vehicletype" & year <= 2020,
        value := 5*value]
  ## following https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf
  ## maintenance costs are 50% higher than than a liquids fuelled airplane
  h2air[vehicle_type =="Domestic Aviation_tmp_vehicletype" & year >= 2020 & variable == "non-fuel OPEX",
          value := 1.5*value]
  ## for hydrogen airplanes, in between 2020 and 2040 the cost follows a linear trend, and reaches a value 30% higher than a liquids fuelled airplane
  h2air[vehicle_type =="Domestic Aviation_tmp_vehicletype" & year >= 2020 & variable == "CAPEX",
          value := ifelse(year <= 2040, value[year==2020] + (1.3*value[year == 2100]-value[year==2020]) * (year-2020) / (2100-2020), 1.3*value[year == 2100]),
          by = c("iso", "UCD_technology", "vehicle_type")]

  costs = rbind(altcost, tmp, h2air)

  return(costs)

}



#' Merge input data
#'
#' Final values:
#'
#'
#' @param input_folder folder hosting raw data
#' @param years time steps
#' @return
#' @author Marianna Rottoli

lvl0_mergeDat = function(UCD_output, EU_data, PSI_costs, altCosts, CHN_trucks, GCAM_data, PSI_int, smartlifestyle, REMIND2ISO_MAPPING){
  ## merge LF
  LF_EU = approx_dt(EU_data$LF_countries_EU,
                                      xdata = unique(GCAM_data$load_factor$year),
                                      xcol = "year",
                                      ycol = "loadFactor",
                                      idxcol = c("iso", "vehicle_type"),
                                      extrapolate = T)
  LF_EU = merge(LF_EU, GCAM_data[["logit_category"]], by = "vehicle_type", allow.cartesian = T, all.x = TRUE)[, univocal_name:= NULL]
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
                    idxcol = c("iso", "vehicle_type"),
                    extrapolate = T)

  setnames(AM_EU, old = "annual_mileage", new = "vkm.veh")

  AM = rbind(AM_EU, UCD_output$UCD_mileage[!(iso %in% unique(AM_EU$iso) & vehicle_type %in% unique(AM_EU$vehicle_type))])
  AM = merge(AM, GCAM_data[["logit_category"]], by = "vehicle_type", allow.cartesian = T, all.x = TRUE)[, univocal_name:= NULL]
  AM = AM[technology != "Hybrid Liquids" & year >= 1990]

  AM = rbind(AM,
             AM[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "BEV"][, technology := "Hybrid Electric"])
  eu_iso = c("DEU", "FRA", "ITA")
  ## calculate PSI costs in terms of 2005$/pkm annualized
  PSI_c = merge(PSI_costs, AM, all.x = TRUE, by = c("vehicle_type", "technology", "year"))
  PSI_c[, c("variable", "value") := list("Capital costs (purchase)", tot_purchasecost/vkm.veh)][, vkm.veh := NULL]
  PSI_c = rbind(PSI_c[iso %in% eu_iso], PSI_c[!iso %in% eu_iso & technology %in% c("BEV", "FCEV", "Hybrid Electric")])
  PSI_c[, tot_purchasecost := NULL]
  ## calculate UCD costs for LDVs in terms of 2005$/pkm annualized
  UCD_c = copy(UCD_output$UCD_cost)
  UCD_c = rbind(UCD_c,
                altCosts)

  logit_cat = copy(GCAM_data[["logit_category"]])
  logit_cat = rbind(logit_cat,
                    logit_cat[vehicle_type %in% unique(altCosts$vehicle_type) & vehicle_type != "Domestic Aviation_tmp_vehicletype"][, technology := "Electric"],
                    logit_cat[vehicle_type %in% unique(altCosts$vehicle_type) & vehicle_type != "Domestic Aviation_tmp_vehicletype"][, technology := "FCEV"],
                    logit_cat[vehicle_type == "Domestic Aviation_tmp_vehicletype"][, technology := "Hydrogen"])
  setnames(UCD_c, old = "UCD_technology", new = "technology")
  UCD_c = merge(UCD_c, AM[,c("iso", "vkm.veh", "year", "vehicle_type", "technology")], all = TRUE, by = c("vehicle_type", "technology", "year", "iso"))
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
  UCD_c = UCD_c[!(technology %in% c("BEV", "FCEV") & subsector_L1 == "trn_pass_road_LDV_4W" & variable %in% unique(PSI_c$variable))]
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
                costs[iso %in% unique(REMIND2ISO_MAPPING[region %in% c("SSA"), iso]) & vehicle_type == "Motorcycle (50-250cc)"][, vehicle_type := "Motorcycle (>250cc)"])  ## OAS has missing 40t

## manca GBR midsize cars LIQUIDS only (and other countries are missing Liq version of the required veh)
  ## merge PSI intensity, GCAM intensity and TRACCS intensity
  LDV_PSI_i = merge(PSI_int$LDV_PSI_int, LF, by = c("year", "vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"))
  ## convert into MJ/pkm
  LDV_PSI_i[, conv_pkm_MJ := conv_pkm_MJ/loadFactor] ## in MJ/pkm
  LDV_PSI_i[, loadFactor := NULL] ##  remove load factor
  ## merge the LDVs
  LDV_PSI_i = rbind(LDV_PSI_i[iso %in% eu_iso],
                    LDV_PSI_i[!iso %in% eu_iso & technology %in% c("BEV", "FCEV", "Hybrid Electric")],
                    GCAM_data[["conv_pkm_mj"]][!(subsector_L1=="trn_pass_road_LDV_4W" &
                                    technology %in% c("BEV", "FCEV", "Hybrid Electric")) &
                                  !(iso %in% eu_iso)])
  LDV_PSI_i[, sector_fuel := NULL]

  Truck_PSI_i = merge(PSI_int$Truck_PSI_int, LF[year %in% unique(PSI_int$Truck_PSI_int$year)], by = c("year", "vehicle_type", "technology"), all.x = T)
  Truck_PSI_i[, conv_pkm_MJ := conv_pkm_MJ/loadFactor]
  Truck_PSI_i = Truck_PSI_i[!is.na(conv_pkm_MJ)]
  Truck_PSI_i[, c("loadFactor") := NULL]

  ## approx to the whole time range
  Truck_PSI_i = approx_dt(Truck_PSI_i,
                            xdata = years,
                            xcol = "year",
                            ycol = "conv_pkm_MJ",
                            idxcols = c("iso", "technology", "vehicle_type", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                            extrapolate = TRUE)

  Truck_PSI_i = rbind(
    Truck_PSI_i,
    GCAM_data[["conv_pkm_mj"]][(subsector_L3 == "trn_freight_road"|subsector_L2 == "Bus") &
                      technology %in% c("Liquids", "NG"),
                      c("conv_pkm_MJ", "iso","year", "technology", "subsector_L1", "subsector_L2",
                         "subsector_L3", "sector", "vehicle_type")])

  ##merge with GCAM intensities and substitute all LDVs for EU and only alternative LDVs for other regions
  int = rbind(Truck_PSI_i,
              LDV_PSI_i,
              GCAM_data[["conv_pkm_mj"]][!subsector_L1 %in% c("trn_pass_road_LDV_4W", "trn_freight_road_tmp_subsector_L1","Bus_tmp_subsector_L1")][, sector_fuel := NULL])

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
  dem = dem[!(iso %in% unique(EU_data$dem_eurostat$iso))]
  demEU = merge(EU_data$dem_eurostat, int, by = c("vehicle_type", "technology", "iso", "year"))
  demEU[, tech_output := MJ/  ## in MJ
          conv_pkm_MJ*    ## in km
          1e-6][, c("MJ", "conv_pkm_MJ") := NULL] ## in million km
  dem = rbind(dem, demEU)

  int = merge(int, unique(dem[,c("iso", "vehicle_type")]), all.y = TRUE, by = c("iso", "vehicle_type"))


  return(list(costs = costs, int = int, LF = LF, AM = AM, dem = dem))


}
