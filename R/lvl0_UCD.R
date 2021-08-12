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

  #Next, match in the number of km per vehicle per year in order to calculate a levelized cost (per vkm)
  UCD_vkm_veh <- UCD_transportation_database[variable == "annual travel per vehicle",][, c("variable", "unit", "UCD_technology", "UCD_fuel") := NULL]
  UCD_vkm_veh <- melt(UCD_vkm_veh, measure.vars = c("2005", "2020", "2035", "2050", "2065", "2080", "2095"), variable.name = "year" )
  setnames(UCD_vkm_veh, old = "value", new = "vkm.veh")

  ## on ISO level
  setnames(UCD_cost, old="UCD_region", new="region")
  UCD_cost[region =="Australia_NZ", region := "Australia NZ"]
  UCD_cost[, year := as.numeric(as.character(year))]
  UCD_cost = UCD_cost[!UCD_technology %in% c("LA-BEV", "Hybrid Liquids", "Tech-Adv-Liquid", "Tech-Adv-Electric")]
  UCD_cost[, c("UCD_fuel", "unit"):=NULL]

  UCD_cost[size.class %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), size.class := "Truck (0-3.5t)"]
  UCD_cost[size.class %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), size.class := "Truck (7.5t)"]
  UCD_cost[size.class %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)"), size.class := "Truck (18t)"]
  UCD_cost[size.class %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), size.class := "Truck (26t)"]
  UCD_cost[size.class %in% c("Truck (>32t)"), size.class := "Truck (40t)"]
  UCD_cost=UCD_cost[,.(value = mean(value)), by = c("region", "UCD_technology", "UCD_sector", "mode", "size.class", "variable", "year")]

  UCD_cost[, vehicle_type := size.class]
  UCD_cost[mode == "Rail", vehicle_type := paste0(mode, UCD_sector, "_tmp_vehicletype")]
  UCD_cost[mode == "Air International", vehicle_type := paste0("International Aviation", "_tmp_vehicletype")]
  UCD_cost[mode == "Air Domestic", vehicle_type := paste0("Domestic Aviation", "_tmp_vehicletype")]
  UCD_cost[mode == "Ship International", vehicle_type := paste0("International Ship", "_tmp_vehicletype")]
  UCD_cost[mode %in% c("Bus", "HSR"), vehicle_type := paste0(mode, "_tmp_vehicletype")]
  UCD_cost[,c("UCD_sector", "mode", "size.class") := NULL]

  UCD_cost = approx_dt(UCD_cost,
                       xdata = years,
                       xcol = "year",
                       ycol = "value",
                       extrapolate = TRUE,
                       idxcols = c("region", "UCD_technology", "vehicle_type","variable"))
  setnames(UCD2iso, old="UCD_region", new="region")

  UCD_cost = disaggregate_dt(UCD_cost,
                             UCD2iso,
                             valuecol = "value",
                             weights = NULL,
                             datacols = c("UCD_technology", "vehicle_type", "variable", "year"))

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

