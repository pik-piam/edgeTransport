#' Load the UCD transportation database
#'
#' Loads and prepares the non_fuel prices.
#' It also load PSI-based purchase prices for EU.
#' Final values:
#'   non fuel price in 1990$/pkt (1990USD/tkm),
#'   annual mileage in vkt/veh/yr (vehicle km traveled per year),
#'   non_fuel_split in 1990$/pkt (1990USD/tkm)
#'
#' @param GCAM_data GCAM based data
#' @param GDP_country GDP ISO level
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param REMIND_scenario SSP scenario
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param input_folder folder hosting raw data
#' @param years time steps
#' @param UCD_dir directory with UCD data
#' @param enhancedtech switch activating optimistic development of alternative technologies
#' @param selfmarket_taxes switch activating decrease in registration taxes
#' @param rebates_febates activating rebates-feebates
#' @param techswitch technology at the center of the policy packages
#' @return non fuel price and annual mileage of vehicles
#' @author Marianna Rottoli
#'
#' @importFrom stats approx
#' @importFrom utils read.csv
#' @importFrom rmndt approx_dt disaggregate_dt aggregate_dt


lvl0_loadUCD <- function(GCAM_data, GDP_country, EDGE_scenario, REMIND_scenario, GCAM2ISO_MAPPING, input_folder, years, UCD_dir="UCD", enhancedtech, selfmarket_taxes, rebates_febates, techswitch){
  subsector_L1 <- vehicle_type <- size.class <- UCD_region <- scenario <- `.` <- technology <- vehicle_type_PSI <- tot_purchase_euro2017 <- y2040 <- y2015 <- y2100 <- variable <- region <- EDGE_category <- value <- Xyear <- UCD_region <- size.class <- mileage <- UCD_technology <- Capital_costs_purchase <- non_fuel_cost <- non_fuel_OPEX <- Operating_subsidy <- Operating_costs_tolls <- Capital_costs_total <- Capital_costs_other <- Capital_costs_infrastructure <- CAPEX_and_non_fuel_OPEX <- CAPEX <- Operating_costs_maintenance <- Operating_costs_registration_and_insurance <- NULL
  #==== Load data ====
  #readUCD
  #load database UCD transportation
  UCD2iso=fread(file.path(input_folder, UCD_dir, "mapping_UCDdb_ISO.csv"))
  logit_category=GCAM_data[["logit_category"]]
  UCD_transportation_database = fread(file.path(input_folder, UCD_dir, "UCD_transportation_database.csv"), stringsAsFactors=F, header = TRUE)
  ## simplify costs removing non used data
  UCD_transportation_database = UCD_transportation_database[!size.class %in% c("Three-Wheeler", "Heavy Bus", "Light Bus")]
  setnames(UCD_transportation_database, old = c("2005", "2020", "2035", "2050", "2065", "2080", "2095"), new = c("X2005", "X2020", "X2035", "X2050", "X2065", "X2080", "X2095"))
  ## read load factor from the previous step, FIXME improper nmes again, not clear who should contain
  load_factor=GCAM_data[["load_factor"]]

  ## list all 4wheelers vehicle types
  list4w = unique(GCAM_data$conv_pkm_mj[subsector_L1 == "trn_pass_road_LDV_4W", vehicle_type])
  ## define years on which the approximation has to work
  yrs = c(1990, seq(2010, 2100, 5))

  ## define depreciation rate
  discount_rate_veh = 0.05   #Consumer discount rate for vehicle purchases (PSI based values)
  nper_amort_veh = 15    #Number of periods (years) over which vehicle capital payments are amortized
  fcr_veh = discount_rate_veh + discount_rate_veh/(((1+discount_rate_veh)^nper_amort_veh)-1)

  ## function that adds the vehicle types entries that are not in the original UCD dataframe
  addvehiclestypes_UCD = function(dt, from, to, vehicle){
    dt=data.table(dt)
    for (r in to) {
      tmp=dt[size.class==vehicle & UCD_region == from,][,UCD_region:=r]
      dt=rbind(dt,tmp)
    }
    return(dt)
  }
  ## function that loads and applies purchase costs from the PSI database. Final values in 2005USD/vkm
  apply_PSI_costs=function(costs_UCD,fcr_veh,mileage_UCD){
    vkm.veh <- NULL
    mileage_UCD=data.table(mileage_UCD)
    costs_UCD=data.table(costs_UCD)

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
    ##cast
    psi_costs=dcast(psi_costs, technology + vehicle_type ~ year, value.var = "tot_purchasecost")
    setnames(psi_costs,old=c("2015","2040"),new=c("y2015","y2040"))
    psi_costs = psi_costs[!(technology %in% c("PHEV-c", "PHEV-e"))] ## PSI reports separately the electric and ICE running modes of a plug-in hybrid

    ## in the conservative scenario, conservative assumptions about costs of alternative technologies
    if (!(enhancedtech)) {
      psi_costs[technology %in% c("BEV", "FCEV", "HEV-p", "PHEV"), y2040 := y2040 + 0.8*(y2015-y2040), by = "technology"]
    }

    ## in the Hydrogen Hype scenario, costs of battery-based powertrain stay high
    if (enhancedtech & techswitch == "FCEV") {
      psi_costs[technology %in% c("BEV", "HEV-p", "PHEV"), y2040 := y2040 + 0.8*(y2015-y2040), by = "technology"]
    }

    ## in the Electric Era scenario, costs of FCEV powertrain stay high
    if (enhancedtech & techswitch == "BEV") {
      psi_costs[technology %in% c("FCEV"), y2040 := y2040 + 0.8*(y2015-y2040), by = "technology"]
    }

    psi_costs[, y2100 := 0.1*(y2040-y2015)/(2040-2015)*(2100-2040) + y2040] ## set a value for 2100 that is not present in PSI database. It's set to 1/10 of the decrease between 2015 and 2040 (quite conservative)

    ## interpolate
    tsteps = as.numeric(unique(gsub("X","", costs_UCD$Xyear)))
    for (t in tsteps[tsteps<2040 & tsteps > 2015]) {
      psi_costs[,paste0("y",t):=(y2015-y2040)*(1-(t-2015)/(2040-2015))+y2040]
    }

    for (t in tsteps[tsteps>2040 & tsteps <2100]) {
      psi_costs[,paste0("y",t):=(y2040-y2100)*(1-(t-2040)/(2100-2040))+y2100]
    }

    for (t in tsteps[tsteps<2015]){
      psi_costs[,paste0("y",t):=y2015]
    }

    for (t in tsteps[tsteps>2100]){
      psi_costs[,paste0("y",t):=y2100]
    }


    psi_costs = melt(psi_costs, id.vars = c("technology", "vehicle_type"),
                     measure.vars = paste0("y",tsteps))

    psi_costs[,variable:=as.numeric(gsub("y","",variable))]

    ## attribute the PSI costs to both Western Europe and Easter Europe
    psi_costs[,region:="Western Europe"]
    tmp=copy(psi_costs)
    tmp[,region:="Eastern Europe"]
    psi_costs=rbind(psi_costs,tmp)
    setnames(psi_costs,old="variable",new="year")
    psi_costs[, EDGE_category:=ifelse(technology=="ICEV-g","NG",NA)]
    psi_costs[, EDGE_category:=ifelse(technology %in% c("ICEV-p","ICEV-d"),"Liquids",EDGE_category)]
    psi_costs[, EDGE_category:=ifelse(grepl("PHEV",technology),"Hybrid Electric",EDGE_category)]
    psi_costs[, EDGE_category:=ifelse(is.na(EDGE_category),technology,EDGE_category)]

    ## average on the EDGE category
    psi_costs=psi_costs[,.(value=mean(value)),by=c("EDGE_category","region","vehicle_type","year")]

    ## rename columns so that the dt has the same dimensions as expected in the original dt with UCD data
    setnames(psi_costs,old=c("region","vehicle_type","EDGE_category"),
             new=c("UCD_region","size.class","UCD_technology"))

    psi_costs[,Xyear:=paste0("X",year)]
    psi_costs=psi_costs[,-"year"]

    ## divide by mileage
    mileage_UCD=mileage_UCD[UCD_region%in%c("Western Europe", "Eastern Europe") &
                                mode=="LDV_4W",
                                c("UCD_region","size.class", "Xyear", "vkm.veh")]
    mileage_UCD=mileage_UCD[,.(size.class=as.character(size.class),UCD_region,mileage=vkm.veh,Xyear)]

    psi_costs=merge(psi_costs,mileage_UCD,all.x = TRUE)
    ## TODO
    psi_costs=psi_costs[!is.na(value),]
    psi_costs=psi_costs[,value:=value/mileage][,-"mileage"]
    ## create a temporary dt that has original UCD data for the LDV4 purchase costs and EU regions
    tmp=costs_UCD[UCD_region %in% c("Western Europe", "Eastern Europe") & ## EU regions trends come from PSI
                  mode=="LDV_4W" &
                  variable=="Capital costs (purchase)" &
                  size.class %in% unique(psi_costs$size.class),]

    tmp = rbind(tmp, tmp[UCD_technology == "BEV"][, c("UCD_technology", "UCD_fuel") := list("Hybrid Electric", "Liquids-Electricity")])

    ## delete column with data that is going to come from PSI
    tmp[,c("value"):=NULL]

    ##merge with UCD prices and substitute EU
    tmp=merge(tmp,
              psi_costs,
              by=intersect(names(psi_costs),names(tmp)),all=TRUE)

    ## delete mini cars as they are added otherwise later (FIXME)
    tmp=tmp[size.class!="Mini Car",]

    ## attribute the other costs components from BEVs to Hybrid Electic for EU
    costs_UCD = rbind(costs_UCD, costs_UCD[mode %in% "LDV_4W" & UCD_region %in% c("Western Europe", "Eastern Europe") & UCD_technology == "BEV"][, c("UCD_technology", "UCD_fuel") := list("Hybrid Electric", "Liquids-Electricity")])

    ## merge back with the original database
    costs_UCD=merge(costs_UCD[!(UCD_region %in% c("Western Europe", "Eastern Europe") & ## EU regions trends come from PSI
                              mode=="LDV_4W" &
                              variable=="Capital costs (purchase)" &
                              size.class %in% unique(psi_costs$size.class)),],
                    tmp,all=TRUE,by=names(tmp))

    costs_UCD[,size.class:=as.character(size.class)]
    return(costs_UCD)
}

  calc_non_energy_price=function(UCD_transportation_database, GCAM2ISO_MAPPING){
    loadFactor <- price_component <- X_UCD_years <- non_fuel_price <- UCD_fuel <- type <- UCD_sector <- vkm.veh <- ID <- NULL
    subsector_L3 <- NULL
    #=== Calculations ====
    UCD_cost <- UCD_transportation_database[unit %in% c("2005$/veh/yr", "2005$/veh", "2005$/vkt"),]

    UCD_cost <- melt(UCD_cost, measure.vars = X_UCD_years, variable.name = "Xyear" )

    #Where the unit is $/vkt, the various cost components are ready for aggregation
    #Where the unit is $/veh, convert to $/veh/yr using an exogenous fixed charge rate
    UCD_cost[unit == "2005$/veh", value := value*fcr_veh ]
    UCD_cost[unit == "2005$/veh", unit := "2005$/veh/yr"]

    #Next, match in the number of km per vehicle per year in order to calculate a levelized cost (per vkm)
    UCD_vkm_veh <- UCD_transportation_database[variable == "annual travel per vehicle",][, c("variable", "unit", "UCD_technology", "UCD_fuel") := NULL]
    UCD_vkm_veh <- melt(UCD_vkm_veh, measure.vars = X_UCD_years, variable.name = "Xyear" )
    setnames(UCD_vkm_veh, old = "value", new = "vkm.veh")
    UCD_cost = merge(UCD_cost, UCD_vkm_veh, by = c("UCD_region", "UCD_sector", "mode", "size.class", "Xyear"), all = TRUE)
    UCD_cost[!is.na(vkm.veh), value := value/vkm.veh]
    UCD_cost[!is.na(vkm.veh), unit := "2005$/vkt"]

    UCD_vkm_veh=addvehiclestypes_UCD(dt = UCD_vkm_veh,
                                              to = c("Western Europe", "Eastern Europe"), ## regions that I want to complete
                                              from = "Former Soviet Union",               ## region from which I take the data
                                              vehicle = "Midsize Car")                    ## vehicle type to complete

    UCD_cost=addvehiclestypes_UCD(dt = UCD_cost,
                                                     to = c("Western Europe", "Eastern Europe"), ## regions that I want to complete
                                                     from = "Former Soviet Union",               ## region from which I take the data
                                                     vehicle = "Midsize Car")                    ## vehicle type to complete

    ## PSI based values: in 2005USD/vkm; UCD based values: in 2005USD/vkm
    UCD_cost=apply_PSI_costs(costs_UCD = UCD_cost,fcr_veh = fcr_veh, mileage_UCD=UCD_vkm_veh)
    UCD_cost[, year := as.numeric(gsub("X", "", Xyear))]
    y = data.table(year = yrs, ID = "id")
    tmp = unique(UCD_cost[,c("UCD_region", "UCD_technology", "mode", "variable", "size.class", "UCD_sector", "UCD_fuel", "unit")])[, ID := "id"]
    tmp = merge(tmp, y, by = "ID", allow.cartesian=TRUE)
    UCD_cost = merge(tmp, UCD_cost, all = TRUE, by = c("UCD_region", "UCD_technology", "mode", "variable", "size.class", "UCD_sector", "UCD_fuel", "unit", "year"))
    UCD_cost = approx_dt(dt = UCD_cost,
                         xdata = unique(UCD_cost$year),
                         xcol = "year",
                         ycol = "value",
                         idxcols = c("UCD_region", "UCD_technology", "mode", "variable"),
                         extrapolate = TRUE)

    CONV_2005USD_1990USD = 0.67
    non_energy_cost = data.table(UCD_cost)
    non_energy_cost[, value := value *                 ## in 2005USD/vkm
                      CONV_2005USD_1990USD]   ## in 1990USD/vkm

    non_energy_cost[, unit := "1990$/vkt"]

    non_energy_cost[, UCD_region := gsub("_"," ", UCD_region)]

    non_energy_cost = non_energy_cost[, .(year, UCD_region, UCD_sector, mode, UCD_technology, variable, value, size.class)]
    non_energy_cost = non_energy_cost[year %in% years,]
    ## attribute categories coherent with the rest of the model
    non_energy_cost[, mode := ifelse(mode=="Rail", paste0(UCD_sector," ",mode), mode)]
    non_energy_cost[, mode := ifelse(size.class!="All", size.class,mode)]

    ## simplfy the names of the costs components
    non_energy_cost[, variable := gsub("-| ","_",variable)]
    non_energy_cost[, variable := gsub("\\(|\\)","",variable)]
    ## to find out the total cost, I separate the cost components into different columns
    non_energy_cost= dcast(non_energy_cost, year + UCD_region + UCD_sector + mode + UCD_technology + size.class ~ variable, value.var="value")
    ## all the cost components that show NA as they ar enot present for the specific transport category are converted in 0
    non_energy_cost[is.na(non_energy_cost)] = 0

    if(rebates_febates){

      incentive_val = 5000  ## dollars incentives starting value, phasing out by 2035, for techswitch
      feebates_val = 1000  ## extra costs starting value, phasing out by 2035, for conventional liquids and

      non_energy_cost[, type := "normal"]
      non_energy_cost[UCD_technology == techswitch & size.class %in% list4w, Capital_costs_purchase := ifelse(year>=2020 & year<=2035,
                                                                                                              Capital_costs_purchase + incentive_val*1.14*0.78*fcr_veh*(1/nper_amort_veh*(year-2020)-1)/(1.5*15000),
                                                                                                              Capital_costs_purchase),
                      by = c("UCD_region", "size.class", "UCD_technology", "type")]

      non_energy_cost[UCD_technology == "Liquids" & size.class %in% list4w, Capital_costs_purchase := ifelse(year>=2020 & year<=2035,
                                                                                                             Capital_costs_purchase - feebates_val*1.14*0.78*fcr_veh*(1/nper_amort_veh*(year-2020)-1)/(1.5*15000),
                                                                                                             Capital_costs_purchase),
                      by = c("UCD_region", "size.class", "UCD_technology", "type")]
      non_energy_cost[UCD_technology == "NG" & size.class %in% list4w, Capital_costs_purchase := ifelse(year>=2020 & year<=2035,
                                                                                                        Capital_costs_purchase - feebates_val*1.14*0.78*fcr_veh*(1/nper_amort_veh*(year-2020)-1)/(1.5*15000),
                                                                                                        Capital_costs_purchase),
                      by = c("UCD_region", "size.class", "UCD_technology", "type")]
    }

    ## self-sustaining market switch: taxes
    if (selfmarket_taxes) {
      non_energy_cost[UCD_technology == techswitch & year >= 2020 & year <= 2035  & size.class %in% list4w, Operating_costs_registration_and_insurance :=
                        Operating_costs_registration_and_insurance*((2/3)/(2035-2020)*(year-2020)+1/3),
                      by = c("UCD_region", "size.class", "UCD_technology", "type")]
    }

    ## add FCEVs and Electric trucks
    ## include Electric buses, electric trucks, h2 buses, h2 trucks
    ## documentation for this section is in the paper Rottoli et al 2020
    trucks = unique(non_energy_cost[grepl("Truck", mode) & !grepl("Light", mode), mode])
    buses = unique(non_energy_cost[grepl("Bus", mode), mode])
    truck_buses = c(trucks, buses)
    electric = non_energy_cost[mode %in% truck_buses & UCD_technology == "Liquids"][, c("UCD_technology", "CAPEX_and_non_fuel_OPEX") := list("Electric", 0)]
    hydrogen = non_energy_cost[mode %in% truck_buses & UCD_technology == "Liquids"][, c("UCD_technology", "CAPEX_and_non_fuel_OPEX") := list("FCEV", 0)]
    liquids = non_energy_cost[mode %in% truck_buses & UCD_technology == "Liquids"]
    newtech = rbind(electric, liquids, hydrogen)
    ## documentation for this section is in the paper Rottoli et al 2020
    ## cost of electric truck is 60% more than a as conventional truck today
    newtech[mode %in% trucks & year <= 2020, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology == "Electric", 1.6*CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]
    ## costs of electric truck breaks even in 2030
    newtech[mode %in% trucks & year >= 2030, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology == "Electric", CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]

    ## cost of a FCEV truck is 80% more than a as conventional truck today
    newtech[mode %in% trucks & year <= 2020, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology == "FCEV", 1.8*CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]
    ## costs of FCEV truck breaks is 10% more than a conventional truck in 2030
    newtech[mode %in% trucks & year == 2030, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology == "FCEV", 1.1*CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]
    ## break even for FCEV truck is assumed to occur in 2035
    newtech[mode %in% trucks & year >= 2035, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology == "FCEV", CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]

    ## for electric and FCEV trucks, in between 2020 and 2030 the cost follows a linear trend
    newtech[mode %in% trucks & year <= 2030 & year >= 2020 & UCD_technology %in% c("Electric", "FCEV"),
            CAPEX_and_non_fuel_OPEX := (CAPEX_and_non_fuel_OPEX[year == 2020] - CAPEX_and_non_fuel_OPEX[year == 2030])*(1 - (year - 2020)/(2030 - 2020)) + CAPEX_and_non_fuel_OPEX[year == 2030],
            by = c("UCD_region", "UCD_technology", "mode")]

    ## cost of electric buses is 40% more of a conventional bus today
    newtech[mode %in% buses & year <= 2020, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology %in% c("Electric", "FCEV"), 1.4*CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]
    ## electric and FCEV buses breaks-even with conventional buses in 2030
    newtech[mode %in% buses & year >= 2030, CAPEX_and_non_fuel_OPEX := ifelse(UCD_technology %in% c("Electric", "FCEV"), CAPEX_and_non_fuel_OPEX[UCD_technology=="Liquids"], CAPEX_and_non_fuel_OPEX), by = c("UCD_region", "year", "mode")]
    ## for electric and FCEV buses, in between 2020 and 2030 the cost follows a linear trend
    newtech[mode %in% buses & year <= 2030 & year >= 2020 & UCD_technology %in% c("Electric", "FCEV"),
            CAPEX_and_non_fuel_OPEX := (CAPEX_and_non_fuel_OPEX[year == 2020]-CAPEX_and_non_fuel_OPEX[year == 2030])*(1 - (year - 2020)/(2030 - 2020)) + CAPEX_and_non_fuel_OPEX[year == 2030], by = c("UCD_region", "UCD_technology", "mode")]

    non_energy_cost = rbind(non_energy_cost, newtech[UCD_technology!="Liquids"])

    ## add hydrogen airplanes
    newtech = non_energy_cost[mode %in% "Air Domestic" & UCD_technology == "Liquids"][, UCD_technology := "Hydrogen"]
    ## CAPEX of hydrogen airplanes is assumed today 5 times more expensive than a conventional airplane (i.e. not present in the market)
    newtech[mode %in% "Air Domestic" & year <= 2020,
            CAPEX := 5*CAPEX]
    ## following https://www.fch.europa.eu/sites/default/files/FCH%20Docs/20200507_Hydrogen%20Powered%20Aviation%20report_FINAL%20web%20%28ID%208706035%29.pdf
    ## maintenance costs are 50% higher than than a liquids fuelled airplane
    newtech[mode %in% "Air Domestic" & year >= 2020 & UCD_technology == "Hydrogen",
            non_fuel_OPEX := 1.5*non_fuel_OPEX]
    ## for hydrogen airplanes, in between 2020 and 2040 the cost follows a linear trend, and reaches a value 30% higher than a liquids fuelled airplane
    newtech[mode %in% "Air Domestic" & year >= 2020 & UCD_technology == "Hydrogen",
            CAPEX := ifelse(year <= 2040, CAPEX[year==2020] + (1.3*CAPEX[year == 2100]-CAPEX[year==2020]) * (year-2020) / (2100-2020), 1.3*CAPEX[year == 2100]),
            by = c("UCD_region", "UCD_technology", "mode")]


    non_energy_cost = rbind(non_energy_cost, newtech)


    ## calculate total fuel price
    non_energy_cost[,non_fuel_cost:=non_fuel_OPEX+
                      Operating_subsidy+
                      Operating_costs_tolls+
                      Operating_costs_registration_and_insurance+
                      Operating_costs_maintenance+
                      Capital_costs_total+
                      Capital_costs_purchase+
                      Capital_costs_other+
                      Capital_costs_infrastructure+
                      CAPEX_and_non_fuel_OPEX+
                      CAPEX]

    non_energy_cost[, type := "normal"]
    ## convert back to long format with the newly calculated total non fuel price
    non_energy_cost=melt(non_energy_cost,id.vars = c("year","UCD_region","UCD_sector", "mode","UCD_technology","size.class", "type"),
                         measure.vars=c("non_fuel_OPEX","Operating_subsidy", "Operating_costs_tolls", "Operating_costs_registration_and_insurance","Operating_costs_maintenance","Capital_costs_total","Capital_costs_purchase","Capital_costs_other","Capital_costs_infrastructure","CAPEX_and_non_fuel_OPEX","CAPEX","non_fuel_cost"),
                         variable.name="price_component",
                         value.name="value")

    ## clean the database
    non_energy_cost[,c("size.class","UCD_sector"):=NULL]
    non_energy_cost[,mode:=ifelse(mode=="Air International","International Aviation",mode)]
    non_energy_cost[,mode:=ifelse(mode=="Air Domestic","Domestic Aviation",mode)]
    non_energy_cost[,mode:=ifelse(mode=="Ship International","International Ship",mode)]
    non_energy_cost[,mode:=ifelse(mode=="Ship Domestic","Domestic Ship",mode)]
    ## downscale to ISO level
    non_energy_cost=disaggregate_dt(non_energy_cost, UCD2iso, fewcol = "UCD_region",
                                    datacols = c("mode","UCD_technology","price_component", "type"))

    ## upscale to regions
    gdp_country=copy(GDP_country)
    non_energy_cost=aggregate_dt(non_energy_cost, GCAM2ISO_MAPPING,
                                 datacols = c("mode", "UCD_technology", "price_component", "type"),
                                 weights = gdp_country)
    ## only EU-15 and EU-12 should have Hybrid Electric
    non_energy_cost = non_energy_cost[(UCD_technology == "Hybrid Electric" & region %in% c("EU-12", "EU-15"))|(UCD_technology != "Hybrid Electric"),]
    dups <- duplicated(non_energy_cost, by=c("region", "UCD_technology", "mode","year","price_component", "type"))

    if(any(dups)){
      warning("Duplicated techs found in supplied demand.")
      print(non_energy_cost[dups])
      non_energy_cost <- unique(non_energy_cost, by=c("region", "UCD_technology", "mode","year","price_component", "type"))
    }


    setnames(non_energy_cost,old=c("mode","UCD_technology"),new=c("univocal_name","technology"))
    ## add logit_category for Hybrid Electric, Electric and FCEVs trucks and buses, hydrogen airplanes
    logit_category = rbind(logit_category, logit_category[technology == "BEV"][, technology := "Hybrid Electric"])
    logit_category = rbind(logit_category, logit_category[technology == "Liquids" & vehicle_type %in% c(truck_buses, "Bus_tmp_vehicletype")][, technology := "Electric"])
    logit_category = rbind(logit_category, logit_category[technology == "Liquids" & vehicle_type %in% c(truck_buses, "Bus_tmp_vehicletype")][, technology := "FCEV"])
    logit_category = rbind(logit_category, logit_category[technology == "Liquids" & subsector_L3 == "Domestic Aviation"][, technology := "Hydrogen"])

    non_energy_cost=merge(non_energy_cost, logit_category, all=FALSE, by = c("univocal_name","technology"))
    non_energy_cost[, c("univocal_name") := NULL]
    non_energy_cost[vehicle_type %in% c("3W Rural", "Truck (0-1t)", "Truck (0-3.5t)", "Truck (0-4.5t)", "Truck (0-2t)", "Truck (0-6t)", "Truck (2-5t)", "Truck (0-2.7t)", "Truck (2.7-4.5t)"), vehicle_type := "Truck (0-3.5t)"]
    non_energy_cost[vehicle_type %in% c("Truck (4.5-12t)", "Truck (6-14t)", "Truck (5-9t)", "Truck (6-15t)", "Truck (4.5-15t)", "Truck (1-6t)"), vehicle_type := "Truck (7.5t)"]
    non_energy_cost[vehicle_type %in% c("Truck (>12t)", "Truck (6-30t)", "Truck (9-16t)","Truck (>14t)"), vehicle_type := "Truck (18t)"]
    non_energy_cost[vehicle_type %in% c("Truck (>15t)", "Truck (3.5-16t)", "Truck (16-32t)"), vehicle_type := "Truck (26t)"]
    non_energy_cost[vehicle_type %in% c("Truck (>32t)"), vehicle_type := "Truck (40t)"]
    non_energy_cost=non_energy_cost[,.(value = mean(value)), by = c("year","region","sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "type", "price_component")]

    ## add load_factor for Hybrid Electric, trucks and buses FCEV and electric, domestic aviation hydrogen
    load_factor = rbind(load_factor, load_factor[technology == "BEV"][, technology := "Hybrid Electric"])
    load_factor = rbind(load_factor, load_factor[technology == "Liquids" & vehicle_type %in% c("Bus_tmp_vehicletype")][, technology := "Electric"])
    load_factor = rbind(load_factor, load_factor[technology == "Liquids" & vehicle_type %in% c("Bus_tmp_vehicletype")][, technology := "FCEV"])
    load_factor = rbind(load_factor, load_factor[technology == "Liquids" & vehicle_type %in% c("Truck (0-3.5t)","Truck (7.5t)","Truck (1-6t)","Truck (18t)", "Truck (26t)", "Truck (40t)")][, technology := "Electric"])
    load_factor = rbind(load_factor, load_factor[technology == "Liquids" & vehicle_type %in% c("Truck (0-3.5t)","Truck (7.5t)","Truck (1-6t)","Truck (18t)", "Truck (26t)", "Truck (40t)")][, technology := "FCEV"])
    load_factor = rbind(load_factor, load_factor[technology == "Liquids" & vehicle_type %in% "Domestic Aviation_tmp_vehicletype"][, technology := "Hydrogen"])
    non_energy_cost=merge(non_energy_cost,load_factor,all = FALSE, by=c("technology","region","year","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector") )

    non_energy_cost=non_energy_cost[,non_fuel_price:=value/loadFactor][,-"loadFactor"]
    non_energy_cost[,value:=NULL]
    ## add Mini Cars to EU-12, Europe Non EU (due to integration with TRACCS database)
    tmp1=copy(non_energy_cost[region=="EU-15" & vehicle_type=="Mini Car",])[,region:="EU-12"]
    tmp2=copy(non_energy_cost[region=="EU-15" & vehicle_type=="Mini Car",])[,region:="Europe Non EU"]
    tmp3=copy(non_energy_cost[region=="EU-15" & vehicle_type=="Mini Car",])[,region:="European Free Trade Association"]

    non_energy_cost=rbindlist(list(tmp1,tmp2,tmp3,non_energy_cost))


    ## create 2 separate dts, one with the total non fuel price the other with all it's components (for reporting purposes)

    non_energy_cost_split=non_energy_cost[price_component!="non_fuel_cost"]
    non_energy_cost=non_energy_cost[price_component=="non_fuel_cost"]
    non_energy_cost[,price_component:=NULL]

    dups <- duplicated(non_energy_cost, by=c("technology","region","year","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector", "type"))


    if(any(dups)){
      warning("Duplicated techs found in supplied demand.")
      print(non_energy_cost[dups])
      non_energy_cost <- unique(non_energy_cost, by=c("technology","region","year","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector"))
    }
    return(list(non_energy_cost=non_energy_cost,non_energy_cost_split=non_energy_cost_split, logit_category = logit_category, load_factor = load_factor))

  }

  calc_annual_mileage=function(logit_category, UCD_transportation_database){
    UCD_sector <- iso <- subsector_L2 <- ID <- X_UCD_years <- NULL
    annual_mileage <- UCD_transportation_database[unit %in% c("vkt/veh/yr"),]
    annual_mileage <- melt(annual_mileage, measure.vars = X_UCD_years, variable.name = "Xyear" )
    annual_mileage[, year := as.numeric(gsub("X", "", Xyear))]

    y = data.table(year = yrs, ID = "id")
    tmp = unique(annual_mileage[,c("UCD_region", "UCD_technology", "mode", "variable", "size.class", "UCD_sector", "UCD_fuel", "unit")])[, ID := "id"]
    tmp = merge(tmp, y, by = "ID", allow.cartesian=TRUE)
    annual_mileage = merge(tmp, annual_mileage, all = TRUE, by = c("UCD_region", "UCD_technology", "mode", "variable", "size.class", "UCD_sector", "UCD_fuel", "unit", "year"))
    annual_mileage = approx_dt(dt = annual_mileage,
                         xdata = unique(annual_mileage$year),
                         xcol = "year",
                         ycol = "value",
                         idxcols = c("UCD_region", "UCD_technology", "mode", "variable"),
                         extrapolate = TRUE)

    annual_mileage$UCD_region=gsub("_"," ",annual_mileage$UCD_region)
    annual_mileage=annual_mileage[,.(year,UCD_region,UCD_sector,mode,UCD_technology ,variable,value,size.class)]

    annual_mileage=annual_mileage[year %in% years,]

    annual_mileage <- disaggregate_dt(annual_mileage, UCD2iso,
                                      valuecol = "value", fewcol = "UCD_region",
                                      datacols = c("UCD_sector", "mode","UCD_technology","variable","size.class"))

    setnames(annual_mileage, old=c("size.class","value"), new = c("vehicle_type", "annual_mileage"))

    annual_mileage[, c("UCD_technology", "variable", "UCD_sector"):=NULL] ##delete the column that stands for "all" technology
    annual_mileage[, ID := "id"]
    techs = data.table(technology = c("BEV", "Liquids", "NG", "FCEV", "Hybrid Electric"))[, ID := "id"]
    annual_mileage = merge(annual_mileage, techs, by = "ID", allow.cartesian = TRUE)
    annual_mileage[, ID := NULL]
    annual_mileage=merge(annual_mileage,logit_category,all=FALSE,by=c("vehicle_type","technology"))[,-"univocal_name"]


    for (isos in unique(annual_mileage$iso)) {
      diffe = setdiff(unique(logit_category[subsector_L2 == "trn_pass_road_LDV",c("vehicle_type")])$vehicle_type,
                      unique(annual_mileage[iso==isos& year ==2010,c("vehicle_type")])$vehicle_type
      )

      aver = annual_mileage[vehicle_type %in% diffe,]
      aver = aver[, .(annual_mileage = mean(annual_mileage)), by = c("year", "mode", "vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3")]
      annual_mileage = rbind(annual_mileage, aver[, iso := isos])
    }


    return(annual_mileage)

  }

  non_energy_cost=calc_non_energy_price(UCD_transportation_database, GCAM2ISO_MAPPING)
  annual_mileage=calc_annual_mileage(logit_category = non_energy_cost$logit_category, UCD_transportation_database)

  UCD_results=list(non_energy_cost=non_energy_cost,
                   annual_mileage=annual_mileage,
                   fcr_veh=fcr_veh)
  return(UCD_results)

}
