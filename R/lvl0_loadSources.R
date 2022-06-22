#' Load EU data
#'
#' Loads absolute values demand (pkm,tkm) for:
#'   Road (pass+freight), Rail (pass+freight), Shipping (freight), Aviation (pass)
#' Loads factors and Energy intensity for Road (pass+freight)
#'
#' Final units for demand: millionkm (tkm and pkm)


#' @param input_folder folder hosting raw data
#' @param EU_dir directory with TRACCS data and Eurostat data
#'
#' @importFrom readxl read_excel


lvl0_loadEU <- function(input_folder, EU_dir = "EU_data") {
  countries <- tech_output <- technologies <- `.` <- region <- EDGE_vehicle_type <- MJ <- mtoe <- km_million <- country_name <- technology <- tkm_million <- MJ_km <- load_factor <- million_tkm <- Liquids <- Electric <- pkm_million <- ktkm <- pkm <- name <- code_airplane_characteristics <- sector_fuel <- vehicle_type <- NULL
  EJ <- RailTraction <- RailTrafficType <- Unit_short <- convert <- value <- annual_mileage <- NULL
  TRACCS_technology <- vkm <- tkm <- NULL
  EU_folder <- file.path(input_folder, EU_dir)

  ## load mappings
  mapfile <- system.file("extdata", "mapping_TRACCS_roadvehicles.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_TRACCS_roadf_categories = fread(mapfile, skip = 0)

  ## conversion unit->million
  CONV_unit_million <- 1e-06
  ## load mapping with country codes
  mapping_TRACCS_iso= fread(
    system.file(
      "extdata", "mapping_countries_EU.csv", package="edgeTransport"), skip=0)

  ## data preparation function
  prepare_data <- function(dt) {
    dt <- melt(dt, id.vars = c("country_name", "TRACCS_category", "TRACCS_vehicle_type",
                               "TRACCS_technology"),
               measure.vars = c("2005", "2006", "2007", "2008", "2009", "2010"),
               variable.name = "year")
    dt[country_name == "FYROM", country_name := "Macedonia, the former Yugoslav Republic of"]
    dt <- merge(dt, mapping_TRACCS_iso, by="country_name")[,-c("country_name")]
    dt[, year := as.numeric(as.character(year))]
    dt[TRACCS_technology %in% c("Gasoline", "Diesel", "Flexi-fuel", "B30"),
       technology := "Liquids"]
    dt[TRACCS_technology %in% c("CNG", "CNG/Biogas", "LPG"),
       technology := "NG"]
    dt <- merge(mapping_TRACCS_roadf_categories, dt)
    dt[, c("TRACCS_category", "TRACCS_technology", "TRACCS_vehicle_type") := NULL]
    return(dt)
  }

  ## load Road data
  list_countries = data.table(
    countries=list.files(path = file.path(EU_folder, "TRACCS_ROAD_Final_EXCEL_2013-12-20"),
                         all.files=FALSE))
  list_countries[, countries := gsub("Road Data |_Output.xlsx", "", countries)] #
  list_countries = list_countries[!grepl("\\$",countries),] #deletes open files, which have a $ in the name

  ##road FE: load demand (million EJ)
  roadFE_eu = rbindlist(lapply(
    list_countries$countries,
    function(x) {
      output = suppressMessages(data.table(read_excel(
        path = file.path(
          EU_folder,
          paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ", x, "_Output.xlsx")),
        sheet="FCcalc","A2:I75")))
      colnames(output) <- c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                         as.character(seq(2005,2010,1)))
      output = output[!TRACCS_technology %in% c("All", "Total", "Other")]
      output$country_name <- x
      return(output)
    }))

  roadFE_eu <- prepare_data(roadFE_eu)
  ## different liquid and gas techs have to be summed
  roadFE_eu <- roadFE_eu[, .(value=sum(value)), by=c("year", "iso", "technology", "vehicle_type")]
  roadFE_eu[, convert := ifelse(technology == "Liquids", 0.043, 0.048)]
  roadFE_eu[, MJ := value* ## in t
                convert*   ## in TJ
                1000000    ## in MJ
            ]
  roadFE_eu[, c("value", "convert") := NULL]

  ## road passenger: load factors for cars and buses
  LF_countries_EU <- rbindlist(lapply(
      list_countries$countries,
      function(x) {
        output = suppressMessages(data.table(read_excel(
          path=file.path(
            EU_folder,
            paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
          sheet="Occupancy ratio","A2:I51")))
        colnames(output) <- c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                           as.character(seq(2005,2010,1)))

        ## only care about LF on vehicle type level
        ## for busses we use the "Total" as it is an average over Coaches and Buses
        output <- output[(TRACCS_category == "Passenger cars" & TRACCS_technology == "All") |
                         (TRACCS_category == "Buses" & TRACCS_technology == "Total")]
        output$country_name <- x
        return(output)
      }))

  LF_countries_EU <- prepare_data(LF_countries_EU)[, technology := NULL]
  setnames(LF_countries_EU, "value", "loadFactor")

  ## road passenger: annual mileage for cars and buses
  am_countries_EU <- rbindlist(lapply(
    list_countries$countries,
    function(x) {
      output = suppressMessages(data.table(read_excel(
        path=file.path(
          EU_folder,
          paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
        sheet="Mileage per Veh. (Km)","A2:I51")))
      colnames(output)=c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                         as.character(seq(2005,2010,1)))
      ## we only keep the average annual mileage accross technologies
      output <- output[(TRACCS_category == "Passenger cars" & TRACCS_technology == "All") |
                       (TRACCS_category == "Buses" & TRACCS_technology == "Total")]
      output$country_name <- x
      return(output)
    }))

  am_countries_EU <- prepare_data(am_countries_EU)[, technology := NULL]
  am_countries_EU <- am_countries_EU[value > 0]
  setnames(am_countries_EU, "value", "annual_mileage")

  ## demand vkm
  demand_vkm_raw <- rbindlist(
    lapply(
      list_countries$countries,
      function(x) {
        output = suppressMessages(data.table(
          read_excel(
            path=file.path(
              EU_folder,
              paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
            sheet="Veh-Km","A2:I73")))
        setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                           as.character(seq(2005,2010,1))))
        output=output[!TRACCS_technology %in% c("Total", "Other", "All")]
        output$country_name <- x
        return(output)
      }))

  demand_vkm <- prepare_data(demand_vkm_raw)
  demand_vkm <- demand_vkm[, .(value=sum(value)), by=c("year", "iso", "technology", "vehicle_type")]
  setnames(demand_vkm, "value", "vkm")

  ## demand tkm
  demand_tkm <- rbindlist(
    lapply(
      list_countries$countries,
      function(x) {
        output = suppressMessages(data.table(
          read_excel(
            path=file.path(
              EU_folder,
              paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
            sheet="Tonne-Km","A2:I18")))
        setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                           as.character(seq(2005,2010,1))))
        output=output[!TRACCS_technology %in% c("Total", "Other", "All")]
        output$country_name <- x
        return(output)
      }))

  demand_tkm <- prepare_data(demand_tkm)
  demand_tkm <- demand_tkm[, .(value=sum(value)), by=c("year", "iso", "technology", "vehicle_type")]
  setnames(demand_tkm, "value", "tkm")

  ## load factor for trucks from vkm and tkm
  ## we derive this from total demands as this is less
  ## ambivalent with the different size classes (how to weight different load factors?)
  LF_trucks <- demand_vkm[demand_tkm, on=c("year", "iso", "vehicle_type", "technology")]
  LF_trucks <- LF_trucks[, .(loadFactor=sum(tkm)/sum(vkm)), by=c("year", "iso", "vehicle_type")]

  LF_countries_EU <- rbind(LF_countries_EU, LF_trucks, use.names=TRUE)

  ## demand pkm
  demand_pkm <- rbindlist(
    lapply(
      list_countries$countries,
      function(x) {
        output = suppressMessages(data.table(
          read_excel(
            path=file.path(
              EU_folder,
              paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
            sheet="Pass-Km","A2:I51")))
        setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                           as.character(seq(2005,2010,1))))
        output=output[!TRACCS_technology %in% c("Total", "Other", "All")]
        output$country_name <- x
        return(output)
      }))

  demand_pkm <- prepare_data(demand_pkm)
  demand_pkm <- demand_pkm[, .(value=sum(value)), by=c("year", "iso", "technology", "vehicle_type")]
  setnames(demand_pkm, "value", "pkm")

  ## road passenger and road freight: load energy intensity
  energy_intensity_raw <- rbindlist(
    lapply(
      list_countries$countries,
      function(x) {
        output = suppressMessages(data.table(
          read_excel(
            path=file.path(
              EU_folder,
              paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
            sheet="FC_EFs","A2:TB73")))
        output <- output[, c(1, 2, 3, 372, 402, 432, 462, 492, 522)]
        setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                           as.character(seq(2005, 2010, 1))))
        output <- output[!TRACCS_technology %in% c("Other", "All", "Total")]
        output$country_name <- x
        return(output)
      }))

  ## use raw data to merge and aggregate technologies using vkm data as weight
  energy_intensity_raw <- melt(
    energy_intensity_raw,
    id.vars = c("country_name", "TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
    measure.vars = c("2005", "2006", "2007", "2008", "2009", "2010"),
    variable.name = "year", value.name = "MJ_km")

  demand_vkm_raw <- melt(
    demand_vkm_raw,
    id.vars = c("country_name", "TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
    measure.vars = c("2005", "2006", "2007", "2008", "2009", "2010"),
    variable.name = "year", value.name = "vkm")

  energy_intensity_EU <- energy_intensity_raw[
    demand_vkm_raw, on=c("country_name", "year", "TRACCS_category",
                         "TRACCS_vehicle_type", "TRACCS_technology")][vkm > 0]
  energy_intensity_EU[TRACCS_technology %in% c("Gasoline", "Diesel", "Flexi-fuel", "B30"),
                      technology := "Liquids"]
  energy_intensity_EU[TRACCS_technology %in% c("CNG", "CNG/Biogas", "LPG"), technology := "NG"]
  energy_intensity_EU <- merge(mapping_TRACCS_roadf_categories, energy_intensity_EU)

  energy_intensity_EU <- energy_intensity_EU[
    , .(MJ_km = sum(vkm*MJ_km)/sum(vkm)),
    by=c("country_name", "year", "vehicle_type", "technology")]

  energy_intensity_EU <- energy_intensity_EU[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name
  ## #include the sector fuel
  ## energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("BEV","Electric"), "elect_td_trn", NA)]
  ## energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Liquids"), "refined liquids enduse", sector_fuel)]
  ## energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("NG"), "delivered gas", sector_fuel)]
  ## energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Coal"), "delivered coal", sector_fuel)]

  energy_intensity_EU <- merge(energy_intensity_EU,
                               mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
  energy_intensity_EU[, year := as.numeric(as.character(year))]

  ## rail pass and freight: load demand
  rail_eu=suppressMessages(data.table(read_excel(
    path=file.path(
      EU_folder,
      paste0("TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx")), sheet="eval_rail_energy","A6:L124")))
  rail_eu = melt(rail_eu, id.vars = c("RailTraction","Unit_short","CountryID","Country","Countrytype_short","RailTrafficType"),
                 measure.vars = c("2005","2006","2007","2008","2009","2010"))
  rail_eu[, technology := ifelse(RailTraction == "Diesel", "Liquids", "Electric")]
  rail_eu[, MJ := numeric()]
  rail_eu[Unit_short == "t", MJ := value*  ## in t
            0.043774*  ## in TJ
            1000000  ## in MJ
  ]

  rail_eu[Unit_short == "Mio kWh", MJ := value*  ## in Mio kWh
            1e6*  ## in kwh
            3.6  ## in MJ
  ]

  setnames(rail_eu,old=c("variable", "Country"),new=c("year", "country_name"))
  rail_eu[, vehicle_type := character()]
  rail_eu[RailTrafficType == "Goods", vehicle_type := "Freight Rail_tmp_vehicletype"]
  rail_eu[RailTrafficType == "Passenger", vehicle_type := "Passenger Rail_tmp_vehicletype"]
  rail_eu=rail_eu[,.(MJ=sum(MJ)),by=c("year","country_name", "technology", "vehicle_type")]
  rail_eu=merge(rail_eu,mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
  rail_eu[, year := as.numeric(as.character(year))]

  ## load eurostata for bunkers
  map_eurostat = data.table(country_name = c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
                            iso = c("BEL","BGR","CZE","DNK","DEU","EST","IRL","GRC","ESP","FRA","HRV","ITA","CYP","LVA","LTU","LUX","HUN","MLT","NLD","AUT","POL","PRT","ROU","SVN","SVK","FIN","SWE","GBR"))
  dem_bunkers <- do.call("rbind",lapply(map_eurostat$country_name,
                                        function(x) {
                                          output = suppressMessages(data.table(read_excel(path=file.path(EU_folder, "Eurostat/energy_statistical_countrydatasheets.xlsx"),
                                                                                          sheet=x,"C8:X95")))
                                          colnames(output)=c("name", as.character(seq(1990,2010,1)))
                                          output = output[name %in% c("International maritime bunkers", "International aviation")]
                                          output = output[, c("name", "1990", "2005", "2010")]
                                          output = melt(output, id.vars = c("name"),
                                                        measure.vars = c("1990","2005","2010"))
                                          setnames(output,old=c("value"),new=c("mtoe"))
                                          output[, MJ := mtoe*41868000000]
                                          output$country_name <- x
                                          return(output)
                                        }))

  ## merge with regional mapping for EUROSTAT
  dem_bunkers = merge(dem_bunkers, map_eurostat)[, c("country_name","mtoe") := NULL]
  dem_bunkers[, vehicle_type := ifelse(name == "International maritime bunkers", "International Ship_tmp_vehicletype", "International Aviation_tmp_vehicletype")][, name := NULL]
  setnames(dem_bunkers, old ="variable", new = "year")
  dem_bunkers[, year := as.numeric(as.character(year))]
  dem_bunkers[, technology := "Liquids"]
  ## load domestic aviation from Eurostat
  dem_dom <- do.call("rbind",lapply(map_eurostat$country_name,
                                    function(x) {
                                      output = suppressMessages(data.table(read_excel(path=file.path(EU_folder, "Eurostat/energy_statistical_countrydatasheets.xlsx"),
                                                                                      sheet=x,"C8:X249")))
                                      colnames(output)=c("name", as.character(seq(1990,2010,1)))
                                      output = output[name %in% c("Domestic aviation", "Domestic navigation")]
                                      output = output[, c("name", "1990", "2005", "2010")]
                                      output = melt(output, id.vars = c("name"),
                                                    measure.vars = c("1990","2005","2010"))
                                      setnames(output,old=c("value"),new=c("mtoe"))
                                      output[, MJ := mtoe*41868000000]
                                      output$country_name <- x
                                      return(output)
                                    }))

  ## merge with regional mapping for EUROSTAT
  dem_dom = merge(dem_dom, map_eurostat)[, c("country_name","mtoe") := NULL]
  dem_dom[, vehicle_type := ifelse(name == "Domestic aviation", "Domestic Aviation_tmp_vehicletype", "Domestic Ship_tmp_vehicletype")][, name := NULL]
  setnames(dem_dom, old ="variable", new = "year")
  dem_dom[, year := as.numeric(as.character(year))]
  dem_dom[, technology := "Liquids"]

  ## extend road to 1990
  roadFE_eu = approx_dt(roadFE_eu, c(1990,unique(roadFE_eu$year)),
                        xcol = "year", ycol = "MJ",
                        idxcols = c("iso", "vehicle_type", "technology"),
                        extrapolate=T)

  ## extend rail to 1990
  rail_eu <- rbind(rail_eu, rail_eu[year == 2005][, year := 1990])

  dem_eurostat = rbind(dem_bunkers, dem_dom, rail_eu, roadFE_eu)
  load_EU_data=list(dem_eurostat = dem_eurostat,
                    energy_intensity_EU=energy_intensity_EU,
                    roadFE_eu = roadFE_eu,
                    LF_countries_EU=LF_countries_EU,
                    am_countries_EU = am_countries_EU,
                    demand_pkm_EU = demand_pkm)
  return(load_EU_data)

}


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
  UCD_sector <- vkm.veh <- unit <- NULL
  ## load database UCD transportation
  UCD2iso=fread(file.path(input_folder, UCD_dir, "mapping_UCDdb_ISO.csv"))
  UCD_transportation_database = fread(file.path(input_folder, UCD_dir, "UCD_transportation_database.csv"), stringsAsFactors=F, header = TRUE)
  ## simplify costs removing non used data
  UCD_transportation_database = UCD_transportation_database[!size.class %in% c("Three-Wheeler", "Heavy Bus", "Light Bus")]

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
  `.` <- scenario <- technology <- vehicle_type_PSI <- vehicle_type_PSI <- tot_purchase_euro2017 <- tot_purchasecost <- vehicle_type <- NULL

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

  ## the costs for 2040 are too pessimistic according to the latest estimates https://www.transportenvironment.org/discover/hitting-the-ev-inflection-point/: they are reduced to 1/3 of the original value for BEVs and hybdrid electric and to 1/2 for FCEVs (only battery and storage components). This leads to about 80% of the total purchase cost
  psi_costs[, tot_purchase_euro2017 := as.numeric(tot_purchase_euro2017)]
  psi_costs[scenario == 2040 & technology %in% c("PHEV-c", "PHEV-e", "BEV"), tot_purchase_euro2017 := 0.8*tot_purchase_euro2017]
  psi_costs[scenario == 2040 & technology %in% c("FCEV"), tot_purchase_euro2017 := 0.9*tot_purchase_euro2017]

  psi_costs=psi_costs[,.(year=scenario, ## rename col scenario with year
                         technology,vehicle_type_PSI,
                         tot_purchasecost=tot_purchase_euro2017   ## in 2017euro
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
  `.` <- value <- region <- vehicle_type <- NULL
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
#' @param PSI_dir PSI data directory
#' @param GCAM_data GCAM data
#' @return CAPEX and non-fuel OPEX conventional trucks
#' @author Marianna Rottoli, Alois Dirnaichner

## function that loads and applies CAPEX costs for CHA conventional trucks. Final values in 2005USD/vkm
lvl0_PSIint=function(GCAM_data, input_folder, PSI_dir="PSI", years){
  vehicle_type <- technology <- powertrain <- `.` <- kJ.per.vkm <- conv_vkm_MJ <- ttw_energy <- NULL
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
  ## remove mild/full hybrids
  LDV_PSI_int = LDV_PSI_int[!powertrain %in% c("HEV-d", "HEV-p")]
  ## average on the EDGE category
  LDV_PSI_int = LDV_PSI_int[, .(kJ.per.vkm=mean(kJ.per.vkm)), by = c("technology", "vehicle_type", "year")]

  LDV_PSI_int[, conv_vkm_MJ := kJ.per.vkm*  ## kj/vkm
                1e-3] ## MJ/vkm]

  LDV_PSI_int[, c("kJ.per.vkm") := NULL]
  ## approx to the whole time range
  LDV_PSI_int = approx_dt(LDV_PSI_int,
                          xdata = years,
                          xcol = "year",
                          ycol = "conv_vkm_MJ",
                          idxcols = c("technology", "vehicle_type"),
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
  Truck_PSI_int[, conv_vkm_MJ := ttw_energy*  ## kj/km
                  1e-3] ## MJ/km]
  Truck_PSI_int[, c("X", "unit", "ttw_energy") := NULL]
  ## Buses are assumed to be as 18 tons truck
  Bus_PSI_int = Truck_PSI_int[vehicle_type == "Truck (18t)"][, vehicle_type := "Bus_tmp_vehicletype"]
  Truck_PSI_int = rbind(Truck_PSI_int, Bus_PSI_int)

  ## approx to the whole time range
  Truck_PSI_int = approx_dt(Truck_PSI_int,
                            xdata = years,
                            xcol = "year",
                            ycol = "conv_vkm_MJ",
                            idxcols = c("technology", "vehicle_type"),
                            extrapolate = TRUE)


  return(list(LDV_PSI_int = LDV_PSI_int, Truck_PSI_int = Truck_PSI_int))

}



lvl0_AltHDV=function(UCD_output){
  vehicle_type <- UCD_technology <- variable <- value <- NULL
  costs = copy(UCD_output$UCD_cost)
  trucks = unique(costs[grepl("Truck", vehicle_type) & !grepl("Light", vehicle_type), vehicle_type])
  buses = unique(costs[grepl("Bus", vehicle_type), vehicle_type])

  electric = costs[vehicle_type %in% c(trucks, buses) & UCD_technology == "Liquids" & variable == "CAPEX and non-fuel OPEX"][, "UCD_technology" := "Electric"]
  hydrogen = costs[vehicle_type %in% c(trucks, buses) & UCD_technology == "Liquids" & variable == "CAPEX and non-fuel OPEX"][, "UCD_technology" := "FCEV"]
  altcost = rbind(electric, hydrogen)

  yeartarget_early = 2035  ## target year for electric trucks and electric and hydrogen buses
  yeartarget_late = 2150  ## target year for FCEV trucks

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
  altcost[vehicle_type %in% buses & UCD_technology == "Electric" & year <= yeartarget_early & year >= 2020,
          value := (value[year == 2020]-value[year == yeartarget_early])*(1 - (year - 2020)/(yeartarget_early - 2020)) + value[year == yeartarget_early],
          by = c("iso", "UCD_technology", "vehicle_type")]

  ## for electric and FCEV buses, in between 2020 and target year the cost follows a linear trend
  altcost[vehicle_type %in% buses & UCD_technology == "FCEV" & year <= yeartarget_late & year >= 2020,
          value := (value[year == 2020]-value[year == yeartarget_late])*(1 - (year - 2020)/(yeartarget_late - 2020)) + value[year == yeartarget_late],
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
