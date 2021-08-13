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


lvl0_loadEU <- function(input_folder, EU_dir = "EU_data"){
    countries <- tech_output <- technologies <- `.` <- region <- EDGE_vehicle_type <- MJ <- mtoe <- km_million <- country_name <- technology <- tkm_million <- MJ_km <- load_factor <- million_tkm <- Liquids <- Electric <- pkm_million <- ktkm <- pkm <- name <- code_airplane_characteristics <- sector_fuel <- vehicle_type <- NULL
    EJ <- RailTraction <- RailTrafficType <- Unit_short <- convert <- value <- annual_mileage <- NULL
    EU_folder <- file.path(input_folder, EU_dir)

    ## load mappings
    mapfile <- system.file("extdata", "mapping_TRACCS_roadvehicles.csv",
                           package = "edgeTransport", mustWork = TRUE)
    mapping_TRACCS_roadf_categories = fread(mapfile, skip = 0)

    ## conversion unit->million
    CONV_unit_million <- 1e-06
    ## load mapping with country codes
    mapping_TRACCS_iso= fread(file.path(EU_folder, "mapping_countries_EU.csv"), skip=0)
    ## load Road data
    list_countries = data.table(countries=list.files(path = file.path(EU_folder, "TRACCS_ROAD_Final_EXCEL_2013-12-20"),
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
        colnames(output)=c("category_TRACCS","vehicle_type","technology",as.character(seq(2005,2010,1)))
        output[,technology := ifelse(technology %in% c("Gasoline","Diesel","Flexi-fuel","LPG","B30"),"Liquids",technology)]
        output[,technology := ifelse(technology %in% c("CNG","CNG/Biogas"),"NG",technology)]
        output[,technology := ifelse(technology %in% c("Other"),"Liquids",technology)]
        output=output[!technology %in% c("All", "Total"),]
        output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                      measure.vars = c("2005","2006","2007","2008","2009","2010"))
        setnames(output,old=c("value","variable"), new = c("t","year"))
        output = merge(mapping_TRACCS_roadf_categories,output)
        output$country_name <- x
        output = output[, .(vehicle_type=EDGE_vehicle_type,technology,year,t,country_name)]
        output = output[, .(t=sum(t)),by=c("year","country_name","technology","vehicle_type")]
        output[, convert := ifelse(technology == "Liquids", 0.043, 0.048)]
        output[, MJ := t*     ## in t
                   convert*  ## in TJ
                   1000000   ## in MJ
               ]
        return(output)
      }))

    roadFE_eu=roadFE_eu[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name
    roadFE_eu=merge(roadFE_eu,mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
    roadFE_eu[, year := as.numeric(as.character(year))]
    roadFE_eu[, c("t", "convert"):=NULL]

    ## road passenger: load load factors for cars
    LF_countries_EU <- rbind(
      rbindlist(lapply(
        list_countries$countries,
        function(x) {
          output = suppressMessages(data.table(read_excel(
            path=file.path(
              EU_folder,
              paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
            sheet="Occupancy ratio","A2:I51")))
          colnames(output)=c("category_TRACCS","vehicle_type","technology",as.character(seq(2005,2010,1)))

          output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                        measure.vars = c("2005","2006","2007","2008","2009","2010"))

          setnames(output,old=c("value","variable"),new=c("load_factor","year"))
          output=merge(mapping_TRACCS_roadf_categories,output)
          output[,technology:=ifelse(technology %in% c("Gasoline","Diesel","LPG","Flexi-fuel","B30"),"Liquids",technology)]
          output[,technology:=ifelse(technology %in% c("CNG","CNG/Biogas"),"NG",technology)]
          output[,technology:=ifelse(technology %in% c("Other"),"BEV",technology)]
          output=output[!technology %in% c("All"),]
          output$country_name <- x
          output=output[,.(vehicle_type=EDGE_vehicle_type,technology,year,load_factor,country_name)]
          return(output)
        })),
      rbindlist(lapply(
        list_countries$countries,
        function(x) {
          output = suppressMessages(data.table(read_excel(
            col_names=FALSE,
            path=file.path(
              EU_folder,
              paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
            sheet="Tonne-Km","A3:AA18")))
          output <- output[, c(1:3, 22:27)]
          colnames(output)=c("category_TRACCS","vehicle_type","technology", 2005:2010)
          output <- output[!is.na(get("2010"))]
          output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                        value.name="load_factor", variable.name="year")

          output=merge(mapping_TRACCS_roadf_categories,output)
          output[, technology := "Liquids"]
          output$country_name <- x
          output=output[,.(vehicle_type=EDGE_vehicle_type,technology,year,load_factor,country_name)]
          return(output)
        }))
    )

    LF_countries_EU=LF_countries_EU[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name

    ## road passenger: load annual mileage
    am_countries_EU<- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                               output = suppressMessages(data.table(read_excel(
                                                 path=file.path(
                                                   EU_folder,
                                                   paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
                                                 sheet="Mileage per Veh. (Km)","A2:I51")))
                                               colnames(output)=c("category_TRACCS","vehicle_type","technology",as.character(seq(2005,2010,1)))
                                               output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                                                             measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                               setnames(output,old=c("value","variable"),new=c("annual_mileage","year"))
                                               output=merge(mapping_TRACCS_roadf_categories,output)
                                               output=output[technology %in% c("All"),]
                                               output=output[annual_mileage>0,]
                                               output$country_name <- x
                                               output=output[,.(vehicle_type=EDGE_vehicle_type,year,annual_mileage,country_name)]
                                               return(output)
                                             }))

    am_countries_EU=am_countries_EU[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name
    am_countries_EU[, year := as.numeric(as.character(year))]

    ## road passenger and road freight: load energy intensity
    energy_intensity_EU <- do.call("rbind",lapply(list_countries$countries,
                                                  function(x) {
                                                    output = suppressMessages(data.table(
                                                      read_excel(
                                                        path=file.path(
                                                          EU_folder,
                                                          paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
                                                        sheet="Energy_intensity_MJ_km","A2:O75")))
                                                    output=output[,c(1,2,3,10,11,12,13,14,15)]
                                                    colnames(output)=c("category_TRACCS","vehicle_type","technology",as.character(seq(2005,2010,1)))
                                                    output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                                                                  measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                                    setnames(output,old=c("value","variable"),new=c("MJ_km","year"))
                                                    output=merge(mapping_TRACCS_roadf_categories,output)
                                                    output[,technology:=ifelse(technology %in% c("Gasoline","Diesel","Flexi-fuel","B30"),"Liquids",technology)]
                                                    output[,technology:=ifelse(technology %in% c("CNG","CNG/Biogas","LPG"),"NG",technology)]
                                                    output=output[!technology %in% c("Other","All"),]
                                                    output=output[!is.na(MJ_km),]
                                                    output$country_name <- x
                                                    output=output[,.(MJ_km=mean(MJ_km)),by=c("EDGE_vehicle_type","year","technology","country_name")]
                                                    return(output)
                                                  }))
    setnames(energy_intensity_EU,old="EDGE_vehicle_type",new="vehicle_type")
    energy_intensity_EU=energy_intensity_EU[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name
    #include the sector fuel
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("BEV","Electric"),"elect_td_trn",NA)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Liquids"),"refined liquids enduse",sector_fuel)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("NG"),"delivered gas",sector_fuel)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Coal"),"delivered coal",sector_fuel)]

    energy_intensity_EU=merge(energy_intensity_EU,mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
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

    LF_countries_EU = merge(LF_countries_EU,mapping_TRACCS_iso, by = "country_name")[, -c("country_name")]
    ## use only one tech
    LF_countries_EU = unique(LF_countries_EU[,c("vehicle_type", "iso", "year", "load_factor")])
    ## and with all the logit categories
    LF_countries_EU[, year := as.numeric(as.character(year))]
    setnames(LF_countries_EU, old = "load_factor", new="loadFactor")


    am_countries_EU = merge(am_countries_EU,mapping_TRACCS_iso, by = "country_name")[, -c("country_name")]
    ## use only one tech
    am_countries_EU = unique(am_countries_EU[,c("vehicle_type", "iso", "year", "annual_mileage")])

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
    # browser()
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


    dem_eurostat = rbind(dem_bunkers, dem_dom, rail_eu, roadFE_eu)
    load_EU_data=list(dem_eurostat = dem_eurostat,
                      energy_intensity_EU=energy_intensity_EU,
                      roadFE_eu = roadFE_eu,
                      LF_countries_EU=LF_countries_EU,
                      am_countries_EU = am_countries_EU)
    return(load_EU_data)

}
