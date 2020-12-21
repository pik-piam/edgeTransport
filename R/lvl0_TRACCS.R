#' Load TRACCS data
#'
#' Loads absolute values demand (pkm,tkm) for:
#'   Road (pass+freight), Rail (pass+freight), Shipping (freight), Aviation (pass)
#' Loads factors and Energy intensity for Road (pass+freight)
#'
#' Final units for demand: millionkm (tkm and pkm)


#' @param input_folder folder hosting raw data
#' @param TRACCS_dir directory with TRACCS data
#'
#' @importFrom readxl read_excel


lvl0_loadTRACCS <- function(input_folder, TRACCS_dir = "TRACCS"){
    countries <- technologies <- `.` <- region <- EDGE_vehicle_type <- km_million <- country_name <- technology <- tkm_million <- MJ_km <- load_factor <- million_tkm <- Liquids <- Electric <- pkm_million <- ktkm <- pkm <- name <- code_airplane_characteristics <- sector_fuel <- vehicle_type <- NULL
    TRACCS_folder <- file.path(input_folder, TRACCS_dir)

    ##==== load mappings ====
    mapping_TRACCS_roadf_categories = fread(file.path(TRACCS_folder, "mapping_TRACCS_roadvehicles.csv"), skip = 0)

    ## conversion unit->million
    CONV_unit_million <- 1e-06
                                        #==== Load Road data ====
    list_countries = data.table(countries=list.files(path = file.path(TRACCS_folder, "TRACCS_ROAD_Final_EXCEL_2013-12-20"),
                                                   all.files=FALSE))
    list_countries[, countries := gsub("Road Data |_Output.xlsx", "", countries)] #
    list_countries = list_countries[!grepl("\\$",countries),] #deletes open files, which have a $ in the name


                                        #road passenger: load demand (million pkm-tkm)
    roadp_eu = do.call("rbind",lapply(list_countries$countries,
                                       function(x) {
                                         output = suppressMessages(data.table(read_excel(
                                           path = file.path(
                                             TRACCS_folder,
                                             paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ", x, "_Output.xlsx")),
                                           sheet="Pass-Km","A2:I51")))

                                         colnames(output)=c("category_TRACCS","vehicle_type","technology",as.character(seq(2005,2010,1)))
                                           output[,technology := ifelse(technology %in% c("Gasoline","Diesel","Flexi-fuel","LPG","B30"),"Liquids",technology)]
                                           output[,technology := ifelse(technology %in% c("CNG","CNG/Biogas"),"NG",technology)]
                                           output[,technology := ifelse(technology %in% c("Other"),"Liquids",technology)]
                                           output=output[!technology %in% c("All"),]
                                           output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                                                         measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                           setnames(output,old=c("value","variable"), new = c("km_million","year"))
                                           output = merge(mapping_TRACCS_roadf_categories,output)
                                           output$country_name <- x
                                           output = output[, .(vehicle_type=EDGE_vehicle_type,technology,year,km_million,country_name)]
                                           output = output[, .(km_million=sum(km_million)),by=c("year","country_name","technology","vehicle_type")]
                                           return(output)
                                       }))

    roadp_eu = roadp_eu[, country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name
                                        #road freight: load demand
    roadf_eu <- do.call("rbind",lapply(list_countries$countries,
                                       function(x) {
                                         output = suppressMessages(data.table(
                                           read_excel(path=file.path(
                                                        TRACCS_folder,
                                                        paste0("TRACCS_ROAD_Final_EXCEL_2013-12-20/Road Data ",x,"_Output.xlsx")),
                                                      sheet="Tonne-Km","A2:I20")))
                                           colnames(output)=c("category_TRACCS", "vehicle_type", "technology", as.character(seq(2005,2010,1)))
                                           output[,technology:=ifelse(technology %in% c("Gasoline","Diesel","LPG","Other", "Flexi-fuel","B30"),"Liquids",technology)]
                                           output[,technology:=ifelse(technology %in% c("CNG","CNG/Biogas"),"NG",technology)]
                                           # output[,technology:=ifelse(technology %in% c("Other"),"BEV",technology)]
                                           output=output[!technology %in% c("All"),]
                                           output = melt(output, id.vars = c("category_TRACCS","vehicle_type","technology"),
                                                         measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                           setnames(output,old=c("value","variable"),new=c("km_million","year"))
                                           output=merge(mapping_TRACCS_roadf_categories,output)
                                           output$country_name <- x
                                           output=output[,.(vehicle_type=EDGE_vehicle_type,technology,year,km_million,country_name)]
                                           return(output)
                                       }))

    roadf_eu=roadf_eu[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name

    road_EU=merge(roadf_eu,roadp_eu,all=TRUE)
    road_EU=road_EU[,.(km_million=sum(km_million)),by=c("year","country_name","vehicle_type","technology")]
                                        #road passenger: load load factors
    LF_countries_EU<- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                               output = suppressMessages(data.table(read_excel(
                                                 path=file.path(
                                                   TRACCS_folder,
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
                                             }))

    LF_countries_EU=LF_countries_EU[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name


                                        #road passenger and road freight: load energy intensity
    energy_intensity_EU <- do.call("rbind",lapply(list_countries$countries,
                                                  function(x) {
                                                    output = suppressMessages(data.table(
                                                      read_excel(
                                                        path=file.path(
                                                          TRACCS_folder,
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
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Adv-Electric","BEV","Electric","LA-BEV","Tech-Adv-Electric"),"elect_td_trn",NA)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Adv-Liquid","Liquids","Tech-Adv-Liquid"),"refined liquids enduse",sector_fuel)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("NG"),"delivered gas",sector_fuel)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Coal"),"delivered coal",sector_fuel)]
    #==== Load Rail data ====
    #rail passenger: load demand
    railp_eu=suppressMessages(data.table(read_excel(
      path=file.path(
        TRACCS_folder,
        paste0("TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx")), sheet="eval_rail_pkm","A6:K61")))
    railp_eu = melt(railp_eu, id.vars = c("CountryID","Country","Countrytype_short","RailTrafficType","Unit_short"),
                    measure.vars = c("2005","2006","2007","2008","2009","2010"))

    setnames(railp_eu,old=c("value","variable"),new=c("pkm_million","year"))
    railp_eu=railp_eu[,.(km_million=sum(pkm_million)),by=c("year","Country")]
    railp_eu[,vehicle_type:="Passenger Rail_tmp_vehicletype"]

                                        #rail freight: load demand
    railf_eu=suppressMessages(data.table(read_excel(
      path=file.path(
        TRACCS_folder,
        paste0("TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx")),
      sheet="eval_rail_tkm","A6:K122")))
    railf_eu = melt(railf_eu, id.vars = c("CountryID","Country","Countrytype_short","RailTrafficType","Cargoclass_long"),
                    measure.vars = c("2005","2006","2007","2008","2009","2010"))

    setnames(railf_eu,old=c("value","variable"),new=c("tkm_million","year"))
    railf_eu=railf_eu[,.(km_million=sum(tkm_million)),by=c("year","Country")]
    railf_eu[,vehicle_type:="Freight Rail_tmp_vehicletype"]

    rail_EU=merge(railf_eu,railp_eu,all=TRUE)
    rail_EU[,Electric:=0.75*km_million]
    rail_EU[,Liquids:=0.25*km_million]
    rail_EU[,km_million:=NULL]
    rail_EU = melt(rail_EU, id.vars = c("year", "Country", "vehicle_type"),
                 measure.vars = c("Liquids", "Electric"))
    setnames(rail_EU,old="Country",new="country_name")
    rail_EU=rail_EU[!country_name %in% c("Liechtenstein"),] #delete Liechtenstein as we don't have it in any other database

    #attribute 75% of demand to Electric trains and 25% to Liquid trains (on average for Europe https://www.iea.org/futureofrail/)

                                        #==== Load Shipping data ====
                                        #shipping: load demand for each type of shipping
    list_countries=data.table(
      countries=list.files(
        path = file.path(TRACCS_folder, "TRACCS_WATERBORNE_Final_EXCEL_2013-12-20"), all.files=FALSE))
    list_countries[,countries:=gsub("_shipping.xlsx","",countries)]
    list_countries=list_countries[!countries%in% c("Turkey","Iceland"),]#don't load the countries that have missing data
    list_countries=list_countries[!grepl("\\$",countries),]

                                        #SSS: short sea shipping
    shippingSSS_eu <- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                               output = suppressMessages(data.table(
                                                 read_excel(
                                                   path=file.path(
                                                     TRACCS_folder,
                                                     paste0("TRACCS_WATERBORNE_Final_EXCEL_2013-12-20/",x,"_shipping.xlsx")),
                                                   sheet="SSS_FREIGHT","A2:J86")))
                                                 colnames(output)=c("vessel","size","cargo_class","distance",as.character(seq(2005,2010,1)))
                                                 output = melt(output, id.vars = c("vessel","size","cargo_class","distance"),
                                                               measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                                 setnames(output,old=c("value","variable"),new=c("ktkm","year"))
                                                 output[,million_tkm:=ktkm*1000]
                                                 output$country_name <- x
                                                 output=output[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")] #summarise to have total demand
                                                 return(output)
                                             }))
                                        #IWW: inland waterways navigation
    shippingIWW_eu <- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                               output = suppressMessages(data.table(
                                                 read_excel(
                                                   path=file.path(
                                                     TRACCS_folder,
                                                     paste0("TRACCS_WATERBORNE_Final_EXCEL_2013-12-20/",x,"_shipping.xlsx")),
                                                   sheet="IWW FREIGHT","A2:J114")))
                                                 colnames(output)=c("vessel","size","cargo_class","distance",as.character(seq(2005,2010,1)))
                                                 output = melt(output, id.vars = c("vessel","size","cargo_class","distance"),
                                                               measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                                 setnames(output,old=c("value","variable"),new=c("ktkm","year"))
                                                 output[,million_tkm:=ktkm*1000]
                                                 output$country_name <- x
                                                 output=output[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")] #summarise to have total demand
                                                 return(output)
                                             }))
                                        #DSS: deep sea shipping
    shippingDSS_eu <- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                                 output = suppressMessages(data.table(read_excel(path=file.path(TRACCS_folder, paste0("TRACCS_WATERBORNE_Final_EXCEL_2013-12-20/",x,"_shipping.xlsx")),
                                                                                sheet="DSS FREIGHT","A2:H6")))
                                                 colnames(output)=c("cargo_class","distance",as.character(seq(2005,2010,1)))
                                                 output = melt(output, id.vars = c("cargo_class","distance"),
                                                               measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                                 setnames(output,old=c("value","variable"),new=c("ktkm","year"))
                                                 output[,million_tkm:=ktkm*1000]
                                                 output$country_name <- x
                                                 output=output[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")] #summarise to have total demand
                                                 return(output)
                                             }))

                                        #merge them together
    shipping_EU=Reduce(function(...) merge(..., all=TRUE), list(shippingSSS_eu,shippingIWW_eu,shippingDSS_eu))
                                        #summarize values per year
    shipping_EU=shipping_EU[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")]
    shipping_EU[,technology:="Liquids"]
    shipping_EU[,vehicle_type:="Domestic Ship_tmp_vehicletype"] #I attribute all of the shipping data to domestic shipping
                                        #==== Load Aviation data ====
    #load demand
    airp_EU=fread(file.path(TRACCS_folder, "TRACCS_AVI_Final_EXCEL_2013-12-20/aviation.csv"), sep=";", na.strings = "")#the CSV file is based on the excel sheet Avi_Pkm_Final_Eval.xlsx, sheet "fromDB_pkm_final"
    airp_EU = melt(airp_EU, id.vars = c("name","code_airplane_characteristics"),
             measure.vars = as.character(2005:2010))
    setnames(airp_EU,old=c("value","variable"),new=c("pkm","year"))
    airp_EU=airp_EU[,.(million_pkm=CONV_unit_million*as.numeric(pkm),country_name=name,year=as.numeric(year),code_airplane_characteristics)]
    airp_EU=airp_EU[country_name=="Austria",]
    # airp_EU=airp_EU[,c_code:=paste0(country_name,"#",code_airplane_characteristics)]
    # airp_EU[,country_name:=NULL]
    # airp_EU[,code_airplane_characteristics:=NULL]
    #                                     airp_EU=airp_EU[c_code=="Austria#13118",]
    #                                     airp_EU=airp_EU[order(c_code,year)]
    #                                     airp_EU=extrapolate_dt(dt = airp_EU,xrange = 2005:2010,xcol = "year") #TODO FIX!!
    #                                     airp_EU=interpolate_dt(dt = airp_EU,xdata = 2005:2010,xcol = "year",ycol = "million_pkm",idxcols = "c_code") #TODO FIX!!
    #                                     ==== Save RDS files ====

    load_TRACCS_data=list(road_EU=road_EU,
                                        #airp_EU=airp_EU,
                          shipping_EU=shipping_EU,
                          rail_EU=rail_EU,
                          energy_intensity_EU=energy_intensity_EU,
                                        # ,
                          LF_countries_EU=LF_countries_EU
                          )
    return(load_TRACCS_data)

}

#' Prepare TRACCS data
#'
#' function that makes the TRACCS database compatible with the GCAM framework.
#' Final values: EI in MJ/km (pkm and tkm), demand in million km (pkm and tkm), LF in p/v
#'
#' @param TRACCS_data TRACCS based data
#' @param GCAM_data GCAM based data
#' @param intensity energy intensity of technologies
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param TRACCS_dir folder hosting TRACCS data
#' 
#' @importFrom rmndt disaggregate_dt


lvl0_prepareTRACCS <- function(TRACCS_data,
                               GCAM_data,
                               intensity,
                               input_folder,
                               GCAM2ISO_MAPPING,
                               TRACCS_dir="TRACCS"){
  
  subsector_L3 <- region <- technology <- subsector_L1 <- NULL
  TRACCS_folder <- file.path(input_folder, TRACCS_dir)

  #load mapping
  mapping_TRACCS_iso= fread(file.path(TRACCS_folder, "mapping_countries_EU.csv"), skip=0)
  #load the logit mapping so that I can attribute the full logit tree to each level
  logit_category=GCAM_data[["logit_category"]]
  ##load LF and vehicle intensity from GCAM
  conv_pkm_mj=intensity

  dups <- duplicated(conv_pkm_mj, by = c("region", "technology", "vehicle_type","year"))
  if(any(dups)){
    warning("Duplicated techs found energy intensity.")
    print(conv_pkm_mj[dups])
    conv_pkm_mj <- unique(conv_pkm_mj, by=c("region", "technology", "vehicle_type","year"))
  }

  load_factor=GCAM_data[["load_factor"]]

  dups <- duplicated(load_factor, by=c("region", "technology", "vehicle_type","year"))
  if(any(dups)){
    warning("Duplicated techs found load_factor.")
    print(load_factor[dups])
    load_factor <- unique(load_factor, by = c("region", "technology", "vehicle_type","year"))
  }

  ##load demand
  dem_TRACCS=TRACCS_data[["road_EU"]]

  #### LF ####
  ##purpose of this section is looking at the LF and analyse the dicsrepancies between TRACCS and GCAM
  ### EU-15
  ## which vehicle types do I need LFs? OK
  vehicles=setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-15"),c("vehicle_type")]) )
  print(paste0(vehicles, " are absent in EU-15 and are integrated from EU-12 for LF"))

  ##are there extra entries in GCAM? Mini Van, Truck (0-1t), Truck (0-3.5t), Truck (6-15t), Truck (>15t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-15"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )

  ### EU-12
  ## which vehicle types do I need LFs? None! I will take it from EU-15
  vehicles=setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-12"),c("vehicle_type")]) )
  print(paste0(vehicles, " are absent in EU-12 and are integrated from EU-15 for LF"))

  ##are there extra entries in GCAM? Van, Truck (0-3.5t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-12"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )

  vehicles=setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("European Free Trade Association"),c("vehicle_type")]) )
  print(paste0(vehicles, " are absent in European Free Trade Association and are integrated from EU-12 for LF"))

  ##are there extra entries in GCAM? Van, Truck (0-3.5t) are not present in the TRACCS database, therefore I have to filter it out
  setdiff(unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("European Free Trade Association"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )
  ### Europe Non EU
  vehicles=setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("Europe Non EU"),c("vehicle_type")]) )
  print(paste0(vehicles, " are absent in European Free Trade Association and are integrated from EU-12 for LF"))

  ##are there extra entries in GCAM? Van, Truck (0-3.5t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(load_factor[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("Europe Non EU"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )

  LF_TRACCS=disaggregate_dt(data = load_factor, GCAM2ISO_MAPPING)
  dups <- duplicated(LF_TRACCS, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(LF_TRACCS[dups])
    tech_output <- unique(LF_TRACCS, by=c("iso", "technology", "vehicle_type","year"))
  }


  #### EI ####
  ##purpose of this section is looking at the EI and analyse the dicsrepancies between TRACCS and GCAM

  ### EU-15
  ## which vehicle types do I need EIs? all vehicle types that are present in TRACCS are also in GCAM, OK
  setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-15"),c("vehicle_type")]) )

  ##are there extra entries in GCAM? Van, Truck (0-1t), Truck (0-3.5t), Truck (6-15t), Truck (>15t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-15"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )

  ### EU-12
  ## which vehicle types do I need EIs? None: OK
  setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-12"),c("vehicle_type")]) )

  ##are there extra entries in GCAM? Mini cars Van, Truck (0-3.5t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                               region %in% c("EU-12"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )
  ###  European Free Trade Association
  ## which vehicle types do I need EIs? None: OK
  setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                                region %in% c("European Free Trade Association"),c("vehicle_type")]) )

  ##are there extra entries in GCAM? Mini cars Van, Truck (0-3.5t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                                region %in% c("European Free Trade Association"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )

  ### Europe Non EU
  ## which vehicle types do I need EIs? None: OK
  setdiff(unique(dem_TRACCS[,c("vehicle_type")]),
          unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                                region %in% c("Europe Non EU"),c("vehicle_type")]) )

  ##are there extra entries in GCAM? Mini cars Van, Truck (0-3.5t) are not present in the TRACCS database: they will be automatically excluded as there is no corresponding demand
  setdiff(unique(conv_pkm_mj[subsector_L3 %in% c("trn_pass_road","trn_freight_road")&
                                region %in% c("Europe Non EU"),c("vehicle_type")]),
          unique(dem_TRACCS[,c("vehicle_type")]) )

  #### demand ####
  ## the TRACCS database has to be with iso names
  dem_TRACCS=merge(dem_TRACCS,mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
  ## and with all the logit categories
  dem_TRACCS=merge(dem_TRACCS,logit_category,by=c("technology","vehicle_type"))[,-c("univocal_name")]
  ## also needs to be renamed the demand column
  setnames(dem_TRACCS,old="km_million",new="tech_output")
  dem_TRACCS[,year:=as.numeric(as.character(year))]
  dem_TRACCS=dem_TRACCS[!year %in% c(2006,2007,2008,2009),] ## these values are not required, I filter them out
  ##for the sake of not having problems, I assume the same values for 1990 (we are never going to look at them anyways)
  dem_TRACCS1990=dem_TRACCS[year==2005,]
  dem_TRACCS1990[, year := 1990]
  dem_TRACCS = rbind(dem_TRACCS,dem_TRACCS1990)

  dem_TRACCSBEV = dem_TRACCS[technology=="Liquids",][, c("technology","tech_output") := list("BEV", 0)]
  dem_TRACCSFCEV = dem_TRACCS[technology=="Liquids",][, c("technology","tech_output") := list("FCEV", 0)]
  dem_TRACCSPIH = dem_TRACCS[technology=="Liquids",][, c("technology","tech_output") := list("Hybrid Electric", 0)]

  dem_TRACCSNG = dem_TRACCS[technology=="Liquids" & subsector_L1 == "trn_freight_road_tmp_subsector_L1",][, c("technology", "tech_output") := list("NG", 0)]

  dem_TRACCS = rbindlist(list(dem_TRACCS, dem_TRACCSFCEV, dem_TRACCSBEV, dem_TRACCSNG, dem_TRACCSPIH))

  dups <- duplicated(dem_TRACCS, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(dem_TRACCS[dups])
    dem_TRACCS <- unique(dem_TRACCS, by=c("iso", "technology", "vehicle_type", "year"))
  }




  ## save demand, load factor
  output=list(dem_TRACCS=dem_TRACCS,
              LF_TRACCS=LF_TRACCS)

  return(output)
}


#' Merge TRACCS data to ISO level output
#'
#' EI in MJ/km, LF in p/v, demand in million pkm or million tkm
#'
#' @param TRACCS_data TRACCS based data
#' @param output iso level data
#' @param REMIND2ISO_MAPPING REMIND2iso mapping

lvl0_mergeTRACCS <- function(TRACCS_data, output, REMIND2ISO_MAPPING){
  iso <- subsector_L3 <- subsector_L2 <- technology <- NULL
  ## load TRACCS-friendly data
  output_EU <- TRACCS_data$dem_TRACCS

  output=output[!(iso %in% unique(output_EU$iso) & subsector_L3 %in% c("trn_pass_road","trn_freight_road")),] #remove all the entries that has to come from TRACCS: road passenger and freight

  ## FCEV and electric Buses and Trucks are not provided by TRACCS, but needs to be added. Same goes for Mini cars with Hybrid Electric (same procedure as in lvl0_correctTechOutput)
  newtech = NULL

  for (i in unique(output_EU$iso)) {
    electric = output_EU[(subsector_L3 %in% c("trn_freight_road")|subsector_L2 == "Bus") &
                           technology == "Liquids" &
                           iso == i][, c("technology", "tech_output") := list("Electric", 0)]
    hydrogen = output_EU[(subsector_L3 %in% c("trn_freight_road")|subsector_L2 == "Bus") &
                            technology == "Liquids" &
                            iso == i][, c("technology", "tech_output") := list("FCEV", 0)]
    newtech = rbind(newtech, electric, hydrogen)

  }

  output=rbind(output, output_EU, newtech)

  dups <- duplicated(output, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(output[dups])
    output <- unique(output, by=c("iso", "technology", "vehicle_type"))
  }
  output = aggregate_dt(output,  REMIND2ISO_MAPPING,
                        valuecol="tech_output",
                        datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "year"))
  return(output)

}
