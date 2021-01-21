#' Load TRACCS data
#'
#' Loads absolute values demand (pkm,tkm) for:
#'   Road (pass+freight), Rail (pass+freight), Shipping (freight), Aviation (pass)
#' Loads factors and Energy intensity for Road (pass+freight)
#'
#' Final units for demand: millionkm (tkm and pkm)


#' @param input_folder folder hosting raw data
#' @param TRACCS_dir directory with TRACCS data
#' @param GCAM_data GCAM based input data
#'
#' @importFrom readxl read_excel


lvl0_loadTRACCS <- function(input_folder, GCAM_data, TRACCS_dir = "TRACCS"){
    countries <- technologies <- `.` <- region <- EDGE_vehicle_type <- km_million <- country_name <- technology <- tkm_million <- MJ_km <- load_factor <- million_tkm <- Liquids <- Electric <- pkm_million <- ktkm <- pkm <- name <- code_airplane_characteristics <- sector_fuel <- vehicle_type <- NULL
    TRACCS_folder <- file.path(input_folder, TRACCS_dir)

    ## load mappings
    mapping_TRACCS_roadf_categories = fread(file.path(TRACCS_folder, "mapping_TRACCS_roadvehicles.csv"), skip = 0)

    ## conversion unit->million
    CONV_unit_million <- 1e-06
    ## load Road data
    list_countries = data.table(countries=list.files(path = file.path(TRACCS_folder, "TRACCS_ROAD_Final_EXCEL_2013-12-20"),
                                                   all.files=FALSE))
    list_countries[, countries := gsub("Road Data |_Output.xlsx", "", countries)] #
    list_countries = list_countries[!grepl("\\$",countries),] #deletes open files, which have a $ in the name

    ##road passenger: load demand (million pkm-tkm)
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
    ## road freight: load demand
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
    road_EU=road_EU[,.(tech_output=sum(km_million)),by=c("year","country_name","vehicle_type","technology")]
    ## road passenger: load load factors
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

    ## road passenger and road freight: load energy intensity
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
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("BEV","Electric"),"elect_td_trn",NA)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Liquids"),"refined liquids enduse",sector_fuel)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("NG"),"delivered gas",sector_fuel)]
    energy_intensity_EU[,sector_fuel:=ifelse(technology %in% c("Coal"),"delivered coal",sector_fuel)]

    ## rail passenger: load demand
    railp_eu=suppressMessages(data.table(read_excel(
      path=file.path(
        TRACCS_folder,
        paste0("TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx")), sheet="eval_rail_pkm","A6:K61")))
    railp_eu = melt(railp_eu, id.vars = c("CountryID","Country","Countrytype_short","RailTrafficType","Unit_short"),
                    measure.vars = c("2005","2006","2007","2008","2009","2010"))

    setnames(railp_eu,old=c("value","variable"),new=c("pkm_million","year"))
    railp_eu=railp_eu[,.(km_million=sum(pkm_million)),by=c("year","Country")]
    railp_eu[,vehicle_type:="Passenger Rail_tmp_vehicletype"]

    ## rail freight: load demand
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
    setnames(rail_EU, old= c("Country", "value","variable"), new=c("country_name","tech_output","technology"))
    rail_EU=rail_EU[!country_name %in% c("Liechtenstein"),] #delete Liechtenstein as we don't have it in any other database

    #attribute 75% of demand to Electric trains and 25% to Liquid trains (on average for Europe https://www.iea.org/futureofrail/)

    ## shipping: load demand for each type of shipping
    list_countries=data.table(
      countries=list.files(
        path = file.path(TRACCS_folder, "TRACCS_WATERBORNE_Final_EXCEL_2013-12-20"), all.files=FALSE))
    list_countries[,countries:=gsub("_shipping.xlsx","",countries)]
    list_countries=list_countries[!countries%in% c("Turkey","Iceland"),]#don't load the countries that have missing data
    list_countries=list_countries[!grepl("\\$",countries),]

    ## SSS: short sea shipping
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
                                                 output[,million_tkm:=ktkm/1000]
                                                 output$country_name <- x
                                                 output=output[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")] #summarise to have total demand
                                                 return(output)
                                             }))
    ## IWW: inland waterways navigation
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
                                                 output[,million_tkm:=ktkm/1000]
                                                 output$country_name <- x
                                                 output=output[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")] #summarise to have total demand
                                                 return(output)
                                             }))
    ## DSS: deep sea shipping
    shippingDSS_eu <- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                                 output = suppressMessages(data.table(read_excel(path=file.path(TRACCS_folder, paste0("TRACCS_WATERBORNE_Final_EXCEL_2013-12-20/",x,"_shipping.xlsx")),
                                                                                sheet="DSS FREIGHT","A2:H6")))
                                                 colnames(output)=c("cargo_class","distance",as.character(seq(2005,2010,1)))
                                                 output = melt(output, id.vars = c("cargo_class","distance"),
                                                               measure.vars = c("2005","2006","2007","2008","2009","2010"))
                                                 setnames(output,old=c("value","variable"),new=c("ktkm","year"))
                                                 output[,million_tkm:=ktkm/1000]
                                                 output$country_name <- x
                                                 output=output[,.(million_tkm=sum(million_tkm)),by=c("year","country_name")] #summarise to have total demand
                                                 return(output)
                                             }))

    ## merge them together
    shipping_EU=Reduce(function(...) merge(..., all=TRUE), list(shippingSSS_eu,shippingIWW_eu,shippingDSS_eu))
    ## summarize values per year
    shipping_EU=shipping_EU[,.(tech_output=sum(million_tkm)),by=c("year","country_name")]
    shipping_EU[,technology:="Liquids"]
    shipping_EU[,vehicle_type:="Domestic Ship_tmp_vehicletype"] #I attribute all of the shipping data to domestic shipping
    shipping_EU[, vehicle_type := "Domestic Ship_tmp_vehicletype"]
    shipping_EU[, technology := "Liquids"]
    ## Domestic Aviation
    #load demand
    airp_EU=fread(file.path(TRACCS_folder, "TRACCS_AVI_Final_EXCEL_2013-12-20/aviation.csv"), sep=";", na.strings = "")#the CSV file is based on the excel sheet Avi_Pkm_Final_Eval.xlsx, sheet "fromDB_pkm_final"
    airp_EU = melt(airp_EU, id.vars = c("name","code_airplane_characteristics"),
             measure.vars = as.character(2005:2010))
    setnames(airp_EU,old=c("value","variable"),new=c("pkm","year"))
    airp_EU[, year := as.character(year)]
    airp_EU=airp_EU[,.(tech_output=CONV_unit_million*as.numeric(pkm),country_name=name,year=as.numeric(year),code_airplane_characteristics)]
    airp_EU = approx_dt(airp_EU,
                        xdata = seq(1990,2010),
                        ycol = "tech_output",
                        xcol = "year",
                        idxcols=c("country_name","code_airplane_characteristics"),
                        extrapolate = T)
    airp_EU = airp_EU[,.(tech_output =sum(tech_output)), by = c("year", "country_name")]
    airp_EU[, vehicle_type := "Domestic Aviation_tmp_vehicletype"]
    airp_EU[, technology := "Liquids"]
    ## merge all data
    dem_EU = rbind(airp_EU, shipping_EU, rail_EU, road_EU)
    ## merge with structure
    mapping_TRACCS_iso= fread(file.path(TRACCS_folder, "mapping_countries_EU.csv"), skip=0)
    #load the logit mapping so that I can attribute the full logit tree to each level
    logit_category=GCAM_data[["logit_category"]]

    ## the TRACCS database has to be with iso names
    dem_EU=merge(dem_EU,mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
    ## and with all the logit categories
    dem_EU=merge(dem_EU,logit_category,by=c("technology", "vehicle_type"))[, -c("univocal_name")]
    dem_EU[, year := as.numeric(as.character(year))]

    LF_countries_EU = merge(LF_countries_EU,mapping_TRACCS_iso, by = "country_name")[, -c("country_name")]
    ## and with all the logit categories
    LF_countries_EU = merge(LF_countries_EU, logit_category, by = c("technology", "vehicle_type"))[, -c("univocal_name")]
    LF_countries_EU[, year := as.numeric(as.character(year))]
    setnames(LF_countries_EU, old = "load_factor", new="loadFactor")
    load_TRACCS_data=list(dem_EU=dem_EU,
                          energy_intensity_EU=energy_intensity_EU,
                          LF_countries_EU=LF_countries_EU)
    return(load_TRACCS_data)

}

#' Prepare TRACCS data
#'
#' function that makes the TRACCS database compatible with the GCAM framework.
#' Final values: EI in MJ/km (pkm and tkm), demand in million km (pkm and tkm), LF in p/v
#'
#' @param TRACCS_data TRACCS based data
#' @param intensity energy intensity of technologies
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param REMIND2ISO_MAPPING
#'
#' @importFrom rmndt disaggregate_dt


lvl0_prepareTRACCS <- function(TRACCS_data,
                               iso_data,
                               intensity,
                               input_folder,
                               GCAM2ISO_MAPPING,
                               REMIND2ISO_MAPPING){

  subsector_L3 <- region <- technology <- subsector_L1 <- NULL

  ## missing time steps extrapolated
  TRACCS_data$dem_EU = approx_dt(dt = TRACCS_data$dem_EU,
                         xdata = c(1990, 2005, 2010),
                         xcol = "year",
                         ycol = "tech_output",
                         idxcols = c("iso", "sector","subsector_L3","subsector_L2","subsector_L1","vehicle_type","technology"),
                         extrapolate = TRUE)

  dups <- duplicated(TRACCS_data$dem_EU, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(TRACCS_data$dem_EU[dups])
    TRACCS_data$dem_EU <- unique(TRACCS_data$dem_EU, by=c("iso", "technology", "vehicle_type", "year"))
  }

  ## select based on the combination of variables available in TRACCS
  iso_data$TO_iso=TRACCS_data$dem_EU[iso_data$TO_iso,
                    on=c("iso", "year","vehicle_type","sector","subsector_L1","subsector_L2","subsector_L3","technology")]
  iso_data$TO_iso[, tech_output := ifelse(is.na(tech_output), i.tech_output, tech_output)]
  iso_data$TO_iso[,i.tech_output:=NULL]
  dups <- duplicated(iso_data$TO_iso, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(iso_data$TO_iso[dups])
    iso_data$TO_iso <- unique(iso_data$TO_iso, by=c("iso", "technology", "vehicle_type", "year"))
  }

  iso_data$TO_iso = aggregate_dt(iso_data$TO_iso,  REMIND2ISO_MAPPING,
                        valuecol="tech_output",
                        datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "year"))


  ## load factor
  iso_data$UCD_results$load_factor=TRACCS_data$LF_countries_EU[iso_data$UCD_results$load_factor,
                             on=c("iso", "year","vehicle_type","sector","subsector_L1","subsector_L2","subsector_L3","technology")]
  iso_data$UCD_results$load_factor[, loadFactor := ifelse(is.na(loadFactor), i.loadFactor, loadFactor)]
  iso_data$UCD_results$load_factor[,i.loadFactor:=NULL]
  dups <- duplicated(iso_data$UCD_results$load_factor, by=c("iso", "technology", "vehicle_type","technology","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(iso_data$iso_data$UCD_results$load_factor[dups])
    iso_data$UCD_results$load_factor <- unique(iso_data$UCD_results$load_factor, by=c("iso", "technology", "vehicle_type", "year"))
  }

  gdp <- getRMNDGDP(scenario = paste0("gdp_", REMIND_scenario), to_aggregate = FALSE, isocol = "iso", usecache = T, gdpfile = "GDPcache_iso.RDS")

  iso_data$UCD_results$load_factor <-  aggregate_dt(iso_data$UCD_results$load_factor, REMIND2ISO_MAPPING,
                               valuecol="loadFactor",
                               datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "year"),
                               weights=gdp)

  ## save demand, load factor
  output=list(LF=iso_data$UCD_results$load_factor,
              demkm = iso_data$TO_iso)

  return(output)
}

