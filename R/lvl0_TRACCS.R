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


lvl0_loadEU <- function(input_folder, EU_dir = "TRACCS"){
    countries <- tech_output <- technologies <- `.` <- region <- EDGE_vehicle_type <- MJ <- mtoe <- km_million <- country_name <- technology <- tkm_million <- MJ_km <- load_factor <- million_tkm <- Liquids <- Electric <- pkm_million <- ktkm <- pkm <- name <- code_airplane_characteristics <- sector_fuel <- vehicle_type <- NULL
    EU_folder <- file.path(input_folder, EU_dir)

    ## load mappings
    mapping_TRACCS_roadf_categories = fread(file.path(EU_folder, "mapping_TRACCS_roadvehicles.csv"), skip = 0)

    ## conversion unit->million
    CONV_unit_million <- 1e-06
    ## load Road data
    list_countries = data.table(countries=list.files(path = file.path(EU_folder, "TRACCS_ROAD_Final_EXCEL_2013-12-20"),
                                                   all.files=FALSE))
    list_countries[, countries := gsub("Road Data |_Output.xlsx", "", countries)] #
    list_countries = list_countries[!grepl("\\$",countries),] #deletes open files, which have a $ in the name

    ##road passenger: load demand (million pkm-tkm)
    roadp_eu = do.call("rbind",lapply(list_countries$countries,
                                       function(x) {
                                         output = suppressMessages(data.table(read_excel(
                                           path = file.path(
                                             EU_folder,
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
                                                        EU_folder,
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
                                             }))

    LF_countries_EU=LF_countries_EU[,country_name:=ifelse(country_name=="FYROM","Macedonia, the former Yugoslav Republic of",country_name)]#fix  FYROM name

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

    ## rail passenger: load demand
    railp_eu=suppressMessages(data.table(read_excel(
      path=file.path(
        EU_folder,
        paste0("TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx")), sheet="eval_rail_pkm","A6:K61")))
    railp_eu = melt(railp_eu, id.vars = c("CountryID","Country","Countrytype_short","RailTrafficType","Unit_short"),
                    measure.vars = c("2005","2006","2007","2008","2009","2010"))

    setnames(railp_eu,old=c("value","variable"),new=c("pkm_million","year"))
    railp_eu=railp_eu[,.(km_million=sum(pkm_million)),by=c("year","Country")]
    railp_eu[,vehicle_type:="Passenger Rail_tmp_vehicletype"]

    ## rail freight: load demand
    railf_eu=suppressMessages(data.table(read_excel(
      path=file.path(
        EU_folder,
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
        path = file.path(EU_folder, "TRACCS_WATERBORNE_Final_EXCEL_2013-12-20"), all.files=FALSE))
    list_countries[,countries:=gsub("_shipping.xlsx","",countries)]
    list_countries=list_countries[!countries%in% c("Turkey","Iceland"),]#don't load the countries that have missing data
    list_countries=list_countries[!grepl("\\$",countries),]

    ## SSS: short sea shipping
    shippingSSS_eu <- do.call("rbind",lapply(list_countries$countries,
                                             function(x) {
                                               output = suppressMessages(data.table(
                                                 read_excel(
                                                   path=file.path(
                                                     EU_folder,
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
                                                     EU_folder,
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
                                                 output = suppressMessages(data.table(read_excel(path=file.path(EU_folder, paste0("TRACCS_WATERBORNE_Final_EXCEL_2013-12-20/",x,"_shipping.xlsx")),
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
    airp_EU=fread(file.path(EU_folder, "TRACCS_AVI_Final_EXCEL_2013-12-20/aviation.csv"), sep=";", na.strings = "")#the CSV file is based on the excel sheet Avi_Pkm_Final_Eval.xlsx, sheet "fromDB_pkm_final"
    airp_EU = melt(airp_EU, id.vars = c("name","code_airplane_characteristics"),
             measure.vars = as.character(2005:2010))
    setnames(airp_EU,old=c("value","variable"),new=c("pkm","year"))
    airp_EU[, year := as.character(year)]
    airp_EU[, pkm := gsub(x = pkm, pattern = ",", replacement = ".")]
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
    ## there is an issue of order of magnitudes in aviation... remove for now
    ## merge all data
    dem_EU = rbind(shipping_EU, rail_EU, road_EU)
    ## merge with structure
    mapping_TRACCS_iso= fread(file.path(EU_folder, "mapping_countries_EU.csv"), skip=0)

    ## the TRACCS database has to be with iso names
    dem_EU=merge(dem_EU,mapping_TRACCS_iso,by="country_name")[,-c("country_name")]
    dem_EU[, year := as.numeric(as.character(year))]

    LF_countries_EU = merge(LF_countries_EU,mapping_TRACCS_iso, by = "country_name")[, -c("country_name")]
    ## use only one tech
    LF_countries_EU = unique(LF_countries_EU[,c("vehicle_type", "iso", "year", "load_factor")])
    ## and with all the logit categories
    LF_countries_EU[, year := as.numeric(as.character(year))]
    setnames(LF_countries_EU, old = "load_factor", new="loadFactor")

    ## load eurostata for bunkers
    map_bunkers = data.table(country_name = c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
    iso = c("BEL","BGR","CZE","DNK","DEU","EST","IRL","GRC","ESP","FRA","HRV","ITA","CYP","LVA","LTU","LUX","HUN","MLT","NLD","AUT","POL","PRT","ROU","SVN","SVK","FIN","SWE","GBR"))
    dem_bunkers <- do.call("rbind",lapply(map_bunkers$country_name,
                                      function(x) {
                                        output = suppressMessages(data.table(read_excel(path=file.path(EU_folder, "Eurostat/energy_statistical_countrydatasheets.xlsx"),
                                                                                               sheet=x,"C8:X95")))
                                        colnames(output)=c("name", as.character(seq(1990,2010,1)))
                                        output = output[name %in% c("International maritime bunkers", "International aviation")]
                                        output = output[, c("name", "1990", "2005", "2010")]
                                        output = melt(output, id.vars = c("name"),
                                                      measure.vars = c("1990","2005","2010"))
                                        setnames(output,old=c("value"),new=c("mtoe"))
                                        output[,MJ := mtoe*41868]
                                        output$country_name <- x
                                        return(output)
                                        }))

    ## merge with regional mapping for EUROSTAT
    dem_bunkers = merge(dem_bunkers, map_bunkers)[, c("country_name","mtoe") := NULL]
    dem_bunkers[, vehicle_type := ifelse(name == "International maritime bunkers", "International Ship_tmp_vehicletype", "International Aviation_tmp_vehicletype")][, name := NULL]
    setnames(dem_bunkers, old ="variable", new = "year")
    dem_bunkers[, year := as.numeric(as.character(year))]

    load_EU_data=list(dem_EU=dem_EU,
                          dem_bunkers = dem_bunkers,
                          energy_intensity_EU=energy_intensity_EU,
                          LF_countries_EU=LF_countries_EU)
    return(load_EU_data)

}

#' Prepare TRACCS and Eurostat data to merge them with other input data
#'
#' function that makes the TRACCS and Eurostat databases compatible with the rest of the data
#' Final values: EI in MJ/km (pkm and tkm), demand in million km (pkm and tkm), LF in p/v
#'
#' @param EU_data TRACCS based data
#' @param iso_data iso level data
#' @param GDP_country GDP ISO level
#' @param intensity energy intensity of technologies
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param REMIND2ISO_MAPPING REMIND2iso mapping
#'
#' @importFrom rmndt approx_dt aggregate_dt


lvl0_prepareEU <- function(EU_data,
                           iso_data,
                           intensity,
                           GDP_country,
                           input_folder,
                           GCAM2ISO_MAPPING,
                           REMIND2ISO_MAPPING){
  subsector_L3 <- region <- technology <- conv_pkm_MJ <-iso <- dem <- NULL
  subsector_L1 <- tech_output <- vehicle_type <- tech_output <- MJ <- `.` <- NULL
  i.tech_output <- i.loadFactor <- loadFactor <- LF_OTHERr <- REMIND_scenario <-  NULL
  dem_OTHER = copy(iso_data$TO_iso)

  ## use intensity and LF from GCAM to convert into tkm/pkm for bunkers
  bunk = merge(EU_data$dem_bunk, intensity[region =="EU-15" & vehicle_type %in% c("International Ship_tmp_vehicletype", "International Aviation_tmp_vehicletype")][,c("year","conv_pkm_MJ","vehicle_type")])
  bunk[, tech_output := MJ/conv_pkm_MJ][, c("MJ", "conv_pkm_MJ") := NULL]
  bunk[, technology := "Liquids"]

  ## missing time steps extrapolated: TRACCS features yearly values between 2005 and 2010, but 1990 is absent. Remove the extra time steps as well
  dem_TRACCS = approx_dt(dt = EU_data$dem_EU,
                         xdata = c(1990, 2005, 2010),
                         xcol = "year",
                         ycol = "tech_output",
                         idxcols = c("iso", "vehicle_type","technology"),
                         extrapolate = TRUE)

  ## merge TRACCS and Eurostat data
  dem_EU = rbind(bunk, dem_TRACCS)

  dups <- duplicated(dem_EU, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(dem_EU[dups])
    dem_EU <- unique(dem_EU, by=c("iso", "technology", "vehicle_type", "year"))
  }

  ## select based on the combination of variables available in TRACCS
  dem_OTHER[dem_EU, on=.(iso, vehicle_type, technology, year), tech_output := i.tech_output]

  dups <- duplicated(dem_OTHER, by=c("iso", "technology", "vehicle_type","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(dem_OTHER[dups])
    dem_OTHER <- unique(dem_OTHER, by=c("iso", "technology", "vehicle_type", "year"))
  }

  ## keep ISO level demand for 2010, to be used as weights in mrremind run
  demISO = dem_OTHER[year == 2010][,.(dem = sum(tech_output)), by = c("iso", "sector")]
  demISO[, dem := round(dem)]
  dem_OTHER = aggregate_dt(dem_OTHER,  REMIND2ISO_MAPPING,
                        valuecol="tech_output",
                        datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "year"))

  ## load factor
  LF_OTHER = copy(iso_data$UCD_results$load_factor)
  LF_TRACCS = EU_data$LF_countries_EU[year == 2010][, year := NULL]
  LF_OTHER[LF_TRACCS, on=.(iso, vehicle_type), loadFactor := i.loadFactor]

  dups <- duplicated(LF_OTHER, by=c("iso", "vehicle_type","technology","year"))

  if(any(dups)){
    warning("Duplicated techs found in supplied demand.")
    print(LF_OTHERr[dups])
    LF_OTHER <- unique(LF_OTHER, by=c("iso", "technology", "vehicle_type", "year"))
  }
  gdp =copy(GDP_country)
  LF_OTHER <-  aggregate_dt(LF_OTHER, REMIND2ISO_MAPPING,
                               valuecol="loadFactor",
                               datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "year"),
                               weights=gdp)

  ## save demand, load factor
  output=list(LF=LF_OTHER,
              demkm = dem_OTHER,
              demISO = demISO)

  return(output)
}

