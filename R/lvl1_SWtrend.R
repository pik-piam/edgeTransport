#' Create regional clusters to bin different share weight trends.
#' 
#' @param input_folder folder hosting raw data
#' @param GDP GDP regional level
#' @param POP population (regional aggregation)
#' @param REMIND_scenario SSP scenario
#' @param REMIND2ISO_MAPPING REMIND2iso mapping
#' @param WDI_dir directory with WDI data
#' @return clusters grouping regions for preference factors trends
#' @author Marianna Rottoli


lvl1_SWclustering <- function(input_folder, POP, GDP, REMIND_scenario, REMIND2ISO_MAPPING, WDI_dir="WDI"){
  region <- `.` <- var <- value <- POP_val <- cluster <- AG.SRF.TOTL.K2 <- gdpcap <- weight <- region3c <- region <- NULL
  iso3c <- sector_fuel <- iso <- NULL
  ## WDI area country is part of internal package data
  ## only one year is used, as area is approximately constant in time
  WDI_file <- function(fname){
    file.path(input_folder, WDI_dir, fname)
  }
  WDI_area_country = readRDS(WDI_file("WDI_area_country.RDS"))
  
  area_country = WDI_area_country[year==2010,.(iso=as.character(iso3c),value=as.numeric(AG.SRF.TOTL.K2),year)]
  ## calculate area of REMIND regions
  ## filter out only the countries that are in REMIND
  area_country=area_country[iso %in% REMIND2ISO_MAPPING$iso,]
  area_country[, year :=NULL]
  ## aggregate
  area = aggregate_dt(area_country,
                      mapping=REMIND2ISO_MAPPING,
                      valuecol = "value",
                      yearcol = NULL,
                      datacols=NULL)

  ## attribute a datacol to the dt
  area[,var:="area"]

  
  ## load and scale population
  POP=POP[,.(region,year,POP,
             POP_val=value              ## in million
             *1e6)]                     ## in units
  
  
  ## calculate density
  density=merge(POP, area,  by =c("region"))
  density[,density:=POP_val      ## in units
          /value]                ## in units per square km
  
  ## find in 2015 the classes of density
  density[year==2015 & density<=100,cluster:="low",by="region"]
  density[year==2015 & density>100 & density<200,cluster:="medium",by="region"]
  density[year==2015 & density>=200,cluster:="high",by="region"]
  
  ## create a dt with all regions and their class
  clusters=density[!is.na(cluster),c("cluster","region")]
  density[,cluster:=NULL]
  density=merge(density,clusters)
  ## find leading region through max GDP capita in 2015 
  gdp=merge(GDP,POP,by=c("region","year"))
  gdp[,gdpcap:=weight/POP_val]
  
  
  gdp=merge(gdp,clusters)
  gdp=gdp[year==2015,]                                ## for year 2015
  gdp=gdp[,.SD[gdpcap==max(gdpcap)], by=c("cluster")] ## find the leading region for each cluster
  gdp=gdp[,.(region_leader=region,cluster)]
  clusters=merge(clusters,gdp,all=TRUE,by="cluster")
  
  result=list(density,clusters)
  
  return(result)
}


#' Calculate a trend for the share weights based on the EDGE scenario and the regional clusters.
#'
#' @param calibration_output historically calibrated preference factors
#' @param GDP regional level 
#' @param clusters clusters of regions based on geographical structure
#' @param years time steps
#' @param REMIND2ISO_MAPPING REMIND2iso mapping
#' @param REMIND_scenario SSP scenario
#' @param EDGE_scenario EDGE transport scenario specifie
#' @importFrom rmndt aggregate_dt disaggregate_dt
#' @return projected trend for preference factors
#' @author Marianna Rottoli, Alois Dirnaichner


lvl1_SWtrend <- function(calibration_output, GDP, clusters, years, REMIND2ISO_MAPPING, REMIND_scenario, EDGE_scenario){
  region<- cluster <- sw_cluster <- sw <- technology <- subsector_L1 <- subsector_L2 <- iso <- subsector_L3 <- NULL
  ## function to converge to average "leader region" level
  aveval=function(dt,path2clusters){
    sw <- delete <- NULL
    dt_tmp=copy(dt)
    ## load region clusters
    dt= aggregate_dt(data=dt,
                    mapping=REMIND2ISO_MAPPING,
                    weights = GDP,
                    datacols = names(dt)[!c(names(dt))%in% c("year","iso","sw")],
                    valuecol = "sw")
    
    dt=dt[year==2100 & region %in% c(clusters$region_leader),]
    ## merge by clusters so that I know the final value 
    dt=merge(dt,unique(clusters[,c("region_leader","cluster")]),all=TRUE,by.x = "region",by.y = "region_leader")
    ## forget about the leader region
    dt[,region:=NULL]
    ## merge with all regions again, by clusters
    dt=merge(dt,unique(clusters[,c("region","cluster")]),all=TRUE,by="cluster",allow.cartesian = TRUE)
    dt[,cluster:=NULL]
    ## attribute to countries again
    dt= disaggregate_dt(data=dt,
                       mapping = REMIND2ISO_MAPPING,
                       datacols = names(dt)[!c(names(dt))%in% c("year","region","sw")],
                       valuecol = "sw")

    ## year 2100 has to come from the leader regions. If it's not provided by the leader region, the country level is kept
    names_merge = intersect(names(dt), names(dt_tmp)[names(dt_tmp)!="sw"]) ## all column except for the sw value
    dt_2100 = merge(dt[year==2100], setnames(dt_tmp[year == 2100], old ="sw", new = "sw_cluster"), all =T, by = names_merge)
    dt_2100[, sw_cluster := ifelse(is.na(sw_cluster), sw, sw_cluster)]     ## for the few combinations country-mode without sw value provided by the cluster, the original value is kept
    dt_2100[,sw:=NULL]
    setnames(dt_2100, old = "sw_cluster", new = "sw")
    ## merge with original database
    dt_tmp = merge(dt_tmp[year!=2100], dt_2100, all = T, by = intersect(names(dt_tmp), names(dt_2100)))

    ##' in order to avoid inconsistencies, only the combinations present in dt_tmp are to be taken from dt, which instead is based on a cartesian product.
    ##' If the sum of all years present in the group is 2100, i.e. 2100 is the only year present, the column has to be deleted
    dt_tmp[,delete:=ifelse(sum(year)==2100,TRUE,FALSE),
           by=c(names(dt_tmp)[!c(names(dt_tmp))%in% c("sw","year")])]
    dt_tmp=dt_tmp[delete==FALSE,]
    dt_tmp[,delete:=NULL]
    
    ## converge to 2100 attributed value
    dt_tmp[year >= 2020,
           sw := sw[year==2020] + (sw[year==2100]-sw[year==2020]) * (year-2020)/(2100-2020),
           by=c(names(dt_tmp)[!c(names(dt_tmp))%in% c("year","sw")])]
    
    ## CHECK if needed
    dt_tmp[sw > 1, sw := 1]
    
    ## constant from 2100 on
    dt_tmp[year >= 2100,
           sw := sw[year==2100],
           by=c(names(dt_tmp)[!c(names(dt_tmp))%in% c("year","sw")])]
    
    return(dt_tmp)
  }
  
    apply_logistic_trends <- function(initial, yrs, ysymm, speed){
        logistic_trend <- function(year){
            a <- speed
            b <- ysymm

            exp(a * (year - b))/(exp(a * (year - b)) + 1)
        }

        scl <- sapply(yrs, logistic_trend)

        initial + scl * (1 - initial)
    }
    
    extr_const <- function(dt){
        tmp_dt <- dt[year==2010]
        for (yr in years[years > 2010]) {
            tmp_dt[, year := yr]
            dt <- rbind(dt, tmp_dt)
        }
        return(dt)
    }

    SWS <- calibration_output

    ## constant trends for all techs
    SWS <- lapply(SWS, extr_const)
    ## apply function that finds the average value in 2100 for levels S1S2, S2S3, S3S 
    SWS_upperlevs=list(S1S2_final_SW=SWS$S1S2_final_SW,S2S3_final_SW=SWS$S2S3_final_SW,S3S_final_SW=SWS$S3S_final_SW)
    SWS_upperlevs <- lapply(X=SWS_upperlevs, FUN=aveval)

    ## substitute the original dts with those with converging values
    SWS$S1S2_final_SW=SWS_upperlevs$S1S2_final_SW
    SWS$S2S3_final_SW=SWS_upperlevs$S2S3_final_SW
    SWS$S3S_final_SW=SWS_upperlevs$S3S_final_SW
    
    if (EDGE_scenario == "progressive") {## 40 total time to take from 0 to 1
      ## technologies
      ## apply S-type trends for renewables
      SWS$FV_final_SW[technology == "BEV" & year >= 2020, sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2030, speed = 0.5),
                      by=c("iso","vehicle_type","technology")]
      SWS$FV_final_SW[technology == "FCEV" & year >= 2020 & subsector_L1 == "trn_pass_road_LDV_4W", sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2045, speed = 0.2),
                      by=c("iso","vehicle_type","technology")]
      SWS$FV_final_SW[technology == "FCEV" & year >= 2020 & subsector_L1 %in% c("trn_freight_road_tmp_subsector_L1", "Bus_tmp_subsector_L1"), sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2055, speed = 0.2),
                      by=c("iso","vehicle_type","technology")]
      ## electric trains develop increasing linearly to 2100
      SWS$FV_final_SW[technology == "Electric" & year >= 2020,
                     sw := sw[year==2020] + (1-sw[year==2020]) * (year-2020) / (2100-2020),
                     by=c("iso","vehicle_type","technology")]

      ## Liquid fuel prefs drop
      SWS$FV_final_SW[technology == "Liquids" & year >= 2040,
                      sw := sw[year==2040] - (sw[year==2040]-0.1) * (year-2040)/(2060-2040),
                      by=c("iso","vehicle_type","technology")]
      SWS$FV_final_SW[technology == "Liquids" & year >= 2060,
                      sw := 0.1,
                      by=c("iso","vehicle_type","technology")]
    
      
      ## Preference for road public transport is doubled in the whole world (not for China, which already has high preference)
      SWS$S2S3_final_SW[subsector_L2 %in% c("Bus","trn_pass_road_bus") & year >= 2020 & !iso %in% c("CHN", "HKG", "MAC"),
                        sw := sw[year==2020] + (2*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020),
                        by = c("iso", "subsector_L2")]
      
      ## Preference for Rail and HSR is doubled by the end of the century for all countries
      SWS$S3S_final_SW[subsector_L3 %in% c("HSR", "Freight Rail", "Passenger Rail") & year >= 2020,
                       sw := sw[year==2020] + (2*sw[year==2020]-sw[year==2020]) * (year-2020) / (2100-2020), by = c("iso","subsector_L3")]
      SWS$S3S_final_SW[subsector_L3 %in% c("HSR", "Freight Rail", "Passenger Rail") & year >= 2100,
                      sw := sw[year==2100], by = c("iso", "subsector_L3")]
    }
    
    if (EDGE_scenario %in% c("Electricity_push", "Hydrogen_push", "Conservative_liquids")) {## 80 total time to take from 0 to 1

      ## apply S-type trends for renewables
      SWS$FV_final_SW[technology == "BEV" & year >= 2020 & subsector_L1 == "trn_pass_road_LDV_4W",
                      sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2040, speed = 0.1),
                      by=c("iso","vehicle_type","technology")]
      SWS$FV_final_SW[technology == "FCEV" & year >= 2020 & subsector_L1 == "trn_pass_road_LDV_4W",
                      sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2050, speed = 0.1),
                      by=c("iso","vehicle_type","technology")]
      SWS$FV_final_SW[technology == "FCEV" & year >= 2020 & subsector_L1 %in% c("trn_freight_road_tmp_subsector_L1", "Bus_tmp_subsector_L1"),
                      sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2060, speed = 0.1),
                      by=c("iso","vehicle_type","technology")]
      
      ## electric trains develop increasing linearly to 2100
      SWS$FV_final_SW[technology == "Electric" & year >= 2020,
                      sw := apply_logistic_trends(sw[year == 2020], year, ysymm = 2050, speed = 0.1),
                      by=c("iso","vehicle_type","technology")]
      
      ## mode types
      ## Public transport evolves in time for China: trn_pass_road_bus trend is converging towards Bus trend for CHN, HGK, MAC
      SWS$S2S3_final_SW[subsector_L2 %in% c("Bus","trn_pass_road_bus") & year >= 2020 & iso %in% c("CHN", "HKG", "MAC"),
                        sw := sw[year==2020] + (sw[year==2100 & subsector_L2 == "Bus"]-sw[year==2020]) * (year-2020) / (2100-2020),
                        by=c("iso")]

    }
    

    ## nat. gas increase linearly to 2100
    SWS$FV_final_SW[technology == "NG" & year >= 2020,
          sw := sw[year==2020] + (1-sw[year==2020]) * (year-2020) / (2200-2020),
          by=c("iso","vehicle_type","technology")]
    
    ## linear convergence is fixed if goes beyond 0 or above 1
    SWS$FV_final_SW[sw > 1, sw := 1]
    SWS$S2S3_final_SW[, sw := ifelse(year >2100 & sw<0, sw[year == 2100], sw), by = c("iso", "subsector_L2")]
    
    return(SWS)
}
