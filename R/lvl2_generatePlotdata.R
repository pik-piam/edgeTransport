#' Generate EDGE-Transport Plot Data for Standalone Report
#'
#' Run this script to prepare the input data for EDGE in EDGE-friendly units and regional aggregation
#' @param listofruns list of folders hosting raw data
#' @param output_folder folder for storage of the resulting report
#' @param cache_folder folder hosting a "local" cache (this is not the mrremid cache, it is specific to EDGE-T). NOTE: the cache folder will be created in the output_folder if it does not exist.
#' @param AggrReg
#' @return generated EDGE-transport plot data
#' @author Johanna Hoppe
#' @import data.table
#' @importFrom magclass
#' @importFrom lusweave
#' @importFrom data.table
#' @importFrom utils
#' @importFrom rmndt
#' @importFrom remind2
#' @importFrom madrat
#' @export

lvl2_generate_plotdata <- function(listofruns, load_Cache=FALSE, mrremind_folder=NULL, AggrReg="H12"){
  
  
  scenNames <- c()
  demand_km <- demand_ej <- vintcomp <- newcomp <- shares <-pref <- mj_km_data <- loadFactor <- annual_mileage <- annual_sale <-prices<-logit_exp <- list()
  count_scen <- 2

  ## ---- Load GDP and POP ----
  if(load_Cache & file.exists(mrremind_folder)){
    GDP_country = readRDS(file.path(mrremind_folder, "GDP_country.RDS"))
    POP = readRDS(file.path(mrremind_folder,"POP_country.RDS"))
  }else {GDP_country <- {
    x <- calcOutput("GDP", aggregate = F)
    getSets(x)[1] <- "ISO3"
    getSets(x)[2] <- "Year"
    x
  }}
  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  GDP_country[, variable := paste0(sub("gdp_", "",variable))]
  setnames(GDP_country, c("ISO3","variable", "value","year"),c("CountryCode","scenario", "weight","period"))
  POP <- as.data.table(POP)
  POP[, year := as.numeric(gsub("y", "", year))]
  POP[, variable := paste0(sub("pop_", "",variable))]
  setnames(POP, c("iso2c","variable","year"),c("CountryCode","scenario","period"))
  
  ## ---- Create mappings ----
  #Region Mapping
  if (AggrReg== "H12"){
     RegionMappingH12 <- fread(system.file("extdata", "regionmappingH12.csv", package = "edgeTransport"))
     Regionmapping_21_EU11 <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
     setnames(Regionmapping_21_EU11,"RegionCode","region")
     Regionmapping <-copy(Regionmapping_21_EU11)
     Regionmapping[, missingH12 := ifelse(missingH12 == "rest", region, missingH12)]
     Regionmapping <- Regionmapping[,-c("X","CountryCode")]
     Regionmapping <- Regionmapping[!duplicated(Regionmapping)]
     setnames(Regionmapping,c("region","missingH12"),c("manycol","fewcol"))
     Regionmapping_Tot <- copy(Regionmapping[,-c("manycol")])
     setnames(Regionmapping_Tot,"fewcol","manycol")
     Regionmapping_Tot[,fewcol:="GLO"]
     Regionmapping_Tot <- Regionmapping_Tot[!duplicated(Regionmapping_Tot)]
     GDP_21 <- aggregate_dt(GDP_country,Regionmapping_21_EU11[,-c("X","missingH12")],yearcol = "period",fewcol = "region", manycol = "CountryCode",datacols = "scenario",valuecol = "weight")
     setnames(GDP_21,"region","manycol")
     GDP_13 <- aggregate_dt(GDP_21,Regionmapping,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("scenario"),valuecol = "weight")
     setnames(GDP_13,"fewcol","manycol")
     GDP_13_glo <- aggregate_dt(GDP_13,Regionmapping_Tot,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("scenario"),valuecol = "weight")
     setnames(GDP_13_glo, "fewcol","region")
     setnames(GDP_13, "manycol","region")
     GDP_13 <- rbind(GDP_13, GDP_13_glo)
     POP_21 <- aggregate_dt(POP,Regionmapping_21_EU11[,-c("X","missingH12")],yearcol = "period",fewcol = "region", manycol = "CountryCode",datacols = "scenario",valuecol = "value")
     setnames(POP_21,"region","manycol")
     POP_13 <- aggregate_dt(POP_21,Regionmapping,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("scenario"),valuecol = "value")
     setnames(POP_13,"fewcol","manycol")
     POP_13_glo <- aggregate_dt(POP_13,Regionmapping_Tot,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("scenario"),valuecol = "value")
     setnames(POP_13_glo, "fewcol","region")
     setnames(POP_13,"manycol","region")
     POP_13 <- rbind(POP_13, POP_13_glo)
     }
  
if (AggrReg== "EU21"){
  Regionmapping <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
  
}
  
  

  
  
  #Mapping for vehicle type aggregation
  Mapp_Aggr_vehtype = data.table(
    gran_vehtype = c("Compact Car","Large Car","Large Car and SUV","Light Truck and SUV", "Midsize Car","Mini Car","Subcompact Car","Van", "International Aviation_tmp_vehicletype",
                     "Domestic Ship_tmp_vehicletype","Freight Rail_tmp_vehicletype","Truck (0-3.5t)" ,"Truck (18t)","Truck (26t)","Truck (40t)","Truck (7.5t)","Domestic Aviation_tmp_vehicletype",
                     "HSR_tmp_vehicletype","Passenger Rail_tmp_vehicletype","Bus_tmp_vehicletype", "Moped","Motorcycle (50-250cc)","Motorcycle (>250cc)","International Ship_tmp_vehicletype", "Cycle_tmp_vehicletype","Walk_tmp_vehicletype") ,
    aggr_vehtype= c("Small Cars","Large Cars","Large Cars","Large Cars", "Large Cars","Small Cars","Small Cars", "Large Cars", "Aircraft international",
                    "Ships domestic","Freight Trains","Trucks" ,"Trucks","Trucks","Trucks","Trucks","Aircraft domestic",
                    "Passenger Trains","Passenger Trains","Busses", "Motorbikes","Motorbikes","Motorbikes","Ships international","Cycling","Walking") ,
    international=c("no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","bunkers","no bunkers","no bunkers","no bunkers","no bunkers",
                    "no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","bunkers","no bunkers","no bunkers")
  )
  
  #Mapping efficiencies for useful energy
  
  Mapp_UE = data.table(
    technology = c("FCEV","BEV","Electric","Liquids","Hydrogen"),
    UE_efficiency =c(0.5,0.8,0.8, 0.3, 0.35))
  
  
  ## ---- Load scenario data ----
  
  level2path <- function(folder, fname){
    path <- file.path(folder, "level_2", fname)
  }
  
  level1path <- function(folder, fname){
    path <- file.path(folder, "level_1", fname)
  }
  
  level0path <- function(folder, fname){
    path <- file.path(folder, "level_0", fname)
  }  
  
  for (i in 1:length(listofruns)) {
    if(any(grepl(listofruns[[i]],scenNames))) {
      scenNames[i] <- sub("_20.*", "", listofruns[[i]])
      scenNames[i] <- paste0(sub(".*/", "", scenNames[i]),"_",count_scen)
      count_scen=count_scen+1
    }else {
      scenNames[i] <- sub("_20.*", "", listofruns[[i]])
      scenNames[i] <- sub(".*/", "", scenNames[i])}
    
    ## load input data from EDGE runs for comparison
    demand_km[[i]] <- readRDS(level2path(listofruns[[i]],"demandF_plot_pkm.RDS")) ## detailed energy services demand, million pkm
    demand_km[[i]]$scenario=scenNames[i]
    demand_ej[[i]] <- readRDS(level2path(listofruns[[i]],"demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
    demand_ej[[i]]$scenario=scenNames[i]
    vintcomp[[i]] <- readRDS(level2path(listofruns[[i]], "vintcomp.RDS"))
    vintcomp[[i]]$scenario=scenNames[i]
    newcomp[[i]] <- readRDS(level2path(listofruns[[i]],"newcomp.RDS"))
    newcomp[[i]]$scenario=scenNames[i]
    shares[[i]] <- readRDS(level2path(listofruns[[i]],"shares.RDS"))
    shares[[i]]$scenario=scenNames[i]
    logit_data <- readRDS(level2path(listofruns[[i]],"logit_data.RDS"))
    prices[[i]] <- logit_data$prices_list
    prices[[i]]$scenario=scenNames[i]
    pref[[i]] <- logit_data$pref_data
    pref[[i]]$scenario=scenNames[i]
    logit_exp_data <- readRDS(level0path(listofruns[[i]],"logit_exp.RDS"))
    logit_exp[[i]] <- logit_exp_data$logit_output
    logit_exp[[i]]$scenario=scenNames[i]
    mj_km_data[[i]] <- readRDS(level2path(listofruns[[i]],"mj_km_data.RDS"))
    mj_km_data[[i]]$scenario=scenNames[i]
    loadFactor[[i]] <- readRDS(level2path(listofruns[[i]],"loadFactor.RDS"))
    loadFactor[[i]]$scenario=scenNames[i]
    annual_mileage[[i]] <- readRDS(level2path(listofruns[[i]],"annual_mileage.RDS"))
    annual_mileage[[i]]$scenario=scenNames[i]
    annual_sale[[i]] <- readRDS(level2path(listofruns[[i]],"annual_sales.RDS"))
    annual_sale[[i]]$scenario=scenNames[i]
  }
  
  #Needs to be changed
  SSP_Scen <- c("SSP2","SSP2","SSP2","SSP2","SSP2","SSP2","SSP2")
  SSP_Scen <- SSP_Scen[1:length(listofruns)]
  
  
  #Extend POP and GDP for all Scenarios
  
  GDP_13_scen <- list()
  POP_13_scen <- list()
  GDP_21_scen <- list()
  POP_21_scen <- list()
  
  
  for (i in 1:length(SSP_Scen)){
    GDP_13_tmp <- GDP_13[scenario == SSP_Scen[i]]
    GDP_13_tmp[,scenario:=scenNames[i]]
    GDP_13_scen <-rbind(GDP_13_scen,GDP_13_tmp)
    POP_13_tmp <- POP_13[scenario == SSP_Scen[i]]
    POP_13_tmp[,scenario:=scenNames[i]]
    POP_13_scen <-rbind(POP_13_scen,POP_13_tmp)
    GDP_21_tmp <- GDP_21[scenario == SSP_Scen[i]]
    GDP_21_tmp[,scenario:=scenNames[i]]
    GDP_21_scen <-rbind(GDP_21_scen,GDP_21_tmp)
    POP_21_tmp <- POP_21[scenario == SSP_Scen[i]]
    POP_21_tmp[,scenario:=scenNames[i]]
    POP_21_scen <-rbind(POP_21_scen,POP_21_tmp)} 

  #Prepare Line Plot data
  
  #FE
  dem_ej <- do.call(rbind.data.frame, demand_ej)
  plot_dem_ej <- copy(dem_ej)
  
  #rename columns for mip
  setnames(plot_dem_ej,c("demand_EJ","year","region"),c("value","period","manycol"))
  plot_dem_ej <- plot_dem_ej[,c("value","period","manycol","scenario","sector","technology", "vehicle_type")]
  plot_dem_ej[,unit:= "EJ/yr"]
  
  #Aggregate regions
  plot_dem_ej <- aggregate_dt(plot_dem_ej,Regionmapping,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_dem_ej,"fewcol","manycol")
  plot_dem_ej_glo <- aggregate_dt(plot_dem_ej,Regionmapping_Tot,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_dem_ej,"manycol", "region")
  setnames(plot_dem_ej_glo, "fewcol", "region")
  plot_dem_ej<- rbind(plot_dem_ej,plot_dem_ej_glo)
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]
  
  #Group vehicle types for plotting
  plot_dem_ej <- merge(plot_dem_ej,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
  plot_dem_ej <- plot_dem_ej[,-c("vehicle_type")]
  setnames(plot_dem_ej,"aggr_vehtype","vehicle_type")
  
  #Aggregate data
  plot_dem_ej <- plot_dem_ej[, .(value=sum(value)), by= c("period","region","scenario","sector","technology", "vehicle_type", "unit","international")]

  #FE|Transport
  FE_Transport <- copy(plot_dem_ej)
  FE_Transport <- FE_Transport[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport"]
  #FE|Transport w/o bunkers
  FE_Transport_wobunk <- copy(plot_dem_ej)
  FE_Transport_wobunk <- FE_Transport_wobunk[international=="no bunkers"]
  FE_Transport_wobunk <- FE_Transport_wobunk[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport w/o bunkers"]
  #FE|Transport|Pass
  FE_Transport_Pass <- copy(plot_dem_ej)
  FE_Transport_Pass <- FE_Transport_Pass[sector %in% c("trn_pass","trn_aviation_intl")]
  FE_Transport_Pass <- FE_Transport_Pass[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Pass"]
  #FE|Transport|Pass|Rail
  FE_Transport_Pass_Rail <- copy(plot_dem_ej)
  FE_Transport_Pass_Rail <- FE_Transport_Pass_Rail[vehicle_type=="Passenger Trains"]
  FE_Transport_Pass_Rail <- FE_Transport_Pass_Rail[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Pass|Rail"]
  #FE|Transport|Pass|Road
  FE_Transport_Pass_Road <- copy(plot_dem_ej)
  FE_Transport_Pass_Road <- FE_Transport_Pass_Road[vehicle_type %in% c("Busses","Small Cars","Large Cars","Motorbikes")]
  FE_Transport_Pass_Road <- FE_Transport_Pass_Road[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Pass|Road"]  
  #FE|Transport|Pass|Road|Bus
  FE_Transport_Pass_Road_Bus <- copy(plot_dem_ej)
  FE_Transport_Pass_Road_Bus <- FE_Transport_Pass_Road_Bus[vehicle_type %in% c("Busses")]
  FE_Transport_Pass_Road_Bus <- FE_Transport_Pass_Road_Bus[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Pass|Road|Bus"]  
  #FE|Transport|Pass|Road|LDV  
  FE_Transport_Pass_Road_LDV<- copy(plot_dem_ej)
  FE_Transport_Pass_Road_LDV <- FE_Transport_Pass_Road_LDV[vehicle_type %in% c("Motorbikes","Small Cars","Large Cars")]
  FE_Transport_Pass_Road_LDV <- FE_Transport_Pass_Road_LDV[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Pass|Road|LDV"]  
  #FE|Transport|Freight 
  FE_Transport_Freight<- copy(plot_dem_ej)
  FE_Transport_Freight <- FE_Transport_Freight[sector %in% c("trn_freight","trn_shipping_intl")]
  FE_Transport_Freight <- FE_Transport_Freight[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Freight"]    
  #FE|Transport|Freight|Navigation
  FE_Transport_Freight_Nav<- copy(plot_dem_ej)
  FE_Transport_Freight_Nav <- FE_Transport_Freight_Nav[vehicle_type %in% c("Ships domestic","Ships international")]
  FE_Transport_Freight_Nav <- FE_Transport_Freight_Nav[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Freight|Navigation"]   
  #FE|Transport|Freight|Rail  
  FE_Transport_Freight_Rail<- copy(plot_dem_ej)
  FE_Transport_Freight_Rail <- FE_Transport_Freight_Rail[vehicle_type %in% c("Freight Trains")]
  FE_Transport_Freight_Rail <- FE_Transport_Freight_Rail[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Freight|Rail"]   
  #FE|Transport|Freight|Road
  FE_Transport_Freight_Road<- copy(plot_dem_ej)
  FE_Transport_Freight_Road <- FE_Transport_Freight_Road[vehicle_type %in% c("Trucks")]
  FE_Transport_Freight_Road <- FE_Transport_Freight_Road[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="FE|Transport|Freight|Road"]   
  
  
  #ES
  dem_pkm <- do.call(rbind.data.frame, demand_km)
  plot_dem_pkm <- copy(dem_pkm)
  setkey(plot_dem_pkm,NULL)
  
  #rename columns for mip
  setnames(plot_dem_pkm,c("demand_F","year","region"),c("value","period","manycol"))
  plot_dem_pkm <- plot_dem_pkm[,c("value","period","manycol","scenario","sector","technology", "vehicle_type")]
  plot_dem_pkm[,unit:= "million pkm/yr"]
  
  #Aggregate regions
  plot_dem_pkm <- aggregate_dt(plot_dem_pkm,Regionmapping,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_dem_pkm,"fewcol","manycol")
  plot_dem_pkm_glo <- aggregate_dt(plot_dem_pkm,Regionmapping_Tot,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_dem_pkm,"manycol", "region")
  setnames(plot_dem_pkm_glo, "fewcol", "region")
  plot_dem_pkm<- rbind(plot_dem_pkm,plot_dem_pkm_glo)
  plot_dem_pkm <- plot_dem_pkm[!duplicated(plot_dem_pkm)]
  
  #Group vehicle types for plotting
  plot_dem_pkm <- merge(plot_dem_pkm,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
  plot_dem_pkm <- plot_dem_pkm[,-c("vehicle_type")]
  setnames(plot_dem_pkm,"aggr_vehtype","vehicle_type")
  
  #Aggregate data
  plot_dem_pkm <- plot_dem_pkm[, .(value=sum(value)), by= c("period","region","scenario","sector","technology", "vehicle_type", "unit","international")]
  
  #ES|Transport
  ES_Transport <- copy(plot_dem_pkm)
  ES_Transport <- ES_Transport[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport"]
  #ES|Transport w/o bunkers
  ES_Transport_wobunk <- copy(plot_dem_pkm)
  ES_Transport_wobunk <- ES_Transport_wobunk[international=="no bunkers"]
  ES_Transport_wobunk <- ES_Transport_wobunk[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport w/o bunkers"]
  #ES|Transport|Pass
  ES_Transport_Pass <- copy(plot_dem_pkm)
  ES_Transport_Pass <- ES_Transport_Pass[sector %in% c("trn_pass","trn_aviation_intl")]
  ES_Transport_Pass <- ES_Transport_Pass[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Pass"]
  #ES|Transport|Pass|Rail
  ES_Transport_Pass_Rail <- copy(plot_dem_pkm)
  ES_Transport_Pass_Rail <- ES_Transport_Pass_Rail[vehicle_type=="Passenger Trains"]
  ES_Transport_Pass_Rail <- ES_Transport_Pass_Rail[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Pass|Rail"]
  #ES|Transport|Pass|Road
  ES_Transport_Pass_Road <- copy(plot_dem_pkm)
  ES_Transport_Pass_Road <- ES_Transport_Pass_Road[vehicle_type %in% c("Busses","Small Cars","Large Cars","Motorbikes")]
  ES_Transport_Pass_Road <- ES_Transport_Pass_Road[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Pass|Road"]  
  #ES|Transport|Pass|Road|Bus
  ES_Transport_Pass_Road_Bus <- copy(plot_dem_pkm)
  ES_Transport_Pass_Road_Bus <- ES_Transport_Pass_Road_Bus[vehicle_type %in% c("Busses")]
  ES_Transport_Pass_Road_Bus <- ES_Transport_Pass_Road_Bus[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Pass|Road|Bus"]  
  #ES|Transport|Pass|Road|LDV  
  ES_Transport_Pass_Road_LDV<- copy(plot_dem_pkm)
  ES_Transport_Pass_Road_LDV <- ES_Transport_Pass_Road_LDV[vehicle_type %in% c("Motorbikes","Small Cars","Large Cars")]
  ES_Transport_Pass_Road_LDV <- ES_Transport_Pass_Road_LDV[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Pass|Road|LDV"]  
  #ES|Transport|Freight 
  ES_Transport_Freight<- copy(plot_dem_pkm)
  ES_Transport_Freight <- ES_Transport_Freight[sector %in% c("trn_freight","trn_shipping_intl")]
  ES_Transport_Freight <- ES_Transport_Freight[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Freight"]    
  #ES|Transport|Freight|Navigation
  ES_Transport_Freight_Nav<- copy(plot_dem_pkm)
  ES_Transport_Freight_Nav <- ES_Transport_Freight_Nav[vehicle_type %in% c("Ships domestic","Ships international")]
  ES_Transport_Freight_Nav <- ES_Transport_Freight_Nav[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Freight|Navigation"]   
  #ES|Transport|Freight|Rail  
  ES_Transport_Freight_Rail<- copy(plot_dem_pkm)
  ES_Transport_Freight_Rail <- ES_Transport_Freight_Rail[vehicle_type %in% c("Freight Trains")]
  ES_Transport_Freight_Rail <- ES_Transport_Freight_Rail[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Freight|Rail"]   
  #ES|Transport|Freight|Road
  ES_Transport_Freight_Road<- copy(plot_dem_pkm)
  ES_Transport_Freight_Road <- ES_Transport_Freight_Road[vehicle_type %in% c("Trucks")]
  ES_Transport_Freight_Road <- ES_Transport_Freight_Road[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="ES|Transport|Freight|Road"]   
  
  
  #EInt
  EInt_mj_km<- do.call(rbind.data.frame, mj_km_data)
  plot_EInt <- copy(EInt_mj_km)
  setkey(plot_EInt,NULL)
  #rename columns for mip
  setnames(plot_EInt ,c("MJ_km","year","region"),c("value","period","manycol"))
  plot_EInt <- plot_EInt [,c("value","period","manycol","scenario","sector","technology", "vehicle_type")]
  plot_EInt[,unit:= "MJ/km"]
  
  #Aggregate regions
  plot_EInt <- aggregate_dt(plot_EInt,Regionmapping,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_EInt,"fewcol","manycol")
  plot_EInt_glo <- aggregate_dt(plot_EInt,Regionmapping_Tot,fewcol ="fewcol",yearcol = "period", manycol = "manycol" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_EInt,"manycol", "region")
  setnames(plot_EInt_glo, "fewcol", "region")
  plot_EInt<- rbind(plot_EInt,plot_EInt_glo)
  plot_EInt <- plot_EInt[!duplicated(plot_EInt)]
  
  #Group vehicle types for plotting
  plot_EInt <- merge(plot_EInt,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
  plot_EInt <- plot_EInt[,-c("vehicle_type")]
  setnames(plot_EInt ,"aggr_vehtype","vehicle_type")
  
  #Aggregate data
  plot_EInt <-plot_EInt[, .(value=sum(value)), by= c("period","region","scenario","sector","technology", "vehicle_type", "unit","international")]
  
  #EInt|Transport
  EInt_Transport <- copy(plot_EInt)
  EInt_Transport <- EInt_Transport[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport"]
  #EInt|Transport w/o bunkers
  EInt_Transport_wobunk <- copy(plot_EInt)
  EInt_Transport_wobunk <- EInt_Transport_wobunk[international=="no bunkers"]
  EInt_Transport_wobunk <- EInt_Transport_wobunk[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport w/o bunkers"]
  #EInt|Transport|Pass
  EInt_Transport_Pass <- copy(plot_EInt)
  EInt_Transport_Pass <- EInt_Transport_Pass[sector %in% c("trn_pass","trn_aviation_intl")]
  EInt_Transport_Pass <- EInt_Transport_Pass[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Pass"]
  #EInt|Transport|Pass|Rail
  EInt_Transport_Pass_Rail <- copy(plot_EInt)
  EInt_Transport_Pass_Rail <- EInt_Transport_Pass_Rail[vehicle_type=="Passenger Trains"]
  EInt_Transport_Pass_Rail <- EInt_Transport_Pass_Rail[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Pass|Rail"]
  #EInt|Transport|Pass|Road
  EInt_Transport_Pass_Road <- copy(plot_EInt)
  EInt_Transport_Pass_Road <- EInt_Transport_Pass_Road[vehicle_type %in% c("Busses","Small Cars","Large Cars","Motorbikes")]
  EInt_Transport_Pass_Road <- EInt_Transport_Pass_Road[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Pass|Road"]  
  #EInt|Transport|Pass|Road|Bus
  EInt_Transport_Pass_Road_Bus <- copy(plot_EInt)
  EInt_Transport_Pass_Road_Bus <- EInt_Transport_Pass_Road_Bus[vehicle_type %in% c("Busses")]
  EInt_Transport_Pass_Road_Bus <- EInt_Transport_Pass_Road_Bus[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Pass|Road|Bus"]  
  #EInt|Transport|Pass|Road|LDV  
  EInt_Transport_Pass_Road_LDV<- copy(plot_EInt)
  EInt_Transport_Pass_Road_LDV <- EInt_Transport_Pass_Road_LDV[vehicle_type %in% c("Motorbikes","Small Cars","Large Cars")]
  EInt_Transport_Pass_Road_LDV <- EInt_Transport_Pass_Road_LDV[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Pass|Road|LDV"]  
  #EInt|Transport|Freight 
  EInt_Transport_Freight<- copy(plot_EInt)
  EInt_Transport_Freight <- EInt_Transport_Freight[sector %in% c("trn_freight","trn_shipping_intl")]
  EInt_Transport_Freight <- EInt_Transport_Freight[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Freight"]    
  #EInt|Transport|Freight|Navigation
  EInt_Transport_Freight_Nav<- copy(plot_EInt)
  EInt_Transport_Freight_Nav <- EInt_Transport_Freight_Nav[vehicle_type %in% c("Ships domestic","Ships international")]
  EInt_Transport_Freight_Nav <- EInt_Transport_Freight_Nav[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Freight|Navigation"]   
  #EInt|Transport|Freight|Rail  
  EInt_Transport_Freight_Rail<- copy(plot_EInt)
  EInt_Transport_Freight_Rail <- EInt_Transport_Freight_Rail[vehicle_type %in% c("Freight Trains")]
  EInt_Transport_Freight_Rail <- EInt_Transport_Freight_Rail[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Freight|Rail"]   
  #EInt|Transport|Freight|Road
  EInt_Transport_Freight_Road<- copy(plot_EInt)
  EInt_Transport_Freight_Road <- EInt_Transport_Freight_Road[vehicle_type %in% c("Trucks")]
  EInt_Transport_Freight_Road <- EInt_Transport_Freight_Road[, .(value=sum(value)), by= c("period","region","scenario","unit")][,variable:="EInt|Transport|Freight|Road"]   
  
  LinePlot_data = rbind(FE_Transport,FE_Transport_wobunk, FE_Transport_Pass, FE_Transport_Pass_Rail,  FE_Transport_Pass_Road, FE_Transport_Pass_Road_Bus, FE_Transport_Pass_Road_LDV, FE_Transport_Freight, FE_Transport_Freight_Nav, FE_Transport_Freight_Rail, FE_Transport_Freight_Road,    
                        ES_Transport,ES_Transport_wobunk, ES_Transport_Pass, ES_Transport_Pass_Rail,  ES_Transport_Pass_Road, ES_Transport_Pass_Road_Bus, ES_Transport_Pass_Road_LDV, ES_Transport_Freight, ES_Transport_Freight_Nav, ES_Transport_Freight_Rail, ES_Transport_Freight_Road,
                        EInt_Transport,EInt_Transport_wobunk, EInt_Transport_Pass, EInt_Transport_Pass_Rail,  EInt_Transport_Pass_Road, EInt_Transport_Pass_Road_Bus, EInt_Transport_Pass_Road_LDV, EInt_Transport_Freight, EInt_Transport_Freight_Nav,FE_Transport_Freight_Rail, EInt_Transport_Freight_Road
                        )
  

  Prices_S2S3 <- Pref_S2S3 <- logit_exp_S2S3 <- list()
  
  #Prepare logit price data S2S3
  for (i in 1:length(listofruns)) {
    Prices_S2S3[[i]] <- copy(prices[[i]]$S1S2)
    Prices_S2S3[[i]]$scenario <- copy(prices[[i]]$scenario)
  }
  
  Prices_S2S3 <- do.call(rbind.data.frame, Prices_S2S3)
  
  #Prepare logit pref data S2S3
  for (i in 1:length(listofruns)) {
    Pref_S2S3[[i]] <- copy(pref[[i]]$S2S3_final_pref)
    Pref_S2S3[[i]]$scenario <- copy(pref[[i]]$scenario)
  }
  
  Pref_S2S3 <- do.call(rbind.data.frame, Pref_S2S3)
  
  #Prepare logit exponents S2S3
  for (i in 1:length(listofruns)) {
    logit_exp_S2S3[[i]] <- copy(logit_exp[[i]]$logit_exponent_S1S2)
    logit_exp_S2S3[[i]]$scenario <- copy(logit_exp[[i]]$scenario)
  }
  
  logit_exp_S2S3 <- do.call(rbind.data.frame, logit_exp_S2S3)
  
  
  #Use ES as share weights
    weight_dem_pkm <- copy(dem_pkm)
    #rename columns for mip
    setnames(weight_dem_pkm,c("demand_F","year"),c("value","period"))
    weight_dem_pkm <- weight_dem_pkm[,c("value","period","region","scenario","sector","subsector_L1","subsector_L2","subsector_L3","technology", "vehicle_type")]
    weight_dem_pkm[,unit:= "million pkm/yr"]
  
    #Group vehicle types for plotting
    weight_dem_pkm <- merge(weight_dem_pkm,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
    weight_dem_pkm <-weight_dem_pkm[,-c("vehicle_type")]
    setnames(weight_dem_pkm,"aggr_vehtype","vehicle_type")
  
    #Aggregate data
    weight_dem_pkm_S2S3 <- weight_dem_pkm[, .(value=sum(value)), by= c("period","region","scenario","sector","subsector_L2","subsector_L3", "unit")]
    setnames(weight_dem_pkm_S2S3,c("value","region"),c("weight","manycol"))
  
  setindex(Prices_S2S3,NULL)
  setkey(Prices_S2S3,NULL)
  #change variable names for mip
  setnames(Prices_S2S3, c("year"),c("period"))
 
  
  setindex(Pref_S2S3,NULL)
  setkey(Pref_S2S3,NULL)
  #change variable names for mip
  setnames(Pref_S2S3, c("year"),c("period"))
  Pref_S2S3 <- Pref_S2S3[period %in% unique(Prices_S2S3$period)]
  
  #Calculate Inconvenience Cost from share Weight
  Pref_S2S3 <- merge(Pref_S2S3,logit_exp_S2S3, all.x=TRUE)
  Pref_S2S3 <- Pref_S2S3[ is.na(logit.exponent), logit.exponent := -10]
  Pref_S2S3 <- merge(Pref_S2S3,Prices_S2S3[,c("region","period","subsector_L2","subsector_L3","sector","tot_price","scenario")],by=c("period","region","sector","subsector_L2","subsector_L3","scenario"))
  
  Pref_S2S3[, value := tot_price*(sw^(1/logit.exponent)-1)]
  Pref_S2S3 <- Pref_S2S3[, c("region","period","subsector_L2","subsector_L3","sector","scenario","value")][,variable:="Inconvenience cost"]
  setnames(Pref_S2S3,"region","manycol")
  
  
  Prices_S2S3 <- melt(Prices_S2S3,id.vars=c("scenario", "region","period","sector", "subsector_L2", "subsector_L3"))
  setnames(Prices_S2S3,"region","manycol")
  Prices_S2S3 <- aggregate_dt(Prices_S2S3,
                                Regionmapping,
                                manycol = "manycol",
                                fewcol = "fewcol",
                                yearcol = "period",
                                datacols = c("sector", "subsector_L2", "subsector_L3", "variable","scenario"),
                                weights = weight_dem_pkm_S2S3[period%in%unique(Prices_S2S3$period)])
  setnames(Prices_S2S3,c("fewcol"),c("region"))
  
  Pref_S2S3 <- aggregate_dt(Pref_S2S3,
                              Regionmapping,
                              manycol = "manycol",
                              fewcol = "fewcol",
                              yearcol = "period",
                              datacols = c("sector", "subsector_L2", "subsector_L3", "variable","scenario"),
                              weights = weight_dem_pkm_S2S3[period%in%unique(Pref_S2S3$period)])
  setnames(Pref_S2S3,c("fewcol"),c("region"))
  

  CONV_2005USD_1990USD <- 0.67
  Prices_S2S3_Plot_data_Bus <- rbind(Prices_S2S3[subsector_L2=="Bus", c("variable","period","scenario","region","value")],Pref_S2S3[subsector_L2=="Bus", c("variable","period","scenario","region","value")])
  Prices_S2S3_Plot_data_Bus <- Prices_S2S3_Plot_data_Bus[,value:= value * CONV_2005USD_1990USD]
  Prices_S2S3_Plot_data_Bus <- Prices_S2S3_Plot_data_Bus[,unit:="$2005/pkm"]
  
  Prices_S2S3_Plot_data_LDV <- rbind(Prices_S2S3[subsector_L2=="trn_pass_road_LDV", c("variable","period","scenario","region","value")],Pref_S2S3[subsector_L2=="trn_pass_road_LDV", c("variable","period","scenario","region","value")])
  Prices_S2S3_Plot_data_LDV <- Prices_S2S3_Plot_data_LDV[,value:= value * CONV_2005USD_1990USD]
  Prices_S2S3_Plot_data_LDV <- Prices_S2S3_Plot_data_LDV[,unit:="$2005/pkm"]
  
  return(list(LinePlot_data=LinePlot_data,
              Prices_S2S3_Plot_data_Bus=Prices_S2S3_Plot_data_Bus,
              Prices_S2S3_Plot_data_LDV=Prices_S2S3_Plot_data_LDV))
}
  
  
  
  
  
  