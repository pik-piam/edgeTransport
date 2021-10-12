#' Read and prepare mrremind data
#'
#' @param SSP_scen SSP/SDP/other REMIND GDP scenario
#' @param REMIND2ISO_MAPPING mapping from REMIND regions to ISO3 country codes
#' @param load_cache (default=FALSE) load cached files from the input folder
#'          to bypass the heavy mrremind machinery.
#' @param mrremind_folder folder hosting mrremind cache, required if load_cache=TRUE
#' @return a list of mrremind-derived data, see end of this file.

lvl0_mrremind <- function(SSP_scen, REMIND2ISO_MAPPING, load_cache=FALSE, mrremind_folder=NULL){

  if(load_cache & file.exists(mrremind_folder)){
    IEAbal = readRDS(file.path(mrremind_folder, "IEAbal.RDS"))
    GDP_country = readRDS(file.path(mrremind_folder, "GDP_country.RDS"))
    RatioPPP2MER_country = readRDS(file.path(mrremind_folder, "RatioPPP2MER_country.RDS"))
    POP_country = readRDS(file.path(mrremind_folder, "POP_country.RDS"))
    JRC_IDEES_Trsp = readRDS(file.path(mrremind_folder, "JRC_IDEES_Trsp.RDS"))
    JRC_IDEES_MarBunk = readRDS(file.path(mrremind_folder, "JRC_IDEES_MarBunk.RDS"))
    trsp_incent = readRDS(file.path(mrremind_folder, "trasp_incent.RDS"))
  }else{
    IEAbal = calcOutput("IO", subtype = "IEA_output", aggregate = TRUE)
    GDP_country = {
      x <- calcOutput("GDPppp", aggregate = F)
      getSets(x)[1] <- "ISO3"
      getSets(x)[2] <- "Year"
      x
    }
    RatioPPP2MER_country = calcOutput("RatioPPP2MER", aggregate = F)
    POP_country = {
      x <- calcOutput("Population", aggregate = F)
      getSets(x)[1] <- "iso2c"
      x
    }
    JRC_IDEES_Trsp = readSource("JRC_IDEES", subtype="Transport")
    JRC_IDEES_MarBunk = readSource("JRC_IDEES", subtype="MBunkers")
    trsp_incent = readSource(type="TransportSubsidies")
  }

  ## rearrange the columns and create regional values
  GDP_country = GDP_country[,,paste0("gdp_", SSP_scen)]
  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(GDP_country, old = "value", new = "weight")
  GDP = merge(GDP_country, REMIND2ISO_MAPPING, by.x = "ISO3", by.y = "iso")
  GDP = GDP[,.(weight = sum(weight)), by = c("region", "year")]
  setnames(GDP_country, c("ISO3"), c("iso"))

  RatioPPP2MER_country = as.data.table(RatioPPP2MER_country)
  RatioPPP2MER_country = RatioPPP2MER_country[, c("year", "d3") := NULL]
  setnames(RatioPPP2MER_country, old = "value", new = "ratio")

  GDP_MER_country = merge(GDP_country, RatioPPP2MER_country, by.x = "iso", by.y = "Region")
  GDP_MER_country[, weight := weight*ratio]
  GDP_MER_country[, ratio := NULL]
  GDP_MER = merge(GDP_MER_country, REMIND2ISO_MAPPING, by = "iso")
  GDP_MER = GDP_MER[,.(weight = sum(weight)), by = c("region", "year")]

  POP_country = POP_country[,,paste0("pop_", SSP_scen)]
  POP_country <- as.data.table(POP_country)
  POP_country[, year := as.numeric(gsub("y", "", year))]
  POP = merge(POP_country, REMIND2ISO_MAPPING, by.x = "iso2c", by.y = "iso")
  POP = POP[,.(value = sum(value)), by = c("region", "year")]
  setnames(POP_country, old = c("iso2c", "variable"), new = c("iso", "POP"),skip_absent=TRUE)

  GDP_POP=merge(GDP,POP[,.(region,year,POP_val=value)],all = TRUE,by=c("region","year"))
  GDP_POP[,GDP_cap:=weight/POP_val]


  GDP_POP_MER=merge(GDP_MER,POP[,.(region,year,POP_val=value)],all = TRUE,by=c("region","year"))
  GDP_POP_MER[,GDP_cap:=weight/POP_val]

  GDP_POP_MER_country = merge(GDP_MER_country, POP_country,by = c("iso", "year"))

  ## filter only non-NA countries from IDEES and transform in data table
  JRC_IDEES=rbind(
    as.data.table(JRC_IDEES_Trsp), as.data.table(JRC_IDEES_MarBunk))
  JRC_IDEES=JRC_IDEES[grep("nergy consumption",variable.unit)]
  JRC_IDEES=JRC_IDEES[!grep("Shares",variable.unit)]
  JRC_IDEES=merge(JRC_IDEES,REMIND2ISO_MAPPING,by.x="iso3c",by.y="iso")
  JRC_IDEES=JRC_IDEES[,.(value=sum(value)),by = c("variable.unit","region","year")]
  JRC_IDEES[,year :=as.numeric(gsub("y","",year))]
  JRC_IDEES=JRC_IDEES[year %in%c(1990,2000,2005,2010,2015)]
  JRC_IDEES[, EJ:= value*  ##in ktoe
                   1e3*    ## in toe
                   4.1868E-8]  ## in EJ
  useful_var = c(
    ## international shipping
    "Maritime Bunkers|Energy Consumption|Total energy consumption.ktoe",
    ## international aviation
    "Transport|Aviation|Energy Consumption|Total energy consumption|Passenger transport|International - Extra-EU.ktoe",
    "Transport|Aviation|Energy Consumption|Total energy consumption|Passenger transport|International - Intra-EU.ktoe",
    ## domestic shipping
    "Transport|Navigation|Energy Consumption|Total energy consumption.ktoe",
    ## domestic aviation
    "Transport|Aviation|Energy Consumption|Total energy consumption|Passenger transport|Domestic.ktoe",
    ## road freight
    "Transport|Energy consumption|Freight transport|Road transport|Heavy duty vehicles.ktoe",
    ## road passenger LDVs
    "Transport|Energy consumption|Passenger transport|Road transport|Passenger cars.ktoe",
    ## road passenger HDVs
    "Transport|Road|Energy Consumption|Total energy consumption|Passenger transport|Motor coaches, buses and trolley buses.ktoe",
    ## rail passenger
    "Transport|Rail|Energy Consumption|Total energy consumption|Passenger transport|Conventional passenger trains.ktoe",
    ## rail freight
    "Transport|Rail|Energy Consumption|Total energy consumption|Freight transport.ktoe"
  )

  JRC_IDEES = JRC_IDEES[variable.unit %in% useful_var]

  return(list(
    GDP_MER_country=GDP_MER_country,
    POP_country=POP_country,
    GDP_country=GDP_country,
    POP=POP,
    GDP=GDP,
    GDP_POP=GDP_POP,
    GDP_POP_MER=GDP_POP_MER,
    IEAbal=IEAbal,
    JRC_IDEES=JRC_IDEES
  ))

}
