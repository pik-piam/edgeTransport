#' Read and prepare mrremind data
#'
#' @param SSP_scen SSP/SDP/other REMIND GDP scenario
#' @param REMIND2ISO_MAPPING mapping from REMIND regions to ISO3 country codes
#' @param load_cache (default=FALSE) load cached files from the input folder
#'          to bypass the heavy mrremind machinery.
#' @param mrremind_folder folder hosting mrremind cache, required if load_cache=TRUE
#' @return a list of mrremind-derived data, see end of this file.
#' @author Alois Dirnaichner, Marianna Rottoli
#' @importFrom magclass getSets getSets<-
#' @importFrom madrat readSource calcOutput


lvl0_mrremind <- function(SSP_scen, REMIND2ISO_MAPPING, load_cache=FALSE, mrremind_folder=NULL){
  Year <- `.` <- weight <- ratio <- value <- region <- GDP_cap <- POP_val <- NULL
  if(load_cache & file.exists(mrremind_folder)){
    IEAbal = readRDS(file.path(mrremind_folder, "IEAbal.RDS"))
    GDP_country = readRDS(file.path(mrremind_folder, "GDP_country.RDS"))
    RatioPPP2MER_country = readRDS(file.path(mrremind_folder, "RatioPPP2MER_country.RDS"))
    POP_country = readRDS(file.path(mrremind_folder, "POP_country.RDS"))
    trsp_incent = readRDS(file.path(mrremind_folder, "trasp_incent.RDS"))
  }else{
    IEAbal = madrat::calcOutput("IO", subtype = "IEA_output", aggregate = TRUE)
    GDP_country = {
      x <- calcOutput("GDP", aggregate = F)
      getSets(x)[1] <- "ISO3"
      getSets(x)[2] <- "Year"
      x
    }
    RatioPPP2MER_country <- calcOutput("RatioPPP2MER", aggregate = F)
    POP_country = {
      x <- calcOutput("Population", aggregate = F)
      getSets(x)[1] <- "iso2c"
      x
    }
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

  return(list(
    GDP_MER_country=GDP_MER_country,
    POP_country=POP_country,
    GDP_country=GDP_country,
    POP=POP,
    GDP=GDP,
    GDP_POP=GDP_POP,
    GDP_POP_MER=GDP_POP_MER,
    IEAbal=IEAbal,
    trsp_incent=trsp_incent
  ))

}
