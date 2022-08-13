#' Loads and prepared historical international aviation demand data
#'
#'
#' @param input_folder input folder
#' @param GDP_country GDP_country data
#' @param ICCT_dir Directory of the ICCT data
#'
#' @author Sebastian Franz
#' 
#' 


toolIntAvPreparation <-function(input_folder, GDP_country, ICCT_dir = "ICCT") {
  `.` <- value <- NULL
  ## load data
  ICCT_data =fread(file.path(input_folder, ICCT_dir, "ICCT_data.csv"))
  ICCT2GCAM =fread(file.path(input_folder, ICCT_dir, "ICCT2GCAM.csv"))
  GDP_country = copy(GDP_country)
  setnames(ICCT_data, old=c("international RPKs (billions)","name"), new =c("value","region"))
  ICCT_data = ICCT_data[,c(6,1)]
  GDP_country = GDP_country[year==2020]
  ICCT_data[, year := 2020]
  ## match ICCT country  mapping to GCAM country mapping
  ICCT_data = disaggregate_dt(ICCT_data, ICCT2GCAM,
                              valuecol="value",
                              datacols = "year",
                              weights = GDP_country)
  
  iso_mapping = fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
  setnames(iso_mapping, "CountryCode", "iso")
  ICCT_data = merge(ICCT_data, iso_mapping, by = c("iso"), all.x=TRUE)
  ICCT_data = transform(ICCT_data, year = 2020)
  ICCT_data = ICCT_data[,c(3,5)]
  ## aggregate the data up to Region Mapping
  ICCT_data = ICCT_data[, .(value = sum(value)), by = c("RegionCode")]
  ICCT_data = transform(ICCT_data, sector = "trn_aviation_intl")
  ICCT_data = transform(ICCT_data, year = 2020)
  setnames(ICCT_data, old=c("RegionCode","value"), new =c("region","demand_tot"))
  return(ICCT_data)
}
