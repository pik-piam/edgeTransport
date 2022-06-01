#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#' Existing files are overwritten silently. Does not return anything.
#'
#' @param input_path the path to the folder containing the input (csv-) files
#' @param data_path the path to the output folder
#' @param SSP_scenario the SSP dimension, usually this refers to the GDP scenario in REMIND
#' @param EDGE_scenario the EDGE-T scenario dimension
#' @import data.table
#' @export

createRDS <- function(input_path, data_path, SSP_scenario, EDGE_scenario){

  SSPscen <- EDGEscen <- vehicle_type <- NULL

  print("Loading csv data from input folder and creating RDS files...")
  dir.create(file.path(data_path), showWarnings = FALSE)

  ## function that loads the csv input files and converts them into RDS local files
  csv2RDS = function(pattern, filename, input_path, names_dt){
    tmp <- fread(
      paste0(input_path, pattern, ".cs4r"),
      col.names = names_dt, skip="gdp_"
    )[SSPscen == SSP_scenario & EDGEscen == EDGE_scenario][, -c("SSPscen", "EDGEscen")]
    if (any(names(tmp) == 'vehicle_type')) {
      tmp[,vehicle_type := gsub("DOT", ".", vehicle_type)]
    }

    tmp_list <- split(tmp,tmp$entry)

    for (i in names(tmp_list)) {
      removecol = names_dt[names_dt %in% c("entry", "varname")]
      tmp_list[[i]][, grep(removecol, colnames(tmp_list[[i]])):=NULL]
      tmp_list[[i]] = tmp_list[[i]][,which(unlist(lapply(tmp_list[[i]], function(x)!any(x == "tmp")))),with=F]
    }

    if (length(tmp_list) == 1) {
      tmp_list = tmp_list[[1]]
    }

    saveRDS(tmp_list, file.path(data_path, paste0(filename,".RDS")))
  }


  ## create RDS files for lists

  csv2RDS(pattern = "pref",
          filename = "pref",
          input_path = input_path,
          names_dt = c("year", "region", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "logit_type", "entry", "value"))

  csv2RDS(pattern = "logit_exponent",
          filename = "logit_exp",
          input_path = input_path,
          names_dt = c("SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "entry", "logit.exponent"))

  csv2RDS(pattern = "value_time",
          filename = "VOT_iso",
          input_path = input_path,
          names_dt = c("year", "region", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "entry", "time_price"))

  csv2RDS(pattern = "price_nonmot",
          filename = "price_nonmot",
          input_path = input_path,
          names_dt = c("year", "region", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "tot_price"))

  ## create RDS files for single dataframes
  csv2RDS(pattern = "harmonized_intensities",
          filename = "harmonized_intensities",
          input_path = input_path,
          names_dt = c("year", "region", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "sector_fuel", "EJ_Mpkm_final"))

  csv2RDS(pattern = "UCD_NEC_iso",
          filename = "UCD_NEC_iso",
          input_path = input_path,
          names_dt = c("year", "region", "SSPscen", "EDGEscen", "vehicle_type", "technology", "price_component", "entry", "non_fuel_price"))

  csv2RDS(pattern = "loadFactor",
          filename = "loadFactor",
          input_path = input_path,
          names_dt = c("year", "region","SSPscen", "EDGEscen", "vehicle_type", "entry", "loadFactor"))

  csv2RDS(pattern = "annual_mileage",
          filename = "annual_mileage",
          input_path = input_path,
          names_dt = c("year", "region","SSPscen", "EDGEscen", "vehicle_type", "entry", "annual_mileage"))

  csv2RDS(pattern = "ptab4W",
          filename = "ptab4W",
          input_path = input_path,
          names_dt = c("SSPscen", "EDGEscen", "param", "entry", "ptab4W"))

}


#' Load EDGE-T input data (RDS format) from a given path.
#'
#' @param data_path path to RDS data files
#' @return A list of data.tables
#' @import data.table
#' @export

loadInputData <- function(data_path){
  price_component <- NULL
  datapathForFile <- function(fname){
    file.path(data_path, fname)
  }

  vot_data <- readRDS(datapathForFile("VOT_iso.RDS"))
  pref_data <- readRDS(datapathForFile("pref.RDS"))
  logit_params <- readRDS(datapathForFile("logit_exp.RDS"))
  int_dat <- readRDS(datapathForFile("harmonized_intensities.RDS"))
  UCD_costs <- readRDS(datapathForFile("UCD_NEC_iso.RDS"))
  price_nonmot <- readRDS(datapathForFile("price_nonmot.RDS"))
  loadFactor <- readRDS(datapathForFile("loadFactor.RDS"))
  ptab4W <- readRDS(datapathForFile("ptab4W.RDS"))

  ## FIXME: hotfix to make the (empty) vot_data$value_time_VS1 and vot_data$value_time_S1S2 with the right column types. Probably there is another way to do that, did not look for it.
  vot_data$value_time_VS1$region = as.character(vot_data$value_time_VS1$region)
  vot_data$value_time_VS1$subsector_L1 = as.character(vot_data$value_time_VS1$subsector_L1)
  vot_data$value_time_VS1$vehicle_type = as.character(vot_data$value_time_VS1$vehicle_type)
  vot_data$value_time_VS1$year = as.numeric(vot_data$value_time_VS1$year)
  vot_data$value_time_VS1$time_price = as.numeric(vot_data$value_time_VS1$time_price)

  vot_data$value_time_S1S2$region = as.character(vot_data$value_time_S1S2$region)
  vot_data$value_time_S1S2$subsector_L1 = as.character(vot_data$value_time_S1S2$subsector_L1)
  vot_data$value_time_S1S2$subsector_L2 = as.character(vot_data$value_time_S1S2$subsector_L2)
  vot_data$value_time_S1S2$year = as.numeric(vot_data$value_time_S1S2$year)
  vot_data$value_time_S1S2$time_price = as.numeric(vot_data$value_time_S1S2$time_price)

  ## change structure of preferences
  pref_data$VS1_final_pref = dcast(pref_data$VS1_final_pref, region + year + vehicle_type + subsector_L1 + subsector_L2 + subsector_L3 + sector ~ logit_type, value.var = "value")
  pref_data$S1S2_final_pref = dcast(pref_data$S1S2_final_pref, region + year + subsector_L1 + subsector_L2 + subsector_L3 + sector ~ logit_type, value.var = "value")
  pref_data$S2S3_final_pref = dcast(pref_data$S2S3_final_pref, region + year + subsector_L2 + subsector_L3 + sector ~ logit_type, value.var = "value")
  pref_data$S3S_final_pref = dcast(pref_data$S3S_final_pref, region + year + subsector_L3 + sector ~ logit_type, value.var = "value")

  ## refill structure of non fuel prices and split between purchase and total costs
  UCD_costs = merge(UCD_costs, unique(pref_data$FV_final_pref[, c("region", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector")]), by = c("region", "year", "vehicle_type"))
  nonfuel_costs = UCD_costs[price_component == "totalNE_cost"][, price_component := NULL]
  capcost4W = UCD_costs[price_component == "Capital_costs_purchase"]

  return(list(vot_data = vot_data,
              pref_data = pref_data,
              logit_params = logit_params,
              int_dat = int_dat,
              nonfuel_costs = nonfuel_costs,
              capcost4W = capcost4W,
              price_nonmot = price_nonmot,
              loadFactor = loadFactor,
              ptab4W = ptab4W))
}
