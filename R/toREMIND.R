

toolREMINDdat <- function(merged_data, VOT_lambdas, REMIND2ISO_MAPPING, GDP_country) {
  
  sector_fuel <- technology <- `.` <- kJ.per.vkm <- conv_vkm_MJ <- ttw_energy <- value <- NULL
  
  gdp = copy(GDP_country)

  ## LF
  LF = copy(merged_data$LF)
  LF = aggregate_dt(data = LF,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "loadFactor",
                    datacols = c("vehicleType", "technology", "sector", "subsectorL3", "subsectorL2", "subsectorL1"),
                    weights = gdp)

  ## AM
  AM = copy(merged_data$AM)
  AM = aggregate_dt(data = AM,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "vkm.veh",
                    datacols = c("vehicleType", "technology", "sector", "subsectorL3", "subsectorL2", "subsectorL1"),
                    weights = gdp)

  ## intensity
  int = copy(merged_data$int)
  int = aggregate_dt(data = int,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "conv_pkm_MJ",
                    datacols = c("vehicleType", "technology", "sector", "subsectorL3", "subsectorL2", "subsectorL1"),
                    weights = gdp)
  int[, sector_fuel := "refined liquids enduse"]
  int[technology == "FCEV", sector_fuel := "H2 enduse"]
  int[technology == "NG", sector_fuel := "delivered gas"]
  int[technology == "Hybrid Electric", sector_fuel := "Liquids-Electricity"]
  int[technology %in% c("BEV", "LA-BEV", "Electric"), sector_fuel := "elect_td_trn"]
  int[technology %in% c("Hydrogen"), sector_fuel := "H2 enduse"]

  ## costs
  costs = copy(merged_data$costs)
  setnames(costs, old = "variable", new = "var")
  costs = aggregate_dt(data = costs,
                     mapping = REMIND2ISO_MAPPING,
                     valuecol = "value",
                     datacols = c("vehicleType", "technology", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "var"),
                     weights = gdp[year%in%unique(costs$year)])
  setnames(costs, old = "var", new = "variable")

  ## demand
  dem = copy(merged_data$dem)
  dem = aggregate_dt(data = dem,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "tech_output",
                       datacols = c("vehicleType", "technology", "sector", "subsectorL3", "subsectorL2", "subsectorL1"),
                       weights = NULL)


  NFcost = costs[,.(non_fuel_price = sum(value)), by = c("vehicleType", "technology", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "year", "region")]

  ## VOT
  vt_FV = copy(VOT_lambdas$VOT_output$value_time_FV)
  vt_FV = aggregate_dt(data = vt_FV,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "time_price",
                       datacols = c("vehicleType", "subsectorL3", "year"),
                       weights = gdp)

  vt_VS3 = copy(VOT_lambdas$VOT_output$value_time_VS3)
  setnames(vt_VS3, old = "iso", new = "region")

  vt_S3S2 = copy(VOT_lambdas$VOT_output$value_time_S3S2)
  setnames(vt_S3S2, old = "iso", new = "region")

  vt_S2S1 = copy(VOT_lambdas$VOT_output$value_time_S2S1)
  vt_S2S1 = aggregate_dt(data = vt_S2S1,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "time_price",
                       datacols = c("subsectorL2", "subsectorL1", "year"),
                       weights = gdp)

  vt_S1S = copy(VOT_lambdas$VOT_output$value_time_S1S)
  vt_S1S = aggregate_dt(data = vt_S1S,
                         mapping = REMIND2ISO_MAPPING,
                         valuecol = "time_price",
                         datacols = c("sector", "subsectorL1", "year"),
                         weights = gdp)

  vt = list(value_time_FV = vt_FV, value_time_VS3 = vt_VS3, value_time_S3S2 = vt_S3S2, value_time_S2S1 = vt_S2S1, value_time_S1S = vt_S1S)

  ## price non-motorized

  pnm = copy(VOT_lambdas$price_nonmot)
  pnm = aggregate_dt(data = pnm,
                        mapping = REMIND2ISO_MAPPING,
                        valuecol = "tot_price",
                        datacols = c("sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology", "year"),
                        weights = gdp)


  return(list(LF = LF, AM = AM, int = int, costs = costs, NFcost = NFcost, dem = dem, vt = vt, pnm = pnm))

}
