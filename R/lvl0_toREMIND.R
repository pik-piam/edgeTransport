

lvl0_REMINDdat = function(merged_data, VOT_lambdas, REMIND2ISO_MAPPING, GDP_country){
  
  sector_fuel <- technology <- `.` <- kJ.per.vkm <- conv_vkm_MJ <- ttw_energy <- value <- NULL
  
  gdp = copy(GDP_country)

  ## LF
  LF = copy(merged_data$LF)
  LF = aggregate_dt(data = LF,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "loadFactor",
                    datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                    weights = gdp)

  ## AM
  AM = copy(merged_data$AM)
  AM = aggregate_dt(data = AM,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "vkm.veh",
                    datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                    weights = gdp)

  ## intensity
  int = copy(merged_data$int)
  int = aggregate_dt(data = int,
                    mapping = REMIND2ISO_MAPPING,
                    valuecol = "conv_pkm_MJ",
                    datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
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
                     datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "var"),
                     weights = gdp[year%in%unique(costs$year)])
  setnames(costs, old = "var", new = "variable")

  ## demand
  dem = copy(merged_data$dem)
  dem = aggregate_dt(data = dem,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "tech_output",
                       datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3"),
                       weights = NULL)


  NFcost = costs[,.(non_fuel_price = sum(value)), by = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "year", "region")]

  ## VOT
  vt_FV = copy(VOT_lambdas$VOT_output$value_time_FV)
  vt_FV = aggregate_dt(data = vt_FV,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "time_price",
                       datacols = c("vehicle_type", "subsector_L1", "year"),
                       weights = gdp)

  vt_VS1 = copy(VOT_lambdas$VOT_output$value_time_VS1)
  setnames(vt_VS1, old = "iso", new = "region")

  vt_S1S2 = copy(VOT_lambdas$VOT_output$value_time_S1S2)
  setnames(vt_S1S2, old = "iso", new = "region")

  vt_S2S3 = copy(VOT_lambdas$VOT_output$value_time_S2S3)
  vt_S2S3 = aggregate_dt(data = vt_S2S3,
                       mapping = REMIND2ISO_MAPPING,
                       valuecol = "time_price",
                       datacols = c("subsector_L2", "subsector_L3", "year"),
                       weights = gdp)

  vt_S3S = copy(VOT_lambdas$VOT_output$value_time_S3S)
  vt_S3S = aggregate_dt(data = vt_S3S,
                         mapping = REMIND2ISO_MAPPING,
                         valuecol = "time_price",
                         datacols = c("sector", "subsector_L3", "year"),
                         weights = gdp)

  vt = list(value_time_FV = vt_FV, value_time_VS1 = vt_VS1, value_time_S1S2 = vt_S1S2, value_time_S2S3 = vt_S2S3, value_time_S3S = vt_S3S)

  ## price non-motorized

  pnm = copy(VOT_lambdas$price_nonmot)
  pnm = aggregate_dt(data = pnm,
                        mapping = REMIND2ISO_MAPPING,
                        valuecol = "tot_price",
                        datacols = c("sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "year"),
                        weights = gdp)


  return(list(LF = LF, AM = AM, int = int, costs = costs, NFcost = NFcost, dem = dem, vt = vt, pnm = pnm))

}

