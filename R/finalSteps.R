#' Provide demand trajectories for REMIND
#'
#' @param regrdemand demand regression
#' @param EDGE2teESmap mapping between EDGE-T and REMIND technologies
#' @param REMINDtall extended REMIND time steps
#' @param SSP_scen SSP scenario
#' @importFrom rmndt approx_dt
#'


toolREMINDdemand <- function(regrdemand, EDGE2teESmap, REMINDtall, SSP_scen) {
    `.` <- demand <- region <- all_in <- all_GDPscen <- value <- NULL
    gdp_ssp_scenario <- paste0("gdp_", SSP_scen)
    regrdemand=merge(regrdemand, unique(EDGE2teESmap[,c("EDGE_top","all_in")]),
                     by.x="sector",by.y="EDGE_top",all.x=TRUE)
    regrdemand=regrdemand[,.(value=demand,region,year,all_in)]
    regrdemand = approx_dt(regrdemand, REMINDtall,
                           xcol = "year", ycol = "value",
                           idxcols = c("region", "all_in"),
                           extrapolate=T)
    ## add SSP dimension
    regrdemand[,all_GDPscen:=gdp_ssp_scenario]
    ## convert into trillionpkm
    regrdemand[,value:=value   ## in [millionpkm]
               *1e-6]          ## in [trillionpkm]
    ## set columns order
    setcolorder(regrdemand, c("year", "region", "all_GDPscen","all_in", "value"))
    return(regrdemand)
}


#' Creates csv files that contains all EDGE-transport entries for mrremind or a list of objects.
#'
#' @param logit_params logit parameters
#' @param pref_data preference factors
#' @param ptab4W table including all the inconvenience cost factors for LDVs
#' @param vot_data value of time
#' @param NEC_data non fuel cots
#' @param capcost4W capital costs 4wheelers
#' @param demByTech demand projections at REMIND level
#' @param int_dat intensity at technology level
#' @param intensity intensity at REMIND level
#' @param capCost capital costs
#' @param price_nonmot non motorized modes price
#' @param loadFactor load factors
#' @param annual_mileage annual mileage
#' @param demISO ISO level demand in 2010 to be used as a weight in mrremind
#' @param SSP_scen SSP scenario
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param level2path directory where data will be saved
#' @param complexValues values for complex module in REMIND
#' @param output_folder directory where the data has to be saved, if set to NULL a list of magclass objects will be saved


toolCreateOutput <- function(logit_params, pref_data, ptab4W, vot_data, NEC_data,
                             capcost4W, demByTech, int_dat, intensity, capCost,
                             price_nonmot, complexValues, loadFactor, annual_mileage,
                             demISO, SSP_scen, EDGE_scenario, level2path, output_folder) {
  price_component <- MJ_km <- NULL
  gdp_scenario <- paste0("gdp_", SSP_scen)

  addScenarioCols <- function(data, nc){
    ## Add scenario cols after column nc
    ## nc == 0: Add scenarios as first cols
    if(nrow(data) == 0){
      ## do nothing
      return(data)
    }
    if(nc == 0){
      as.data.table(cbind(
        GDP_scenario = gdp_scenario,
        EDGE_scenario = EDGE_scenario,
        data))
    }else{
      data = as.data.table(cbind(
        data[, 1:nc, with=F],
        GDP_scenario = gdp_scenario,
        EDGE_scenario = EDGE_scenario,
        data[, (nc + 1):ncol(data), with=F]))
    }
  }


  ## to all the internal data (which is solely used in the iterative version)
  ## the scenario information is prepended (cols 1 and 2)
  logit_params <- lapply(logit_params, function(item){
    ## do not have region and time info
    addScenarioCols(item, 0)
  })

  pref_data <- lapply(pref_data, function(item){
    addScenarioCols(item, 0)
  })

  vot_data <- lapply(vot_data, function(item){
    addScenarioCols(item, 0)
  })

  complexValues$dem <- lapply(complexValues$dem, function(item){
    addScenarioCols(item, 0)
  })

  NEC_data <- addScenarioCols(NEC_data, 0)
  int_dat <- addScenarioCols(int_dat, 0)
  price_nonmot <- addScenarioCols(price_nonmot, 0)
  capcost4W <- addScenarioCols(capcost4W, 0)
  complexValues$shLDV <- addScenarioCols(complexValues$shLDV, 0)
  demISO <- addScenarioCols(demISO, 0)

  ptab4W[, c("SSPscen", "techscen") := NULL]
  ptab4W <- addScenarioCols(ptab4W, 0)

  ## for the data used in REMIND directly, we put the scens after time and region
  demByTech <- addScenarioCols(demByTech, 2)
  capCost <- addScenarioCols(capCost, 2)
  intensity <- addScenarioCols(intensity, 2)

  ## NEC costs are merged with Capital Costs for 4W and the number of columns is reduced
  NEC_data[, c("sector", "subsector_L3", "subsector_L2", "subsector_L1") := NULL]
  NEC_data[, price_component := "totalNE_cost"]

  capcost4W[, c("sector", "subsector_L3", "subsector_L2", "subsector_L1") := NULL]
  setnames(capcost4W, old = c("variable", "value"), new = c("price_component", "non_fuel_price"))
  NEC_data = rbind(NEC_data, capcost4W)

  ## all the preference dfs have to be with the same structure as FV_pref
  pref_datatmp = pref_data["FV_final_pref"] ## this is not to be modified
  pref_data = pref_data[c("VS1_final_pref", "S1S2_final_pref", "S2S3_final_pref", "S3S_final_pref")]
  ## melt and rearrange all other levels
  pref_data <- mapply(melt, pref_data, id.vars = list(c("year", "region", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "GDP_scenario", "EDGE_scenario"),
                                                      c("year", "region", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "GDP_scenario", "EDGE_scenario"),
                                                      c("year", "region", "sector", "subsector_L3", "subsector_L2", "GDP_scenario", "EDGE_scenario"),
                                                      c("year", "region", "sector", "subsector_L3", "GDP_scenario", "EDGE_scenario")
                                                      ))
  ## rename column in all datatables
  pref_data = lapply(pref_data, setnames, old = "variable", new = "logit_type")
  ## variable is a factor, change to character
  f = function(x) {
    x$logit_type = as.character(x$logit_type)
    return(x)
    }

  pref_data = lapply(pref_data, f)
  pref_data$FV_final_pref = pref_datatmp$FV_final_pref

  ## remove column not needed in REMIND from int_dat
  int_dat[, MJ_km := NULL]

  ## remove technology column from load factor
  load_factor <- load_factor[year >= 1990, .(loadFactor = mean(loadFactor)),
                             by=c("region", "year", "vehicle_type")]
  ## add scenario column to load factor
  loadFactor <- addScenarioCols(loadFactor, 0)

  ## add scenario column to annual mileage
  annual_mileage <- addScenarioCols(annual_mileage, 0)
  ## select only the relevant columns
  annual_mileage <- unique(annual_mileage[,c("GDP_scenario", "EDGE_scenario", "region", "year", "vkm.veh", "vehicle_type")])
  setnames(annual_mileage, old = "vkm.veh", new = "annual_mileage")
  if (!is.null(output_folder)) {
    dir.create(file.path(level2path("")), showWarnings = FALSE)
    ## writes csv files for intensity, shares and budget, and demand (for calibration and starting point of REMIND)
    print("Creating cs4r files...")
    fwrite(demByTech, file = level2path("fe_demand_tech.cs4r"))
    fwrite(intensity, file = level2path("fe2es.cs4r"))
    fwrite(capCost, file = level2path("esCapCost.cs4r"))

    ## writes csv files for lambda parameters, SW, VOT and non-motorized costs, energy intensity, non-energy costs (for the EDGE run in between REMIND iterations)
    print("Creating csv files for lambdas...")
    mapply(
      fwrite,
      x=logit_params, file=level2path(paste0(names(logit_params), ".csv")),
      MoreArgs=list(row.names=FALSE, sep=",", quote=F)
    )
    print("Creating csv files for preferences...")
    mapply(
      fwrite,
      x=pref_data, file=level2path(paste0(names(pref_data), ".csv")),
      MoreArgs=list(row.names=FALSE, sep=",", quote=F)
    )
    print("Creating csv files for VOT...")
    mapply(
      fwrite,
      x=vot_data, file=level2path(paste0(names(vot_data), ".csv")),
      MoreArgs=list(row.names=FALSE, sep=",", quote=F)
    )

    ## writes csv file for complex realization and reporting
    print("Creating csv files for complex realization...")
    mapply(
      fwrite,
      x=complexValues$dem, file=level2path(paste0("EDGE_output_", names(complexValues$dem), ".csv")),
      MoreArgs=list(row.names=FALSE, sep=",", quote=F)
    )

    fwrite(complexValues$shLDV, file = level2path("shares_LDV_transport.cs4r"))

    print("Creating csv files for energy demand weights...")
    fwrite(demISO, file = level2path("demISO.csv"))
    print("Creating csv files for non-motorized costs...")
    fwrite(price_nonmot, file = level2path("price_nonmot.csv"))
    print("Creating csv files for energy intensity...")
    fwrite(int_dat, file = level2path("harmonized_intensities.csv"))
    print("Creating csv files for non-energy costs...")
    fwrite(NEC_data, file = level2path("UCD_NEC_iso.csv"))
    print("Creating csv files for load factor...")
    fwrite(loadFactor, file = level2path("loadFactor.csv"))
    print("Creating csv files for annual mileage...")
    fwrite(annual_mileage, file = level2path("annual_mileage.csv"))
    print("Creating csv files for inconvenience cost factors...")
    fwrite(ptab4W, file = level2path("ptab4W.csr4"))
  } else {
    EDGETrData = list(fe_demand_tech = demByTech,
                      fe2es = intensity,
                      esCapCost = capCost,
                      logit_params = logit_params,
                      pref_data = pref_data,
                      vot_data = vot_data,
                      complexdem = complexValues$dem,
                      shares_LDV_transport = complexValues$shLDV,
                      demISO = demISO,
                      price_nonmot = price_nonmot,
                      harmonized_intensities = int_dat,
                      UCD_NEC_iso = NEC_data,
                      loadFactor = loadFactor,
                      annual_mileage = annual_mileage,
                      ptab4W = ptab4W)

    return(EDGETrData)
  }

}
