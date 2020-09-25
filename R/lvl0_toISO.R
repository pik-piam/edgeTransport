#' Produce ISO versions of all files.
#'
#' No conversion of units happening.
#'
#' @param input_data demand and energy intensity input data
#' @param VOT_data value of time data
#' @param price_nonmot price non motorized transport
#' @param UCD_data UCD data
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param REMIND_scenario SSP scenario
#' @return ISO level data for all entries
#' @author Marianna Rottoli


lvl0_toISO <- function(input_data, VOT_data, price_nonmot, UCD_data, GCAM2ISO_MAPPING, EDGE_scenario, REMIND_scenario="SSP2"){
    scenario <- subsector_L1 <- price_component <- NULL
    ## GCAM data
    tech_output <- input_data[["tech_output"]]
    intensity <- input_data[["conv_pkm_mj"]]

    gdp <- getRMNDGDP(scenario = paste0("gdp_", REMIND_scenario), usecache = T)

    ## tech output is extensive: use GDP weight
    TO_iso <- disaggregate_dt(tech_output, GCAM2ISO_MAPPING,
                              valuecol="tech_output",
                              datacols=c("sector", "subsector_L1", "subsector_L2",
                                         "subsector_L3", "vehicle_type", "technology"),
                              weights=gdp)
    ## check if there are NAs
    stopifnot(!any(is.na(TO_iso$tech_output)))

    ## intensity and load factor are intensive
    int_iso <- disaggregate_dt(intensity, GCAM2ISO_MAPPING)


    dups <- duplicated(int_iso, by=c("iso", "technology", "vehicle_type","year"))

    if(any(dups)){
      warning("Duplicated techs found in supplied energy intensity.")
      print(int_iso[dups])
      int_iso <- unique(int_iso, by=c("iso", "technology", "vehicle_type","year"))
    }

    iso_GCAMdata_results = list(
      tech_output = TO_iso,
      conv_pkm_mj = int_iso)

    ## VOT an non-motorized cost, intensive
    vots <- names(VOT_data)

    vot_iso <- lapply(VOT_data[vots], function(item){
        disaggregate_dt(item, GCAM2ISO_MAPPING)
    })

    iso_VOT_results = vot_iso

    ## non-motorized cost, intensive
    iso_pricenonmot_results = disaggregate_dt(price_nonmot, GCAM2ISO_MAPPING)

    ## UCD data
    nec_cost <- UCD_data$non_energy_cost$non_energy_cost ## non energy cost aggregated
    nec_iso <-disaggregate_dt(nec_cost,GCAM2ISO_MAPPING)

    nec_cost_split <- UCD_data$non_energy_cost$non_energy_cost_split ## non energy cost disaggregated
    nec_cost_split_iso <-disaggregate_dt(nec_cost_split,GCAM2ISO_MAPPING,
                                  datacols = c("mode","UCD_technology","price_component", "type"))


    annual_mileage_iso <- UCD_data[["annual_mileage"]] #already on ISO level


    load_factor <- UCD_data$non_energy_cost[["load_factor"]]
    load_iso <- disaggregate_dt(load_factor, GCAM2ISO_MAPPING)
    
    capcost4W <- nec_cost_split_iso[subsector_L1 == "trn_pass_road_LDV_4W" & price_component == "Capital_costs_purchase"][, c("sector", "subsector_L3", "subsector_L2", "subsector_L1") := NULL]

    iso_UCD_results = list(nec_cost_split_iso = nec_cost_split_iso,
                           capcost4W = capcost4W,
                           annual_mileage_iso = annual_mileage_iso,
                           nec_iso = nec_iso,
                           nec_cost_split_iso = nec_cost_split_iso,
                           load_iso = load_iso)

    result =list(iso_VOT_results = iso_VOT_results,
                 iso_pricenonmot_results = iso_pricenonmot_results,
                 iso_UCD_results = iso_UCD_results,
                 iso_GCAMdata_results = iso_GCAMdata_results)

    return(result)
}
