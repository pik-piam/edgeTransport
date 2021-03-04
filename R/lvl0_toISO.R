#' Produce ISO versions of all files.
#'
#' No conversion of units happening.
#'
#' @param input_data demand and energy intensity input data
#' @param VOT_data value of time data
#' @param price_nonmot price non motorized transport
#' @param UCD_data UCD data
#' @param GDP_country GDP ISO level
#' @param GDP regional level
#' @param POP population regional level
#' @param GDP_POP per capita GDP
#' @param GCAM2ISO_MAPPING GCAM2iso mapping
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param REMIND_scenario SSP scenario
#' @param REMIND2ISO_MAPPING regional mapping
#'
#' @importFrom rmndt aggregate_dt disaggregate_dt
#' @return ISO level data for all entries
#' @author Marianna Rottoli


lvl0_toISO <- function(input_data, VOT_data, price_nonmot, UCD_data, GDP, GDP_country, POP, GDP_POP, GCAM2ISO_MAPPING, REMIND2ISO_MAPPING, EDGE_scenario, REMIND_scenario="SSP2"){
    scenario <- subsector_L1 <- price_component <- GDP_cap <- region <- conv_pkm_MJ <- `.` <- weight <- POP_val <- vehicle_type <- year_at_yearconv <- conv_pkm_MJ_trend <- conv_pkm_MJ_conv <- technology <- NULL
    subsector_L2 <- subsector_L3 <- sector <- sector_fuel <- yearconv <- time <- non_fuel_price <-non_fuel_price_trend <- non_fuel_price_conv <- type <- NULL
    ## GCAM data
    tech_output <- input_data[["tech_output"]]
    intensity <- input_data[["conv_pkm_mj"]]
    ## tech output is extensive: use GDP weight
    gdp_country = copy(GDP_country)
    TO_iso <- disaggregate_dt(tech_output, GCAM2ISO_MAPPING,
                              valuecol="tech_output",
                              datacols=c("sector", "subsector_L1", "subsector_L2",
                                         "subsector_L3", "vehicle_type", "technology"),
                              weights=gdp_country)
    ## check if there are NAs
    stopifnot(!any(is.na(TO_iso$tech_output)))

    ## intensity and load factor are intensive
    int <- disaggregate_dt(intensity, GCAM2ISO_MAPPING)
    ## re-aggregate to REMIND regions
    gdp =copy(GDP_country)
    int <- aggregate_dt(int, REMIND2ISO_MAPPING,
                           valuecol="conv_pkm_MJ",
                           datacols=c("sector", "subsector_L1", "subsector_L2",
                                      "subsector_L3", "vehicle_type", "technology"),
                           weights=gdp)
    ## convergence of intensity according to GDPcap
    ## working principle: intensity follows linear convergence between 2010 and the year it reaches GDPcap@(2010,richcountry). Values from richcountry for the following time steps (i.e. when GDPcap@(t,developing)>GDPcap@(2010,richcountry))
    ## load gdp per capita
    gdp_pop=copy(GDP_POP)
    tmp = merge(int, gdp_pop, by = c("region", "year"))
    ## define rich regions
    richregions = unique(unique(tmp[year == 2010 & GDP_cap > 25000, region]))
    ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
    richave = tmp[region %in% richregions & conv_pkm_MJ > 0,]
    richave = richave[, .(conv_pkm_MJ = sum(conv_pkm_MJ*weight)/sum(weight)), by = c("subsector_L1", "vehicle_type", "technology", "year")]
    gdp_pop = gdp_pop[region %in% richregions,]
    gdp_pop = gdp_pop[, .(GDP = sum(weight), POP_val = sum(POP_val)), by = c("year")]
    richave = merge(richave, gdp_pop, by = "year")
    ## average gdp per capita of the rich countries
    richave[, GDP_cap := GDP/POP_val]
    ## missing trucks categories are attributed an average cost for rich countries
    richave = rbind(richave, richave[vehicle_type == "Truck (0-3.5t)"][,vehicle_type := "Truck (0-6t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (0-1t)"][,vehicle_type := "Truck (0-2t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (1-6t)"][,vehicle_type := "Truck (2-5t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (4.5-12t)"][,vehicle_type := "Truck (5-9t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (4.5-15t)"][,vehicle_type := "Truck (6-14t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (6-15t)"][,vehicle_type := "Truck (9-16t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (>15t)"][,vehicle_type := "Truck (>14t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck"][,vehicle_type := "Truck (>16t)"])

    ## dt on which the GDPcap is checked
    tmp1 = tmp[!region %in% richregions, c("region", "year",
                                           "GDP_cap", "technology", "vehicle_type",
                                           "subsector_L1", "subsector_L2", "subsector_L3",
                                           "sector", "conv_pkm_MJ", "sector_fuel")]
    ## dt contaning the gdp towards which to converge
    tmp2 = richave[, c("year", "GDP_cap")]
    ## dt containing the non fuel price for rich countries
    tmp3 = richave[, c("year", "technology", "vehicle_type")]
    ## names has to be different across dts for roll join
    setnames(tmp2, old = c("year"), new = c("time"))
    setnames(tmp3, old = c("year"), new = c("time"))

    setkey(tmp1,GDP_cap)
    setkey(tmp2,GDP_cap)
    ## find the time step at which the GDPcap matches the GDPcap of the rich countries
    tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

    ## merge with non fuel price of corresponding values
    tmp2 = merge(tmp2, tmp3, by = c("time", "technology", "vehicle_type"))

    ## find year closest to 2010 for each region, this is the year at which is going to converge
    tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("region")]

    ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
    tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("region", "time")]
    tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("region", "time")]
    ## if year of convergence is 2010, 2015 is selected
    tmp2[yearconv == 2010, yearconv := 2020]
    tmp2[yearconv == 2015, yearconv := 2020]
    ## year at which the convergence happens
    tmp2[, year_at_yearconv := year[time == yearconv], by = c("region", "technology", "vehicle_type")]
    tmp2[is.na(year_at_yearconv), year_at_yearconv := year[time == yearconv + 5], by = c("region", "technology", "vehicle_type")]

    ## value of the non-fuel price after the convergence
    tmp3 = richave[, c("year", "conv_pkm_MJ", "technology", "vehicle_type")]
    setnames(tmp3, old = c("conv_pkm_MJ"), new = c("conv_pkm_MJ_trend"))
    tmp2 = merge(tmp2,tmp3,by=c("year", "technology", "vehicle_type"))

    ## after the year of convergence, the values are the "average" developed countries values
    tmp2[year >= year_at_yearconv & year > 2010, conv_pkm_MJ := conv_pkm_MJ_trend, by = c("region","technology", "vehicle_type")]

    ## value of yearconv represents the convergence value
    tmp2[, conv_pkm_MJ_conv := conv_pkm_MJ_trend[time==yearconv], by = c("region","technology", "vehicle_type")]
    tmp2[is.na(conv_pkm_MJ_conv), conv_pkm_MJ_conv := conv_pkm_MJ_trend[time==yearconv+5], by = c("region","technology", "vehicle_type")]

    ## convergence is linear until the value corresponding to 2010 is reached
    tmp2[year <= year_at_yearconv & year >= 2010, conv_pkm_MJ := conv_pkm_MJ[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(conv_pkm_MJ_conv-conv_pkm_MJ[year == 2010]), by =c("technology", "vehicle_type", "region")]
    ## select only useful columns
    tmp2 = tmp2[,.(region, year, conv_pkm_MJ, technology, vehicle_type, subsector_L1, subsector_L2, subsector_L3, sector, sector_fuel)]
    ## rich countries need to be reintegrated
    int = rbind(tmp2, int[region %in% richregions])

    dups <- duplicated(int, by=c("region", "technology", "vehicle_type","year"))

    if(any(dups)){
      warning("Duplicated techs found in supplied energy intensity.")
      print(int[dups])
      int <- unique(int, by=c("region", "technology", "vehicle_type","year"))
    }

    iso_GCAMdata_results = list(
      tech_output = TO_iso,
      conv_pkm_mj = int)

    ## VOT an non-motorized cost, intensive
    vots <- names(VOT_data)

    vot <- lapply(VOT_data[vots], function(item){
      disaggregate_dt(item, GCAM2ISO_MAPPING)
    })

    ## aggregation is on gdp
    vot$value_time_FV = aggregate_dt(vot$value_time_FV, REMIND2ISO_MAPPING,
                                     valuecol="time_price",
                                     datacols=c("subsector_L1", "vehicle_type"),
                                     weights=gdp)
    vot$value_time_VS1 = aggregate_dt(vot$value_time_VS1, REMIND2ISO_MAPPING,
                                     valuecol="time_price",
                                     datacols=c("subsector_L1",  "vehicle_type"),
                                     weights=gdp)
    vot$value_time_S1S2 = aggregate_dt(vot$value_time_S1S2, REMIND2ISO_MAPPING,
                                      valuecol="time_price",
                                      datacols=c("subsector_L1",  "subsector_L2"),
                                      weights=gdp)
    vot$value_time_S2S3 = aggregate_dt(vot$value_time_S2S3, REMIND2ISO_MAPPING,
                                       valuecol="time_price",
                                       datacols=c("subsector_L2",  "subsector_L3"),
                                       weights=gdp)
    vot$value_time_S3S = aggregate_dt(vot$value_time_S3S, REMIND2ISO_MAPPING,
                                       valuecol="time_price",
                                       datacols=c("sector", "subsector_L3"),
                                       weights=gdp)

    ## non-motorized cost, intensive
    price_nonmot = disaggregate_dt(price_nonmot, GCAM2ISO_MAPPING)
    price_nonmot = aggregate_dt(price_nonmot, REMIND2ISO_MAPPING,
                                valuecol="tot_price",
                                datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "year"),
                                weights=gdp)
    ## UCD data
    nec_cost <- UCD_data$non_energy_cost ## non energy cost aggregated
    nec_cost <-disaggregate_dt(nec_cost,GCAM2ISO_MAPPING)
    nec_cost = aggregate_dt(nec_cost, REMIND2ISO_MAPPING,
                            valuecol="non_fuel_price",
                            datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "type", "year"),
                            weights=gdp)

    ## convergence of non_fuel_price according to GDPcap
    ## working principle: non_fuel_price follows linear convergence between 2010 and the year it reaches GDPcap@(2010,richcountry). Values from richcountry for the following time steps (i.e. when GDPcap@(t,developing)>GDPcap@(2010,richcountry))
    ## load gdp per capita
    gdp_pop=copy(GDP_POP)
    tmp = merge(nec_cost, gdp_pop, by = c("region", "year"))

    ## define rich regions
    richregions = unique(unique(tmp[year == 2010 & GDP_cap > 25000, region]))
    ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
    richave = tmp[region %in% richregions & non_fuel_price > 0,]
    richave = richave[, .(non_fuel_price = sum(non_fuel_price*weight)/sum(weight)), by = c("subsector_L1", "vehicle_type", "technology", "year")]
    gdp_pop = gdp_pop[region %in% richregions,]
    gdp_pop = gdp_pop[, .(GDP = sum(weight), POP_val = sum(POP_val)), by = c("year")]
    richave = merge(richave, gdp_pop, by = "year")
    ## average gdp per capita of the rich countries
    richave[, GDP_cap := GDP/POP_val]
    ## missing trucks categories are attributed an average cost for rich countries
    richave = rbind(richave, richave[vehicle_type == "Truck (0-3.5t)"][,vehicle_type := "Truck (0-6t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (0-1t)"][,vehicle_type := "Truck (0-2t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (1-6t)"][,vehicle_type := "Truck (2-5t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (4.5-12t)"][,vehicle_type := "Truck (5-9t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (4.5-15t)"][,vehicle_type := "Truck (6-14t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (6-15t)"][,vehicle_type := "Truck (9-16t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck (>15t)"][,vehicle_type := "Truck (>14t)"])
    richave = rbind(richave, richave[vehicle_type == "Truck"][,vehicle_type := "Truck (>16t)"])

    ## dt on which the GDPcap is checked
    tmp1 = tmp[!region %in% richregions, c("region", "year", "non_fuel_price", "GDP_cap", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "type")]
    ## dt contaning the gdp towards which to converge
    tmp2 = richave[, c("year", "GDP_cap")]
    ## dt containing the non fuel price for rich countries
    tmp3 = richave[, c("year", "technology", "vehicle_type")]
    ## names has to be different across dts for roll join
    setnames(tmp2, old = c("year"), new = c("time"))
    setnames(tmp3, old = c("year"), new = c("time"))

    setkey(tmp1,GDP_cap)
    setkey(tmp2,GDP_cap)
    ## find the time step at which the GDPcap matches the GDPcap of the rich countries
    tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

    ## merge with non fuel price of corresponding values
    tmp2 = merge(tmp2, tmp3, by = c("time", "technology", "vehicle_type"))

    ## find year closest to 2010 for each region, this is the year at which is going to converge
    tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("region")]

    ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
    tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("region", "time")]
    tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("region", "time")]

    ## year at which the convergence happens
    tmp2[, year_at_yearconv := year[time == yearconv], by = c("region","technology", "vehicle_type")]

    ## value of the non-fuel price after the convergence
    tmp3 = richave[, c("year", "non_fuel_price", "technology", "vehicle_type")]
    setnames(tmp3, old = c("non_fuel_price"), new = c("non_fuel_price_trend"))
    tmp2 = merge(tmp2,tmp3,by=c("year", "technology", "vehicle_type"))

    ## after the year of convergence, the values are the "average" developed countries values
    tmp2[year >= year_at_yearconv & year > 2010, non_fuel_price := non_fuel_price_trend, by = c("region","technology", "vehicle_type")]

    ## value of yearconv represents the convergence value
    tmp2[, non_fuel_price_conv := non_fuel_price_trend[time==yearconv], by = c("region","technology", "vehicle_type")]
    ## convergence is linear until the value corresponding to 2010 is reached
    tmp2[year <= year_at_yearconv & year >= 2010, non_fuel_price := non_fuel_price[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(non_fuel_price_conv-non_fuel_price[year == 2010]), by =c("technology", "vehicle_type", "region")]
    ## select only useful columns
    tmp2 = tmp2[,.(region, year, non_fuel_price, technology, vehicle_type, subsector_L1, subsector_L2, subsector_L3, sector, type)]

    ## rich countries need to be reintegrated
    nec_cost = rbind(tmp2, nec_cost[region %in% richregions])


    nec_cost_split <- UCD_data$non_energy_cost_split ## non energy cost disaggregated
    nec_cost_split <-disaggregate_dt(nec_cost_split,GCAM2ISO_MAPPING,
                                  datacols = c("mode","UCD_technology","price_component", "type"))
    nec_cost_split <-aggregate_dt(nec_cost_split, REMIND2ISO_MAPPING,
                                  valuecol="non_fuel_price",
                                  datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "type", "year", "price_component"),
                                  weights=gdp)

    annual_mileage <- UCD_data$annual_mileage #already on ISO level
    annual_mileage <- aggregate_dt(annual_mileage, REMIND2ISO_MAPPING,
                                  valuecol="annual_mileage",
                                  datacols=c("sector", "subsector_L3", "subsector_L2", "subsector_L1","vehicle_type", "technology", "year"),
                                  weights=gdp)

    load_factor <- UCD_data$load_factor
    load_factor <- disaggregate_dt(load_factor, GCAM2ISO_MAPPING)


    capcost4W <- nec_cost_split[subsector_L1 == "trn_pass_road_LDV_4W" & price_component == "Capital_costs_purchase"][, c("sector", "subsector_L3", "subsector_L2", "subsector_L1") := NULL]


    UCD_results = list(nec_cost_split = nec_cost_split,
                       capcost4W = capcost4W,
                       annual_mileage = annual_mileage,
                       nec_cost = nec_cost,
                       load_factor = load_factor)

    result =list(vot = vot,
                 price_nonmot = price_nonmot,
                 UCD_results = UCD_results,
                 TO_iso = TO_iso,
                 int = int)

    return(result)
}
