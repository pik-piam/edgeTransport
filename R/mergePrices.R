#' Merge REMIND-derived fuel prices with non-fuel costs.
#'
#' @param gdx path to REMIND binary output, gdx format
#' @param REMINDmapping mapping of REMIND regions to ISO3 country codes
#' @param REMINDyears range of REMIND timesteps
#' @param intensity_data logit level intensity data
#' @param nonfuel_costs logit level non-fuel costs
#' @param module GDX input is based on old "complex" module or new "edge_esm" module
#' @param FE_Pricetab ship an external csv that includes FE prices. The prices from the gdx file will be overwritten for affected regions.
#' @import data.table
#' @importFrom rmndt disaggregate_dt magpie2dt
#' @importFrom gdx readGDX
#' @importFrom magclass time_interpolate lowpass dimSums mbind getYears
#' @importFrom magrittr %>%
#' @export

toolMergePrices <- function(gdx, REMINDmapping, REMINDyears,
                         intensity_data,
                         nonfuel_costs,
                         module="edge_esm", FE_Pricetab = NULL) {
  sector_fuel <- ttot <- value <- fuel_price <- fuel_price_pkm <- EJ_Mpkm_final <- non_fuel_price <-
    technology <- GDP_cap <- region <- `.` <- weight <- POP_val <- GDP <- `elect_td_trn` <- time <-
      `Liquids-Electricity` <- `refined liquids enduse` <- vehicle_type <- subsector_L3 <-
        subsector_L2 <- subsector_L1 <- sector <- tot_price <- value.x <- value.y <- NULL
    ## report prices from REMIND gdx in 2005$/GJ

    tdptwyr2dpgj <- 31.71  #TerraDollar per TWyear to Dollar per GJ
    ## load entries from the gdx, values below 2020 do not make sense
    pfe <- readGDX(gdx, "pm_FEPrice", format = "first_found", restore_zeros = FALSE)[,, "trans.ES", pmatch=TRUE]
    startyear <- getYears(pfe, as.integer=TRUE)[1]
    if(startyear < 2020){
        startyear <- 2020
    }
    years <- REMINDyears[REMINDyears >= startyear]
    ## smooth prices
    pfe <- pfe[, years] %>% lowpass() %>% magpie2dt()
    pfe[
      , year := as.numeric(ttot)][
      , value := value * tdptwyr2dpgj][ # 2005$ per GJ
      , c("sector", "emiMkt", "ttot") := NULL
    ]
    pfe <- approx_dt(pfe, c(1990, seq(2005, startyear - 5, 5), unique(pfe$year)),
                     xcol = "year", ycol = "value", idxcols = c("all_regi", "all_enty"), extrapolate = TRUE)
    setnames(pfe, "all_regi", "region")

    if (!is.null(FE_Pricetab)){
      FE_Pricetab <- approx_dt(FE_Pricetab, c(seq(1990, 2060, by = 5),
                                              seq(2070, 2110, by = 10),
                                              2130, 2150),
                xcol = "year", ycol = "value", idxcols = c("region", "all_enty"), extrapolate = TRUE)

      pfe <- merge(pfe, FE_Pricetab, by = c("region", "year", "all_enty"), all = TRUE)
      pfe[, value := ifelse(region %in% unique(FE_Pricetab$region), value.y, value.x)]

      pfe[, value.y := NULL][, value.x := NULL]
      print("FE prices from gdx file were partially overwritten by externally supplied FE prices")

    }


    varmap <- fread(text="
all_enty,sector_fuel
fedie,refined liquids enduse
fepet,refined liquids enduse
fegat,delivered gas
feelt,elect_td_trn
feh2t,H2 enduse
")
    pfe <- varmap[pfe, on="all_enty"][, .(value=mean(value)), by=c("region", "year", "sector_fuel")]

    test <- pfe[year > 2005 & value <= 1]

    if(nrow(test)){
        print(paste("Fuel prices lower than 1$/GJ found. Regions:", paste(unique(test$region), collapse = ", ")))
        print("The weighted averages of non-zero regions of the corresponding fuel will be used.")
        pfe[, value := ifelse(value <= 1, mean(value[value>1]), value), by = c("year", "sector_fuel")]
    }

    ## hybrids price from electricity and liquids
    pfe <- pfe %>%
      dcast(...~sector_fuel) %>%
      .[, `Liquids-Electricity` := 0.6*`refined liquids enduse` + 0.4 * `elect_td_trn`] %>%
      melt(id.vars=c("region", "year"), variable.name="sector_fuel")

    setnames(pfe, old = c("value"), new = c("fuel_price"))

    ## fuel price in 2005USD/GJ -> 2005USD/EJ
    pfe[, fuel_price := fuel_price * 1e9]

    ## join with vehicle intensity and load factor to get 2005USD/pkm
    fuel_price_REMIND <- merge(pfe, intensity_data, by = c("region",
        "year", "sector_fuel"), all.y = TRUE)

    ## fuel_price [$/EJ * EJ/Mpkm * Mpkm/pkm],
    tech_cost <- fuel_price_REMIND[, fuel_price_pkm := fuel_price * EJ_Mpkm_final * 1e-6]

    ## merge the non energy prices, they are $/pkm
    tech_cost <- merge(tech_cost, nonfuel_costs,
                        by = c("region", "year", "technology",
                               "vehicle_type", "subsector_L1",
                               "subsector_L2", "subsector_L3","sector"),
                        all.x = TRUE)

    ## calculate the total price
    tech_cost[, tot_price := fuel_price_pkm + non_fuel_price]

    return(tech_cost)

}
