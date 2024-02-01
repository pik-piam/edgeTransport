#' Estimate future transport demand using a price trajectory, GDP, and historical demand.
#'
#'
#' @param tech_output historically calibrated demand
#' @param price_baseline baseline prices
#' @param GDP_POP GDP per capita
#' @param ssp_factors table with SSP/SDP scenario factors to be added to the baseline elasticity trends
#' @param regional_factors table with SSP/SDP and regionally specific factors to be added to the baseline elasticity trends
#' @param demscen_factors table with reduction factors on the total demands,
#'   example: demscen_factors <-
#'   fread("demandScen,  region,sector,  year, factor
#'          SSP2EU_lowEn,DEU,   trn_pass,2030, 0.5")
#' @param SSP_scen REMIND SSP scenario
#' @importFrom rmndt approx_dt
#' @importFrom magrittr %>%
#' @return transport demand projections
#' @author Marianna Rottoli, Alois Dirnaichner
#'
#' @importFrom data.table shift frank
#' @export


toolDemandReg <- function(tech_output, price_baseline, GDP_POP,
                           SSP_scen, ssp_factors, regional_factors=NULL, demscen_factors=NULL) {
  rich <- var <- eps <- GDP_cap <- region <- eps1 <- eps2 <- GDP_val <- POP_val <- demand <-
    index_GDP <- income_elasticity_freight_sm <- income_elasticity_freight_lo <- index_GDPcap <-
      income_elasticity_pass_sm <- income_elasticity_pass_lo <- price_elasticity_pass_lo <- sector <-
        index_price <- tot_price <- trn_freight <- price_elasticity_freight_sm <- trn_shipping_intl <-
          price_elasticity_freight_lo <- trn_pass <- price_elasticity_pass_sm <- trn_aviation_intl <-
            `.` <- index_price_f_sm <- index_price_f_lo <- index_GDP_f_sm <- index_GDPcap_p_lo <-
              index_GDP_f_lo <- NULL
  index_price_p_sm <- index_GDPcap_p_sm <- index_POP <- index_price_p_lo <- D_star_f_sm <- D_star_p_sm <- NULL
  D_star_p_lo <- D_star_f_lo <- D_star_f_sm <- value <- variable <- vrich <- vpoor <-NULL
  SSP_factor <- SSP_scenario <- region_factor <- approxfun <- gdp_cap <- target <- tmp <- demand <- NULL

  ## Create a dt with GDP, POP and GDP_cap with EDGE regions
  gdp_pop = copy(GDP_POP)
  setnames(gdp_pop, old = "weight", new = "GDP_val")

  facts <- ssp_factors[SSP_scenario == SSP_scen][, SSP_scenario := NULL]

  income_el <- rbindlist(lapply(unique(facts$var), function(cat) {
    appfun <- approxfun(
      x=facts[var == cat, gdp_cap],
      y=facts[var == cat, target], rule = 2)
    copy(gdp_pop)[, `:=`(var=cat, eps=appfun(GDP_cap))]
  }))


  if(!is.null(regional_factors) && SSP_scen %in% unique(regional_factors$SSP_scenario)) {
    ## apply regional factors
    income_el <- regional_factors[SSP_scenario == SSP_scen][, SSP_scenario := NULL] %>%
      melt(id.vars = c("region", "var"), variable.name = "year", value.name = "region_factor") %>%
      .[, year := as.numeric(as.character(year))] %>%
      .[income_el, on=c("region", "year", "var")] %>%
      .[year <= 2010, region_factor := 0] %>%
      .[year >= 2010 & year <= 2100, region_factor := na.approx(region_factor, x=year),
        by=c("region", "var")] %>%
      .[year <= 2100 & is.na(region_factor), region_factor := 0]

    income_el[year > 2100, region_factor := income_el[year == 2100, region_factor], by="year"]

    income_el[, eps := eps + region_factor]
  }

  ## zero price elasticity as there were issues
  full_el <- rbind(
    income_el,
    rbindlist(lapply(c("price_elasticity_freight_lo",
                       "price_elasticity_freight_sm",
                       "price_elasticity_pass_lo",
                       "price_elasticity_pass_sm"),
                     function(cat){
                       income_el[var == "income_elasticity_pass_sm"][, `:=`(var=cat, eps=0)]
                     })))


  full_el = dcast(full_el[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")

  #calculate growth rates
  gdp_pop[,`:=`(index_GDP=GDP_val/shift(GDP_val), index_GDPcap=GDP_cap/shift(GDP_cap), index_POP=POP_val/shift(POP_val)), by=c("region")]
  ## merge GDP_POP and price elasticity
  gdp_pop = merge(gdp_pop, full_el[,c("region", "year", "income_elasticity_pass_lo", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("region", "year"))

  #calculate the indexes raised to the corresponding elasticities
  gdp_pop[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo=index_GDPcap^income_elasticity_pass_lo)]
  gdp_pop[,c("income_elasticity_freight_sm", "income_elasticity_freight_lo", "income_elasticity_pass_sm", "income_elasticity_pass_lo") := NULL]

  #order the prices according to the year, within the sector
  price_baseline=price_baseline[order(-frank(sector), year)]
  #calculate "index" which represent the growth of total price
  price_baseline[,index_price:=tot_price/shift(tot_price),by=c("region","sector")]
  #select only the needed columns
  price_baseline=price_baseline[, c("region","year","sector","index_price")]
  #from long to wide format, so that the df has separate columns for all transport modes
  price_baseline=dcast(price_baseline, region + year  ~ sector, value.var = "index_price", fun.aggregate = sum, margins="sector")
  ## merge with elasticities
  price_baseline = merge(price_baseline, full_el[,c("region", "year", "price_elasticity_pass_lo", "price_elasticity_pass_sm", "price_elasticity_freight_sm", "price_elasticity_freight_lo")], by = c("region", "year"))
  #calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_f_sm=trn_freight^price_elasticity_freight_sm,
                             index_price_f_lo=trn_shipping_intl^price_elasticity_freight_lo,
                             index_price_p_sm=trn_pass^price_elasticity_pass_sm,
                             index_price_p_lo=trn_aviation_intl^price_elasticity_pass_lo)]

  price_baseline[,c("price_elasticity_freight_sm", "price_elasticity_freight_lo", "price_elasticity_pass_sm", "price_elasticity_pass_lo") := NULL]

  #create the D* df
  D_star=merge(price_baseline, gdp_pop, by = c("region","year"))

  #calculate D* for each mode separately, and select only the useful cols
  D_star=D_star[,.(D_star_f_sm=index_price_f_sm*index_GDP_f_sm,
               D_star_f_lo=index_price_f_lo*index_GDP_f_lo,
               D_star_p_sm=index_price_p_sm*index_GDPcap_p_sm*index_POP,
               D_star_p_lo=index_price_p_lo*index_GDPcap_p_lo*index_POP,
               region,
               year)]
  #calculate demand at a sector level
  demand_tot_sector=tech_output[, .(demand_tot=sum(tech_output)), by=c("region", "year", "sector")]
  #from long to wide format, so that the df has separate columns for all transport modes
  demand_tot_sector=dcast(demand_tot_sector, region + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")

  #merge D* and historical demand
  D_star=merge(D_star,demand_tot_sector, by = c("region","year"),all.x = TRUE)

  #for loop that calculates the value of the following time step of demand based on the growth of the indexes
  i=NULL
  for (i in seq(1,length(unique(D_star$year)),1)) {
    D_star[year>=2010,tmp:=shift(trn_freight)*D_star_f_sm,by=c("region")]
    D_star[is.na(trn_freight) & !is.na(tmp),trn_freight:=tmp]
    D_star[year>=2010,tmp:=shift(trn_pass)*D_star_p_sm,by=c("region")]
    D_star[is.na(trn_pass) & !is.na(tmp),trn_pass:=tmp]
    D_star[year>=2010,tmp:=shift(trn_shipping_intl)*D_star_f_lo,by=c("region")]
    D_star[is.na(trn_shipping_intl) & !is.na(tmp),trn_shipping_intl:=tmp]
    D_star[year>=2010,tmp:=shift(trn_aviation_intl)*D_star_p_lo,by=c("region")]
    D_star[is.na(trn_aviation_intl) & !is.na(tmp),trn_aviation_intl:=tmp]
    i=i+1
  }

  ## select only the columns that contains the demand
  D_star[, c("tmp", "D_star_f_sm", "D_star_p_sm", "D_star_f_lo", "D_star_p_lo") := NULL]

  D_star = melt(D_star, id.vars = c("region", "year"),
                  measure.vars = c("trn_aviation_intl", "trn_freight", "trn_pass", "trn_shipping_intl"))
  D_star = D_star[,.(region, year, demand = value, sector = variable)]

  #Apply factors for specific demand scenario on output of demand regression if given/otherwise use default values from demand regression
  #Application: linear regression to given support points for the factors starting from 2020, constant factors after support points
  D_star[, factor := demscen_factors[.SD, factor, on=c("region", "sector", "year")]]
  mods <- D_star[!is.na(factor)]
    if (nrow(mods) > 0){
      print(paste0("You selected the ", unique(demscen_factors$demandScen), " demand scenario"))
      D_star[year <= 2020, factor := 1]
      D_star[, factor := na.approx(factor, x=year, rule=2), by=c("region", "sector")]
      D_star[, demand := factor * demand]
    } else {
      print(paste0("You selected the default demand scenario"))
    }
  D_star[, "factor" := NULL]



  return(D_star)

  }
