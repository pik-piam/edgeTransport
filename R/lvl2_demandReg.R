#' Estimate future transport demand using a price trajectory, GDP, and historical demand.
#'
#'
#' @param tech_output historically calibrated demand
#' @param price_baseline baseline prices
#' @param REMIND_scenario SSP scenario
#' @param smartlifestyle switch activating sustainable lifestyles
#'
#' @return transport demand projections
#' @author Marianna Rottoli
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table shift frank


lvl2_demandReg <- function(tech_output, price_baseline, REMIND_scenario, smartlifestyle){
  rich <- var <- eps <- GDP_cap <- region <- eps1 <- eps2 <- GDP_val <- POP_val <- NULL
  index_GDP <- income_elasticity_freight_sm <- income_elasticity_freight_lo <- index_GDPcap <- NULL
  income_elasticity_pass_sm <- income_elasticity_pass_lo <- price_elasticity_pass_lo <- sector <- NULL
  index_price <- tot_price <- trn_freight <- price_elasticity_freight_sm <- trn_shipping_intl <- NULL
  price_elasticity_freight_lo <- trn_pass <- price_elasticity_pass_sm <- trn_aviation_intl <- `.` <- NULL
  index_price_f_sm <- index_price_f_lo <- index_GDP_f_sm <- index_GDPcap_p_lo <- index_GDP_f_lo <- NULL
  index_price_p_sm <- index_GDPcap_p_sm <- index_POP <- index_price_p_lo <- D_star_f_sm <- D_star_p_sm <- NULL
  D_star_p_lo <- D_star_f_lo <- D_star_f_sm <- value <- variable <- vrich <- vpoor <-NULL
  ## conversion rate 2005->1990 USD
  CONV_2005USD_1990USD = 0.67
  ## Create a dt with GDP, POP and GDP_cap with EDGE regions
  GDP_POP = getRMNDGDPcap(scenario = paste0("gdp_", REMIND_scenario), usecache = T, isocol = "region", to_aggregate = T, gdpCapfile = "GDPcapCache.rds")
  setnames(GDP_POP, old = "weight", new = "GDP_val")
  ## create ct with the various elasticities
  price_el = GDP_POP[,-"variable"]
  tmp = CJ(region=unique(price_el$region), var =c("income_elasticity_pass_sm",
                                            "price_elasticity_pass_sm",
                                            "income_elasticity_pass_lo",
                                            "price_elasticity_pass_lo",
                                            "income_elasticity_freight_sm",
                                            "price_elasticity_freight_sm",
                                            "income_elasticity_freight_lo",
                                            "price_elasticity_freight_lo"))
  ## define max and min values of the elasticities
  ## pass sm
  tmp[, vrich := ifelse(var == "income_elasticity_pass_sm", 0.25, NA)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_sm", -0.65, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_sm", 0.5, NA)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_sm", -1.25, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_sm", 0.8, NA)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_sm", -1, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_sm", 1, NA)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_sm", -0.625, norm)]
  ## pass lo (see The income elasticity of air travel a meta analysis, Gallet et al 2014)
  tmp[, vrich := ifelse(var == "income_elasticity_pass_lo", 1.5, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_lo", -0.25, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo", 1.5, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo", -0.5, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_lo", 1.5, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_lo", -0.7, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo", 1.5, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo", -1, norm)]
  ## freight sm
  tmp[, vrich := ifelse(var == "income_elasticity_freight_sm", 0.1875, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_freight_sm", -0.1875, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_freight_sm", 0.375, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_sm", -0.325, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_freight_sm", 0.6, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_freight_sm", -0.4, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_sm", 0.75, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_sm", -0.65, norm)]
  ## freight lo
  tmp[, vrich := ifelse(var == "income_elasticity_freight_lo", 0.1, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_freight_lo", -0.1625, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_freight_lo", 0.2, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_lo", -0.325, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_freight_lo", 0.3, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_freight_lo", -0.5, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_lo", 0.4, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_lo", -0.65, norm)]

  price_el = merge(price_el, tmp, by = "region", allow.cartesian = TRUE)
  price_el[, eps := ifelse(GDP_cap < 15000, vpoor, NA)]
  price_el[, eps := ifelse(GDP_cap >= 25000, rich, eps)]
  price_el[, eps := ifelse(GDP_cap > 25000 & GDP_cap < 30000, vrich, eps)]
  price_el[, eps := ifelse(GDP_cap < 25000 & GDP_cap >= 15000, norm, eps)]

  ## interpolate of gdpcap values
  price_el = approx_dt(dt = price_el,
                       xdata=unique(price_el$GDP_cap),
                       xcol = "GDP_cap",
                       ycol="eps",
                       idxcols="var",
                       extrapolate = TRUE)

  price_el[region =="REF" & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0]
  price_el[region %in% c("OAS", "MEA", "LAM") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.1]
  price_el[region %in% c("IND") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.5]
  price_el[region %in% c("SSA", "CHA") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.3]
  price_el[region %in% c("EUR", "NEU", "USA", "CAZ", "JPN") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.1]

  if (smartlifestyle) {
    price_el[region =="REF" & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0]
    price_el[region %in% c("OAS", "MEA", "LAM") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.1]
    price_el[region %in% c("IND") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.4]
    price_el[region %in% c("CHA") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.2]
    price_el[region %in% c("SSA") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.5]
    price_el[region %in% c("EUR", "NEU", "USA", "CAZ", "JPN") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0]

  }

  price_el[var %in% c("price_elasticity_freight_lo", "price_elasticity_freight_sm", "price_elasticity_pass_lo", "price_elasticity_pass_sm"), eps := 0]
  price_el = dcast(price_el[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")

  #calculate growth rates
  GDP_POP[,`:=`(index_GDP=GDP_val/shift(GDP_val), index_GDPcap=GDP_cap/shift(GDP_cap), index_POP=POP_val/shift(POP_val)), by=c("region")]
  ## merge GDP_POP and price elasticity
  GDP_POP = merge(GDP_POP, price_el[,c("region", "year", "income_elasticity_pass_lo", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("region", "year"))

  #calculate the indexes raised to the corresponding elasticities
  GDP_POP[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo=index_GDPcap^income_elasticity_pass_lo)]
  GDP_POP[,c("income_elasticity_freight_sm", "income_elasticity_freight_lo", "income_elasticity_pass_sm", "income_elasticity_pass_lo") := NULL]

  #order the prices according to the year, within the sector
  price_baseline=price_baseline[order(-frank(sector), year)]
  #calculate "index" which represent the growth of total price
  price_baseline[,index_price:=tot_price/shift(tot_price),by=c("region","sector")]
  #select only the needed columns
  price_baseline=price_baseline[, c("region","year","sector","index_price")]
  #from long to wide format, so that the df has separate columns for all transport modes
  price_baseline=dcast(price_baseline, region + year  ~ sector, value.var = "index_price", fun.aggregate = sum, margins="sector")
  ## merge with elasticities
  price_baseline = merge(price_baseline, price_el[,c("region", "year", "price_elasticity_pass_lo", "price_elasticity_pass_sm", "price_elasticity_freight_sm", "price_elasticity_freight_lo")], by = c("region", "year"))
  #calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_f_sm=trn_freight^price_elasticity_freight_sm,
                             index_price_f_lo=trn_shipping_intl^price_elasticity_freight_lo,
                             index_price_p_sm=trn_pass^price_elasticity_pass_sm,
                             index_price_p_lo=trn_aviation_intl^price_elasticity_pass_lo)]

  price_baseline[,c("price_elasticity_freight_sm", "price_elasticity_freight_lo", "price_elasticity_pass_sm", "price_elasticity_pass_lo") := NULL]

  #create the D* df
  D_star=merge(price_baseline,GDP_POP,by = c("region","year"))

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

  return(D_star)

  }
