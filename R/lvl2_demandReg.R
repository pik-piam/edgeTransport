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
  rich <- var <- eps <- GDP_cap <- iso <- eps1 <- eps2 <- GDP_val <- POP_val <- index_GDP <- income_elasticity_freight_sm <- income_elasticity_freight_lo <- index_GDPcap <- income_elasticity_pass_sm <- income_elasticity_pass_lo <- price_elasticity_pass_lo <- sector <- index_price <- tot_price <- trn_freight <- price_elasticity_freight_sm <- trn_shipping_intl <- price_elasticity_freight_lo <- trn_pass <- price_elasticity_pass_sm <- trn_aviation_intl <- `.` <- index_price_f_sm <- index_price_f_lo <- index_GDP_f_sm <- index_GDPcap_p_lo <- index_GDP_f_lo <- index_price_p_sm <- index_GDPcap_p_sm <- index_POP <- index_price_p_lo <- D_star_f_sm <- D_star_p_sm <- D_star_p_lo <- D_star_f_lo <- D_star_f_sm <- value <- variable <- NULL

  ## conversion rate 2005->1990 USD
  CONV_2005USD_1990USD = 0.67

  ## Create a dt with GDP, POP and GDP_cap with EDGE regions
  GDP_POP = getRMNDGDPcap(scenario = REMIND_scenario)
  setnames(GDP_POP, old = "weight", new = "GDP_val")
  ## create ct with the various elasticities
  price_el = GDP_POP[,-"variable"]
  tmp = CJ(iso=unique(price_el$iso), var =c("income_elasticity_pass_sm",
                                            "price_elasticity_pass_sm",
                                            "income_elasticity_pass_lo",
                                            "price_elasticity_pass_lo",
                                            "income_elasticity_freight_sm",
                                            "price_elasticity_freight_sm",
                                            "income_elasticity_freight_lo",
                                            "price_elasticity_freight_lo"))
  ## define max and min values of the elasticities
  ## pass sm
  tmp[, rich := ifelse(var == "income_elasticity_pass_sm", 0.2, NA)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_sm", -0.01, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_sm", 1, NA)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_sm", -1.25, norm)]
  ## pass lo
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo", 0.05, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo", -0.05, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo", 1, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo", -1, norm)]
  ## freight sm
  tmp[, rich := ifelse(var == "income_elasticity_freight_sm", 0.4, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_sm", -0.3, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_sm", 0.75, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_sm", -0.65, norm)]
  ## freight lo
  tmp[, rich := ifelse(var == "income_elasticity_freight_lo", 0.2, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_lo", -0.3, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_lo", 0.4, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_lo", -0.65, norm)]

  if (smartlifestyle) {
    tmp[grep("income", var), rich := 0.2*rich]
  }


  price_el = merge(price_el, tmp, by = "iso", allow.cartesian = TRUE)
  price_el[, eps := ifelse(GDP_cap >= floor(GDP_cap[iso=="JPN" & year == 2020]), rich, NA)]
  price_el[, eps := ifelse(GDP_cap == min(GDP_cap), norm, eps)]

  ## interpolate of gdpcap values
  price_el = approx_dt(dt = price_el,
                       xdata=unique(price_el$GDP_cap),
                       xcol = "GDP_cap",
                       ycol="eps",
                       idxcols="var",
                       extrapolate = TRUE)


  ## correct price elasticity for IND, otherwise the demand is misleadingly low
  ## https://ideas.repec.org/p/ekd/006356/7355.html
  price_el[, eps := ifelse(var == "price_elasticity_pass_sm" & iso == "IND", -0.35, eps)]

  ## correct elasticity for SSA, otherwise the demand does not grow enough as compared to other regions
  price_el[, eps := ifelse(var == "income_elasticity_pass_sm" & iso %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "DJI", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", "REU", "RWA", "SEN", "SHN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"), eps*1.1, eps)]

  ## CHN demand grows too quickly in the first time step: smoothen down the elasticity increase
  price_el[iso == "CHN" & var == "price_elasticity_pass_sm" & year == 2015, eps := 0.8*eps]

  price_el = dcast(price_el[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps")

  #calculate growth rates
  GDP_POP[,`:=`(index_GDP=GDP_val/shift(GDP_val), index_GDPcap=GDP_cap/shift(GDP_cap), index_POP=POP_val/shift(POP_val)), by=c("iso")]
  ## merge GDP_POP and price elasticity
  GDP_POP = merge(GDP_POP, price_el[,c("iso", "year", "income_elasticity_pass_lo", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("iso", "year"))

  #calculate the indexes raised to the corresponding elasticities
  GDP_POP[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo=index_GDPcap^income_elasticity_pass_lo)]
  GDP_POP[,c("income_elasticity_freight_sm", "income_elasticity_freight_lo", "income_elasticity_pass_sm", "income_elasticity_pass_lo") := NULL]

  #order the prices according to the year, within the sector
  price_baseline=price_baseline[order(-frank(sector), year)]
  #calculate "index" which represent the growth of total price
  price_baseline[,index_price:=tot_price/shift(tot_price),by=c("iso","sector")]
  #select only the needed columns
  price_baseline=price_baseline[, c("iso","year","sector","index_price")]
  #from long to wide format, so that the df has separate columns for all transport modes
  price_baseline=dcast(price_baseline, iso + year  ~ sector, value.var = "index_price", fun.aggregate = sum, margins="sector")
  ## merge with elasticities
  price_baseline = merge(price_baseline, price_el[,c("iso", "year", "price_elasticity_pass_lo", "price_elasticity_pass_sm", "price_elasticity_freight_sm", "price_elasticity_freight_lo")], by = c("iso", "year"))
  #calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_f_sm=trn_freight^price_elasticity_freight_sm,
                             index_price_f_lo=trn_shipping_intl^price_elasticity_freight_lo,
                             index_price_p_sm=trn_pass^price_elasticity_pass_sm,
                             index_price_p_lo=trn_aviation_intl^price_elasticity_pass_lo)]

  price_baseline[,c("price_elasticity_freight_sm", "price_elasticity_freight_lo", "price_elasticity_pass_sm", "price_elasticity_pass_lo") := NULL]

  #create the D* df
  D_star=merge(price_baseline,GDP_POP,by = c("iso","year"))

  #calculate D* for each mode separately, and select only the useful cols
  D_star=D_star[,.(D_star_f_sm=index_price_f_sm*index_GDP_f_sm,
               D_star_f_lo=index_price_f_lo*index_GDP_f_lo,
               D_star_p_sm=index_price_p_sm*index_GDPcap_p_sm*index_POP,
               D_star_p_lo=index_price_p_lo*index_GDPcap_p_lo*index_POP,
               iso,
               year)]
  #calculate demand at a sector level
  demand_tot_sector=tech_output[, .(demand_tot=sum(tech_output)), by=c("iso", "year", "sector")]
  #from long to wide format, so that the df has separate columns for all transport modes
  demand_tot_sector=dcast(demand_tot_sector, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")

  #merge D* and historical demand
  D_star=merge(D_star,demand_tot_sector, by = c("iso","year"),all.x = TRUE)

  #for loop that calculates the value of the following time step of demand based on the growth of the indexes
  i=NULL
  for (i in seq(1,length(unique(D_star$year)),1)) {
    D_star[year>=2010,tmp:=shift(trn_freight)*D_star_f_sm,by=c("iso")]
    D_star[is.na(trn_freight) & !is.na(tmp),trn_freight:=tmp]
    D_star[year>=2010,tmp:=shift(trn_pass)*D_star_p_sm,by=c("iso")]
    D_star[is.na(trn_pass) & !is.na(tmp),trn_pass:=tmp]
    D_star[year>=2010,tmp:=shift(trn_shipping_intl)*D_star_f_lo,by=c("iso")]
    D_star[is.na(trn_shipping_intl) & !is.na(tmp),trn_shipping_intl:=tmp]
    D_star[year>=2010,tmp:=shift(trn_aviation_intl)*D_star_p_lo,by=c("iso")]
    D_star[is.na(trn_aviation_intl) & !is.na(tmp),trn_aviation_intl:=tmp]

    i=i+1
  }

  ## select only the columns that contains the demand
  D_star[, c("tmp", "D_star_f_sm", "D_star_p_sm", "D_star_f_lo", "D_star_p_lo") := NULL]

  D_star = melt(D_star, id.vars = c("iso", "year"),
                  measure.vars = c("trn_aviation_intl", "trn_freight", "trn_pass", "trn_shipping_intl"))
  D_star = D_star[,.(iso, year, demand = value, sector = variable)]

  return(D_star)

  }
