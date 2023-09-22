#' Calculate vintages composition, costs and intensity
#'
#' @param shares logit tree shares
#' @param totdem_regr total demand as obtained from the regression
#' @param prices costs of transport options in the logit tree
#' @param mj_km_data energy intensity of transport options in the logit tree
#' @param years range of years for the vintage calculation
#' @import rmndt
#' @export


toolCalcVint <- function(shares, totdem_regr, prices, mj_km_data, years){

  `.` <- C_2010x <- Ddt_2010_new <- share <- region <- subsector_L3 <- sector <- NULL
  subsector_L2 <- subsector_L1 <- vehicle_type <- technology <- shareS3S <- shareS2S3 <- NULL
  shareS1S2 <- totdem <- demand <- D <- index_yearly <- k <- vint <- C_2010 <- check <- C_t <- decrease <- NULL
  index <- value <- shareVS1 <- shareFV <- shareFVVS1 <- variable <- demVintEachYear <- NULL
  vintdem <- sharetech <- sharetech_vint <- sharetech_new <- relative_share <- NULL
  non_fuel_price <- non_fuel_price_vint <- tot_price <- fuel_price_pkm <- MJ_km <- MJ_km_vint <- NULL
  lifetime <- years_origin <-NULL
  ## vintages all equal time steps, equal to 1 year
  tall = seq(2010, 2100,1)
  ## last historical year
  baseyear = 2010

  ## rename column in each share data table
  S3S = shares$S3S_shares[,.(shareS3S = share, region, year, subsector_L3, sector)]
  S2S3 = shares$S2S3_shares[,.(shareS2S3 = share, region, year, subsector_L2, subsector_L3)]
  S1S2 = shares$S1S2_shares[,.(shareS1S2 = share, region, year, subsector_L1, subsector_L2)]
  VS1 = shares$VS1_shares[,.(shareVS1 = share, region, year, vehicle_type, subsector_L1)]
  FV = shares$FV_shares[,.(shareFV = share, region, year, technology, vehicle_type, subsector_L1)]
  ## calculate the share of the vt (vintaging technologies) on the total sector demand
  shares_vt = merge(S3S, S2S3, by = c("region", "year", "subsector_L3"))
  shares_vt[, share := shareS3S*shareS2S3]
  shares_vt = merge(shares_vt, S1S2, by = c("region", "year", "subsector_L2"))
  shares_vt[, share := share*shareS1S2]
  shares_vt = shares_vt[subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1"), c("region", "year", "subsector_L1", "share", "sector")]
  ## find total demand LDV
  dem = totdem_regr[sector %in% c("trn_pass", "trn_freight")]
  dem = merge(dem, shares_vt, by = c("region", "year", "sector"))
  dem[, totdem := demand*share][, c("demand", "share") := NULL]
  dem = approx_dt(dt = dem, xdata = tall,                       ## extrapolate to the whole time frame
                      xcol="year", ycol="totdem",
                      idxcols = c("region", "subsector_L1", "sector"),
                      extrapolate=T)

  paux = data.table(subsector_L1 = c("trn_pass_road_LDV_4W",
                                     "Bus_tmp_subsector_L1",
                                     "trn_freight_road_tmp_subsector_L1"),
                    lifetime = c(15, 7, 7))   ## approximate lifetime of each vehicle
  Ddt = data.table(index_yearly = seq(1,length(tall)-1,1))
  Ddt = CJ(index_yearly= Ddt$index_yearly, subsector_L1 = unique(paux$subsector_L1))
  Ddt = merge(Ddt, paux, by = "subsector_L1")

  Ddt[, D := 1-((index_yearly-0.5)/lifetime)^4, by = "subsector_L1"]
  Ddt[, D := ifelse(D<0,0,D)]
  Ddt[, lifetime := NULL]
  ## first time step has no depreciation
  Ddt = rbind(data.table(index_yearly = 0, D = 1, subsector_L1 = unique(paux$subsector_L1)), Ddt)

  ## composition of 2010 is from equal contributions from the 14 years before, +1 (2010 itself)
  sum_dep = Ddt[, sum_dep := sum(D), by = subsector_L1] ## each year has same weight (new additions each year are constant)
  sum_dep = unique(sum_dep[,c("subsector_L1", "sum_dep")])
  Cap_eachyear = dem[year == 2010][, year:=NULL]
  Cap_eachyear = merge(sum_dep, Cap_eachyear, by = "subsector_L1")
  Cap_eachyear[, totdem := totdem/sum_dep]
  Cap_eachyear[, sum_dep:=NULL]
  lf_LDV = 15
  lf_bus = 7
  lf_truck = 7

  years_past = data.table(subsector_L1 = c(rep("trn_pass_road_LDV_4W", lf_LDV +1),
                                        rep("Bus_tmp_subsector_L1", lf_bus +1),
                                        rep("trn_freight_road_tmp_subsector_L1", lf_truck + 1)),
                          index_yearly = c(seq(0,lf_LDV,1), seq(0,lf_bus,1), seq(0,lf_truck,1)))
  ## all the yearly additions (from the year before the lifetime, e.g. 1995 if 15 years lf) depreciate at the same pace, each starting at its original year
  Cap_eachyear = merge(Cap_eachyear, years_past, by = "subsector_L1", allow.cartesian=TRUE)
  Cap_2010 = merge(Cap_eachyear, Ddt, all.x=TRUE, by = c("subsector_L1", "index_yearly"))
  Cap_2010[, totdem := totdem*D]
  ## attribute an year of origin to all the initial capacities
  years_originLDV = CJ(years_origin=seq(2010-paux[subsector_L1=="trn_pass_road_LDV_4W", lifetime],2010,1),index_yearly = c(seq(0,lf_LDV,1))) ##oldest non 0 value is a lf before 2010, since it all previous vehicles depreciated to 0
  years_originBus = CJ(years_origin=seq(2010-paux[subsector_L1=="Bus_tmp_subsector_L1", lifetime],2010,1),index_yearly = c(seq(0,lf_bus,1))) ##oldest non 0 value is a lf before 2010, since it all previous vehicles depreciated to 0
  years_originTruck = CJ(years_origin=seq(2010-paux[subsector_L1=="trn_freight_road_tmp_subsector_L1", lifetime],2010,1),index_yearly = c(seq(0,lf_truck,1))) ##oldest non 0 value is a lf before 2010, since it all previous vehicles depreciated to 0

  Cap_2010_LDV = merge(Cap_2010[subsector_L1=="trn_pass_road_LDV_4W"], years_originLDV, by = c("index_yearly"), allow.cartesian=T)
  Cap_2010_Bus = merge(Cap_2010[subsector_L1=="Bus_tmp_subsector_L1"], years_originBus, by = c("index_yearly"), allow.cartesian=T)
  Cap_2010_Truck = merge(Cap_2010[subsector_L1=="trn_freight_road_tmp_subsector_L1"], years_originTruck, by = c("index_yearly"), allow.cartesian=T)
  Cap_2010=rbind(Cap_2010_LDV,Cap_2010_Bus,Cap_2010_Truck)

  ## what survives in every year depends on when the sales were done and the years the car is old
  Cap_2010[, year := years_origin + index_yearly]
  Cap_2010 = Cap_2010[year>=2010, .(C_2010=sum(totdem)), by = c("region","year","subsector_L1", "sector")]
  Cap_2010 = rbind(Cap_2010,
                   Cap_2010[year>=(2010+lf_bus+1)&year<=(2010+lf_LDV) & subsector_L1=="trn_pass_road_LDV_4W"][, c("subsector_L1", "C_2010"):=list("Bus_tmp_subsector_L1", 0)],
                   Cap_2010[year>=(2010+lf_truck+1)&year<=(2010+lf_LDV) & subsector_L1=="trn_pass_road_LDV_4W"][, c("subsector_L1", "sector", "C_2010"):=list("trn_freight_road_tmp_subsector_L1", "trn_freight", 0)])
  ## after the last year when 2010 sales disappear from the fleet, all values should be 0
  tmp = CJ(
    year = seq(2010 + lf_LDV + 1,2100,1),
    region = unique(Cap_2010$region),
    subsector_L1 = unique(Cap_2010$subsector_L1),
    sector = unique(Cap_2010$sector),
    C_2010 = 0)

  tmp = tmp[subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1") & sector == "trn_pass" | subsector_L1 == "trn_freight_road_tmp_subsector_L1" & sector == "trn_freight"]

  Cap_2010 = rbind(Cap_2010, tmp)

  Vint = Cap_2010[year > baseyear]

  Vint[year == tall[tall>baseyear][1], vint := C_2010]

  ## five years time steps: only what happens every 5 years matters
  setnames(Ddt, old = "index_yearly", new = "index")

  ## create tmp structure that is used in the for loop
  tmp = CJ(year = tall, region = unique(Cap_2010$region), subsector_L1 = unique(Cap_2010$subsector_L1), sector = unique(Cap_2010$sector))
  tmp = tmp[subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1") & sector == "trn_pass" | subsector_L1 == "trn_freight_road_tmp_subsector_L1" & sector == "trn_freight"]

  for (i in seq(1,length(tall)-1,1)) {
    ## time step that is considered in the current iteration
    y = tall[tall>baseyear][i]
    ## starting value of capacity built up in the current year
    Cval_t = merge(dem[year == y, c("totdem", "region", "subsector_L1", "sector")], Vint[year == y, c("vint", "region", "subsector_L1", "sector")],
                   by = c("region", "subsector_L1", "sector"))
    ## distinguish between the standard vintaging and the early retirement case (when the vintages are higher than the new additions)
    Cval_t = Cval_t[, check := ifelse(totdem-vint>0, "standardVintaging", "earlyRet"), by = c("region", "subsector_L1", "sector")]
    Cval_t[check =="standardVintaging", C_t := totdem-vint]         ## for the standard vintaging, the new additions are total-vintages
    ## early vintages assumes 10% new additions as first step (90%vintages)
    perc=0.1
    Cval_t[check =="earlyRet", C_t := perc*totdem, by = c("region", "subsector_L1", "sector")]            ## for the early retirement cases, half of the demand goes to new additions
    Cval_t[check =="earlyRet", decrease := (1-perc)*totdem/vint]         ## this is how much we need the vintages to contract
    ## extract only early retired entries
    earlyret = Cval_t[check == "earlyRet"][,c("region", "subsector_L1", "sector", "check", "decrease")]

    Cval_t = Cval_t[,.(C_t, region, subsector_L1, sector)]
    Cap_t = merge(Cval_t, tmp, by = c("region", "subsector_L1", "sector"))
    ## merge with depreciation (the 1st year is the current one, so there is no depreciation)
    Ddt2merge = Ddt[index %in% c(seq(0,length(Ddt[subsector_L1=="trn_pass_road_LDV_4W",index])-i-1,1))]
    val_tmp = cbind(index=unique(Ddt2merge$index), year =  tall[tall>=y])
    Ddt2merge = merge(Ddt2merge, val_tmp, by = "index")
    Cap_t = merge(Cap_t, Ddt2merge, by = c("year", "subsector_L1"), allow.cartesian=TRUE) ## smaller than length(index)-i-1
    ## depreciated capacity
    Cap_t = Cap_t[, C_t := C_t*D]
    ## only relevant columns
    Cap_t = Cap_t[,c("year", "C_t", "region", "subsector_L1", "sector")]
    ## applied the year name to the value column
    setnames(Cap_t, old = "C_t", new = paste0("C_", y))
    ## add the vintaging capacity to the vintage dt
    Vint = merge(Vint, Cap_t[year > y], by = c("region","subsector_L1", "year", "sector"), all = TRUE)
    ## decrease the vintages if needed (from the current time step on)
    Vint = merge(Vint, earlyret, all = TRUE, by = c("region", "subsector_L1", "sector"))
    Vint[, decrease := ifelse(is.na(decrease), 1, decrease)]  ## attribute the decrease to 1 if there is no decrease due to early retirement
    Vint[year < y, decrease := 1]                             ## for the years of the past, vintages should be left untouched
    Vint[year == y, vint := vint*decrease]                    ## aggregated vintages is decreased for the current time step
    ## only the colums of the old capacities need to be decreased (all but the current year)
    n = grep("C_", colnames(Vint))
    n = n[n!=grep(paste0(y), colnames(Vint))]
    listCol <- colnames(Vint)[n]
    ## apply the decrease to all the columns
    for (col in listCol)
      Vint[, (col) := Vint[[col]]*decrease]

    Vint[,c("check", "decrease") := NULL]
    ## find all column names of the capacities
    listCol <- colnames(Vint)[grep("C_", colnames(Vint))]
    ## sum up all depreciating capacity for the current time step
    Vint = Vint[year == tall[tall>baseyear][i+1], vint := Reduce(`+`, .SD), .SDcols=c(listCol), by = c("region", "subsector_L1", "sector")]

  }

  ## melt according to the columns of the "starting" year
  listCol <- colnames(Vint)[grep("C_", colnames(Vint))]
  Vint = melt(Vint, id.vars = c("year", "vint", "region", "subsector_L1", "sector"), measure.vars = listCol)
  ## remove all columns with NAs (e.g. what is build in 2030, has NA vintage in 2020)
  Vint = Vint[!is.na(value)]
  Vint$variable <- factor(Vint$variable, levels = sort(listCol, decreasing =TRUE))


  ## composition of the vintages is inherited from the logit (depending on the base year): find the share of each tech-veh with respect to the starting total demand of passenger/freight transport
  shares_tech = merge(VS1[subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1")], FV[subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1")], by = c("region", "year", "vehicle_type", "subsector_L1"), all.y =TRUE)
  shares_tech[, share := shareVS1*shareFV]

  shares_tech = approx_dt(dt = shares_tech, xdata = tall,
                          xcol = "year", ycol = "share",
                          idxcols = c("region",  "subsector_L1", "vehicle_type", "technology"),
                          extrapolate=T)
  setnames(shares_tech, old = "share", new = "shareFVVS1")
  shares_tech = shares_tech[, .(region, year, subsector_L1, vehicle_type, technology, shareFVVS1)]
  shares_tech[, variable := paste0("C_", year)] ## attribute to the column variable the year in wich the logit based value starts
  shares_tech = shares_tech[, year := NULL] ## the composition is interesting only concerning the starting year
  ## find vintages composition
  vintcomp_startyear = merge(shares_tech, Vint, by = c("region", "variable", "subsector_L1"), allow.cartesian =TRUE)
  vintcomp_startyear[, demVintEachYear := shareFVVS1*value]
  vintcomp_startyear[, shareFVVS1:= NULL]

  ## vintages represent a certain share of the total demand
  sharevint = Vint[year %in% tall][,.(vintdem = mean(vint)), by = c("region", "year", "subsector_L1","sector")]
  sharevint= merge(dem, sharevint, by = c("region", "year", "subsector_L1", "sector"))
  sharevint[, sharevint := vintdem/totdem]


  ## I don't care anymore when something was built, so I aggregate by vintaging starting year, and then find the share of every technology within every vehicle type
  vintcomp = vintcomp_startyear[,.(value = sum(demVintEachYear)), by = c("region", "year", "sector", "subsector_L1", "vehicle_type", "technology")]
  vintcomp[, sharetech:= value/sum(value),c("region", "year", "subsector_L1", "sector", "vehicle_type") ]
  vintcomp = merge(vintcomp, sharevint, by = c("region", "sector", "subsector_L1", "year"))
  vintcomp[, sharetech_vint := sharevint*sharetech] ## relative weight of the vintage structure is given by the shares multiplied times the share of vintages on total demand of each subsector
  vintcomp = vintcomp[,.(sharetech_vint, totdem, region, sector, subsector_L1, vehicle_type, technology, year)]

  ## composition of the new additions is given by the same shares used for the vintages composition, but not delayed in time
  newcomp = merge(FV, sharevint, by = c("region", "subsector_L1", "year"))
  newcomp[, sharetech_new := (1-sharevint)*shareFV]

  ## find the average composition of the new additions+vintages (vt->vintaging_techs)
  FV_vt = merge(vintcomp, newcomp, by = c("region", "subsector_L1", "year", "totdem", "vehicle_type", "technology", "sector"))
  FV_vt = FV_vt[,.(shareFV = sum(sharetech_vint, sharetech_new)), by = c("region", "year", "subsector_L1", "vehicle_type", "technology")]
  ## extend to 2110 2130 2150 using 2100 value
  FV_vt = rbind(FV_vt, FV_vt[year == 2100][, year := 2110], FV_vt[year == 2100][, year := 2130], FV_vt[year == 2100][, year := 2150])


  ## updated values of FV_shares
  shares$FV_shares = merge(FV[(!subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1"))| (subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1") & year %in% c(1990, 2005,2010))],
                           FV_vt[year %in% years[years>baseyear]], by = names(FV_vt), all = TRUE)
  setnames(shares$FV_shares, old = "shareFV", new = "share")

  ## calculate the non fuel price
  price_vt = prices$base[ subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1"),]
  price_vt[, variable := paste0("C_", year)] ## attribute to the column variable the year in wich the logit based value starts
  price_vt_techtmp = copy(price_vt) ## create a temporary copy, it is used not to delete the year column in the original dt
  price_vt_techtmp = price_vt_techtmp[, c("year", "tot_VOT_price", "tot_price") := NULL]

  ## calculate costs of the average vintage category
  vintcost = merge(price_vt_techtmp,      ## costs of the technology depending on the original year
                   vintcomp_startyear,   ## vintages composition by starting year
                   by = c("region", "variable", "sector", "subsector_L1", "technology", "vehicle_type"), allow.cartesian =TRUE)

  ## calculate relative share of each "starting year" with respect of the current year
  vintcost[, relative_share := demVintEachYear/sum(demVintEachYear), by = c("region", "year","technology", "vehicle_type", "subsector_L1")]
  ## only entries that are "really" in the mix have to be averaged
  vintcost = vintcost[!is.nan(relative_share)]
  ## aggregate non fuel price of the vintages fleet
  vintcost = vintcost[,.(non_fuel_price_vint = sum(non_fuel_price*relative_share)), by = c("region", "year","technology", "vehicle_type",
                                                                                           "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel")]
  ## tot cost of LDVs vehicle types and technologies is given by the average between the vintages and the new addtions
  totcost = merge(vintcost, price_vt, by = c("region", "year","technology", "vehicle_type",
                                            "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel"), all = TRUE)
  totcost = merge(totcost, sharevint, all = TRUE, by = c("region", "year", "subsector_L1", "sector"))
  ## entries that have no vintages are for sake of simiplicity as expensive as new additions (to get the weighted average right)
  totcost[, non_fuel_price_vint := ifelse(is.na(non_fuel_price_vint), non_fuel_price, non_fuel_price_vint)]
  ## weighted average to find the total cost of the LDV fleet
  totcost = totcost[, totcost := sum(sharevint*non_fuel_price_vint +(1-sharevint)*non_fuel_price), by = c("region", "year", "subsector_L1", "technology", "vehicle_type", "sector")]
  ## calculate new tot cost as sum of fuel cost and non fuel cost
  totcost[, tot_price := fuel_price_pkm + non_fuel_price]
  ## extend to 2110 2130 2150 using 2100 value
  totcost = rbind(totcost, totcost[year == 2100][, year := 2110], totcost[year == 2100][, year := 2130], totcost[year == 2100][, year := 2150])
  ## updated values of FV_shares
  prices$base = merge(prices$base[(!subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1"))| (subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1") & year %in% c(1990, 2005,2010))],
                totcost[year %in% years[years>baseyear], c("region", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "non_fuel_price", "tot_price", "fuel_price_pkm",  "tot_VOT_price", "sector_fuel")], by = c("region", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "non_fuel_price", "tot_price", "fuel_price_pkm",  "tot_VOT_price", "sector_fuel"), all = TRUE)

  ## calculate the average intensity of the fleet
  mj_km_data_vt = mj_km_data[ subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1"),]
  mj_km_data_vt[, variable := paste0("C_", year)] ## attribute to the column variable the year in wich the logit based value starts
  mj_km_data_vt_techtmp = copy(mj_km_data_vt) ## create a temporary copy, it is used not to delete the year column in the original dt
  mj_km_data_vt_techtmp = mj_km_data_vt_techtmp[, c("year") := NULL]

  ## calculate intensities of the average vintage category
  vintint = merge(mj_km_data_vt_techtmp,      ## intensity of the technology depending on the original year
                   vintcomp_startyear,        ## vintages composition by starting year
                   by = c("region", "variable", "sector", "subsector_L1", "technology", "vehicle_type"), allow.cartesian =TRUE)

  ## calculate relative share of each "starting year" with respect of the current year
  vintint[, relative_share := demVintEachYear/sum(demVintEachYear), by = c("region", "year","technology", "vehicle_type", "subsector_L1")]
  ## only entries that are "really" in the mix have to be averaged
  vintint = vintint[!is.nan(relative_share)]
  ## aggregate non fuel price of the vintages fleet
  vintint = vintint[,.(MJ_km_vint = sum(MJ_km*relative_share)), by = c("region", "year","technology", "vehicle_type",
                                                                       "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel")]
  ## tot intensity of LDVs vehicle types and technologies is given by the average between the vintages and the new addtions
  totint = merge(vintint, mj_km_data_vt, by = c("region", "year","technology", "vehicle_type",
                                            "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel"), all = TRUE)
  totint = merge(totint, sharevint, all = TRUE, by = c("region", "year", "subsector_L1", "sector"))
  ## entries that have no vintages are for sake of simiplicity as intensive as new additions (to get the weighted average right)
  totint[, MJ_km_vint := ifelse(is.na(MJ_km_vint), MJ_km, MJ_km_vint)]
  ## entries that have no vintages are for sake of simiplicity with 0 vintage share (to get the weighted average right)
  totint[, sharevint := ifelse(year <=2005 & is.na(sharevint), 0, sharevint)]
  ## weighted average to find the total cost of the LDV fleet
  totint = totint[, totint := sum(sharevint*MJ_km_vint +(1-sharevint)*MJ_km), by = c("region", "year", "subsector_L1", "technology", "vehicle_type", "sector")]
  ## extend to 2110 2130 2150 using 2100 value
  totint = rbind(totint, totint[year == 2100][, year := 2110], totint[year == 2100][, year := 2130], totint[year == 2100][, year := 2150])
  ## updated values of FV_shares
  mj_km_data = merge(mj_km_data[(!subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1"))| (subsector_L1 %in% c("trn_pass_road_LDV_4W", "Bus_tmp_subsector_L1", "trn_freight_road_tmp_subsector_L1") & year %in% c(1990, 2005,2010))],
                      totint[year %in% years[years>baseyear], c("region", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel", "MJ_km")], by = names(mj_km_data), all = TRUE)

  return(list(prices = prices,
              shares = shares,
              mj_km_data = mj_km_data,
              vintcomp = vintcomp[year %in% years],
              newcomp = newcomp[year %in% years],
              vintcomp_startyear = vintcomp_startyear))

}
