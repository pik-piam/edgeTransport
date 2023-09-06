#' Derive shares, demand and intensities from the logit tree.
#'
#' @param logit_shares logit tree level shares
#' @param MJ_km_base logit tree level intensities
#' @param REMINDyears range of REMIND time steps
#' @param scenario REMIND GDP scenario
#' @param demand_input full REMIND CES level ES demand, optional. If not given, logit level demand output is normalized.
#' @import data.table
#' @importFrom rmndt aggregate_dt approx_dt
#' @importFrom stats complete.cases
#' @export

toolSharesIntensityDemand <- function(logit_shares,
                                      MJ_km_base,
                                      REMINDyears,
                                      scenario,
                                      demand_input=NULL) {

    ## variable masks for code checking facility
    `.` <- share <- region <- sector <- subsectorL1 <- subsectorL2 <- subsectorL3 <- NULL
    demand_L2 <- demand_L1 <- demand_L3 <- vehicleType <- demand_V <- demand_EJ <- demand_F <- NULL
    technology <- MJ_km <- demand_EJel <- demand_EJliq <- MJ_kmel <- MJ_kmliq <- NULL
    variable <- CES_node <- Value_demand <- value <- CES_parent <- fuel <- NULL
    ## load the shares at each level
    S1S_shares <- logit_shares[["S1S_shares"]]
    S2S1_shares <- logit_shares[["S2S1_shares"]]
    S3S2_shares <- logit_shares[["S3S2_shares"]]
    VS3_shares <- logit_shares[["VS3_shares"]]
    FV_shares <- logit_shares[["FV_shares"]]

    ## create a normalized total demand OR loads absolute demand if given
    if (is.null(demand_input)) {
        demand=CJ(region=unique(S1S_shares$region),
                  sector=unique(S1S_shares$sector),
                  year=unique(S1S_shares$year))
        demand[,demand:=1]
    }else {
        demand=demand_input
    }
    ## calculate demand in million pkm for each level
    #S->S3
    demand = merge(demand, S1S_shares, all.y = TRUE, by = c("region", "year", "sector"))
    demand = demand[,.(demand_L3 = demand*share, region, year, sector, subsectorL1)]
    #S3->S2
    demand = merge(demand, S2S1_shares, all=TRUE, by = c("region", "year", "sector", "subsectorL1"))
    demand = demand[,.(demand_L2 = demand_L3*share, region, sector, year, subsectorL1, subsectorL2)]
    #S2->S1
    demand = merge(demand, S3S2_shares, all=TRUE, by = c("region", "year", "sector", "subsectorL1", "subsectorL2"))
    demand = demand[,.(demand_L1 = demand_L2*share, region, sector, year, subsectorL1, subsectorL2, subsectorL3)]
    #S1->V
    demand = merge(demand, VS3_shares, all=TRUE, by = c("region", "year", "subsectorL3"))
    demand = demand[,.(demand_V = demand_L1*share, region, sector, year, subsectorL1, subsectorL2, subsectorL3, vehicleType)]
    #V->F
    demand = merge(demand, FV_shares, all=TRUE, by = c("region", "year", "subsectorL3", "vehicleType"))
    demand = demand[,.(demand_F = demand_V*share, region, sector, year, subsectorL1, subsectorL2, subsectorL3, vehicleType, technology)]


    ## put aside the non motorized modes
    demandNM = demand[subsectorL1 %in% c("Cycle", "Walk")]

    ## treat hybrid electric vehicles. These vehicles are listed in ES demand tables but not in FE
    ## the reason is that we set technology = fuel, which can be used for all technologies except
    ## hybrid electric cars
    ## we therefore add a column `fuel` to the intensity tables
    tech2fuel <- data.table(
      technology = c("BEV", "FCEV", "Hybrid Electric", "Electric"),
      fuel       = c("Electricity", "Hydrogen", "Liquids", "Electricity"))

    demshare_liq <- 0.6

    MJ_km_base <- tech2fuel[MJ_km_base, on = "technology"]
    MJ_km_base[is.na(fuel), fuel := technology]
    MJ_km_base <- rbind(
        MJ_km_base, MJ_km_base[technology == "Hybrid Electric"][
                   , `:=`(fuel="Electricity", MJ_km=MJ_km*(1-demshare_liq))])
    MJ_km_base[technology == "Hybrid Electric" & fuel == "Liquids",
            `:=`(MJ_km = MJ_km*demshare_liq)]

    ## Calculate demand in EJ
    ## merge the demand in pkm with the energy intensity
    demandF = merge(demand, MJ_km_base, all=FALSE, by = c("region", "sector", "year", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology"))

    ## for the demand output we add non-motorized modes
    demandF_plot_pkm <- copy(unique(rbind(
        demandF, demandNM, use.names=T, fill=T)[, c("demand_F", "year","region", "sector",
                                                    "subsectorL1", "subsectorL2","subsectorL3",
                                                    "vehicleType", "technology")]))
    demandF_plot_mjkm <- copy(demandF[, c("MJ_km", "year","region", "sector",
                                    "subsectorL1", "subsectorL2","subsectorL3",
                                    "vehicleType", "technology", "fuel")])

    demandF[, demand_EJ:=demand_F # in Mpkm or Mtkm
            * 1e6 # in pkm or tkm
            * MJ_km # in MJ
            * 1e-12 # in EJ
            ]

    demandF_plot_EJ <- copy(demandF[, c("demand_EJ", "year","region", "sector",
                                        "subsectorL1", "subsectorL2","subsectorL3",
                                        "vehicleType", "technology", "fuel")])

    EDGE2CESmap <- fread(system.file("extdata", "mapping_EDGECES.csv", package = "edgeTransport"))
    ## first I need to merge with a mapping that represents how the entries match to the CES
    demandF <- merge(demandF, EDGE2CESmap, all=TRUE,
                    by = c("sector", "subsectorL2", "fuel"))
    ## calculate both shares and average energy intensity
    demandF = demandF[,.(region, year, Value_demand = demand_EJ, demand_F, CES_node, sector)]

    demandF = demandF[,.(Value_demand = sum(Value_demand),
                       Value_intensity = sum(demand_F)/sum(Value_demand)), #in million pkm/EJ
                    by=c("region","year","CES_node","sector")]

    ## from wide to long format
    demandF = melt(demandF, id.vars = c("region","year","CES_node","sector"),
                   measure.vars = c("Value_demand", "Value_intensity"))

    ## get rid on NaNs energy intensity (they appear wherever demand is 0, so they are not useful)
    demandF = demandF[!is.nan(value),]

    ## calculate demand
    demand = demandF[variable == "Value_demand", .(region, year, CES_node, value)]
    demand = approx_dt(demand, REMINDyears,
                     xcol = "year", ycol = "value",
                     idxcols = c("region", "CES_node"),
                     extrapolate=T)


    ## create parent node
    demand[, CES_parent:= sub("^[^_]*_", "",CES_node)]
    setcolorder(demand, neworder = c("region", "year", "CES_parent", "CES_node", "value"))

    ## calculate intensity
    demandI = demandF[variable == "Value_intensity", .(region, year, CES_node, value)]
    demandI = approx_dt(demandI, REMINDyears,
                      xcol = "year", ycol = "value",
                      idxcols = c("region", "CES_node"),
                      extrapolate=T)

    for(dt in list(demand, demandI)){
        nas <- dt[!complete.cases(dt)]
        if(nrow(nas) > 0){
            print("NAs found in REMIND output table")
            browser()
        }
    }
    demand_list = list(demand = demand,
                     demandI = demandI,
                     demandF_plot_pkm = demandF_plot_pkm,
                     demandF_plot_mjkm = demandF_plot_mjkm,
                     demandF_plot_EJ = demandF_plot_EJ)


    return(demand_list)
}
