#' Prepare outputs for REMIND: Set correct tech names, CES node names and years.
#'
#' @param demByTech the share of FEs for different REMIND CES nodes.
#' @param intensity the energy intensity of the REMIND CES nodes wrt. the FE carriers
#' @param capCost levelized capital costs of REMIND CES nodes
#' @param EDGE2teESmap map EDGE-T technologies to REMIND ES techs
#' @param REMINDtall the full REMIND timestep range as required for input data
#' @import data.table
#' @importFrom rmndt approx_dt
#' @export

prepare4REMIND <- function(demByTech, intensity, capCost,
                           EDGE2teESmap,
                           REMINDtall){
    value <- NULL

    ## load conversion factor
    EJ_2_Twa <- 31.71e-03 ## TWa is the unit expected in REMIND for the final energy values
    ## energy intensity
    intensity=merge(intensity, EDGE2teESmap[,c("CES_node","teEs")],
                    by="CES_node",all.x=TRUE)
    intensity=intensity[, c("year", "region", "teEs", "value"),with = F]
    setnames(intensity, old = c("year", "teEs"), new = c("tall", "all_teEs"))
    intensity=approx_dt(dt=intensity, xdata=REMINDtall,
                        xcol="tall", ycol="value",
                        idxcols=c("region", "all_teEs"),
                        extrapolate=T)
    intensity[,value:=value ## in [milliokm/EJ]
              /EJ_2_Twa     ## in [millionkm/Twa]
              *1e-6         ## in [trillionkm/Twa]
              ]
    setcolorder(intensity, c("tall", "region", "all_teEs", "value"))
    setnames(intensity, old = "region", new = "all_regi")
    ## non-fuel price
    budget=merge(capCost, unique(EDGE2teESmap[,c("teEs","EDGE_top")]),
                 by="teEs",all.x=TRUE)
    budget=budget[, c("year", "region", "teEs", "value"),with = F]
    setnames(budget, old = c("year", "teEs", "region"), new = c("tall", "all_teEs", "all_regi"))

    budget=approx_dt(dt=budget, xdata=REMINDtall,
                     xcol="tall", ycol="value",
                     idxcols=c("all_regi", "all_teEs"),
                     extrapolate=T)

    setcolorder(budget, c("tall", "all_regi", "all_teEs", "value"))

    ## demand by technology
    demByTech=merge(demByTech, EDGE2teESmap[,c("CES_node","all_in","all_enty","teEs")],
                by="CES_node", all.x=TRUE)
    demByTech=demByTech[, c("year", "region", "all_enty", "all_in", "teEs", "value"),with = F]
    setnames(demByTech, old = c("year", "teEs", "region"), new = c("tall", "all_teEs", "all_regi"))
    demByTech <- approx_dt(dt=demByTech, xdata=REMINDtall,
                        xcol="tall", ycol="value",
                        idxcols=c("all_regi","all_in","all_enty","all_teEs"),
                        extrapolate=T)[, value := EJ_2_Twa * value] ## in TWa
    setcolorder(demByTech, c("tall", "all_regi","all_enty","all_in","all_teEs","value"))
    
    result=list(demByTech=demByTech,
                intensity=intensity,
                capCost=budget)
    return(result)

}
