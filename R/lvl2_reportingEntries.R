#' Provides entries that are useful for reporting and complex realization
#'
#' @param ESdem energy services demand
#' @param FEdem final energy demand
#' @param gdp_country GDP on ISO level
#' @param REMIND2ISO_MAPPING REMIND2ISO_MAPPING mapping


lvl2_reportingEntries = function(ESdem, FEdem, gdp_country, REMIND2ISO_MAPPING){
  category <- share_LDV_totroad <- region <- subsector_L3 <- subsector_L2 <- subsector_L1 <- share_LDV_totliq <- technology <- demand_EJ <- totdem <- demand_F <- entry <- `.` <- NULL
  aggrCategory = function(dt){
    node <- category <- technology <- sector <- subsector_L1 <- subsector_L2 <- subsector_L3 <- fuel <- NULL
    ## Split results according to nodes (LDV, HDV, El. Trains)
    dt[, node := ifelse(subsector_L2 == "trn_pass_road_LDV", "LDV", NA)]
    dt[, node := ifelse(grepl("Rail", subsector_L3) & grepl( "Elect", technology), "Electric Trains", node)]
    dt[, node := ifelse(grepl("Cycle_tmp_technology|Walk_tmp_technology", technology), "Non-Motorized", node)]
    dt[, node := ifelse(is.na(node), "HDV", node)]

    ## Split results according to categories
    dt[, category := ifelse(grepl("Bus|trn_pass_road_bus|LDV|Passenger Rail", subsector_L2) | grepl("Cycle_tmp_technology|Walk_tmp_technology", technology), "Pass", NA)]
    dt[, category := ifelse(sector %in% c("trn_aviation_intl","trn_shipping_intl"), "Bunkers", category)]
    dt[, category := ifelse(is.na(category), "Freight", category)]

    ## Split results according to powertrain
    dt[, fuel := ifelse(technology == "LA-BEV", "BEV", technology)] ## attribute single BEV technology
    dt[, fuel := ifelse(node == "Non-Motorized", "Non-motorized", fuel)] ## attribute Non-Motorized to Cycling and Walking


    return(dt)
  }

  dem = list(ESdem = ESdem, FEdem = FEdem)
  dem = lapply(dem, aggrCategory)
  ## ES demand mif entries
  dem$ESdem[, totdem := demand_F/   ## million pkm million tkm
                1000]             ## billions

  dem$ESdem[, entry := "ES"]
  dem$ESdem[, unit := ifelse(category  == "Pass", "billion pkm", "billion tkm")]
  dem$ESdem=dem$ESdem[,.(totdem = sum(totdem)), by = c("region", "year", "node", "category", "fuel", "entry", "unit")]

  ## FE demand mif entries
  dem$FEdem[,entry := "FE"]
  dem$FEdem[, unit := "EJ"]
  dem$FEdem=dem$FEdem[,.(totdem = sum(demand_EJ)), by = c("region", "year", "node", "category", "fuel", "entry", "unit")]

  dem$iso_FEdem = disaggregate_dt(dem$FEdem, REMIND2ISO_MAPPING,
                                  valuecol="totdem",
                                  datacols=c("year", "node", "category",
                                             "fuel", "entry", "unit"),
                                  weights=gdp_country)

  ## shares of fepet->LDVs, with respect to total liquid-fuelled transport (share_LDV_totliq) and total road transport (share_LDV_totroad)
  shLDV = FEdem[technology %in% c("Liquids")]
  shLDV = shLDV[,.(demand_EJ = sum(demand_EJ)), by = c("sector","subsector_L3", "subsector_L2" , "region", "year")]
  shLDV[, share_LDV_totliq := demand_EJ[subsector_L2 == "trn_pass_road_LDV"]/sum(demand_EJ), by = c("region", "year")]
  shLDV = shLDV[subsector_L3 %in% c("trn_pass_road", "trn_freight_road"),]
  shLDV[, share_LDV_totroad := demand_EJ[subsector_L2 == "trn_pass_road_LDV"]/sum(demand_EJ), by = c("region", "year")]
  shLDV = shLDV[,share_LDV_totliq := ifelse(is.na(share_LDV_totliq), share_LDV_totliq[year == 2090], share_LDV_totliq), by =c("region","subsector_L2")]
  shLDV = shLDV[,share_LDV_totroad := ifelse(is.na(share_LDV_totroad), share_LDV_totroad[year == 2090], share_LDV_totroad), by =c("region","subsector_L2")]
  shLDV = shLDV[subsector_L2 == "trn_pass_road_LDV", .(region, year, share_LDV_totliq, share_LDV_totroad)]

  out = list(dem = dem, shLDV = shLDV)

  return(out)
}
