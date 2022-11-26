#' Perform adjustments to EDGE-T input data.
#'
#' @author Alois Dirnaichner
#' @param qobj a quitte data object
#' @param subtype the input parameter type
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolAdjustData <- function(qobj, subtype) {
  ## Comment: Maybe this function has to be split in toolAdjustIntensity etc.
  ## since the dis/aggregation of vehicle classes etc might be quite lengthy
  dt <- as.data.table(qobj)

  switch(
    subtype,
    "histEsDemand" = {
      dt <- mapping_GCAM[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]
      dt <- unique(dt[, .(value=sum(value)), by=c("iso", "year", "vehicle_type", "technology", "Units")])
      ## add full logit
      dt <- lstruct[dt, on=c("vehicle_type", "technology")]
      full <- rbind(
        CJ(iso=dt$iso, year=dt$year, vehicle_type="Walk_tmp_vehicletype",
           technology="Walk_tmp_technology", Units="million pass-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year, vehicle_type="Cycle_tmp_vehicletype",
           technology="Cycle_tmp_technology", Units="million pass-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year,
           vehicle_type=c("International Aviation_tmp_vehicletype", "Domestic Aviation_tmp_vehicletype"),
           technology="Hydrogen", Units="million pass-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year,
           vehicle_type=unique(dt[subsector_l1 == "trn_pass_road_LDV_4W", vehicle_type]),
           technology="Hybrid Electric", Units="million pass-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year,
           vehicle_type="Bus_tmp_vehicletype",
           technology="BEV", Units="million pass-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year,
           vehicle_type="Bus_tmp_vehicletype",
           technology="FCEV", Units="million pass-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year,
           vehicle_type=unique(dt[subsector_l3 == "trn_freight_road", vehicle_type]),
           technology="BEV", Units="million ton-km", unique=TRUE),
        CJ(iso=dt$iso, year=dt$year,
           vehicle_type=unique(dt[subsector_l3 == "trn_freight_road", vehicle_type]),
           technology="FCEV", Units="million ton-km", unique=TRUE))
      dt[, c("sector", "subsector_l3", "subsector_l2", "subsector_l1", "univocal_name") := NULL]
      dt <- rbind(
        dt,
        dt[full, on=c("iso", "year", "vehicle_type", "technology", "Units")]
      )
      ## cycle and walk has to be replaced by global averages (this data is simply missing)
      dt[, total := sum(value, na.rm=TRUE), by=c("iso", "year")]
      dt[vehicle_type == "Cycle_tmp_vehicletype", share := value/total]
      dt[, mean_share := mean(share, na.rm=TRUE), by="year"]
      dt[vehicle_type == "Cycle_tmp_vehicletype" & is.na(value), value := total * mean_share]
      dt[, c("share", "mean_share") := NULL]

      dt[vehicle_type == "Walk_tmp_vehicletype", share := value/total]
      dt[, mean_share := mean(share, na.rm=TRUE), by="year"]
      dt[vehicle_type == "Walk_tmp_vehicletype" & is.na(value), value := total * mean_share]
      dt[, c("share", "mean_share", "total") := NULL]
      ## everything else is probably 0
      dt[is.na(value), value := 0]
      setnames(dt, "Units", "unit")
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]

      test <- dt[is.na(value)]
      if(nrow(test) > 0){
        print("Missing tkm/vkm data in GCAM prepare")
        browser()
      }
    },
    "loadFactor" = {
      ## weights only available for these years
      dt <- dt[year %in% c(1990, 2005, 2010)]
      weight <- readSource("GCAM", subtype="esDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on=c("iso", "year", "subsector", "technology")]
      ## using a very low demand leads to equal distribution if there is no demand
      ## for all available technologies
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mapping_GCAM[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]

      dt <- dt[, .(value=sum(value*esdem)/sum(esdem)),
               by=c("iso", "year", "vehicle_type", "technology")]

      dt <- dt[!technology %in% c("Coal", "Tech-Adv-Electric", "Adv-Electric",
                                  "Hybrid Liquids", "Tech-Adv-Liquid", "Adv-Liquid")]

      lstruct <- lstruct[!subsector_l3 %in% c("Walk", "Cycle")]
      dt <- lstruct[dt, on=c("vehicle_type", "technology")]

      dt <- rbind(
        dt,
        dt[vehicle_type == "International Aviation_tmp_vehicletype" & technology == "Liquids"][
          , technology := "Hydrogen"],
        dt[vehicle_type == "Domestic Aviation_tmp_vehicletype" & technology == "Liquids"][
          , technology := "Hydrogen"],
        dt[subsector_l1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][
          , technology := "Hybrid Electric"],
        dt[vehicle_type == "Bus_tmp_vehicletype" & technology == "Liquids"][
          , technology := "BEV"],
        dt[vehicle_type == "Bus_tmp_vehicletype" & technology == "Liquids"][
          , technology := "FCEV"],
        dt[subsector_l3 == "trn_freight_road" & technology == "Liquids"][
          , technology := "BEV"],
        dt[subsector_l3 == "trn_freight_road" & technology == "Liquids"][
          , technology := "FCEV"])

      dt[, c("sector", "subsector_l3", "subsector_l2", "subsector_l1",
             "univocal_name") := NULL]

      dt <- dt[lstruct, on=c("vehicle_type", "technology")]

      test <- dt[is.na(value)]
      if(nrow(test) > 0){
        print("Missing loadfactor data in GCAM prepare")
        browser()
      }

      nc <- colnames(dt)[colnames(dt) != "value"]
      test <- dt[, ..nc]
      test <- test[duplicated(test)]
      if(nrow(test) > 0){
        print("Duplicates in loadfactor data in GCAM prepare")
        browser()
      }
    })

  setnames(dt, "iso", "region")
  switch(
    subtype,
    "annualMileage" = {
    }
  )
  return(as.quitte(dt))

}
