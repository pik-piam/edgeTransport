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
    },


    "speedMotorized" = {

    ## Apply convergence in time to the fastest vehicle across regions
    speed[, maxspeed := max(speed[year == 2100]), by = .(tranSubsector)]
    speed[year >= 2020, speed := speed[year == 2020]*(2100-year)/(2100-2020) + maxspeed*(year-2020)/(2100-2020), by =c("tranSubsector", "region")]
    speed[, maxspeed := NULL]
    ## Speed correction to enhance influence of VOT for 2W (Robert's idea)
    speed[supplysector == "trn_pass_road_LDV_2W" & speed != 1, speed := speed * 0.75]
}

    )

  return(as.quitte(dt))

}
