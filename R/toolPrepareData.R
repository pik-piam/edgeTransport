#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Alois Dirnaichner
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolPrepareGCAM <- function(magpieobj, subtype) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))
  dt <- magpie2dt(magpieobj)[year <= 2010]

  mapfile <- system.file("extdata", "mapping_GCAM_categories.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_GCAM = fread(mapfile)

  switch(
    subtype,
    "esDemand" = {
      dt <- mapping_GCAM[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]
      dt[, c("sector", "subsector") := NULL]
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
    })

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}


#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Alois Dirnaichner
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolPrepareTRACCS <- function(magpieobj, subtype) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  ## load mappings
  mapfile <- system.file("extdata", "mapping_TRACCS_roadvehicles.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_TRACCS = fread(mapfile, skip = 0)
  techmapfile <- system.file("extdata", "mapping_TRACCS_techs.csv",
                         package = "edgeTransport", mustWork = TRUE)
  techmap = fread(techmapfile, skip = 0)

  weight <- readSource("TRACCS", subtype="roadVkmDemand")
  weight <- magpie2dt(weight)[, unit := NULL]
  setnames(weight, "value", "vkm")
  dt <- magpie2dt(magpieobj)
  dt <- dt[TRACCS_technology != "Other"]

  switch(
    subtype,
    "annualMileage" = {
      wcols <- c("iso", "period", "TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology")
      dt <- weight[dt, on=wcols]

      dt <- techmap[dt, on="TRACCS_technology"]
      dt <- mapping_TRACCS[dt, on=c("TRACCS_category", "TRACCS_vehicle_type")]

      dt <- unique(dt[,
               .(unit, value=sum(value*vkm)/sum(vkm)),
               by=c("iso", "period", "vehicle_type", "technology")])
      ## some small countries do not have 40t, we use mean mileage
      dt[, value := ifelse(is.na(value), mean(value, na.rm=TRUE), value),
         by=c("period", "vehicle_type", "technology")]

      lstruct <- lstruct[vehicle_type %in% unique(dt$vehicle_type)]
      full_table <- CJ(iso=dt$iso, period=dt$period, vehicle_type=dt$vehicle_type,
                       technology=lstruct$technology, unit=dt$unit, unique=T)
      dt <- dt[full_table, on=c("iso", "period", "vehicle_type", "technology", "unit")]
      dt[, value := ifelse(is.na(value), .SD[technology == "Liquids", value], value),
         by=c("iso", "period", "vehicle_type")]
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]
    },
    ## note that leaving the case empty defaults to the next case in R
    "roadPkmDemand" = ,
    "roadTkmDemand" = {
      dt <- techmap[dt, on="TRACCS_technology"]
      dt <- mapping_TRACCS[dt, on=c("TRACCS_category", "TRACCS_vehicle_type")]

      dt <- unique(dt[
        , .(unit, value=sum(value)),
        by=c("iso", "period", "vehicle_type", "technology")])

      lstruct <- lstruct[vehicle_type %in% unique(dt$vehicle_type)]
      full_table <- CJ(iso=dt$iso, period=dt$period, vehicle_type=dt$vehicle_type,
                       technology=lstruct$technology, unit=dt$unit, unique=T)
      dt <- dt[full_table, on=c("iso", "period", "vehicle_type", "technology", "unit")]
      dt[is.na(value), value := 0]
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]

    }
  )

  test <- dt[is.na(value)]
  if(nrow(test) > 0) {
    print("Missing tkm/vkm data in TRACCS prepare")
    browser()
  }

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}


#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Alois Dirnaichner
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolPrepareUCD <- function(magpieobj, subtype) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  ## mapping_UCD <- fread("~/git/edgeTransport/inst/extdata/mapping_UCD_categories.csv")
  mapfile <- system.file("extdata", "mapping_UCD_categories.csv",
                             package = "edgeTransport", mustWork = TRUE)
  mapping_UCD = fread(mapfile, skip = 0)

  weight <- readSource("UCD", subtype="feDemand")
  weight <- magpie2dt(weight)[, unit := NULL]

  dt <- magpie2dt(magpieobj)
  setnames(weight, "value", "fe")

  switch(
    subtype,
    "annualMileage" = {
      ## fe data only available for 2005
      weight <- weight[, year := NULL]
      wcols <- c("iso", "UCD_sector", "mode", "size_class")
      weight <- weight[, .(fe=sum(fe), UCD_technology="All", UCD_fuel="All"), by=wcols]
      dt <- weight[dt, on=c(wcols, "UCD_technology", "UCD_fuel")]

      dt <- mapping_UCD[dt, on=c("UCD_sector", "mode", "size_class")]
      dt <- unique(dt[, .(unit, value=sum(value*fe)/sum(fe)), by=c("iso", "year", "vehicle_type")])
      dt <- lstruct[dt, on="vehicle_type", allow.cartesian=T]
    })

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
