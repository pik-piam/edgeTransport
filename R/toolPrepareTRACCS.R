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
    "loadFactor" = ,
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

  nc <- colnames(dt)[colnames(dt) != "value"]
  test <- dt[, ..nc]
  test <- test[duplicated(test)]
  if(nrow(test) > 0){
    print("Duplicates in data in TRACCS prepare")
    browser()
  }

  test <- dt[is.na(value)]
  if(nrow(test) > 0) {
    print("Missing data in TRACCS prepare")
    browser()
  }

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
