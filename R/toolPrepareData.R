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

toolPrepareTRACCS <- function(magpieobj, subtype, weight=NULL) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  ## load mappings
  mapfile <- system.file("extdata", "mapping_TRACCS_roadvehicles.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_TRACCS = fread(mapfile, skip = 0)
  techmapfile <- system.file("extdata", "mapping_TRACCS_techs.csv",
                         package = "edgeTransport", mustWork = TRUE)
  techmap = fread(techmapfile, skip = 0)

  unit <- gsub("unit: ", "", getComment(magpieobj))

  weight <- readSource("TRACCS", subtype="roadVkmDemand")
  weight <- magpie2dt(weight)
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

      dt <- dt[,
               .(value=mean(value)),
               by=c("iso", "period", "vehicle_type", "technology")][!is.na(value) & value > 0]
    })

  setnames(dt, "iso", "region")
  dt$unit <- unit
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

toolPrepareUCD <- function(magpieobj, subtype, weight=NULL) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  ## mapping_UCD <- fread("~/git/edgeTransport/inst/extdata/mapping_UCD_categories.csv")
  mapfile <- system.file("extdata", "mapping_UCD_categories.csv",
                             package = "edgeTransport", mustWork = TRUE)
  mapping_UCD = fread(mapfile, skip = 0)

  weight <- readSource("UCD", subtype="feDemand")
  weight <- magpie2dt(weight)

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
      dt[, .(value=sum(value*fe)/sum(fe)), by=c("iso", "year", "vehicle_type", "unit")]
    })

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
