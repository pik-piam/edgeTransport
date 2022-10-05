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
  mapping_TRACCS_roadf_categories = fread(mapfile, skip = 0)

  unit <- gsub("unit: ", "", getComment(magpieobj))

  ## translate dt
  dt <- magpie2dt(magpieobj)
  dt <- dt[TRACCS_technology != "Other"]
  dt[TRACCS_technology %in% c("Gasoline", "Diesel", "Flexi-fuel", "B30"),
     technology := "Liquids"]
  dt[TRACCS_technology %in% c("CNG", "CNG/Biogas", "LPG"),
     technology := "NG"]
  dt <- data.table::merge(mapping_TRACCS_roadf_categories, dt)
  dt[, c("TRACCS_category", "TRACCS_technology", "TRACCS_vehicle_type") := NULL]

  dt <- dt[lstruct, on=c("vehicle_type", "technology")]

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

  dt <- magpie2dt(magpieobj)
  weight=readSource("UCD", subtype="feDemand")
  weight <- magpie2dt(weight)
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
      dt <- dt[lstruct, on="vehicle_type"]
    })


  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
