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

toolPrepareData <- function(magpieobj, sourcetype) {
  ## Comment: Maybe this function has to be split in toolPrepareTRACCS etc.
  ## since the dis/aggregation of vehicle classes etc might be quite lengthy
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))
    switch(
      sourcetype,
      "TRACCS" = {
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
      }

    )
  setnames(dt, "iso", "region")
  dt$unit <- unit
  return(as.quitte(dt))
  
}
