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
  dt <- magpie2dt(magpieobj)[year %in% c(1990, 2005, 2010)]
  mapfile <- system.file("extdata", "mapping_GCAM_categories.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_GCAM = fread(mapfile)
  switch(
    subtype,
    "histEsDemand" = {
      dt <- mapping_GCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]
    },
    "loadFactor" = {
      browser()
      weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on=c("iso", "year", "subsector", "technology")]
      ## using a very low demand leads to equal distribution if there is no demand
      ## for all available technologies
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mapping_GCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value=sum(value*esdem)/sum(esdem)),
               by=c("iso", "year", "sector", "subsector_l3", "subsector_l2", "subsector_l1", "vehicle_type", "technology")]
    }
  )

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
