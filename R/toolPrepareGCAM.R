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
  lstruct <- fread(system.file("extdata/logit_structure.csv", package = "edgeTransport", mustWork = TRUE))
  dt <- magpie2dt(magpieobj)
  mapfile <- system.file("extdata", "mapping_GCAM_categories.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mappingGCAM <- fread(mapfile)
  switch(
    subtype,
    "feVkmIntensity" = {
      browser()
      weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("iso", "year", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value*esdem)/sum(esdem)),
               by = c("iso", "year", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology")]

      #unit conversion from Mbtu/vehkm to MJ/vehkm
      ConvBTUtoMJ <- 1.055e-3
      dt[, value := value*ConvBTUtoMJ][, unit := "MJ/vehkm"]
      },
    "loadFactor" = {
      browser()
      weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("iso", "year", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value*esdem)/sum(esdem)),
               by = c("iso", "year", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology")]
      dt[sector %in% c("trn_pass", "trn_aviation_intl"), unit := "p/veh"]
      dt[sector %in% c("trn_freight", "trn_shipping_intl"), unit := "t/veh"]
    },
    "histEsDemand" = {
      #use only historical demand
      dt <- dt[year %in% c(1990, 2005, 2010)]
      #map
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology")]
      dt <- dt[!is.na(sector)]
      #aggregate
      dt <- dt[, .(value = sum(value)), by = c("sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType", "technology", "univocalName", "iso", "year", "Units")]
      #convert to quitte
      setnames(dt, c("Units", "year"), c("unit", "period"))
      #rename units
      dt[, unit := gsub("million pass-km", "million pkm", unit)]
      dt[, unit := gsub("million ton-km", "million tkm", unit)]
      },
    "speedMotorized" = {
      weight <- readSource("GCAM", subtype = "histEsDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      #Speed is not differentiated between different technologies -> aggregate weights (ES demand) to VehicleType level
      weight <- weight[, .(value = sum(value)),
               by = c("iso", "year", "sector", "subsector")]
      setnames(weight, "value", "esdem")
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      dt <- weight[dt, on = c("iso", "year", "sector", "subsector")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      #Neglect technology level in GCAM/EDGET mapping
      mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      browser()
      dt <- mappingGCAM[dt, on = c(GCAMsubsector = "subsector")]
      dt <- dt[!is.na(sector)]

      dt <- dt[, .(value = sum(value*esdem)/sum(esdem)),
               by = c("iso", "year", "sector", "subsectorL3", "subsectorL2", "subsectorL1", "vehicleType")]
      dt[, unit := "km/h"]
      },

    "speedNonMotorized" = {

    },

    "valueOfTimeMultiplier" = {

    },

  )

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
