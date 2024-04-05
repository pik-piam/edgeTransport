#' Load the energy service demand from a REMIND fulldata.gdx on sector level (CES level) in [bn (p|t)km/yr] and the requested temporal resolution
#'
#' @param gdxPath path to REMIND fulldata.gdx
#' @param yrs requested temporal resolution
#'
#' @import data.table
#' @importFrom gdx readGDX
#' @export


toolLoadREMINDesDemand <- function(gdxPath) {
  value <- unit <- variable <- NULL

  mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv", package = "edgeTransport", mustWork = TRUE))
  mapEdgeToREMIND <- unique(mapEdgeToREMIND[, c("all_in", "sector")])

  ESdemand <- readGDX(gdxPath, c("vm_cesIO"), field = "l")
  ESdemand <- ESdemand[, , c("entrp_pass_sm", "entrp_pass_lo", "entrp_frgt_sm", "entrp_frgt_lo")]

  ESdemand <- magpie2dt(ESdemand, regioncol = "region",
                   yearcol = "period", datacols = "all_in")

  # map on EDGE-T structure
  ESdemand <- merge(ESdemand, mapEdgeToREMIND, by = "all_in")[, all_in := NULL]

  ## convert unit
  trillionToBillion <- 1e3
  ESdemand[, value := value
                  * trillionToBillion]
  ESdemand[, unit := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "bn pkm/yr", "bn tkm/yr")][, variable := "ES"]

  setkey(ESdemand, region, sector, subsectorL1, subsectorL2, subsectorL3, vehicleType,
         technology, univocalName, period)

  if (anyNA(ESdemand) == TRUE) {
    stop("Energy service demand from REMIND contain NAs")
  }
  return(ESdemand)
}
