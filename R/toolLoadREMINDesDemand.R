#' Load REMIND energy service demand
#'
#' Load the energy service demand from a REMIND fulldata.gdx on sector level (CES level)
#' in [bn (p|t)km/yr] and the requested temporal resolution
#'
#' @param gdxPath path to REMIND fulldata.gdx
#' @param helpers list of helpers
#'
#' @import data.table
#' @importFrom gdx readGDX
#' @export

toolLoadREMINDesDemand <- function(gdxPath, helpers) {
  value <- unit <- variable <- NULL

  mapEdgeToREMIND <- merge(helpers$mapEdgeToREMIND, unique(helpers$decisionTree[, c("sector", "univocalName")]), by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
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
  ESdemand[, unit := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "billion pkm/yr", "billion tkm/yr")][, variable := "ES"]

  setcolorder(ESdemand, c("region", "period", "sector", "value", "unit"))

  if (anyNA(ESdemand) == TRUE) {
    stop("Energy service demand from REMIND contain NAs")
  }
  return(ESdemand)
}
