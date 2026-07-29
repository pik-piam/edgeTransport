#' Derive the mixed temporal resolution used across EDGE-T from a reference data.table
#'
#' Vehicle types that feature fleet tracking are resolved annually (highRes), all other
#' modes use a coarser resolution (lowTimeRes). The split is based on which univocalNames
#' provide data for the full set of highRes periods. Shared by toolLoadInputs() and
#' toolReLoadInputs() so the full-load and reload paths cannot drift apart.
#'
#' @author Johanna Hoppe, Alex K. Hagen
#' @param dt data.table containing at least univocalName and period columns (e.g. energy
#'           intensity data) that spans the full temporal resolution of the model
#' @returns list with dtTimeRes (unique univocalName/period combinations) and
#'          lowTimeRes (the coarse resolution periods)
#' @import data.table
#' @export

toolDeriveTimeRes <- function(dt) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- univocalName <- test <- . <- NULL

  dtTimeRes <- unique(dt[, c("univocalName", "period")])
  highRes <- unique(dtTimeRes$period)
  lowResUnivocalNames <- copy(dtTimeRes)
  lowResUnivocalNames <- lowResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  lowResUnivocalNames <- lowResUnivocalNames[test == FALSE, univocalName]
  lowTimeRes <- unique(dtTimeRes[univocalName %in% lowResUnivocalNames]$period)

  return(list(dtTimeRes = dtTimeRes, lowTimeRes = lowTimeRes))
}
