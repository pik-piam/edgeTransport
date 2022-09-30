#' Perform adjustments to EDGE-T input data.
#'
#' @author Alois Dirnaichner
#' @param qobj a quitte data object
#' @param subtype the input parameter type
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolAdjustData <- function(qobj, subtype) {
  ## Comment: Maybe this function has to be split in toolAdjustIntensity etc.
  ## since the dis/aggregation of vehicle classes etc might be quite lengthy
  dt <- as.data.table(qobj)
  switch(
    sourcetype,
      "energyIntensity" = {
      }
    )
  return(as.quitte(dt))
  
}
