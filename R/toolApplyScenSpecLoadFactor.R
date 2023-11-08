#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param loadFactor load factor input data supplied by mrtransport
#' @param demandScen tranport demand scenario
#' @param SSPscenario shared socioeconomic pathway
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated univocalNames
#' @import data.table


toolApplyScenSpecLoadFactor <- function(loadFactor, demScenario, SSPscenario, filter) {

  # initialize zero change as default
  percentChange = 0
  targetYear = 2050 # dummy year

  if(SSPscenario == "SDP_RC"){
    percentChange = 0.3
    targetYear = 2060
  }

  if (!is.null(demScenario)){
    if (demScenario == "SSP2EU_lowdem"){
      percentChange = 0.4
      targetYear = 2050}
  }

  loadFactor[
    univocalName %in% filter$trn_pass_road_LDV_4W &
      period >= 2020 &
      period <= targetYear,
    value := value * (1 + percentChange * (period - 2020)/(targetYear - 2020))]

  loadFactor[
    univocalName %in% filter$trn_pass_road_LDV_4W &
      period >= 2020 &
      period >= targetYear,
    value := value * (1 + percentChange)]

  return(loadFactor)
}
