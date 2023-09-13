#' Apply demand scenario specific adjustments to the load Factor
#' @author Johanna Hoppe
#' @param loadF load factor input data supplied by mrtransport
#' @param demandScen tranport demand scenario
#' @import data.table


toolAdjustScenSpecLoadFactor <- function(loadF, demScenario, SSPscenario) {

  # initialize zero change as default
  percentChange = 0
  targetYear = 2050 # dummy year

  if(SSP_scen == "SDP_RC"){
    percentChange = 0.3
    targetYear = 2060
  }

  if (!is.null(Dem_Scen)){
    if (Dem_Scen == "SSP2EU_lowdem"){
      percentChange = 0.4
      targetYear = 2050}
  }

  loadF[
    subsectorL3 == "trn_pass_road_LDV_4W" &
      period >= 2020 & year <= targetYear,
    value := loadFactor * (1 + percentChange*(year - 2020)/(targetYear - 2020))]

  loadF[
    subsectorL3 == "trn_pass_road_LDV_4W" &
      period >= targetYear,
    value := loadFactor * (1 + percentChange)]

  return(loadF)
}
