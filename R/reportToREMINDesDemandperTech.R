#'Report energy service demand to REMIND
#'
#'Note that this is a helpers function that accounts for structural differences in REMIND and EDGE-T.
#'The REMIND CES tree cannot represent active modes, as they deliver valuable energy service demand without capital cost and final energy production.
#'Just filtering out the active modes energy service demand allocated by EDGE-Transport would lead to inconsistencies between both models:
#'E.g. REMIND would split the whole trn_pass energy service demand according to the FE shares and energy efficiencies reported by EDGE-T (without considering the active modes)
#' and would calculate a different FE value compared to EDGE-T that reserves a part of the trn_pass energy service demand
#'to the active modes.
#'
#'Solution: split and evenly distribute the energy service demand of active modes across all technologies to calculate energy efficiency and capital cost per pkm.
#'Consequence: This leads to the fact that in REMIND energy service demand, energy efficiencies, an capital costs on technology level are rather abstract values that are not technically correct.
#'Hence, they should not be reported on technology level by REMIND (only by EDGE-T). The FE values on technology level are represented correctly as well as the energy service demand on CES node level.
#'The decision of the technology share is anyway made by EDGE-T and not by REMIND.
#'
#'
#' @param fleetESdemand energy service demand on fleet level
#' @param hybridElecShare share of electric driving for hybrid electric vehicles
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers
#' @returns Energy service demand in [trillion pkm/trillion tkm]
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportToREMINDesDemand <- function(fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  fleetESdemand <- copy(fleetESdemand)
  fleetESdemand <- fleetESdemand[period %in% timeResReporting]
  # convert billion pkm|tkm to trillion pkm|tkm
  fleetESdemand[, value := value * 1e-3]
  test <- fleetESdemand[, .(value = sum(value)), by = c("region", "period", "sector")]
  test <- test[sector == "trn_pass"][, sector := NULL]
  #split hybrid energy service demand
  fleetESdemandWoHybrid <- copy(fleetESdemand)
  hybrids <- fleetESdemandWoHybrid[technology == "Hybrid electric"]
  hybrids[, value := hybridElecShare * value][, technology := "BEV"]
  fleetESdemandWoHybrid[technology == "Hybrid electric", value := (1 - hybridElecShare) * value]
  fleetESdemandWoHybrid[technology == "Hybrid electric", technology := "Liquids"]
  fleetESdemandWoHybrid <- rbind(fleetESdemandWoHybrid, hybrids)
  byCols <- names(fleetESdemandWoHybrid)
  byCols <- byCols[!byCols %in% c("value")]
  fleetESdemand <- fleetESdemandWoHybrid[, .(value = sum(value)), by = eval(byCols)]

  # Split and distribute the active modes energy service demand equally to the technologies as
  # they are not represented in REMIND (see explanation in function header)
  demandMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  demandMap <- demandMap[!is.na(all_teEs)]
  techOptions <- length(unique(demandMap[grepl(".*pass_sm", all_teEs)]$all_teEs))
  activeModes <- fleetESdemand[grepl(".*Cycle|Walk.*", subsectorL1)]
  REMINDesDemand <- fleetESdemand[!grepl(".*Cycle|Walk.*", subsectorL1)]
  activeModes <- activeModes[, .(value = sum(value)), by = c("region", "period", "sector")]
  activeModes[,  activeESaddition := value/techOptions]
  REMINDesDemand <- merge(REMINDesDemand, demandMap, by = c("univocalName", "technology"), all.x = TRUE)                                                      # nolint: object_name_linter
  REMINDesDemand <- REMINDesDemand[, .(value = sum(value)), by = c("region", "period", "sector", "all_teEs")]

  REMINDesDemand <- merge(REMINDesDemand, activeModes[, c("region", "period", "sector", "activeESaddition")], by = c("region", "period", "sector"), all.x = TRUE)
  REMINDesDemand[sector == "trn_pass", value := value + activeESaddition][, c("activeESaddition", "sector") := NULL]

  test2 <- REMINDesDemand[grepl(".*pass_sm", all_teEs), .(value = sum(value)), by = c("region", "period")]
  setkey(test, region, period)
  setkey(test2, region, period)
  if (!all.equal(test, test2)) stop("Something went wrong with the hybrids or active modes split in reportToREMINDesDemand()")
  checkForNAsAndDups(REMINDesDemand, "REMINDesDemand", "reportToREMINDesDemand()")
  REMINDesDemand <- prepareForREMIND(REMINDesDemand, demScen, SSPscen, transportPolScen)
  setnames(REMINDesDemand, c("period", "region"), c("tall", "all_regi"))

return(REMINDesDemand)
}
