#' Provide EDGE-Transport input parameters
#' @author Alois Dirnaichner
#' @param subtype one of the parameters required for EDGE-T
#' @param adjustments adjust historical data (boolean, defaults to TRUE)

calcEdgeTransportSAinputs <- function(subtype, adjustments = TRUE) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package = "edgeTransport", mustWork = TRUE))

  switch(
    subtype,
    "annualMileage" = {
      amUnit <- "vkm/yr"
      ## in the prepare function we prepare the mileage for all vehicleTypes that
      ## are found in the data and extend the data to the other technologies
      ## i.e., FCEVs, BEVs. We fill some gaps and map the vehicleTypes.
      ## we do not yet: 1) extend data to other regions or 2) to other vehicleTypes
      amTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      amUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)

      amTRACCS[, c("model", "scenario", "variable") := NULL]
      amUCD[, c("model", "scenario", "variable") := NULL]

      amUCD[, unit := amUnit]
      amTRACCS[, unit := amUnit]

      ## use same periods in both sources
      amTRACCS <- amTRACCS[period %in% c(2005, 2010)]
      amUCD <- rbind(amUCD[period == 2005], amUCD[period == 2005][, period := 2010])

      ## trucks mileage and buses only from TRACCS, this has to be applied to UCD
      tm <- amTRACCS[subsectorL2 %in% c("Bus" ,"trn_freight_road_tmp_subsector_L2")]
      ## use mean values for non-TRACCS countries
      tm <- tm[, .(value = mean(value)), by = c("period", "vehicleType")]

      nonTRACCStm <- CJ(region = amUCD$region, period = tm$period, unique = TRUE)
      nonTRACCStm <- nonTRACCStm[tm, on = "period", allow.cartesian = TRUE]
      nonTRACCStm <- lstruct[nonTRACCStm, on = "vehicle_type", allow.cartesian = TRUE]
      nonTRACCStm[, unit := unique(amUCD$unit)]

      dt <- rbind(amUCD, nonTRACCStm, use.names = TRUE, fill = TRUE)

      ## update-join - prefer TRACCS values wherever we have them
      ## dt[, traccs_value := am_TRACCS[.SD, on=colnames(dt)[1:10], x.value]]
      dt[amTRACCS, value := i.value, on = colnames(dt)[1:10]]

      planeTypes <- unique(lstruct[grepl("Aviation", vehicleType), vehicleType])
      ## for planes we assume 3000 working hours per year at 750 km/h ~ 2e6 km/yr
      ## https://eu.usatoday.com/story/travel/columnist/cox/2012/11/19/ask-the-captain-how-far-does-a-jet-fly-during-its-lifetime/1712269/
      planes <- CJ(region = dt$region, period = dt$period, vehicleType = planeTypes, value = 2e6,
                   unit = am_unit, unique = TRUE)
      planes <- lstruct[planes, on = "vehicle_type", allow.cartesian = TRUE]

      shipTypes <- unique(lstruct[grepl("Ship", vehicleType), vehicleType])
      ## for international ships we assume 300.000 km/yr
      ## https://www.ioscm.com/blog/industry-facts-101-the-shipping-industry-is-enormous/
      ships <- CJ(region = dt$region, period = dt$period, vehicleType = shipTypes, value = 3e5,
                  unit = amUnit, unique = TRUE)
      ## domestic ships one third
      ships[vehicleType == "Domestic Ship_tmp_vehicletype", value := 1e5]
      ships <- lstruct[ships, on = "vehicleType", allow.cartesian = TRUE]

      dt <- rbind(dt, planes, ships, use.names = TRUE)

      q <- as.quitte(dt)
      if(adjustments) {
        q <- toolAdjustData(q, subtype)
      }

      ## check for missing data by joining the logit structure and looking for NAs,
      ## the join has to happen on subtype level since it does not make sense
      ## slash we do not have data for all categories. E.g., mileage for non-motorized modes or
      ## trains does not make sense
      amCheck <- lstruct[!subsectorL3 %in% c("Walk", "Cycle", "HSR", "Passenger Rail", "Freight Rail")]
      q <- q[amCheck, on = colnames(amCheck)]
      ## note that the actual check is done for all subtypes at the end of the function

      weight <- calcOutput("GDP", aggregate = FALSE)[, unique(q$period), "gdp_SSP2"]
      unit <- amUnit
      description <- "Annual mileage data for LDV, trucks, trains and ships. Sources: TRACCS, UCD."

    },

    "histEsDemand" = {
      frUnit <- "million tkm"
      paUnit <- "million pkm"
      ## the GCAM data is more-or-less complete, we use this as a starting point
      esGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      esGCAM[sector == "trn_freight", unit := frUnit]
      esGCAM[sector == "trn_pass", unit := paUnit]
      esGCAM[, c("model", "scenario", "variable") := NULL]

      esTRACCS <- rbind(
        toolPrepareTRACCS(readSource("TRACCS", "roadTkmDemand"), "roadTkmDemand")[, unit := frUnit],
        toolPrepareTRACCS(readSource("TRACCS", "roadPkmDemand"), "roadPkmDemand")[, unit := paUnit]
      )[period %in% c(2005, 2010)]
      esTRACCS[, c("model", "scenario", "variable") := NULL]

      esGCAM[esTRACCS, value := i.value, on = colnames(es_GCAM)[1:10]]

      q <- as.quitte(esGCAM)
      if(adjustments) {
        q <- toolAdjustData(q, subtype)
      }

      ## expand and check
      q <- q[lstruct, on = intersect(colnames(q), colnames(lstruct))]

      weight <- calcOutput("GDP", aggregate = FALSE)[, unique(q$period), "gdp_SSP2"]
      unit <- sprintf("%s or %s", paUnit, frUnit)
      description <- "Energy service demand. Sources: TRACCS, GCAM."

    },

    "loadFactor" = {
      frUnit <- "tkm/veh"
      paUnit <- "pkm/veh"
      lfGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      lfGCAM[sector == "trn_freight", unit := frUnit]
      lfGCAM[sector == "trn_pass", unit := paUnit]
      lfGCAM[, c("model", "scenario", "variable") := NULL]

      lfTRACCS <- toolPrepareTRACCS(readSource("TRACCS", "loadFactor"), "loadFactor")
      lfTRACCS[sector == "trn_freight", unit := frUnit]
      lfTRACCS[sector == "trn_pass", unit := paUnit]
      lfTRACCS[, c("model", "scenario", "variable") := NULL]

      lfGCAM[lfTRACCS, value := i.value, on = colnames(lfGCAM)[1:10]]

      q <- as.quitte(lfGCAM)
      if(adjustments) {
        q <- toolAdjustData(q, subtype)
      }

      ## expand and check
      lstruct <- lstruct[!subsectorL3 %in% c("Walk", "Cycle")]
      q <- q[lstruct, on = intersect(colnames(q), colnames(lstruct))]

      weight <- calcOutput("GDP", aggregate = FALSE)[, unique(q$period), "gdp_SSP2"]
      unit <- sprintf("%s or %s", pa_unit, frUnit)
      description <- "Load factor, also called occupancy ration for passenger vehicles. Sources: TRACCS, GCAM."

    }
  )

  tst <- q[is.na(value)]
  if(nrow(tst) > 0) {
    print(sprintf("Missing elements in inputdata subtype %s.", subtype))
    browser()
  }

  nonvalcols <- colnames(q)[colnames(q) != "value"]
  tst <- q[, ..nonvalcols]
  tst <- tst[duplicated(tst)]
  if(nrow(tst) > 0) {
    print(sprintf("Duplicated elements in inputdata subtype %s.", subtype))
    browser()
  }

  return(list(
    x           = as.magpie(as.data.frame(q)),
    weight      = weight,
    unit        = unit,
    description = description
  ))
}
