#' Provide EDGE-Transport input parameters
#' @author Alois Dirnaichner
#' @param subtype one of the parameters required for EDGE-T
#' @param adjustments adjust historical data (boolean, defaults to TRUE)

calcEDGETinputs <- function(subtype, adjustments = TRUE) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  switch(
    subtype,
    "annualMileage" = {
      am_unit <- "vkm/yr"
      ## in the prepare function we prepare the mileage for all vehicle_types that
      ## are found in the data and extend the data to the other technologies
      ## i.e., FCEVs, BEVs. We fill some gaps and map the vehicle_types.
      ## we do not yet: 1) extend data to other regions or 2) to other vehicle_types
      am_TRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      am_UCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)

      am_TRACCS[, c("model", "scenario", "variable") := NULL]
      am_UCD[, c("model", "scenario", "variable") := NULL]

      am_UCD[, unit := am_unit]
      am_TRACCS[, unit := am_unit]

      ## use same periods in both sources
      am_TRACCS <- am_TRACCS[period %in% c(2005, 2010)]
      am_UCD <- rbind(am_UCD[period == 2005], am_UCD[period == 2005][, period := 2010])

      ## trucks mileage and buses only from TRACCS, this has to be applied to UCD
      tm <- am_TRACCS[subsector_l2 %in% c("Bus" ,"trn_freight_road_tmp_subsector_L2")]
      ## use mean values for non-TRACCS countries
      tm <- tm[, .(value = mean(value)), by=c("period", "vehicle_type")]

      nonTRACCS_tm <- CJ(region=am_UCD$region, period=tm$period, unique=TRUE)
      nonTRACCS_tm <- nonTRACCS_tm[tm, on="period", allow.cartesian=TRUE]
      nonTRACCS_tm <- lstruct[nonTRACCS_tm, on="vehicle_type", allow.cartesian=TRUE]
      nonTRACCS_tm[, unit := unique(am_UCD$unit)]

      dt <- rbind(am_UCD, nonTRACCS_tm, use.names=TRUE, fill=TRUE)

      ## update-join - prefer TRACCS values wherever we have them
      ## dt[, traccs_value := am_TRACCS[.SD, on=colnames(dt)[1:10], x.value]]
      dt[am_TRACCS, value := i.value, on=colnames(dt)[1:10]]

      plane_types <- unique(lstruct[grepl("Aviation", vehicle_type), vehicle_type])
      ## for planes we assume 3000 working hours per year at 750 km/h ~ 2e6 km/yr
      ## https://eu.usatoday.com/story/travel/columnist/cox/2012/11/19/ask-the-captain-how-far-does-a-jet-fly-during-its-lifetime/1712269/
      planes <- CJ(region=dt$region, period=dt$period, vehicle_type=plane_types, value=2e6,
                   unit=am_unit, unique=TRUE)
      planes <- lstruct[planes, on="vehicle_type", allow.cartesian=TRUE]

      ship_types <- unique(lstruct[grepl("Ship", vehicle_type), vehicle_type])
      ## for international ships we assume 300.000 km/yr
      ## https://www.ioscm.com/blog/industry-facts-101-the-shipping-industry-is-enormous/
      ships <- CJ(region=dt$region, period=dt$period, vehicle_type=ship_types, value=3e5,
                  unit=am_unit, unique=TRUE)
      ## domestic ships one third
      ships[vehicle_type == "Domestic Ship_tmp_vehicletype", value := 1e5]
      ships <- lstruct[ships, on="vehicle_type", allow.cartesian=TRUE]

      dt <- rbind(dt, planes, ships, use.names=TRUE)

      q <- as.quitte(dt)
      if(adjustments) {
        q <- toolAdjustData(q, subtype)
      }

      ## check for missing data by joining the logit structure and looking for NAs,
      ## the join has to happen on subtype level since it does not make sense
      ## slash we do not have data for all categories. E.g., mileage for non-motorized modes or
      ## trains does not make sense
      am_check <- lstruct[!subsector_l3 %in% c("Walk", "Cycle", "HSR", "Passenger Rail", "Freight Rail")]
      q <- q[am_check, on=colnames(am_check)]
      ## note that the actual check is done for all subtypes at the end of the function

      weight <- calcOutput("GDP", aggregate = F)[, unique(q$period), "gdp_SSP2"]
      unit <- am_unit
      description <- "Annual mileage data for LDV, trucks, trains and ships. Sources: TRACCS, UCD."

    }
  )

  tst <- q[is.na(value)]
  if(nrow(tst) > 0) {
    print(sprintf("Missing elements in inputdata subtype %s.", subtype))
    browser()
  }

  tst <- q[duplicated(q)]
  if(nrow(tst) > 0) {
    print(sprintf("Duplicated elements in inputdata subtype %s.", subtype))
    browser()
  }

  return(list(
    x           = as.magpie(as.quitte(as.data.frame(q))),
    weight      = weight,
    unit        = unit,
    description = description
  ))
}

