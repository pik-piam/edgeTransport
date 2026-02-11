#' Function to rename scenarios between REMIND and EDGE-Transport naming conventions
#' It is used at the interfaces REMIND-EDGETiterative, EDGETiterative-REMIND
#' The same translations have to be done in mrremind readEDGETransport for REMIND input data generation
#'
#' @param demScen demand scenario,
#'                if demScen is in mapping it will be translated otherwise returned unchanged
#' @param transportPolScen transport policy scenario
#'                if transportPolScen is in mapping it will be translated otherwise returned unchanged
#' @param direction one of two labels indicating the direction of the translation
#' @returns list of translated demScen and transportPolScen in the desired model name logic
#' @author Alex K. Hagen
#' @import data.table

toolTranslateTransportScenario <- function(demScen,
                                           transportPolScen,
                                           direction = c("EDGEtoREMIND", "REMINDtoEDGE")) {

  DEM_edge <- EDGE_scenario <- DEM_remind <- EDGEtr_scen_remind <- NULL

  direction <- match.arg(direction, c("EDGEtoREMIND", "REMINDtoEDGE"))

  # ------------------------------------------------------------
  # Load mapping
  # ------------------------------------------------------------
  scenarioMapping <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDscen.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # ------------------------------------------------------------
  # Determine valid inputs based on direction
  # ------------------------------------------------------------
  if (direction == "EDGEtoREMIND") {
    validDemScens <- unique(scenarioMapping$DEM_edge)
    validTransPolScens <- unique(scenarioMapping$EDGE_scenario)
  } else {
    validDemScens <- unique(scenarioMapping$DEM_remind)
    validTransPolScens <- unique(scenarioMapping$EDGEtr_scen_remind)
  }

  # ------------------------------------------------------------
  # Validate demScen
  # ------------------------------------------------------------
  if (!demScen %in% validDemScens) {
    message("demScen ", demScen, " was adopted without translation")
    return(list(demScen = demScen, transportPolScen = transportPolScen))
  }

  # ------------------------------------------------------------
  # Validate transportPolScen (if supplied)
  # ------------------------------------------------------------
  if (!transportPolScen %in% validTransPolScens) {
    message("transportPolScen ", transportPolScen, " was adopted without translation")
    return(list(demScen = demScen, transportPolScen = transportPolScen))
  }

  # ------------------------------------------------------------
  # Perform lookup
  # ------------------------------------------------------------
  if (direction == "EDGEtoREMIND") {
    # Optional filtering by transport scenario
    scenSelection <- scenarioMapping[
      DEM_edge == demScen & EDGE_scenario == transportPolScen
    ]

    translatedDemScen <- unique(scenSelection$DEM_remind)
    translatedPolScen <- unique(scenSelection$EDGEtr_scen_remind)

  } else { # REMINDtoEDGE

    scenSelection <- scenarioMapping[
      DEM_remind == demScen & EDGEtr_scen_remind == transportPolScen
    ]

    translatedDemScen <- unique(scenSelection$DEM_edge)
    translatedPolScen <- unique(scenSelection$EDGE_scenario)
  }

  # ------------------------------------------------------------
  # Handle missing or ambiguous matches
  # ------------------------------------------------------------
  if (length(translatedDemScen) == 0) {
    message("demScen ", demScen, " was adopted without translation")
    translatedDemScen <- demScen
  } else if (length(translatedDemScen) > 1) {
    stop("Ambiguous mapping for demScen '", demScen, "' in direction ",
         direction, " in toolTranslateTransportScenario.")
  }

  if (length(translatedPolScen) == 0) {
    message("transportPolScen ", transportPolScen, " was adopted without translation")
    translatedPolScen <- transportPolScen
  } else if (length(translatedPolScen) > 1) {
    stop("Ambiguous mapping for transportPolScen '", transportPolScen, "' in direction ",
         direction, ", in toolTranslateTransportScenario.")
  }

  # ------------------------------------------------------------
  # Return results
  # ------------------------------------------------------------
  # add logging/console output of translation results
  demInfo   <- sprintf("%-14s %-20s %-4s %-20s", "demScen:", demScen, "->", translatedDemScen)
  polInfo   <- sprintf("%-14s %-20s %-4s %-20s", "transportPol:", transportPolScen, "->", translatedPolScen)

  message(
    "\nTransport scenario translation ", direction, ":\n\n",
    demInfo, "\n",
    polInfo, "\n"
  )

  return(list(
    demScen = translatedDemScen,
    transportPolScen = translatedPolScen
  ))
}
