#' Function to rename demand scenarios between REMIND and EDGE-Transport naming conventions
#' It is used at the interfaces REMIND-EDGETiterative, EDGETiterative-REMIND
#'
#' @param demScen demand scenario,
#'                if demScen is in mapping it will be translated otherwise returned unchanged
#' @param transportPolScen transport policy scenario as it is used in EDGET
#' @param direction one of two labels indicating the direction of the translation
#' @returns demScen in the desired model name logic
#' @author Alex K. Hagen
#' @import data.table

toolTranslateDemScen <- function(demScen,
                             transportPolScen = NULL,
                             direction = c("EDGEtoREMIND", "REMINDtoEDGE")) {

  direction <- match.arg(direction, c("EDGEtoREMIND", "REMINDtoEDGE"))

  # ------------------------------------------------------------
  # Load mapping
  # ------------------------------------------------------------
  mapping <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDdemScen.csv",
                               package = "edgeTransport", mustWork = TRUE), header = TRUE)

  # ------------------------------------------------------------
  # Validate demScen
  # ------------------------------------------------------------
  valid_demScens <- if (direction == "EDGEtoREMIND") {
    unique(mapping$DEM_edge)
  } else {
    unique(mapping$DEM_remind)
  }

  if (!demScen %in% valid_demScens) {
    print(paste0("demScen ", demScen,
                 " was adopted without translation"))
    return(demScen)
  }

  # ------------------------------------------------------------
  # Derive & validate EDGE_scenario if needed
  # ------------------------------------------------------------
  if (!is.null(transportPolScen) &&
      !transportPolScen %in% unique(mapping$EDGE_scenario)) {
    print(paste0("demScen ", demScen,
                " was adopted without translation"))
    return(demScen)
  }

  # ------------------------------------------------------------
  # Perform lookup
  # ------------------------------------------------------------
  if (direction == "EDGEtoREMIND") {

    if (!is.null(transportPolScen)) {
      hit <- mapping$DEM_remind[
        mapping$DEM_edge == demScen &
          mapping$EDGE_scenario == transportPolScen
      ]
    } else {
      hit <- mapping$DEM_remind[
        mapping$DEM_edge == demScen
      ]
    }

  } else {  # REMINDtoEDGE
    hit <- mapping$DEM_edge[
      mapping$DEM_remind == demScen
    ]
  }

  hit <- unique(hit)

  # ------------------------------------------------------------
  # Check if proper unique match was found
  # ------------------------------------------------------------
  if (length(hit) == 0) {
    print(paste0("demScen ", demScen,
                   " was adopted without translation"))
    return(demScen)
  }

  if (length(hit) > 1) {
    stop(
      "Ambiguous mapping for demScen '", demScen, "' from ", direction, ". ",
      "Please supply transportPolScen in toolTranslateDemScen."
    )
  }

  print(paste0("demScen ", demScen,
                 " was translated to ", hit))
  return(hit)

}
