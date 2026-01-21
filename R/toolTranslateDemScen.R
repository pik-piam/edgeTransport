#' Function to rename demand scenarios between REMIND and EDGE-Transport naming conventions
#' It is used at the interfaces REMIND-EDGETiterative, EDGETiterative-REMIND
#' 
#'
#' @param demScen demand scenario, to be renamed 
#' @param transportPolScen transport policy scenario as it is used in EDGET
#' @param isICEban boolean indicationg if ICEban is on or off
#' @param direction one of two labels indicating the direction of the translation
#' @returns demScen in the desired model name logic
#' @author Alex K. Hagen
#' @import data.table

toolTranslateDemScen <- function(demScen,
                             transportPolScen = NULL,
                             isICEban = NULL,
                             direction = c("EDGEtoREMIND", "REMINDtoEDGE")) {
  
  direction <- match.arg(direction)
  
  # ------------------------------------------------------------
  # Load mapping
  # ------------------------------------------------------------
  mapping <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDdemScen.csv",
                               package = "edgeTransport", mustWork = TRUE), header = TRUE)
  
  # ------------------------------------------------------------
  # Derive EDGE_scenario if needed
  # ------------------------------------------------------------
  EDGE_scenario <- transportPolScen
  
  if (!is.null(transportPolScen)) {
    
    if (!transportPolScen %in% unique(mapping$EDGE_scenario)) {
      if (!paste0(transportPolScen, "ICEban") %in% mapping$EDGE_scenario) {
        stop("Unknown transportPolScen: '", transportPolScen, "'.")
      }
    }
    
    if (isTRUE(isICEban)) {
      EDGE_scenario <- paste0(EDGE_scenario, "ICEban")
    }
  }
  
  # ------------------------------------------------------------
  # Validate EDGE_scenario
  # ------------------------------------------------------------
  if (!is.null(EDGE_scenario) &&
      !EDGE_scenario %in% unique(mapping$EDGE_scenario)) {
    stop("Unknown EDGE_scenario: '", EDGE_scenario, "'.")
  }
  
  # ------------------------------------------------------------
  # Validate demScen
  # ------------------------------------------------------------
  valid_demScens <- if (direction == "EDGEtoREMIND") {
    unique(mapping$DEM_edge)
  } else {
    unique(mapping$DEM_remind)
  }
  
  if (!demScen %in% valid_demScens) {
    stop(
      "Unknown demScen '", demScen, "'. Valid values are:\n  ",
      paste(valid_demScens, collapse = ", ")
    )
  }
  
  # ------------------------------------------------------------
  # Perform lookup
  # ------------------------------------------------------------
  if (direction == "EDGEtoREMIND") {
    
    if (!is.null(EDGE_scenario)) {
      hit <- mapping$DEM_remind[
        mapping$DEM_edge == demScen &
          mapping$EDGE_scenario == EDGE_scenario
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
  # Error case if no match
  # ------------------------------------------------------------
  if (length(hit) == 0) {
    stop("No mapping found for demScen '", demScen, "'.")
  }
  
  if (length(hit) > 1) {
    stop(
      "Ambiguous mapping for demScen '", demScen, "'. ",
      "Please supply EDGE_scenario or transportPolScen + isICEban."
    )
  }
  
  return(hit)
}
