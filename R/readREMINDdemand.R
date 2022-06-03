#' Loads the total demand of transport CES nodes from last iteration
#'
#' @param gdx REMIND binary output, gdx format
#' @param REMINDmapping map REMIND regions to ISO3 country codes
#' @param EDGE2teESmap map EDGE-T technologies to REMIND ES techs
#' @param years REMIND timesteps
#' @param scenario REMIND SSP scenario
#' @import data.table
#' @export


readREMINDdemand= function(gdx, REMINDmapping,EDGE2teESmap, years, scenario){
  `.` <- region <- EDGE_top <- value <- demand <- NULL

  dem <- readGDX(gdx, c("vm_cesIO"),field = "l")
  dem <- dem[, , c("entrp_pass_sm","entrp_pass_lo","entrp_frgt_sm","entrp_frgt_lo")]

  dem <- magpie2dt(dem, regioncol = "region",
                   yearcol = "year", datacols = "sector_remind")
  ## downscale to iso level
  dem <- dem[year %in% years]

  ## attribute EDGE sector names to CES values
  dem <- merge(x=dem, y=unique(EDGE2teESmap[,c("all_in","EDGE_top")]), all = TRUE, by.x = "sector_remind", by.y = "all_in")
  dem <- dem[,.(region, year, sector = EDGE_top, demand = value)]

  ## find right units
  dem[, demand := demand  ## in trillion pkm or tkm
                  *1e6]   ## in million pkm or tkm

  return(dem)
}
