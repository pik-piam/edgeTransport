#' Correct GCAM road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("GCAM", subtype="histEsDemand")
#' }
#' @author Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom rmndt magpie2dt
correctGCAM <- function(x, subtype) {
  switch(
    subtype,
    "histEsDemand" = {
      dt <- magpie2dt(x)
      ## HSR data decreases significantly in 2005 and falls to zero in 2010 -> that is not right and needs to be corrected
      #linear interpolation from first value in 1990 to value in 2015
      dt[year %in% c(2005, 2010) & region == "EU-12" & subsector == "HSR", value := NA]
      dt[region == "EU-12" & subsector == "HSR", value := na.approx(value, x = year),
                  by = c("region", "sector", "subsector", "technology")]

      ## Electric trains do not exist in certain countries and need to be listed as zero demand
      miss <- CJ(region = dt$region, year = dt$year, sector = "trn_freight", Units = "million ton-km",
                 subsector = "Freight Rail", technology = "Electric",
                 unique = TRUE)
      fr_train <- dt[miss, on = c("region", "year", "sector", "subsector", "technology", "Units")]
      fr_train[is.na(value), value := 0]
      dt <- rbind(dt[!(subsector == "Freight Rail" & technology == "Electric")], fr_train)
      x <- as.magpie(as.data.frame(dt), temporal = 2, spatial = 1)
    })

  return(x)
}
