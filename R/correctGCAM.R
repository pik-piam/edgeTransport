#' Correct GCAM road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("GCAM", subtype="esDemand")
#' }
#' @author Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom rmndt magpie2dt
correctGCAM <- function(x, subtype) {
  switch(
    subtype,
    "esDemand" = {
      dt <- magpie2dt(x)
      ## faulty HSR data
      dt[year %in% c(2005, 2010) & region == "EU-12" & subsector == "HSR", value := NA]
      dt[region == "EU-12" & subsector == "HSR", value := na.approx(value, x=year),
                  by=c("region", "sector", "subsector", "technology")]
      dt <- dt[year <= 2010 & !(subsector %in% c("road","LDV","bus","4W","2W"))]
      dt <- dt[technology != "Coal"]

      ## missing electric trains
      miss <- CJ(region=dt$region, year=dt$year, sector="trn_freight", Units="million ton-km",
                 subsector="Freight Rail", technology=c("Liquids", "Electric"),
                 unique=TRUE)
      fr_train <- dt[miss, on=c("region", "year", "sector", "subsector", "technology", "Units")]
      fr_train[is.na(value), value := 0]
      dt <- rbind(dt[subsector != "Freight Rail"], fr_train)
      x <- as.magpie(as.data.frame(dt), temporal=2, spatial=1)
    })

  return(x)
}
