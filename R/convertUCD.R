#' Convert UCD road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("UCD")
#' }
#' @author Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom data.table fread
#' @importFrom madrat toolAggregate
convertUCD <- function(x, subtype) {
  ## UCD2iso <- fread("mapping_UCDdb_ISO.csv")
  ## UCD2iso[UCD_region == "Australia NZ", UCD_region := "Australia_NZ"]
  UCD2iso <- fread(system.file("extdata/iso_UCD.csv", package="edgeTransport"))
  gdp <- calcOutput("GDP", aggregate=FALSE)[, getYears(x),  "gdp_SSP2"]

  if(subtype == "feDemand") {
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)    
  } else {
    x <- toolAggregate(x, rel = UCD2iso)    
  }
  getSets(x)["d1.1"] <- "iso"
  return(x)
  }
