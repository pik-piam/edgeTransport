#' Convert TRACCS road transportation data to iso country.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("TRACCS")
#' }
#' @author Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @importFrom readxl read_xlsx
#' @importFrom data.table as.data.table
#' @importFrom madrat toolGetMapping toolCountryFill getIsoList toolCountry2isocode
convertTRACCS <- function(x, subtype) {
  getItems(x, dim=1) <- toolCountry2isocode(getItems(x, dim=1))
  getSets(x)["d1.1"] <- "iso"
  x <- toolCountryFill(x, fill = NA, countrylist = getISOlist(), verbosity = 2)
  return(x)
}

