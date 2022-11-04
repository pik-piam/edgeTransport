#' Read UCD road transportation data.
#'
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
#' @importFrom magclass as.magpie
readUCD <- function(subtype = c(
                      "feVkmIntensity", "feDemand", "loadFactor", "annualMileage",
                      "nonMotorizedDemand", "speed", "costs")) {
  variable <- NULL
  db <- fread("UCD_transportation_database.csv", header = TRUE)
  switch(
    subtype,
    "feVkmIntensity" = {
      ret <- db[variable == "intensity"][, variable := NULL]
    },
    "feDemand" = {
      ret <- db[variable == "energy"][, variable := NULL]
    },
    "loadFactor" = {
      ret <- db[variable == "load factor"][, variable := NULL]
    },
    "annualMileage" = {
      ret <- db[variable == "annual travel per vehicle"][, variable := NULL]
    },
    "nonMotorizedDemand" = {
      ret <- db[variable == "service output"][, variable := NULL]
    },
    "speed" = {
      ret <- db[variable == "speed"][, variable := NULL]
    },
    "costs" = {
      ret <- db[grepl("CAPEX|OPEX|costs|subsidy", variable)]
    }
  )
  ret <- data.table::melt(ret, id.vars=colnames(ret)[1:7], variable.name="year")
  setnames(ret, "size.class", "size_class")
  return(as.magpie(ret, spatial="UCD_region"))

}
