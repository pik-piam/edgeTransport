#' Read GCAM road transportation data.
#'
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
#' @importFrom data.table fread
#' @importFrom magclass as.magpie
readGCAM <- function(subtype = c(
                      "feVkmIntensity", "esDemand", "loadFactor", "speedMotorized",
                      "speedNonMotorized", "valueOfTime")) {
  switch(
    subtype,
    "feVkmIntensity" = {
      dt <- fread("L254.StubTranTechCoef.csv", skip=4)[, market.name := NULL]
      setnames(dt, gsub(".", "_", colnames(dt), fixed=TRUE))
      mp <- as.magpie(as.data.frame(dt), temporal=5, spatial=1)
    },
    "loadFactor" = {
      dt <- fread("L254.StubTranTechLoadFactor.csv", skip=4)
      setnames(dt, gsub(".", "_", colnames(dt), fixed=TRUE))
      mp <- as.magpie(as.data.frame(dt), temporal=5, spatial=1)
    },
    "esDemand" = {
      dt <- fread("tech_output.csv", skip = 1, sep=";", header = T) %>%
        melt(measure.vars=6:26, variable.name = "year")
      dt[, scenario := NULL]
      mp <- as.magpie(as.data.frame(dt), temporal=6, spatial=1)
    },
    "speedMotorized" = {
      dt <- unique(fread("L254.tranSubsectorSpeed.csv", skip=4))
      mp <- as.magpie(as.data.frame(dt), temporal=4, spatial=1, datacol=5)
    },
    "speedNonMotorized" = {
      dt <- fread("A54.globaltech_nonmotor.csv", skip=1, header=T)
      setnames(dt, gsub(".", "_", colnames(dt), fixed=TRUE))
      dt[, share_weight := NULL]
      mp <- as.magpie(dt, datacol=3)
    },
    "valueOfTime" = {
      dt = fread("A54.tranSubsector_VOTT.csv", skip = 1)[!grepl("#", supplysector)] %>%
        unique() %>%
        setnames(gsub(".", "_", colnames(.), fixed=TRUE))
      dt[, speed_source := NULL]
      dt <- melt(dt, id.vars=c("supplysector", "tranSubsector"), na.rm = TRUE)
      dt[, value := as.numeric(value)]
      mp <- as.magpie(dt)
    }
  )

  return(mp)
}
