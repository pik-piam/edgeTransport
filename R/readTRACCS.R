#' Read TRACCS road transportation data.
#'
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
#' @importFrom magclass setComment
readTRACCS <- function(subtype = c("roadDemand", "loadFactor", "mileage", "energyIntensity", "railDemand")) {
  `.` <- iso <- period <- categoryTRACCS <- vehicleType <- technology <- value <- NULL
  subtype <- match.arg(subtype)

  isomap <- as.data.table(toolGetMapping("regionmappingTRACCS.csv", type = "regional"))

  countries <- list.files(
    path = file.path("./TRACCS_ROAD_Final_EXCEL_2013-12-20"),
    all.files = FALSE)
  countries <- gsub("Road Data |_Output.xlsx", "", countries)
  countries <- countries[!grepl("\\$", countries)] #deletes open files, which have a $ in the name
  switch(
    subtype,
    "roadDemand" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(
              "TRACCS_ROAD_Final_EXCEL_2013-12-20",
              paste0("Road Data ", x, "_Output.xlsx")),
            sheet = "FCcalc","A2:I75")))
          colnames(output) <- c("categoryTRACCS", "vehicleType", "technology",
                                2005:2010)
          output <- output[!technology %in% c("All", "Total")]
          output <- data.table::melt(output, id.vars = c("categoryTRACCS", "vehicleType", "technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[isomap, on = "country_name"][
        , .(iso, period, categoryTRACCS, vehicleType, technology, value)] %>%
        as.magpie() %>%
        setComment("unit: t"))
    },
    "loadFactor" = {
      data <- rbind(
        rbindlist(lapply(
          countries,
          function(x) {
            output <- suppressMessages(data.table(read_excel(
              path = file.path(
                "TRACCS_ROAD_Final_EXCEL_2013-12-20",
                paste0("Road Data ", x, "_Output.xlsx")),
              sheet = "Occupancy ratio","A2:I51")))
            colnames(output) <- c("categoryTRACCS", "vehicleType", "technology",
                                  2005:2010)
            output <- output[!technology %in% c("All", "Total")]
            output <- data.table::melt(output, id.vars = c("categoryTRACCS", "vehicleType", "technology"),
                                       variable.name = "period")
            output$country_name <- x
            return(output)
          })),
        rbindlist(lapply(
          countries,
          function(x) {
            output <- suppressMessages(data.table(read_excel(
              path = file.path(
                "TRACCS_ROAD_Final_EXCEL_2013-12-20",
                paste0("Road Data ", x, "_Output.xlsx")),
              sheet = "Tonne-Km", "A3:AA18")))
            output <- output[, c(1:3, 22:27)]
            colnames(output) <- c("categoryTRACCS", "vehicleType", "technology",
                                  2005:2010)
            output <- output[!technology %in% c("All", "Total")]
            output <- output[!is.na(get("2010"))]
            output <- melt(output, id.vars = c("categoryTRACCS", "vehicleType", "technology"),
                      variable.name = "period")
            output$country_name <- x
            return(output)
          })))

      return(data[isomap, on = "country_name"][
      , .(iso, period, categoryTRACCS, vehicleType, technology, value)] %>%
      as.magpie() %>%
      setComment("unit: person/ton per vehicle"))
    },
    "mileage" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(
              "TRACCS_ROAD_Final_EXCEL_2013-12-20",
              paste0("Road Data ", x, "_Output.xlsx")),
            sheet="Mileage per Veh. (Km)","A2:I51")))
          colnames(output) <- c("categoryTRACCS", "vehicleType", "technology",
                                2005:2010)
          output <- output[!technology %in% c("All", "Total")]
          output <- data.table::melt(output, id.vars = c("categoryTRACCS", "vehicleType", "technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[isomap, on = "country_name"][
        , .(iso, period, categoryTRACCS, vehicleType, technology, value)] %>%
        as.magpie() %>%
        setComment("unit: km per vehicle and year"))

    },
    "energyIntensity" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(
              "TRACCS_ROAD_Final_EXCEL_2013-12-20",
              paste0("Road Data ", x, "_Output.xlsx")),
            sheet = "Energy_intensity_MJ_km","A2:O75")))
          output <- output[, c(1, 2, 3, 10, 11, 12, 13, 14, 15)]
          colnames(output) <- c("categoryTRACCS", "vehicleType", "technology",
                                2005:2010)
          output <- output[!technology %in% c("All", "Total", "Other")]
          output <- data.table::melt(output, id.vars = c("categoryTRACCS", "vehicleType", "technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[isomap, on = "country_name"][
        , .(iso, period, categoryTRACCS, vehicleType, technology, value)] %>%
        as.magpie() %>%
        setComment("unit: MJ/km"))
    },
    "railDemand" = {
      data <- suppressMessages(data.table(read_excel(
        path = "TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx",
        sheet = "eval_rail_energy", "A6:L124")))

      data <- data.table::melt(
        data,
        id.vars = c("RailTraction", "Unit_short", "CountryID", "Country",
                    "Countrytype_short", "RailTrafficType"),
        variable.name = "period")
      setnames(data, c("RailTraction", "Country"), c("technology", "country_name"))
      return(data[isomap, on = "country_name"][!is.na(value)][
      , .(iso, period, RailTrafficType, technology, value)] %>%
        as.magpie() %>%
        setComment("unit(Electric): Mio kWh, unit(Diesel): t"))
    }

  )

}
