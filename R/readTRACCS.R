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
#' @importFrom stringr str_extract
readTRACCS <- function(subtype = c("roadFeDemand", "fuelEnDensity", "loadFactor", "annualMileage",
                                   "roadVkmDemand", "roadTkmDemand", "roadPkmDemand", "railFeDemand")) {
  `.` <- iso <- period <- categoryTRACCS <- vehicleType <- technology <- value <- NULL
  subtype <- match.arg(subtype)

  countries <- list.files(
    path = file.path("./TRACCS_ROAD_Final_EXCEL_2013-12-20"),
    all.files = FALSE)
  countries <- gsub("Road Data |_Output.xlsx", "", countries)
  countries <- countries[!grepl("\\$", countries)] #deletes open files, which have a $ in the name
  switch(
    subtype,
    "fuelEnDensity" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          conv <- suppressMessages(data.table(read_excel(
            path = file.path(
              "TRACCS_ROAD_Final_EXCEL_2013-12-20",
              paste0("Road Data ", x, "_Output.xlsx")),
            sheet = "INFO","A32:B38")))
          colnames(conv) <- c("TRACCS_technology", "cf")
          conv[, cf := as.numeric(str_extract(cf, "\\d+\\.\\d+"))]
          conv[, country_name := x]
          return(conv)
        }))

      return(data[
        , .(country_name, TRACCS_technology, unit="TJ/t", cf)] %>%
        as.magpie(spatial=1, temporal=0))
    },
    "roadFeDemand" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(
              "TRACCS_ROAD_Final_EXCEL_2013-12-20",
              paste0("Road Data ", x, "_Output.xlsx")),
            sheet = "FCcalc","A2:I75")))
          colnames(output) <- c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                                2005:2010)
          output <- output[!TRACCS_technology %in% c("All", "Total")]
          output <- data.table::melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[
        , .(country_name, period, TRACCS_category, TRACCS_vehicle_type,
            TRACCS_technology, unit="t", value)] %>%
        as.magpie(spatial=1))
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
            colnames(output) <- c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                                  2005:2010)
            output <- output[!TRACCS_technology %in% c("All", "Total")]
            output <- data.table::melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
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
            colnames(output) <- c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                                  2005:2010)
            output <- output[!TRACCS_technology %in% c("All", "Total")]
            output <- output[!is.na(get("2010"))]
            output <- melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
                      variable.name = "period")
            output$country_name <- x
            return(output)
          })))

      return(data[
        , .(country_name, period, TRACCS_category, TRACCS_vehicle_type,
          TRACCS_technology, unit="person/ton per vehicle", value)] %>%
      as.magpie(spatial=1))
    },
    "annualMileage" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(
              "TRACCS_ROAD_Final_EXCEL_2013-12-20",
              paste0("Road Data ", x, "_Output.xlsx")),
            sheet="Mileage per Veh. (Km)","A2:I74")))
          colnames(output) <- c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                                2005:2010)
          output <- output[!TRACCS_technology %in% c("All", "Total")]
          output <- data.table::melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      mpobj <- data[
        , .(country_name, period, TRACCS_category, TRACCS_vehicle_type,
          TRACCS_technology, unit="km/veh", value)] %>%
        as.magpie(spatial=1)
      return(mpobj)

    },
    "roadVkmDemand" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output = suppressMessages(data.table(
            read_excel(
              path = file.path(
                "TRACCS_ROAD_Final_EXCEL_2013-12-20",
                paste0("Road Data ", x, "_Output.xlsx")),
              sheet="Veh-Km","A2:I73")))
          setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                             as.character(seq(2005,2010,1))))
          output=output[!TRACCS_technology %in% c("Total", "All")]
          output <- data.table::melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[
        , .(country_name, period, TRACCS_category, TRACCS_vehicle_type,
          TRACCS_technology, unit="vkm", value)] %>%
        as.magpie(spatial=1))

    },
    "roadTkmDemand" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output = suppressMessages(data.table(
            read_excel(
              path = file.path(
                "TRACCS_ROAD_Final_EXCEL_2013-12-20",
                paste0("Road Data ", x, "_Output.xlsx")),
              sheet="Tonne-Km", "A2:I18")))
          setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                             as.character(seq(2005,2010,1))))
          output=output[!TRACCS_technology %in% c("Total", "All")]
          output <- data.table::melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[
        , .(country_name, period, TRACCS_category, TRACCS_vehicle_type,
          TRACCS_technology, unit="tkm", value)] %>%
        as.magpie(spatial=1))

    },
    "roadPkmDemand" = {
      data <- rbindlist(lapply(
        countries,
        function(x) {
          output = suppressMessages(data.table(
            read_excel(
              path = file.path(
                "TRACCS_ROAD_Final_EXCEL_2013-12-20",
                paste0("Road Data ", x, "_Output.xlsx")),
              sheet="Pass-Km", "A2:I51")))
          setnames(output, c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology",
                             as.character(seq(2005,2010,1))))
          output=output[!TRACCS_technology %in% c("Total", "All")]
          output <- data.table::melt(output, id.vars = c("TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology"),
                                     variable.name = "period")
          output$country_name <- x
          return(output)
        }))

      return(data[
        , .(country_name, period, TRACCS_category, TRACCS_vehicle_type,
          TRACCS_technology, unit="pkm", value)] %>%
        as.magpie(spatial=1))

    },
    "railFeDemand" = {
      data <- suppressMessages(data.table(read_excel(
        path = "TRACCS_RAIL_Final_EXCEL_2013-12-20/TRACCS_Rail_Final_Eval.xlsx",
        sheet = "eval_rail_energy", "A6:L124")))

      data <- data.table::melt(
        data,
        id.vars = c("RailTraction", "Unit_short", "CountryID", "Country",
                    "Countrytype_short", "RailTrafficType"),
        variable.name = "period")
      setnames(data, c("RailTraction", "Country"), c("TRACCS_technology", "country_name"))
      return(data[!is.na(value)][
        , .(country_name, period, RailTrafficType, TRACCS_technology, unit="Mio kWh or t", value)] %>%
        as.magpie(spatial=1))
    }

  )

}
