#' Load fuel prices from a REMIND fulldata.gdx in [US$2005/MJ] and map them on to the edgeTransport decision tree.
#' The output is provided in the same spatial resolution as the transferred gdx file and the temporal resolution is set according to the param yrs.
#'
#' @param gdxPath path to REMIND fulldata.gdx
#' @param yrs requested temporal resolution
#'
#' @import data.table
#' @importFrom rmndt approx_dt magpie2dt
#' @importFrom gdx readGDX
#' @importFrom magclass lowpass
#' @importFrom magrittr `%>%`

toolLoadREMINDfuelCosts <- function(gdxPath, yrs){
 value <- unit <- variable <- `Hybrid electric` <- fuel <- NULL

 mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv", package = "edgeTransport", mustWork = TRUE))
 mapEdgeToREMIND <- unique(mapEdgeToREMIND[, c("all_enty", "fuel")])

 # load prices from REMIND gdx
 fuelCosts <- readGDX(gdxPath, "pm_FEPrice", format = "first_found", restore_zeros = FALSE)[,, "trans.ES", pmatch = TRUE]
 ## smooth prices from REMIND gdx (over years) and convert to data.table
 fuelCosts <- fuelCosts %>% lowpass() %>% magpie2dt()
 setnames(fuelCosts, c("all_regi", "ttot"), c("region", "period"))
 fuelCosts <- fuelCosts[, c("region", "period", "all_enty", "value")]
 # convert from TerraUS2005$ per TWyear to US2005$ per EJ
 tdptwyr2dpgj <- 31.71  # TerraDollar per TWyear to Dollar per GJ
 GJtoMJ <- 1e-3 # dollar per GJ to dollar per MJ
 fuelCosts[, value := value * tdptwyr2dpgj * GJtoMJ] # US$2005/MJ
 # map on EDGE-T structure
 fuelCosts <- merge(fuelCosts, mapEdgeToREMIND, by = "all_enty")[, all_enty := NULL]
 #Average over diesel and gasoline
 fuelCosts <- fuelCosts[, .(value = mean(value)), by = c("region", "period", "fuel")]
 # calculate price for hybrids
 fuelCosts <- fuelCosts %>%
   dcast(... ~ fuel) %>%
   .[, `Hybrid electric` := 0.6 * Liquids + 0.4 * Electricity] %>%
   melt(id.vars = c("region", "period"), variable.name = "fuel")

 # map on decisiontree for provided spatial resolution
 if (length(unique(fuelCosts$region)) == 21) {
   decisionTree <- toolLoadDecisionTree("regionCode21")
 } else if (length(unique(fuelCosts$region)) == 12) {
   decisionTree <- toolLoadDecisionTree("regionCode12")
 } else {
   decisionTree <- toolLoadDecisionTree("iso")
 }

 # fuel needs to be mapped on different technology names
 decisionTree[, fuel := ifelse(technology %in% c("BEV", "Electric"), "Electricity", technology)]
 decisionTree[, fuel := ifelse(technology == "FCEV", "Hydrogen", fuel)]
 decisionTree <- decisionTree[!technology %in% c("Cycle_tmp_technology", "Walk_tmp_technology")]
 decisionTree <- unique(decisionTree[, c("region", "univocalName", "technology", "fuel")])

 fuelCosts <- merge(fuelCosts, decisionTree, by = c("fuel", "region"), allow.cartesian = TRUE, all.y = TRUE)[, fuel := NULL]

 # corrections to the data
 # prices before 2020 are often not plausible -> choose 2020 as a start date if previous years are provided
 fuelCosts[period >= 2020]
 test <- fuelCosts[value <= 0.001]
 if(nrow(test)){
   print(paste("Fuel prices lower than 1$/GJ found. Regions:", paste(unique(test$region), collapse = ", ")))
   print("The weighted averages of non-zero regions of the corresponding fuel will be used.")
   fuelCosts[, value := ifelse(value <= 0.001, mean(value[value > 0.001]), value), by = c("period", "technology")]
 }

 fuelCosts[, variable := "Fuel costs"][, unit := "US$2005/MJ"]
 # adjust to temporal resolution
 fuelCosts <- approx_dt(fuelCosts, yrs, "period", "value",
                c("region", "univocalName", "technology",
                  "variable", "unit"), extrapolate = TRUE)
 setkey(fuelCosts, region, univocalName, technology, period)

 if (anyNA(fuelCosts) == TRUE) {
   stop("Fuel prices contain NAs")
 }

 return(list(
   fuelCosts = fuelCosts))
}
