#' Load fuel prices from a REMIND fulldata.gdx in [US$2017/MJ] and map them on to
#' the edgeTransport decision tree. The output is provided in the same spatial resolution
#' as the transferred gdx file and the temporal resolution is set according to the param yrs.
#'
#' @param gdxPath path to REMIND fulldata.gdx
#' @param hybridElecShare Share of electricity in Hybrid electric vehicles
#' @param helpers list with helpers
#' @returns fuel costs on technology level
#' @import data.table
#' @importFrom rmndt approx_dt magpie2dt
#' @importFrom gdx readGDX
#' @importFrom magclass lowpass
#' @importFrom magrittr `%>%`

toolLoadREMINDfuelCosts <- function(gdxPath, hybridElecShare, helpers){
 value <- unit <- variable <- `Hybrid electric` <- fuel <- NULL

   mapEdgeToREMIND <- unique(helpers$mapEdgeToREMIND[, c("all_enty", "univocalName", "technology")])
   # active modes do not feed into all_enty
   mapEdgeToREMIND <- mapEdgeToREMIND[!is.na(all_enty)]
   decisionTree <- copy(helpers$decisionTree)

   # load prices from REMIND gdx
   fuelCosts <- readGDX(gdxPath, "pm_FEPrice", format = "first_found", restore_zeros = FALSE)[,, "trans.ES", pmatch = TRUE]
   ## smooth prices from REMIND gdx (over years) and convert to data.table
   fuelCosts <- fuelCosts %>% lowpass() %>% magpie2dt()
   setnames(fuelCosts, c("all_regi", "ttot"), c("region", "period"))
   fuelCosts <- fuelCosts[, c("region", "period", "all_enty", "value")]
   # convert from TerraUS$2017 per TWyear to US$2017 per EJ
   tdptwyr2dpgj <- 31.71  # TerraDollar per TWyear to Dollar per GJ
   GJtoMJ <- 1e-3 # dollar per GJ to dollar per MJ
   fuelCosts[, value := value * tdptwyr2dpgj * GJtoMJ] # US$2017/MJ
   # map on EDGE-T structure
   fuelCosts <- merge(fuelCosts, mapEdgeToREMIND, by = "all_enty", all.y = TRUE, allow.cartesian = TRUE)[, all_enty := NULL]
   # calculate price for hybrids
   dummy <- fuelCosts[univocalName %in% helpers$filterEntries$trn_pass_road_LDV_4W & technology %in% c("BEV", "Liquids")]
   fuelCostsHybrids <- dummy %>%
     dcast(... ~ technology) %>%
     .[, `Hybrid electric` := (1 - hybridElecShare) * Liquids + hybridElecShare * BEV] %>%
     melt(id.vars = c("region", "period", "univocalName"), variable.name = "technology")
   fuelCosts <- rbind(fuelCosts, fuelCostsHybrids[technology == "Hybrid electric"])

   # corrections to the data
   # prices before 2020 are often not plausible -> choose 2025 as a start date if previous years are provided
   test <- fuelCosts[period > 2020 & value <= 0.001]
   fuelCosts <- fuelCosts[period > 2020 & value >= 0.001]
   if(nrow(test)){
     print(paste("Fuel prices lower than 1$/GJ found. Regions:", paste(unique(test$region), collapse = ", ")))
     print("Values are filtered out and are interpolated from other timesteps.")
   }

   fuelCosts[, variable := "Fuel costs"][, unit := "US$2017/MJ"]

   # get right temporal resolution
   fuelCosts <- toolApplyMixedTimeRes(fuelCosts, helpers)

   if (anyNA(fuelCosts) == TRUE) {
     stop("Fuel costs contain NAs")
   }

 return(fuelCosts)
}
