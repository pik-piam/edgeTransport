#' EDGE-Transport iterative
#'
#' Run in the REMIND output folder.
#'
#' @md
#' @param reporting generate EDGE-T reporting data
#' @return NULL
#' @author Alois Dirnaichner
#' @importFrom data.table freadtraceback()
#' @importFrom gdxrrw rgdx
#' @export


toolIterativeEDGETransport <- function() {

  print(paste("---", Sys.time(), "Start of the EDGE-T iterative model run."))

  #############################################################
  ## Settings
  #############################################################
  # Set paths to folders
  edgeTransportFolder <- "EDGE-T"
  edgeTransportPath <- function(fname) {
    file.path(edgeTransportFolder, fname)
  }
  REMINDpath <- function(fname) {
    file.path("../../", fname)
  }

  # Set gdx
  gdx <- "input.gdx"
  if(file.exists("fulldata.gdx"))
    gdx <- "fulldata.gdx"

  # Load config
  load("config.Rdata")
    SSPscen <- cfg$gms$cm_GDPscen
    transportPolScen <- cfg$gms$cm_EDGEtr_scen
    demScen <- cfg$gms$cm_demScen

  #############################################################
  ## Load input data
  #############################################################
  inputFolder = paste0("./")
  # share of electricity in hybrid electric vehicles
  hybridElecShare <- 0.4
  numberOfRegions <- lenght(readGDX(gdx, "all_regi"))
  iterationNumber <- as.vector(rgdx(gdx, list(name = "o_iterationNumber"))$val)

  inputs <- toolLoadIterativeInputs(edgeTransportFolder, numberOfRegions, SSPscenario,
                                       transportPolScenario, demScenario = NULL)

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  RDSinputs <- inputs$RDSinputs

  # Data from previous REMIND iteration
  ## Load REMIND energy service demand
  REMINDsectorESdemand <- toolLoadREMINDsectorESdemand(gdx)
  ## Load REMIND fuel cost
  REMINDfuelCost <- toolLoadREMINDfuelCosts(gdx, hybridElecShare, helpers)

  if (file.exists(edgeTransportPath(" REMINDfuelCostIterations.RDS"))) {
    REMINDfuelCostIterations <- readRDS(edgeTransportPath(" REMINDfuelCostIterations.RDS"))
  }
  REMINDfuelCostIterations <- rbind(REMINDfuelCostIterations, copy(REMINDfuelCost)[, iteration := iterationNumber])
  averagePricesOverIterations <- TRUE
  if(averagePricesOverIterations) {
    if(max(unique(REMINDfuelCostIterations$iteration)) >= 20 &
       max(unique(REMINDfuelCostIterations$iteration)) <= 30)

      ## apply moving avg
      byCols <- names(REMINDfuelCostIterations)
      byCols <- byCols[!byCols %in% c("value", "iteration")]
      REMINDfuelCost <- copy(REMINDfuelCostIterations[iteration >= 20])
      REMINDfuelCost[, .(value = mean(value)), by = ..byCols]
  }
  combinedCAPEXandOPEX <- rbind(inputs$CAPEXandNonFuelOPEX, REMINDfuelCost)

  # Data from previous EDGE-T iteration
  fleetVehiclesPerTech <- NULL
  if (file.exists(edgeTransportPath("fleetVehiclesPerTech.RDS"))) {
    fleetVehiclesPerTech <- readRDS(edgeTransportPath("fleetVehiclesPerTech.RDS"))
  }

  inputData <- list(
    prefTrends = RDSinputs$scenSpecPrefTrends,
    loadFactor = RDSinputs$scenSpecLoadFactor,
    enIntensity = RDSinputs$scenSpecEnIntensity,
    initialIncoCosts = RDSinputs$initialIncoCosts,
    annualMileage = RDSinputs$annualMileage,
    timeValueCosts = RDSinputs$timeValueCosts,
    combinedCAPEXandOPEX = combinedCAPEXandOPEX,
    REMINDsectorESdemand = REMINDsectorESdemand
  )
  ########################################################
  ## Prepare data for
  ## endogenous costs update
  ########################################################

  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(inputData, genModelPar$lambdasDiscreteChoice, policyStartYear, helpers)
  vehicleDepreciationFactors <- toolCalculateVehicleDepreciationFactors(genModelPar$annuityCalc, helpers)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------
  storeEndogenousCostsIterations <- list()
  if (file.exists(edgeTransportPath("storeEndogenousCostsIterations.RDS"))) {
    storeEndogenousCostsIterations <- readRDS(edgeTransportPath("storeEndogenousCostsIterations.RDS"))
  }
  storeFleetSizeAndCompositionIterations <- list()
  if (file.exists(edgeTransportPath("storeFleetSizeAndCompositionIterations.RDS"))) {
    storeFleetSizeAndCompositionIterations <- readRDS(edgeTransportPath("storeFleetSizeAndCompositionIterations.RDS"))
  }

    #################################################
    ## Cost module
    #################################################
    # provide endogenous updates to cost components -----------
    # number of vehicles changes in the vehicle stock module and serves as new input for endogenous cost update
    endogenousCosts <- toolUpdateEndogenousCosts(dataEndogenousCosts, vehicleDepreciationFactors, scenModelPar$scenParIncoCost,
                                                 policyStartYear, inputData$timeValueCosts, inputData$prefTrends, genModelPar$lambdasDiscreteChoice,
                                                 helpers, years, fleetVehiclesPerTech)
    storeEndogenousCostsIterations[[iteration]] <- copy(endogenousCosts)
    lapply(storeEndogenousCostsIterations[[iteration]], function(dt) dt[, variable := paste0(variable, "|Iteration ", iteration)])
    print("Endogenous updates to cost components finished")
    #################################################
    ## Discrete choice module
    #################################################
    # calculate vehicle sales shares and mode shares for all levels of the decisionTree
    vehSalesAndModeShares <- toolDiscreteChoice(inputData, genModelPar, endogenousCosts$updatedEndogenousCosts, years, helpers)

    fuelVehicleESdemand <- toolCalculateFVdemand(inputData$REMINDsectorESdemand, vehSalesAndModeShares, helpers)
    print("Calculation of vehicle sales and mode shares finished")
    #################################################
    ## Vehicle stock module
    #################################################
    # Calculate vehicle stock for cars, trucks and busses -------
    fleetSizeAndComposition <- toolCalculateFleetComposition(fuelVehicleESdemand, vehicleDepreciationFactors,vehSalesAndModeShares,
                                                             inputData$annualMileage, inputData$loadFactor, helpers)
    fleetVehiclesPerTech <- fleetSizeAndComposition$fleetVehiclesPerTech
    storeFleetSizeAndCompositionIterations[[iteration]] <- copy(fleetSizeAndComposition$fleetVehNumbers)
    storeFleetSizeAndCompositionIterations[[iteration]][, variable := paste0(variable, "|Iteration ", iteration)]
    print("Calculation of vehicle stock finished")

  #------------------------------------------------------
  # End of iterative section
  #------------------------------------------------------

  #################################################
  ## Reporting
  #################################################
  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an interpolation step in toolCalculateOutputVariables()
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  vars <- toolCalculateOutputVariables(fuelVehicleESdemand, inputData$enIntensity, inputData$loadFactor, fleetSizeAndComposition,
                                       inputDataRaw$CAPEXtrackedFleet, inputDataRaw$subsidies,
                                       inputData$combinedCAPEXandOPEX, gdx, timeResReporting, hybridElecShare, REMINDinputOnly = TRUE)

  REMINDInputs <- prepareREMINDinputs(outputVarsExt, outputVarsInt)

  ## CapCosts
  writegdx.parameter("p35_esCapCost.gdx", REMINDInputs$capCost, "p35_esCapCost",
                     valcol="value", uelcols=c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs"))

  ## Intensities
  writegdx.parameter("p35_fe2es.gdx", finalInputs$intensity, "p35_fe2es",
                     valcol="value", uelcols = c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs"))

  ## Shares: demand can represent the shares since it is normalized
  writegdx.parameter("p35_shFeCes.gdx", finalInputs$shFeCes, "p35_shFeCes",
                     valcol="value",
                     uelcols = c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_enty", "all_in", "all_teEs"))



  ## use logit to calculate costs
  REMINDCapCost <- toolCapCosts(
    base_price=prices$base,
    Fdemand_ES = shares_int_dem[["demandF_plot_pkm"]],
    stations = num_veh_stations$stations,
    EDGE2CESmap = EDGE2CESmap,
    EDGE2teESmap = EDGE2teESmap,
    REMINDyears = REMINDyears,
    scenario = scenario)
  saveRDS(REMINDCapCost[["capCostPerTech"]], file = datapath("capCostPerTech.RDS"))

  ## full REMIND time range for inputs
  REMINDtall <- c(seq(1900,1985,5),
                  seq(1990, 2060, by = 5),
                  seq(2070, 2110, by = 10),
                  2130, 2150)

  ## prepare the entries to be saved in the gdx files: intensity, shares, non_fuel_price. Final entries: intensity in [trillionkm/Twa], capcost in [2005USD/trillionpkm], shares in [-]
  finalInputs <- toolPrepare4REMIND(
    demByTech = demByTech,
    intensity = intensity,
    capCost = REMINDCapCost[["CESlevelCosts"]],
    EDGE2teESmap = EDGE2teESmap,
    REMINDtall = REMINDtall)



  ## add the columns of SSP scenario and EDGE scenario to the output parameters
  for (i in names(finalInputs)) {
    finalInputs[[i]]$SSP_scenario <- scenario
    finalInputs[[i]]$DEM_scenario <- demScen
    finalInputs[[i]]$EDGE_scenario <- EDGE_scenario
  }


  ## calculate shares
  finalInputs$shFeCes = finalInputs$demByTech[, value := value/sum(value), by = c("tall", "all_regi", "all_in")]
  ## 7 decimals the lowest accepted value
  finalInputs$shFeCes[, value := round(value, digits = 7)]
  finalInputs$shFeCes[, value := ifelse(value == 0, 1e-7, value)]
  finalInputs$shFeCes[, sumvalue := sum(value), by = c("tall", "all_regi", "all_in")]
  finalInputs$shFeCes[, maxtech := ifelse(value == max(value), TRUE, FALSE), by =c("tall", "all_regi", "all_in")]

  ## attribute the variation to the maximum share value
  finalInputs$shFeCes[sumvalue!=1 & maxtech==TRUE, value := value + (1-sumvalue), by = c("tall", "all_regi")]
  ## remove temporary columns
  finalInputs$shFeCes[, c("sumvalue", "maxtech") := NULL]



  print(paste("---", Sys.time(), "End of the EDGE-T iterative model run."))

}
