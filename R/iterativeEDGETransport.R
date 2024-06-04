#' EDGE-Transport iterative
#' EDGE-Transport iterative script
#'
#' Run in the REMIND output folder in between iterations
#'
#' @author Johanna Hoppe
#' @importFrom data.table fread
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
  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- 0.4
  numberOfRegions <- length(readGDX(gdx, "all_regi"))
  iterationNumber <- as.vector(rgdx(gdx, list(name = "o_iterationNumber"))$val)

  InputFiles <- c("CAPEXandNonFuelOPEX",
                  "scenSpecPrefTrends",
                  "scenSpecLoadFactor",
                  "scenSpecEnIntensity",
                  "initialIncoCosts",
                  "annualMileage",
                  "timeValueCosts")

  inputs <- toolLoadIterativeInputs(edgeTransportFolder, inputFolder, InputFiles, numberOfRegions, SSPscenario,
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
    vehSalesAndModeShares <- toolDiscreteChoice(inputData, genModelPar, endogenousCosts$updatedEndogenousCosts, helpers)

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

    outputFolder <- edgeTransportPath

    outputRaw <- list(
      SSPscen = SSPscen,
      transportPolScen = transportPolScen,
      demScen = demScen,
      gdxPath = gdxPath,
      hybridElecShare = hybridElecShare,
      histPrefs = histPrefs,
      fleetSizeAndComposition = fleetSizeAndComposition,
      endogenousCosts = endogenousCosts,
      vehSalesAndModeShares = vehSalesAndModeShares,
      ESdemandFVsalesLevel = ESdemandFVsalesLevel,
      helpers = helpers
    )
    # not all data from inputdataRaw and inputdata is needed for the reporting
    add <- append(inputDataRaw[!names(inputDataRaw) %in% c("GDPpcMER", "GDPpcPPP", "population", "GDPMER")],
                  inputData[!names(inputData) %in% c("histESdemand", "GDPMER", "GDPpcMER", "GDPpcPPP", "population")])
    outputRaw <- append(outputRaw, add)

    #store analytics data
    outputRaw <- append(outputRaw, list(endogenousCostsIterations = endogenousCostsIterations,
                                        fleetVehNumbersIterations = fleetVehNumbersIterations))

    storeData(outputFolder = outputFolder, outputRaw = outputRaw)

    baseOutput <- toolReportEdgeTransport(outputFolder,
                                          outputRaw,
                                          isTransportReported = FALSE)

    f35_esCapCost <- reportToREMINDcapitalCosts(baseOutput$int$fleetCost[variable == "Capital costs"], baseOutput$ext$fleetESdemand, timeResReporting,
                                                demScen, SSPscen, transportPolScen, helpers)
    f35_fe2es <- reportToREMINDenergyEfficiency(baseOutput$int$fleetEnergyIntensity, timeResReporting, helpers)
    f35_shFeCes <- reportToREMINDfinalEnergyShares(baseOutput$ext$fleetFEdemand, timeResReporting, helpers)

    ## CapCosts
    writegdx.parameter("p35_esCapCost.gdx", f35_esCapCost, "p35_esCapCost",
                       valcol="value", uelcols=c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs"))

    ## Intensities
    writegdx.parameter("p35_fe2es.gdx", f35_fe2es, "p35_fe2es",
                       valcol="value", uelcols = c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs"))

    ## Shares: demand can represent the shares since it is normalized
    writegdx.parameter("p35_shFeCes.gdx", f35_shFeCes, "p35_shFeCes",
                       valcol="value",
                       uelcols = c("tall", "all_regi", "SSP_scenario", "DEM_scenario", "EDGE_scenario", "all_enty", "all_in", "all_teEs"))

    print(paste("---", Sys.time(), "End of the EDGE-T iterative model run."))
}
