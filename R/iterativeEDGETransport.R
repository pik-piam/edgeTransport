#' EDGE-Transport iterative
#'
#' Run in the REMIND output folder in between iterations
#'
#' @author Johanna Hoppe, Alex K. Hagen
#' @importFrom reporttransport storeData reportEdgeTransport reportToREMINDcapitalCosts
#' reportToREMINDenergyEfficiency reportToREMINDfinalEnergyShares
#' @import data.table
#' @export
#'
iterativeEdgeTransport <- function() {

  print(paste("---", Sys.time(), "Start of the EDGE-T iterative model run."))

  #############################################################
  ## Settings
  #############################################################
  cfg <- NULL

  # Set paths to folders
  edgeTransportFolder <- "EDGE-T"
  edgeTransportPath <- function(fname) {
    file.path(edgeTransportFolder, fname)
  }
  REMINDpath <- function(fname) {
    file.path("../../", fname)
  }

  # Set gdx in the current REMIND folder
  gdx <- "input.gdx"
  if (file.exists("fulldata.gdx"))
    gdx <- "fulldata.gdx"

  # Load config of REMIND reference run for fixing before startyear
  if (!is.na(cfg$files2export$start["input_ref.gdx"])) {
    load(file.path(dirname(cfg$files2export$start["input_ref.gdx"]), "config.Rdata"))
    startyear <- cfg$gms$cm_startyear
    # REMIND startyear is the year in which differences are observed
    # allEqYear in EDGE-T is the last year of the previous scenario and differentiation sets in directly after that, earliest: 2020
    allEqYear <- startyear - 5
    if (allEqYear < 2020){
      allEqYear <- 2020
    }
    SSPscen <- cfg$gms$cm_GDPpopScen
    transportPolScen <- cfg$gms$cm_EDGEtr_scen
    demSce <- cfg$gms$cm_demScen
    cfg <- NULL
  } else {
    # default start scenario as placeholder but no scenario differentiation before 2020
    SSPscen <- "SSP2"
    transportPolScen <- "Mix2"
    demScen <- "default"
    allEqYear <- 2020
  }

  # Load config
  load("config.Rdata")
  SSPscen[2] <- cfg$gms$cm_GDPpopScen
  transportPolScen[2] <- cfg$gms$cm_EDGEtr_scen
  demScen[2] <- cfg$gms$cm_demScen

  isICEban <- c(FALSE, FALSE)
  # Check if ICEban is on and isolate string before "ICEban" for transportPolScen
  # ToDo: check if this is a problem later in the code
  if (grepl(".*ban$", transportPolScen[1])) {
    isICEban[1] <- TRUE
    transportPolScen[1] <- gsub('ICEban','',transportPolScen[1])
  } else if(grepl(".*ban$", transportPolScen[2])) {
    isICEban[2] <- TRUE
    transportPolScen[2] <- gsub('ICEban','',transportPolScen[2])
  }

  # find years in which ICEban is used
  if (isICEban[1] & isICEban[2]) {
    ICEbanYears <- c(seq(2021, 2100, 1), 2110, 2130, 2150)
    isICEban <- TRUE
  } else if (isICEban[1] & allEqYear > 2020) {
    ICEbanYears  <- seq(2021, allEqYear, 1)
    isICEban <- TRUE
  } else if (isICEban[2]){
    ICEbanYears <-  c(seq(allEqYear, 2100, 1), 2110, 2130, 2150)
    isICEban <- TRUE
  } else {
    ICEbanYears <- NULL
    isICEban <- FALSE
  }

  # for ES regression
  baseYear <- 2010

  #############################################################
  ## Load input data
  #############################################################
  inputFolder <- paste0("./")

  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- unit <- univocalName <- iteration <- type <- variable <- . <- NULL

  # share of electricity in Hybrid electric vehicles
  hybridElecShare <- 0.4
  numberOfRegions <- length(gdx::readGDX(gdx, "all_regi"))
  iterationNumber <- as.vector(gdxrrw::rgdx(gdx, list(name = "o_iterationNumber"))$val)

  # ToDo: delete
  inputFiles <- c("CAPEXandNonFuelOPEX",
                  "scenSpecPrefTrends",
                  "scenSpecLoadFactor",
                  "scenSpecEnIntensity",
                  "initialIncoCosts",
                  "annualMileage",
                  "timeValueCosts",
                  "f29_trpdemand")

  inputs <- toolLoadIterativeInputs(edgeTransportFolder, inputFolder, inputFiles, numberOfRegions,
                                    SSPscen, transportPolScen, demScen)
  inputs <- toolLoadInputs(SSPscen, transportPolScen, demScen, hybridElecShare)

  helpers <- inputs$helpers
  genModelPar <- inputs$genModelPar
  scenModelPar <- inputs$scenModelPar
  RDSinputs <- inputs$RDSfiles



  # Data from previous REMIND iteration
  ## Load REMIND energy service demand
  REMINDsectorESdemand <- toolLoadREMINDesDemand(gdx, helpers)
  REMINDsectorESdemand <- rbind(REMINDsectorESdemand, RDSinputs$f29_trpdemand[
    !period %in% unique(REMINDsectorESdemand$period)
  ])

  ## Load REMIND fuel cost
  REMINDfuelCost <- toolLoadREMINDfuelCosts(gdx, hybridElecShare, helpers)
  # Convert fuel costs from US$/MJ to US$/vehkm
  # Merge with energy intensity
  energyIntensity <- copy(RDSinputs$scenSpecEnIntensity)
  energyIntensity[, c("variable", "unit") := NULL]
  setnames(energyIntensity, "value", "energyIntensity")
  REMINDfuelCost <- merge(REMINDfuelCost, energyIntensity, by = c("region", "univocalName", "technology", "period"))
  REMINDfuelCost[, value := value * energyIntensity][, unit := gsub("MJ", "vehkm", unit)][, energyIntensity := NULL]
  # Convert fuel costs from US$/vehkm to US$/(p|t)km
  loadFactor <- copy(RDSinputs$scenSpecLoadFactor)
  loadFactor[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  REMINDfuelCost <- merge(REMINDfuelCost, loadFactor, by = c("region", "univocalName", "technology", "period"))
  REMINDfuelCost[, value := value / loadFactor][, loadFactor := NULL]
  REMINDfuelCost[, unit := ifelse(univocalName %in% c(helpers$filterEntries$trn_pass, "International Aviation"),
                                  gsub("vehkm", "pkm", unit), gsub("vehkm", "tkm", unit))]

  pathFuelCosts <- list.files(file.path(".", edgeTransportFolder), "REMINDfuelCostIterations.RDS", recursive = TRUE,
                              full.names = TRUE)
  if (length(pathFuelCosts) > 0) {
    REMINDfuelCostIterations <- readRDS(list.files(file.path(".", edgeTransportFolder), "REMINDfuelCostIterations.RDS", recursive = TRUE, full.names = TRUE))
    REMINDfuelCostIterations <- rbind(REMINDfuelCostIterations, copy(REMINDfuelCost)[, iteration := iterationNumber])
    storeData(file.path(".", edgeTransportFolder), REMINDfuelCostIterations = REMINDfuelCostIterations)
    # average fuel costs over REMIND iterations
    averagePricesOverIterations <- TRUE
    if (averagePricesOverIterations) {
      if (max(unique(REMINDfuelCostIterations$iteration)) >= 20 &&
          max(unique(REMINDfuelCostIterations$iteration)) <= 30) {
        ## apply moving avg
        byCols <- names(REMINDfuelCostIterations)
        byCols <- byCols[!byCols %in% c("value", "iteration")]
        REMINDfuelCost <- copy(REMINDfuelCostIterations[iteration >= 20])
        REMINDfuelCost <- REMINDfuelCost[, .(value = mean(value)), by = eval(byCols)]
      }
    }
  } else {
    REMINDfuelCost[, iteration := iterationNumber]
    storeData(file.path(".", edgeTransportFolder), REMINDfuelCostIterations = REMINDfuelCost)
    REMINDfuelCost[, iteration := NULL]
  }
  setcolorder(RDSinputs$CAPEXandNonFuelOPEX, names(REMINDfuelCost))
  combinedCAPEXandOPEX <- rbind(RDSinputs$CAPEXandNonFuelOPEX, REMINDfuelCost)
  # Data from previous EDGE-T iteration
  fleetVehiclesPerTech <- NULL
  pathTofleetVehiclesPerTech <- list.files(file.path(".", edgeTransportFolder),
                                           "fleetVehiclesPerTech.RDS",
                                           recursive = TRUE,
                                           full.names = TRUE)
  if (length(pathTofleetVehiclesPerTech) > 0) {
    fleetVehiclesPerTech <- readRDS(pathTofleetVehiclesPerTech)
  }

  inputData <- list(
    scenSpecPrefTrends = RDSinputs$scenSpecPrefTrends,
    scenSpecLoadFactor = RDSinputs$scenSpecLoadFactor,
    scenSpecEnIntensity = RDSinputs$scenSpecEnIntensity,
    combinedCAPEXandOPEX = combinedCAPEXandOPEX,
    initialIncoCosts = RDSinputs$initialIncoCosts,
    annualMileage = RDSinputs$annualMileage,
    timeValueCosts = RDSinputs$timeValueCosts,
    REMINDsectorESdemand = REMINDsectorESdemand
  )
  ########################################################
  ## Prepare data for
  ## endogenous costs update
  ########################################################

  vehicleDepreciationFactors <- toolCalculateVehicleDepreciationFactors(genModelPar$annuityCalc, helpers)

  dataEndogenousCosts <- toolPrepareDataEndogenousCosts(inputData,
                                                        genModelPar$lambdasDiscreteChoice,
                                                        helpers)

  #------------------------------------------------------
  # Start of iterative section
  #------------------------------------------------------

  #################################################
  ## Cost module
  #################################################
  # provide endogenous updates to cost components -----------
  # number of vehicles changes in the vehicle stock module and serves
  # as new input for endogenous cost update
  endogenousCosts <- toolUpdateEndogenousCosts(dataEndogenousCosts,
                                               vehicleDepreciationFactors,
                                               scenModelPar$scenParIncoCost,
                                               allEqYear,
                                               inputData$timeValueCosts,
                                               inputData$scenSpecPrefTrends,
                                               genModelPar$lambdasDiscreteChoice,
                                               helpers,
                                               isICEban,
                                               ICEbanYears,
                                               fleetVehiclesPerTech)

  print("Endogenous updates to cost components finished")
  #################################################
  ## Discrete choice module
  #################################################
  # calculate vehicle sales shares and mode shares for all levels of the decisionTree
  vehSalesAndModeShares <- toolDiscreteChoice(inputData,
                                              genModelPar,
                                              endogenousCosts$updatedEndogenousCosts,
                                              helpers)

  ESdemandFVsalesLevel <- toolCalculateFVdemand(inputData$REMINDsectorESdemand,
                                                vehSalesAndModeShares$shares,
                                                helpers,
                                                inputData$histESdemand,
                                                baseYear)
  print("Calculation of vehicle sales and mode shares finished")
  #################################################
  ## Vehicle stock module
  #################################################
  # Calculate vehicle stock for cars, trucks and busses -------
  fleetSizeAndComposition <- toolCalculateFleetComposition(ESdemandFVsalesLevel,
                                                           vehicleDepreciationFactors,
                                                           vehSalesAndModeShares$shares,
                                                           inputData$annualMileage,
                                                           inputData$scenSpecLoadFactor,
                                                           helpers)
  fleetVehiclesPerTech <- fleetSizeAndComposition$fleetVehiclesPerTech
  storeData(file.path(".", edgeTransportFolder), fleetVehiclesPerTech = fleetVehiclesPerTech)
  print("Calculation of vehicle stock finished")

  #------------------------------------------------------
  # End of iterative section
  #------------------------------------------------------

  #################################################
  ## Reporting
  #################################################
  # if you want to change timeResReporting to timesteps outside the modeleled timesteps, please add an
  # interpolation step in toolCalculateOutputVariables()
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  outputFolder <- file.path(".", edgeTransportFolder)

  outputRaw <- list(
    combinedCAPEXandOPEX = inputData$combinedCAPEXandOPEX,
    scenSpecEnIntensity = inputData$scenSpecEnIntensity,
    scenSpecLoadFactor = inputData$scenSpecLoadFactor,
    SSPscen = SSPscen,
    transportPolScen = transportPolScen,
    demScen = demScen,
    hybridElecShare = hybridElecShare,
    fleetSizeAndComposition = fleetSizeAndComposition,
    endogenousCosts = endogenousCosts,
    vehSalesAndModeShares = vehSalesAndModeShares$shares,
    ESdemandFVsalesLevel = ESdemandFVsalesLevel,
    helpers = helpers
  )

  storeData(outputFolder = outputFolder, varsList = outputRaw)

  baseOutput <- reportEdgeTransport(outputFolder,
                                    outputRaw,
                                    isTransportReported = FALSE)

  f35_esCapCost <- reportToREMINDcapitalCosts(baseOutput$int$fleetCost[variable == "Capital costs"],
                                              baseOutput$ext$fleetESdemand, hybridElecShare, timeResReporting,
                                              demScen, SSPscen, transportPolScen, helpers)
  f35_fe2es <- reportToREMINDenergyEfficiency(baseOutput$ext$fleetFEdemand,
                                              baseOutput$ext$fleetESdemand,
                                              hybridElecShare,
                                              timeResReporting,
                                              demScen,
                                              SSPscen,
                                              transportPolScen,
                                              helpers)

  f35_shFeCes <- reportToREMINDfinalEnergyShares(baseOutput$ext$fleetFEdemand,
                                                 timeResReporting,
                                                 demScen,
                                                 SSPscen,
                                                 transportPolScen,
                                                 helpers)

  ## CapCosts
  gdxdt::writegdx.parameter(
    "p35_esCapCost.gdx",
    f35_esCapCost,
    "p35_esCapCost",
    valcol = "value",
    uelcols = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs")
  )

  ## Intensities
  gdxdt::writegdx.parameter(
    "p35_fe2es.gdx",
    f35_fe2es,
    "p35_fe2es",
    valcol = "value",
    uelcols = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs")
  )

  ## Shares: demand can represent the shares since it is normalized
  gdxdt::writegdx.parameter(
    "p35_shFeCes.gdx",
    f35_shFeCes,
    "p35_shFeCes",
    valcol = "value",
    uelcols = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_enty", "all_in", "all_teEs")
  )

  print(paste("---", Sys.time(), "End of the EDGE-T iterative model run."))
}
