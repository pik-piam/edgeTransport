#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#' Existing files are overwritten silently. Does not return anything.
#'
#' @param inputPath the path to the folder containing the input (csv-) files
#' @param edgeTransportFolder the path to the EDGE-Transport folder
#' @param SSPscen SSP scenario
#' @param demScen demand scenario
#' @param transportPolScen Transport policy scenario
#' @import data.table
#' @export

toolCreateRDS <- function(inputPath, edgeTransportFolder, SSPscen, demScen, transportPolScen) {

  print("Loading csv data from input folder and creating RDS files...")
  dir.create(file.path(edgeTransportFolder), showWarnings = FALSE)

  # Loads the csv input files chooses the correct scenario and
  # converts the files into RDS local files
  csv2RDS <- function(filename, inputPath) {
    tmp <- fread(
      file.path(inputPath, paste0(filename, ".cs4r")))
    tmp <- tmp[SSPscen == SSPscen & demScen == demScen & transportPolScen == transportPolScen]
    saveRDS(tmp, file.path(edgeTransportFolder, paste0(filename,".RDS")))
  }

  ## Create RDS files for lists
  files <- c()
  lapply(files, csv2RDS)

  return()
}

toolLoadRDSinputs <- function(edgeTransportFolder) {

  loadRDS <- function(filename, edgeTransportFolder) {
    tmp <- readRDS(file.path(edgeTransportFolder, filename))
  }

  files <- c()
  inputData <- sapply(files, loadRDS, simplify = FALSE, USE.NAMES = TRUE)

  return(inputData)
}

toolLoadIterativeInputs <- function(edgeTransportFolder, numberOfRegions, SSPscenario, transportPolScenario, demScenario = NULL) {

  # Input from REMIND input data
  # In the first iteration input data needs to be loaded
  if (length(list.files(path = edgeTransportFolder, pattern = "RDS")) < 8) {
    toolCreateRDS(inputFolder, edgeTransportFolder,
                  SSPscen = SSPscen,
                  demScen = demScen,
                  transportPolScen = transportPolScen)
  }
  RDSinputs <- toolLoadRDSinputs(edgeTransportFolder)

  # Model input parameters from the package
  ## Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)

  ## Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost <- scenParIncoCost[SSPscen == SSPscenario & transportPolScen == transportPolScenario][, c("SSPscen", "transportPolScen") := NULL]

  annuityCalc <- fread(system.file("extdata/genParAnnuityCalc.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Interest Rate and vehicle service life for annuity calculation
  # NOTE: right now there is only "default". If we add scenario specific annuity parameters, we can shift annuityCalc to the scenPar's
  if  (transportPolScenario %in% annuityCalc$transportPolScen){
    annuityCalc <- annuityCalc[transportPolScen == transportPolScenario][, transportPolScen := NULL]} else {
      annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
    }

  ## Decision tree
  # map on decisiontree for provided spatial resolution
  if (numberOfRegions == 21) {
    decisionTree <- toolLoadDecisionTree("regionCode21")
  } else if (numberOfRegions == 12) {
    decisionTree <- toolLoadDecisionTree("regionCode12")
  } else {
    stop("EDGE-Transport iterative does not suppoert the spatial resolution of ", numberOfRegions, "regions provided by the REMIND gdx. Choose either 12 or 21 regions")
  }

  # Time resolution
  dtTimeRes <- unique(RDSinputs$energyIntensity[, c("univocalName", "period")])
  highRes <- unique(dtTimeRes$period)
  lowResUnivocalNames <- copy(dtTimeRes)
  lowResUnivocalNames <- lowResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  lowResUnivocalNames <- lowResUnivocalNames[test == FALSE, univocalName]
  lowTimeRes <- unique(dtTimeRes[univocalName %in% lowResUnivocalNames]$period)
  helpers <- list(dtTimeRes = dtTimeRes,
                  decisionTree = decisionTree)

  # general model parameters
  genModelPar <- list(
    lambdasDiscreteChoice = packageData$lambdasDiscreteChoice,
    annuityCalc = packageData$annuityCalc
  )

  # transport scenario (SSPscen + demScen + polScen) specific model parameters
  scenModelPar <- list(
    scenParIncoCost = packageData$scenParIncoCost
  )


  return(
    list(
      genModelPar = genModelPar,
      scenModelPar = scenModelPar,
      RDSinputs = RDSinputs,
      helpers = helpers
    )
  )
}
