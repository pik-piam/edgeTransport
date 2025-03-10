#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#' Existing files are overwritten silently. Does not return anything.
#'
#' @param inputPath the path to the folder containing the input (csv-) files
#' @param filename name of the file
#' @param SSPscenario SSP scenario
#' @param demScenario demand scenario
#' @param transportPolScenario Transport policy scenario
#' @import data.table
#' @export
# Loads the csv input files chooses the correct scenario and
# converts the files into RDS local files
csv2RDS <- function(filename, inputPath, SSPscenario, demScenario, transportPolScenario) {
    # bind variables locally to prevent NSE notes in R CMD CHECK
    SSPscen <- demScen <- transportPolScen <- NULL

    if (filename == "scenSpecPrefTrends") colNames <- c("period", "region", "SSPscen", "demScen", "transportPolScen",
                                                        "sector", "subsectorL1", "subsectorL2", "subsectorL3",
                                                        "vehicleType", "technology", "level", "variable", "unit", "value")
    else if (filename == "initialIncoCosts") colNames <- c("period", "region", "SSPscen", "demScen", "transportPolScen",
                                                           "univocalName", "technology", "variable", "unit", "type", "value")
    else if (filename == "timeValueCosts") colNames <-  c("period", "region", "SSPscen", "demScen", "transportPolScen",
                                                          "univocalName", "variable", "unit", "value")
    else if (filename == "f29_trpdemand") colNames <-  c("period", "region", "SSPscen", "demScen", "transportPolScen", "all_in", "value")
    else colNames <-  c("period", "region", "SSPscen", "demScen", "transportPolScen",
                        "univocalName", "technology", "variable", "unit", "value")
    tmp <- fread(
      file.path(inputPath, paste0(filename, ".cs4r")), skip = 5, col.names = eval(colNames))
    tmp <- tmp[SSPscen == SSPscenario & demScen == demScenario & transportPolScen == transportPolScenario][, c("SSPscen", "demScen", "transportPolScen") := NULL]
    # magclass enforces the same temporal resolution for all vehicletypes -> get rid of the introduced NAs
    assign(filename, tmp[!is.na(tmp$value)])
    tmp <- stats::setNames(list(get(eval(filename))), filename)
    return(tmp)
  }

toolLoadRDSinputs <- function(edgeTransportFolder, inputFiles) {

  loadRDS <- function(filename, edgeTransportFolder) {
    filePath <- list.files(file.path(".", edgeTransportFolder), paste0(filename, ".RDS"), recursive = TRUE, full.names = TRUE)
    tmp <- readRDS(filePath)
  }

  inputData <- sapply(inputFiles, loadRDS, edgeTransportFolder, simplify = FALSE, USE.NAMES = TRUE)
  return(inputData)
}
#' Load iterative inputs
#'
#' @param edgeTransportFolder transport folder
#' @param inputFolder the path to the folder containing the input (csv-) files
#' @param inputFiles names of the input files
#' @param numberOfRegions regional resolution
#' @param SSPscenario SSP scenario
#' @param demScenario demand scenario
#' @param transportPolScenario Transport policy scenario
#' @importFrom reporttransport storeData
#' @import data.table
#' @export

toolLoadIterativeInputs <- function(edgeTransportFolder, inputFolder, inputFiles, numberOfRegions, SSPscenario, transportPolScenario, demScenario) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  transportPolScen <- all_in <- period <- value <- unit <- sector <- variable <- . <- univocalName <- test <- SSPscen <- NULL
  
  #As a starting point, we only use GDP and Population data from the IND-scenarios. Changes in transport policy scenarios to the SSP2 scenario are not considered.
  if (SSPscenario %in% c("SSP2IndiaHigh", "SSP2IndiaDEAs", "SSP2IndiaMedium")){SSPscenario <- "SSP2"}

  # Model input parameters from the package
  ## Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)

  ## Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost <- scenParIncoCost[SSPscen == SSPscenario & transportPolScen == gsub("ICEban", "", transportPolScenario)][, c("SSPscen", "transportPolScen") := NULL]

  annuityCalc <- fread(system.file("extdata/genParAnnuityCalc.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Interest Rate and vehicle service life for annuity calculation
  # NOTE: right now there is only "default". If we add scenario specific annuity parameters, we can shift annuityCalc to the scenPar's
  if  (gsub("ICEban", "", transportPolScenario) %in% annuityCalc$transportPolScen) {
    annuityCalc <- annuityCalc[transportPolScen == gsub("ICEban", "", transportPolScenario)][, transportPolScen := NULL]
} else {
    annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
  }

  mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv", package = "edgeTransport", mustWork = TRUE))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                package = "edgeTransport"))
  reportingNames <- fread(system.file("extdata", "helpersReportingNames.csv",
                                      package = "edgeTransport"), skip = 1)
  reportingAggregation <- fread(system.file("extdata", "helpersReportingAggregation.csv",
                                            package = "edgeTransport"), skip = 1)
  mitigationTechMap <- fread(system.file("extdata", "helpersMitigationTechmap.csv",
                                         package = "edgeTransport"))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                package = "edgeTransport"))

  ## Decision tree
  # map on decisiontree for provided spatial resolution
  if (numberOfRegions == 21) {
    decisionTree <- toolLoadDecisionTree("regionCode21")
  } else if (numberOfRegions == 12) {
    decisionTree <- toolLoadDecisionTree("regionCode12")
  } else {
    stop("EDGE-Transport iterative does not suppoert the spatial resolution of ", numberOfRegions, "regions provided by the REMIND gdx. Choose either 12 or 21 regions")
  }

  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_2W", "trn_freight_road", "trn_pass", "trn_freight")
  filterEntries <- getFilterEntriesUnivocalName(categories, decisionTree)
  filterEntries[["trackedFleet"]] <- c(filterEntries[["trn_pass_road_LDV_4W"]], filterEntries[["trn_freight_road"]],
                                       getFilterEntriesUnivocalName("Bus", decisionTree)[["Bus"]])

  # Input from REMIND input data
  # In the first iteration input data needs to be loaded
  if (!dir.exists(file.path(edgeTransportFolder))) {
    print("Loading csv data from input folder and creating RDS files...")
}
  RDSfiles <- list()
  for (filename in inputFiles) {
    if (length(list.files(file.path(".", edgeTransportFolder), paste0(filename, ".RDS"), recursive = TRUE, full.names = TRUE)) < 1) {
      RDSfiles <- append(RDSfiles, csv2RDS(filename, inputFolder, SSPscenario, demScenario, transportPolScenario))
    }
  }
  if (!is.null(RDSfiles$f29_trpdemand)) {
    mapEdgeSectorToREMIND <- merge(mapEdgeToREMIND, unique(decisionTree[, c("sector", "univocalName")]), by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
    mapEdgeSectorToREMIND <- mapEdgeSectorToREMIND[!is.na(all_in)]
    mapEdgeSectorToREMIND <- unique(mapEdgeSectorToREMIND[, c("all_in", "sector")])
    RDSfiles$f29_trpdemand <- merge(RDSfiles$f29_trpdemand[period >= 1990], mapEdgeSectorToREMIND, by = "all_in")[, all_in := NULL]
    ## convert unit
    trillionToBillion <- 1e3
    RDSfiles$f29_trpdemand[, value := value
                           * trillionToBillion]
    RDSfiles$f29_trpdemand[, unit := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "billion pkm/yr", "billion tkm/yr")][, variable := "ES"]
    setcolorder(RDSfiles$f29_trpdemand, c("region", "period", "sector", "value", "unit"))
  }
  if (length(RDSfiles) > 0) storeData(file.path(".", edgeTransportFolder), varsList = RDSfiles)

  if (!length(RDSfiles) == length(inputFiles)) RDSfiles <- toolLoadRDSinputs(edgeTransportFolder, inputFiles)

  # Time resolution
  dtTimeRes <- unique(RDSfiles$scenSpecEnIntensity[, c("univocalName", "period")])
  highRes <- unique(dtTimeRes$period)
  lowResUnivocalNames <- copy(dtTimeRes)
  lowResUnivocalNames <- lowResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  lowResUnivocalNames <- lowResUnivocalNames[test == FALSE, univocalName]
  lowTimeRes <- unique(dtTimeRes[univocalName %in% lowResUnivocalNames]$period)
  helpers <- list(dtTimeRes = dtTimeRes,
                  lowTimeRes = lowTimeRes,
                  decisionTree = decisionTree,
                  filterEntries = filterEntries,
                  mapEdgeToREMIND  = mapEdgeToREMIND,
                  reportingNames = reportingNames,
                  reportingAggregation = reportingAggregation,
                  mitigationTechMap = mitigationTechMap,
                  regionmappingISOto21to12 = regionmappingISOto21to12)

  # general model parameters
  genModelPar <- list(
    lambdasDiscreteChoice = lambdasDiscreteChoice,
    annuityCalc = annuityCalc
  )

  # transport scenario (SSPscen + demScen + polScen) specific model parameters
  scenModelPar <- list(
    scenParIncoCost = scenParIncoCost
  )

  return(
    list(
      genModelPar = genModelPar,
      scenModelPar = scenModelPar,
      RDSfiles = RDSfiles,
      helpers = helpers
    )
  )
}
