#' called from iterativeEdgeTransport()
#' reloads scenario specific inputData from RDS files in EDGE-T folder
#' instead of generation via mrtransport
#' @author Alex K. Hagen
#' @param edgeTransportFolder folder where the RDS files from last iterativeEdgeTransport() run are stored
#' @returns list with different input data sets
#' @import data.table
#' @export

toolReLoadInputs <- function(edgeTransportFolder) {

  ### load inputs  ------------------------------------------------------------
  # load edgeTransport package data
  # in the standalone version this data is loaded via toolLoadPackageData()
  # but here we only need a small subset of data which we load directly

  # Decision tree discrete choice model
  decisionTree <- toolLoadDecisionTree("regionCode21")
  # Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv",
                                             package = "edgeTransport", mustWork = TRUE), header = TRUE)

  annuityCalc <- fread(system.file("extdata/genParAnnuityCalc.csv",
                                   package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Interest Rate and vehicle service life for annuity calculation
  # NOTE: right now there is only "default". If we add scenario specific annuity parameters,
  # we need to shift annuityCalc to the scenPar's and adjust read-in here

  # general model parameters
  genModelPar <- list(
    lambdasDiscreteChoice = lambdasDiscreteChoice,
    annuityCalc = annuityCalc
  )

  # these are the scenario specific files which are read in from the EDGE-T folder from the previous run
  inputFiles <- c("scenSpecPrefTrends",
                  "scenSpecLoadFactor",
                  "scenSpecEnIntensity",
                  "CAPEXandNonFuelOPEX",
                  "upfrontCAPEXtrackedFleet",
                  "initialIncoCosts",
                  "annualMileage",
                  "timeValueCosts",
                  "histESdemand"
                  )

  RDSinputs <- toolLoadRDSinputs(edgeTransportFolder, inputFiles)

  # mappings and other helpers, incl. filterEntries and temporal resolution
  # scenSpecEnIntensity provides the reference for the mixed temporal resolution
  helpers <- toolBuildHelpers(
    decisionTree = decisionTree,
    timeResDataBase = RDSinputs$scenSpecEnIntensity
  )

  input <- list(
    helpers = helpers,
    genModelPar = genModelPar,
    RDSinputs = RDSinputs
  )

  return(input)
}
