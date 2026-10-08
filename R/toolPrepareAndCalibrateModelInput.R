#' Prepare scenario specific input data and calibrate historical preferences
#'
#' Applies the scenario specific changes to the raw input data, calibrates the historical
#' preferences from the historical energy service demand and assembles the preference trends
#' (calibrated + scenario specific, mixed time resolution, optional ICE ban, normalized).
#' Shared by toolEdgeTransportSA() (standalone) and iterativeEdgeTransport() (first call in a
#' REMIND run) so that the two paths cannot drift apart.
#'
#' The returned inputData is complete for standalone mode. The iterative caller appends
#' CAPEXandNonFuelOPEX, which is returned separately and is the only mode specific entry left.
#' histPrefs is returned because both callers hand it on to storeData(), which writes it to
#' 3_Calibration; nothing else reads it. It is a calibration artefact rather than a scenario
#' input: the calibration runs on historical periods only, and scenario differentiation starts
#' after allEqYear (2020 at the earliest), so in standalone mode the result does not vary with
#' the transport policy scenario. In iterative mode it does, but only through the REMIND fuel
#' costs, which toolLoadREMINDfuelCosts() back-extrapolates from 2025 into the calibration years.
#'
#' inputData is the interface that both producers of model input must satisfy: this function on
#' the first call, and toolReLoadInputs() on every later REMIND Nash iteration. That is why
#' annualMileage, timeValueCosts and histESdemand travel through inputData instead of being read
#' from inputDataRaw at the point of use - inputDataRaw does not exist on a reload iteration.
#' Keep the members in step with the file list in toolReLoadInputs().
#'
#' What belongs in inputData, and what does not:
#' \itemize{
#'   \item scenario processed data consumed by the model (scenSpecPrefTrends, scenSpecLoadFactor,
#'         combinedCAPEXandOPEX, initialIncoCosts)
#'   \item raw data consumed unchanged by the model (annualMileage, timeValueCosts, histESdemand)
#'   \item scenSpecEnIntensity and upfrontCAPEXtrackedFleet, which no model function reads; they
#'         travel in inputData only so that storeData() writes them to 2_InputDataPolicy
#'   \item CAPEXandNonFuelOPEX (iterative only), which is persisted to RDS so that the next
#'         REMIND Nash iteration can recombine it with the updated fuel costs
#' }
#' Raw inputs that scenario processing supersedes (energyIntensityRaw, loadFactorRaw, the CAPEX
#' and OPEX tables, subsidies) and the driver data (GDP, population) stay out of inputData. They
#' are read from inputDataRaw where they are needed - by toolPrepareScenInputData() inside this
#' function and by toolDemandRegression() in the caller - and reach the output path from
#' inputDataRaw.
#'
#' Note on regional resolution: the function is agnostic to the number of regions. In iterative
#' mode the REMIND fuel costs must already be deaggregated to 21 regions and appended to
#' inputDataRaw before this function is called, since they enter the cost combination and thereby
#' the preference calibration.
#'
#' @author Alex K. Hagen
#' @param genModelPar General model parameters
#' @param scenModelPar Transport scenario (SSPscen + demScen + polScen) specific model parameters
#' @param inputDataRaw Raw input data, including the (deaggregated) REMINDfuelCosts
#' @param commonParams List of common parameters from toolGetCommonParameters(), supplying
#'          allEqYear, GDPcutoff and ICEbanYears
#' @param isICEban Vector of length two indicating an ICE ban before/after the startyear
#' @param helpers List with helpers
#' @returns List with inputData, the calibrated historical preferences (histPrefs) and
#'          CAPEXandNonFuelOPEX, which only the iterative caller needs
#' @import data.table
#' @export

toolPrepareAndCalibrateModelInput <- function(genModelPar,
                                              scenModelPar,
                                              inputDataRaw,
                                              commonParams,
                                              isICEban,
                                              helpers) {

  # bind variables locally to prevent NSE notes in R CMD CHECK
  level <- subsectorL3 <- NULL

  allEqYear <- commonParams$allEqYear
  GDPcutoff <- commonParams$GDPcutoff
  ICEbanYears <- commonParams$ICEbanYears

  ########################################################
  ## Prepare input data and apply scenario specific changes
  ########################################################

  scenSpecInputData <- toolPrepareScenInputData(genModelPar,
                                                scenModelPar,
                                                inputDataRaw,
                                                allEqYear,
                                                GDPcutoff,
                                                helpers)

  ########################################################
  ## Calibrate historical preferences
  ########################################################
  sharesToBeCalibrated <- toolCalculateSharesDecisionTree(inputDataRaw$histESdemand, helpers)
  histPrefs <- toolCalibratePreferences(sharesToBeCalibrated,
                                        scenSpecInputData$combinedCAPEXandOPEX,
                                        inputDataRaw$timeValueCosts,
                                        genModelPar$lambdasDiscreteChoice,
                                        helpers)
  # Don't use calibrated shareweights for LDV 4w, as they receive inconvenience costs
  histPrefs$calibratedPreferences <- histPrefs$calibratedPreferences[
    !(subsectorL3 == "trn_pass_road_LDV_4W" & level == "FV")
  ]

  scenSpecPrefTrends <- rbind(histPrefs$calibratedPreferences,
                              scenSpecInputData$scenSpecPrefTrends)
  scenSpecPrefTrends <- toolApplyMixedTimeRes(scenSpecPrefTrends,
                                              helpers)
  if (isICEban[1] || isICEban[2]) {
    scenSpecPrefTrends <- toolApplyICEbanOnPreferences(scenSpecPrefTrends, helpers, ICEbanYears)
  }
  scenSpecPrefTrends <- toolNormalizePreferences(scenSpecPrefTrends)

  # collect the input data that standalone and iterative mode have in common
  inputData <- list(
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecInputData$scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecInputData$scenSpecEnIntensity,
    combinedCAPEXandOPEX = scenSpecInputData$combinedCAPEXandOPEX,
    upfrontCAPEXtrackedFleet = scenSpecInputData$upfrontCAPEXtrackedFleet,
    initialIncoCosts = scenSpecInputData$initialIncoCosts,
    annualMileage = inputDataRaw$annualMileage,
    timeValueCosts = inputDataRaw$timeValueCosts,
    histESdemand = inputDataRaw$histESdemand
  )

  return(list(
    inputData = inputData,
    histPrefs = histPrefs,
    CAPEXandNonFuelOPEX = scenSpecInputData$CAPEXandNonFuelOPEX
  ))
}
