#' Energy Demand Generator (EDGE)- Transport Model
#'
#' The Edge Transport Model includes the transport specific input data preparation,
#' a choice model to determine transport mode and technology shares, a demand regression and a fleet tracking for cars, busses and trucks
#'
#' @param SSPscen SSP or SDP scenario
#' @param transportPolScen EDGE-T transport policy scenario
#' @param demScen Demand scenario, used to apply reduction factors on total demands from the regression
#' @param gdxPath Path to a GDX file to load price signals from a REMIND run
#' @param outputFolder Path to folder for storing output data
#' @param storeRDS Optional saving of intermediate RDS files
#' @param reportMif Optional transport reporting in MIF format
#' @param generateREMINDinputData generate the REMIND input data cs4 files
#' @return generated REMIND transport input data
#' @author Johanna Hoppe, Jarusch Müßel, Alois Dirnaichner, Marianna Rottoli
#' @import data.table
#' @importFrom rmarkdown render
#' @importFrom quitte write.mif
#' @export

toolEdgeTransport <- function(SSPscen, transportPolScen, demScen = "default", gdxPath, outputFolder = NULL, storeRDS = TRUE, reportMIF = TRUE, generateREMINDinputData = TRUE){


#################################################
## Load data
#################################################

### Package data  ----------------------------------------------------------
packageData <- toolLoadPackageData(SSPscen, transportPolScen, demScen)

### Input data  ------------------------------------------------------------
## from mrtransport
mrtransportData <- toolLoadmrtransportData(SSPscen)
# choose years according to inputdata
years <- unique(mrtransportData$energyIntensity$period)
# set GDP cutoff to differentiate between regions
GDPcutoff <- 25000 # [constant 2005 US$MER]

## from mrcommons
mrdriversData <- toolLoadmrdriversData(SSPscen, years)

## from mrremind
mrremindData <- toolLoadmrremindData(packageData$decisionTree, years)

## from REMIND
REMINDdata <- toolLoadREMINDfuelPrices(gdxPath, years)
#REMINDdata$energyServiceDemand <- toolLoadREMINDenServ(gdxPath, years) move to iterative script

#################################################
## Calculate data
#################################################

# Transport Policyscenario specific adjustments ---------------------
# Load Factor
scenSpecLoadFactor <- toolApplyScenSpecLoadFactor(mrtransportData$loadFactor, demScen, SSPscen)
# Energy intensity
if (!is.null(packageData$policyParEnergyIntensity)) {
  scenSpecEnIntensity <- toolApplyScenSpecEnInt(mrtransportData$energyIntensity, packageData$policyParEnergyIntensity,
                                                packageData$mitigationTechMap, years)
}

# Calculate preference trends (Applying factors)
scenSpecPrefTrends <- toolApplyScenPrefTrends(packageData$baselinePrefTrends, packageData$policyParPrefTrends, packageData$mitigationTechMap, years,
                                              mrdriversData$GDPpcMER, GDPcutoff)

annuity <- toolCalculateAnnuity(packageData$annuityCalc, packageData$mitigationTechMap)

combinedCostperES <- toolCombineCosts(mrtransportData, annuity, REMINDdata$fuelPrices, mrremindData$transportSubsidies, packageData$decisionTree, years)

initialIncoCost <- toolApplyInitialIncoCost(combinedCostperES, packageData$incoCostStartVal, annuity, mrtransportData$loadFactor, mrtransportData$annualMileage,
                                            packageData$regionmappingISOto21to12, packageData$decisionTree, packageData$mitigationTechMap, years)


#################################################
## Calibration module
#################################################
# Calibrate historical shareweights/inconvenience cost -----------------------------
 # Check
 # Store

#------------------------------------------------------
#Start of iterative section
#------------------------------------------------------

#################################################
## Cost module
#################################################
# Provide endogenous updates to cost components -----------
  #Input: All cost components, Start values inconveneience costs, fleet data
  #Output: TCO + i.a. inconvenience/VOT costs for all levels of the decision tree


#################################################
## Discrete choice module
#################################################
# Discrete Choice module: Transport mode, vehicle and technology choice -----------
  #Input: TCO + i.a. inconvenience/VOT costs, preference trends, logit exponents
  #Output: Shares for all levels of the decision tree

#################################################
## Demand regression module
#################################################
# Calculate future energy service demand ----------------
  #Input: SDP/SSP + regional regression factors, historical energy service demand
  #Output: Energy Service demand for top nodes of decision tree

#################################################
## Vehicle stock module
#################################################
# Calculate vehicle stock for cars, trucks and busses -------
  #Input: (Sales) shares for all levels of the decision tree, Energy Service demand for top nodes of decision tree
  #Ouput: Fleet data, adjusted energy intensity for the fleet (in comparison to sales energy efficiency)

# Check all output data
# Store all outut data
## End of iterative section


# Reporting ---------------------------------------------------------------
# report MIF
# write MIF

#report REMIND calibration data
#return REMIND calibration data

#report REMIND input data
#return REMIND input data

}
