#' Edge Transport Stand Alone
#'
#' Stand alone version of the EDGE Transport model. It includes the transport specific input data preparation,
#' a choice model to determine transport mode and technology shares, a demand regression and a fleet tracking for cars, busses and trucks
#'
#' @param SSPscen SSP or SDP scenario
#' @param TransportPolScen EDGE-T transport policy scenario
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

toolEdgeTransportSA <- function(SSPscen, techScen, demScen, gdxPath, outputFolder = NULL, storeRDS = TRUE, reportMif = TRUE, generateREMINDinputData = TRUE){

# Input data ----------------------------------------------------------------------

 # Energy Service demand
 # Energy Efficiency
 # Load Factor
 # CAPEX and non-fuel OPEX
 # Annual Mileage
 # Speed of modes

 # Calculation of value of time
 # IEA Harmonization: Adjust energy efficiency to meet IEA energy balances in 2010

 # Check
 # Store

# Transport Policy scenario specific parameters & levers --------------------------
 # Baseline preference trends
 # Transport policy scenario preference factors
 ## Function to adjust preference trends (Applying factors)
 # Startparameter inconvenience costs
 # Transport policy scenario inconvenience floor costs
 # logit exponents
 # SSP/SDP specific regression factors
 # Regional regression factors

# Transport Policyscenario specific adjustments of input data ---------------------

 # Load Factor
 # Energy efficiency

 # Check
 # Store


# Calibration module: Calibrate historical shareweights/inconvenience cost --------

 # Check
 # Store

## Start of iterative section
# Cost module: Calculate cost components and provide endogenous updates -----------
  #Input: All cost components, Start values inconveneience costs, fleet data
  #Output: TCO + i.a. inconvenience/VOT costs for all levels of the decision tree

# Discrete Choice module: Transport mode, vehicle and technology choice -----------
  #Input: TCO + i.a. inconvenience/VOT costs, preference trends, logit exponents
  #Output: Shares for all levels of the decision tree

# Demand regression module: Calculate future energy service demand ----------------
  #Input: SDP/SSP + regional regression factors, historical energy service demand
  #Output: Energy Service demand for top nodes of decision tree

# Vehicle stock module: Calculate vehicle stock for cars, trucks and busses -------
  #Input: (Sales) shares for all levels of the decision tree, Energy Service demand for top nodes of decision tree
  #Ouput: Fleet data, adjusted energy intensity for the fleet (in comparison to sales energy efficiency)

# Check all output data
# Store all outut data
## End of iterative section


# Reporting ---------------------------------------------------------------
# report MIF
# write MIF

#report REMIND input data
#return REMIND input data

}
