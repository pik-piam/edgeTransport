#' Energy service demand projection
#'
#' @param historicalESdemand Historical energy service demand
#' @param GDPperCapitaPPP GDP per capita based on purchase power parity
#' @param POP Population data
#' @param genParDemRegression General regression factors
#' @param scenParDemRegression Scenario specific general regression factors
#' @param scenParRegionalDemRegression Scenario specific regionally differentiated regression factors
#' @param scenParDemandFactors Demand scenario factors
#' @param baseYear End year of historical energy service demand data
#' @param allEqYear Start year of scenario differentiation
#' @param helpers list with helpers
#' @returns Scenario specific energy service demand for all model years on CES level
#' @author Johanna Hoppe
#' @import data.table
#' @export
#'
toolDemandRegression <- function(historicalESdemand, GDPperCapitaPPP, POP, genParDemRegression,
                                  scenParDemRegression, scenParRegionalDemRegression, scenParDemandFactors,
                                    baseYear, allEqYear, helpers) {

  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- sector <- value <- GDPpcPPP <- regionGDPpcPPP <- regionGDPppp <- regionalSummand <- NULL
  population <- GDPgrowthRate <- incomeElasticity <- GDPpcgrowthRate <- unit <- GDPpcterm <- NULL
  GDPterm <- POPgrowthRate <- . <- startYearCat <- NULL

  # interpolate SSP specific elasticities based on GDP PPP per capita ----------------------------
  GDPperCapitaPPP <- copy(GDPperCapitaPPP)[, c("variable", "unit") := NULL]
  setnames(GDPperCapitaPPP, "value", "regionGDPpcPPP")
  GDPperCapitaPPP <- GDPperCapitaPPP[period %in% helpers$lowTimeRes]
  POP <- copy(POP)[, c("variable", "unit") := NULL]
  POP <- POP[period %in% helpers$lowTimeRes]

  approxElasticities <- function(category, elasticityGDPValues, GDPpc) {
    # creates a function to interpolate elasticitis for certain GDP values
    appfun <- stats::approxfun(
    x = elasticityGDPValues[sector == category, GDPpcPPP],
    y = elasticityGDPValues[sector == category, value], rule = 2)

    elasticityRegionValues <- copy(GDPpc)[, `:=`(sector = category, value = appfun(regionGDPpcPPP))]
    return(elasticityRegionValues)
  }

  categories <- unique(scenParDemRegression$sector)
  # Regional elasticities until 2020
  regionalIncomeElasticities <- rbindlist(lapply(categories, approxElasticities, genParDemRegression, GDPperCapitaPPP[period <= 2020]))
  # apply SSP specific global changes from 2025 till 2150
  scenSpecRegionalIncomeElasticities <- rbindlist(lapply(categories, approxElasticities,
                                                         scenParDemRegression[startYearCat == "origin"], GDPperCapitaPPP[period > 2020]))
  # apply change of SSPscen with startyear
  if ("final" %in% unique(scenParRegionalDemRegression$startYearCat)) {
    scenSpecRegionalIncomeElasticitiesF <- rbindlist(lapply(categories, approxElasticities,
                                                            scenParDemRegression[startYearCat == "final"], GDPperCapitaPPP[period > allEqYear]))
    scenSpecRegionalIncomeElasticities <- rbind(scenSpecRegionalIncomeElasticities[period <= allEqYear], scenSpecRegionalIncomeElasticitiesF)
  }
  regionalIncomeElasticities <- rbind(regionalIncomeElasticities, scenSpecRegionalIncomeElasticities)

  # apply SSP specific regional changes------------------------------------------------------------
  if (!is.null(scenParRegionalDemRegression)) {
    # reshape data in long format( period = [2015, ])
    scenParRegionalDemRegression <- melt(scenParRegionalDemRegression,
                                               id.vars = c("region", "sector", "startYearCat"), variable.name = "period",
                                                  value.name = "regionalSummand")
    scenParRegionalDemRegression[, period := as.numeric(as.character(period))]
    # linear interpolation of factors in time
    scenParRegionalDemRegressionO <- approx_dt(scenParRegionalDemRegression[startYearCat == "origin"], unique(regionalIncomeElasticities$period),
                                      "period", "regionalSummand", c("region", "sector", "startYearCat"), extrapolate = TRUE)

    # check if SSP changes with startyear
    if ("final" %in% unique(scenParRegionalDemRegression$startYearCat)) {
      scenParRegionalDemRegressionO <- scenParRegionalDemRegressionO[period <= allEqYear][, startYearCat := NULL]
      scenParRegionalDemRegressionF <- approx_dt(scenParRegionalDemRegression[startYearCat == "final"], unique(regionalIncomeElasticities$period),
                                                 "period", "regionalSummand", c("region", "sector", "startYearCat"), extrapolate = TRUE)
      scenParRegionalDemRegressionF <- scenParRegionalDemRegressionF[period > allEqYear][, startYearCat := NULL]
      scenParRegionalDemRegression <- rbind(scenParRegionalDemRegressionO, scenParRegionalDemRegressionF)
    } else {
      scenParRegionalDemRegression <- scenParRegionalDemRegressionO[, startYearCat := NULL]
    }
    regionalIncomeElasticities <- merge(regionalIncomeElasticities, scenParRegionalDemRegression,
                                        by = c("region", "period", "sector"), all.x = TRUE)
    # some regions do not get a SSP specific regional summand or not for all elasticity types
    regionalIncomeElasticities[is.na(regionalSummand), regionalSummand := 0]
    regionalIncomeElasticities[period > baseYear, value := value + regionalSummand]
    regionalIncomeElasticities[, regionalSummand := NULL]
  }

  # merge with Population data ------------------------------------------------------------
  setnames(POP, "value", "population")
  GDP <- merge(GDPperCapitaPPP, POP, by = c("region", "period"))
  GDP[, regionGDPppp := regionGDPpcPPP * population][, regionGDPpcPPP := NULL]

  setnames(regionalIncomeElasticities, "value", "incomeElasticity")
  regressionData <- merge(regionalIncomeElasticities, GDP, by = c("region", "period"))

  # calculate growth rates
  regressionData[, `:=` (GDPgrowthRate = regionGDPppp / shift(regionGDPppp),
                         GDPpcgrowthRate = regionGDPpcPPP / shift(regionGDPpcPPP),
                         POPgrowthRate = population / shift(population)), by = c("region", "sector")]

  regressionData[, `:=` (GDPterm = GDPgrowthRate ^ incomeElasticity,
                         GDPpcterm = GDPpcgrowthRate ^ incomeElasticity), by = c("period", "region", "sector")]

  histESdemand <- merge(historicalESdemand, helpers$decisionTree, by = c("univocalName", "technology", "region"))
  histESdemand <- histESdemand[period %in% helpers$lowTimeRes]
  histESdemand <- histESdemand[, .(value = sum(value)), by = c("region", "period", "sector", "unit")]

  demandData <- merge(regressionData, histESdemand, by = c("region", "period", "sector"), all.x = TRUE)
  demandData[, unit := unit[period == baseYear], by = c("region", "sector")]
  regressionTimeRange <- unique(demandData[period > baseYear]$period)

  for (i in regressionTimeRange) {
    demandData[period <= i & sector %in% c("trn_pass", "trn_aviation_intl"),
               value := ifelse(is.na(value), shift(value) * GDPpcterm * POPgrowthRate, value),
               by = c("region", "sector")]
    demandData[period <= i  & sector %in% c("trn_freight", "trn_shipping_intl"),
               value := ifelse(is.na(value), shift(value) * GDPterm, value),
               by = c("region", "sector")]
  }

  if (!is.null(scenParDemandFactors)) {
    # Apply factors for specific demand scenario on output of demand regression if given
    # Application: linear regression to given support points for the factors
    # starting from 2021 or from startyear
    # constant factors after last support point
    if ("full" %in% scenParDemandFactors$startYearCat) {
      cutoff <- 2020
    } else if (("final" %in% scenParDemandFactors$startYearCat)) {
      cutoff <- max(allEqYear, 2020)
    } else { # "origin" %in% scenParDemandFactors$startYearCat
      stop("Error in demand scenario specific changes: only delayed switch-on with allEqYear possible. Please check toolDemandRegression()")
    }
    demandData <- merge(demandData, scenParDemandFactors, by = c("region", "period", "sector"), all.x = TRUE)
    demandData[period <= cutoff, factor := 1][, startYearCat := NULL]
    demandData[, factor := zoo::na.approx(factor, x = period, rule = 2), by = c("region", "sector")]
    demandData[, value := factor * value]
    print(paste0("Demand scenario specific changes were applied on energy service demand"))
 } else {
    print(paste0("No demand scenario specific changes were applied on energy service demand"))
  }

  demandData <- demandData[, c("region", "period", "sector", "value", "unit")]
  if (anyNA(demandData)) stop("energy service demand calculated in toolDemandRegression() includes NAs")

  return(demandData)

}
