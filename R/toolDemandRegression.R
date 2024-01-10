


toolDemandRegression <- function(historicalESdemand, CAPEXandOPEX, GDPperCapitaMER, POP,
                                  scenParDemandRegression, scenParRegionalDemRegression, scenParDemandFactors) {

  # interpolate SSP specific elasticities based on GDP MER per capita ----------------------------
  setnames(GDPperCapitaMER, "value", "regionGDPpcMER")
  approxElasticities <- function(category, elasticityGDPValues, GDPpc) {
    # creates a function to interpolate elasticitis for certain GDP values
    appfun <- approxfun(
    x = elasticityGDPValues[sector == category, GDPpcMER],
    y = elasticityGDPValues[sector == category, value], rule = 2)

    elasticityRegionValues <- copy(GDPpc)[, `:=`(sector = category, value = appfun(regionGDPpcMER))]
    return(elasticityRegionValues)
  }
  categories <- unique(scenParDemandRegression$sector)
  regionalIncomeElasticities <- rbindlist(lapply(categories, approxElasticities, scenParDemandRegression, GDPperCapitaMER))

  # apply SSP specific regional changes------------------------------------------------------------
  if (!is.null(scenParRegionalDemRegression)) {
    scenParRegionalDemRegression <- melt(scenParRegionalDemRegression,
                                               id.vars = c("region", "sector"), variable.name = "period", value.name = "regionalSummand")
    scenParRegionalDemRegression[, period := as.numeric(as.character(period))]
    scenParRegionalDemRegression <- approx_dt(scenParRegionalDemRegression, unique(regionalIncomeElasticities$period),
                                      "period", "regionalSummand", c("region", "sector"), extrapolate = TRUE)
    regionalIncomeElasticities <- merge(regionalIncomeElasticities, scenParRegionalDemRegression,
                                        by = c("region", "period", "sector"), all.x = TRUE)
    # do not apply scenario specific changes before policy start year
    regionalIncomeElasticities[period < policyStartYear, regionalSummand := 0]
    # some regions do not get a SSP specific regional summand or not for all elasticity types
    regionalIncomeElasticities[is.na(regionalSummand), regionalSummand := 0]
    regionalIncomeElasticities[, value := value + regionalSummand][, regionalSummand := NULL]
  }

  # merge with Population data ------------------------------------------------------------
  setnames(POP, "value", "population")
  GDP <- merge(GDPperCapitaMER, POP, by = c("region", "period"))
  GDP[, regionGDPMER := regionGDPpcMER * population][, regionGDPpcMER := NULL]

  setnames(regionalIncomeElasticities, "value", "incomeElasticity")
  regressionData <- merge(regionalIncomeElasticities, GDP, by = c("region", "period"))

  # calculate growth rates
  regressionData[, `:=` (GDPgrowthRate = regionGDPMER/shift(regionGDPMER),
                         GDPpcgrowthRate = regionGDPpcMER/shift(regionGDPpcMER),
                         POPgrowthRate = population/shift(population)), by = c("region", "sector")]

  regressionData[, `:=` (GDPterm = GDPgrowthRate ^ incomeElasticity,
                         GDPpcterm = GDPpcgrowthRate ^ incomeElasticity), by = c("period", "region", "sector")]

  histESdemand <- merge(historicalESdemand, helpers$decisionTree, by = c("univocalName", "technology", "region"))
  histESdemand <- histESdemand[, .(value = sum(value)), by = c("region", "period", "sector")]

  demandData <- merge(regressionData, histESdemand, by = c("region", "period", "sector"), all.x = TRUE)
  baseYear <- max(unique(histESdemand$period))
  regressionTimeRange <- unique(demandData[period > baseYear]$period)
  # passenger transport
  for (i in regressionTimeRange) {
    demandData[period <= i & sector %in% c("trn_pass", "trn_aviation_intl"),
               value := ifelse(is.na(value), shift(value) * GDPpcterm * POPgrowthRate, value),
               by = c("region", "sector")]
    demandData[period <= i  & sector %in% c("trn_freight", "trn_shipping_intl"),
               value := ifelse(is.na(value), shift(value) * GDPterm, value),
               by = c("region", "sector")]
  }

  if (!is.null(scenParDemandFactors)) {
    print(paste0("Demand scenario specific changes were applied on energy service demand"))
    #Apply factors for specific demand scenario on output of demand regression if given/otherwise use default values from demand regression
    #Application: linear regression to given support points for the factors starting from 2020, constant factors after support points
    demandData <- merge(demandData, scenParDemandFactors, by = c("region", "period", "sector"), all.x = TRUE)
    demandData[period < policyStartYear, factor := 1]
    demandData[, factor := na.approx(factor, x = period, rule = 2), by = c("region", "sector")]
    demandData[, value := factor * value]
  } else {
    print(paste0("No demand scenario specific changes were applied on energy service demand"))
  }

  demandData <- demandData[, c("region", "period", "sector", "value")]
  if (anyNA(demandData)) stop("energy service demand calculated in toolDemandRegression() includes NAs")

  return(demandData)

}






