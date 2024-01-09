


toolDemandRegression <- function(historicalESdemand, CAPEXandOPEX, GDPperCap, POP,
                                  scenParDemandRegression, scenParRegionalParDemandRegression, scenParDemandFactors) {

  historicalESdemand <- copy(inputData$histESdemand)
  CAPEXandOPEX <-copy(inputData$combinedCAPEXandOPEX)
  GDPperCap <-copy(inputData$GDPpcMER)
  regionalParDemandRegression <-scenModelPar$scenParRegionalDemRegression
  scenParDemandRegression <-scenModelPar$scenParDemRegression
  scenParDemandFactors <-scenModelPar$scenParDemFactors

  # interpolate SSP specific elasticities based on GDP MER per capita ----------------------------
  setnames(GDPperCap, "value", "regionGDPpcMER")
  approxElasticities <- function(category, elasticityGDPValues, GDPperCap) {
    # creates a function to interpolate elasticitis for certain GDP values
    appfun <- approxfun(
    x = elasticityValues[variable == category, GDPpcMER],
    y = elasticityValues[variable == category, value], rule = 2)

    elasticityRegionValues <- copy(GDPperCap)[, `:=`(variable = category, value = appfun(regionGDPpc))]
    return(elasticityRegionValues)
  }
  categories <- unique(scenParDemandRegression$variable)
  regionalIncomeElasticities <- rbindlist(lapply(categories, approxElasticities, scenParDemandRegression, GDPperCap))

  # apply SSP specific regional changes------------------------------------------------------------
  if (!is.null(scenParRegionalParDemandRegression)) {
    scenParRegionalParDemandRegression <- melt(scenParRegionalParDemandRegression,
                                               id.vars = c("region", "variable"), variable.name = "period", value.name = "regionalSummand")
    regionalIncomeElasticities <- approx_dt(regionalIncomeElasticities, unique(regionalIncomeElasticities$period), "period", "regionalSummand",
                                        c("variable", "region"), extrapolate = TRUE)
    regionalIncomeElasticities <- merge(regionalIncomeElasticities, scenParRegionalParDemandRegression,
                                        by = c("region", "period", "variable"), all.x = TRUE)
    regionalIncomeElasticities[period < policyStartYear, regionalSummand := 0]
    regionalIncomeElasticities[, value := value + regionalSummand]
  }

  # merge with Population data ------------------------------------------------------------
  setnames(POP, "value", "population")
  GDP <- merge(GDP, POP, by = c("region", "period"))
  GDP[, regionGDPMER := regionGDPpcMER * population]

  regressionData <- dcast(regressionData, region + period + regionGDPpcMER ~ var, value.var = "value")
  regressionData <- merge(regionalIncomeElasticities, GDP, by = c("region", "period"))

  # calculate growth rates
  regressionData[,`:=`(GDPgrowthRate = regionGDPMER/shift(regionGDPMER),
                       GDPpcgrowthRate = regionGDPpcMER/shift(regionGDPpcMER),
                       POPgrowthRate = population/shift(population), by = c("region")]
  ## merge GDP_POP and price elasticity
  gdp_pop = merge(gdp_pop, full_el[,c("region", "year", "income_elasticity_pass_lo", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("region", "year"))

  #calculate the indexes raised to the corresponding elasticities
  gdp_pop[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo=index_GDPcap^income_elasticity_pass_lo)]


}
