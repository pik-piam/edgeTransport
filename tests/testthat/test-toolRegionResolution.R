regionMap <- unique(data.table::fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                  package = "edgeTransport"))[, c("regionCode12", "regionCode21")])
testHelpers <- list(regionmappingISOto21to12 = regionMap)
regions12 <- unique(regionMap$regionCode12)
regions21 <- unique(regionMap$regionCode21)

# the functions rename their inputs in place, so each test passes copies of the shared tables

# 12-region data in the format of the REMIND ES demand
esDemand12 <- data.table::CJ(region = regions12, period = c(2020, 2030), sector = c("trn_pass", "trn_freight"))
esDemand12[, value := seq_len(.N) * 10]
esDemand12[, unit := ifelse(sector == "trn_pass", "billion pkm/yr", "billion tkm/yr")]
esDemand12[, variable := "ES"]

# 21-region weights in the format of the demand regression output (without variable column)
weights21 <- data.table::CJ(region = regions21, period = c(2020, 2030), sector = c("trn_pass", "trn_freight"))
weights21[, value := seq_len(.N)]

test_that("unweighted disaggregation copies the 12-region value to each 21-region", {
  fuelCosts12 <- data.table::CJ(region = regions12, period = c(2020, 2030), technology = c("BEV", "Liquids"))
  fuelCosts12[, value := seq_len(.N) * 1.5]
  fuelCosts12[, unit := "US$2017/MJ"]
  fuelCosts12[, variable := "Fuel costs"]

  out <- toolDisaggregateRegions12to21(data.table::copy(fuelCosts12), testHelpers)

  expected <- merge(fuelCosts12, regionMap, by.x = "region", by.y = "regionCode12", allow.cartesian = TRUE)
  expected[, region := regionCode21][, regionCode21 := NULL]
  setcolorder(expected, names(out))
  expect_equal(out[order(region, period, technology)], expected[order(region, period, technology)],
               ignore_attr = TRUE)
})

for (withVariable in c(FALSE, TRUE)) {
  # the cached sectorESdemand.RDS carries a variable column, the regression output does not
  w <- data.table::copy(weights21)
  if (withVariable) w[, variable := "ES"]
  label <- if (withVariable) "cached weights" else "regression weights"

  test_that(paste("weighted disaggregation preserves group totals and weight shares,", label), {
    out <- toolDisaggregateRegions12to21(data.table::copy(esDemand12), testHelpers, data.table::copy(w))
    out <- merge(out, regionMap, by.x = "region", by.y = "regionCode21")

    # total per 12-region, period and sector is preserved
    totals <- merge(out[, .(value = sum(value)), by = c("regionCode12", "period", "sector")], esDemand12,
                    by.x = c("regionCode12", "period", "sector"), by.y = c("region", "period", "sector"))
    expect_equal(totals$value.x, totals$value.y)

    # within each group, the shares equal the weight shares
    shares <- merge(out, w[, c("region", "period", "sector", "value")], by = c("region", "period", "sector"))
    shares[, `:=`(outShare = value.x / sum(value.x), weightShare = value.y / sum(value.y)),
           by = c("regionCode12", "period", "sector")]
    expect_equal(shares$outShare, shares$weightShare)
  })
}

test_that("weighted disaggregation fails if a weight is missing", {
  w <- weights21[!(region == "DEU" & period == 2030)]
  expect_error(suppressWarnings(toolDisaggregateRegions12to21(data.table::copy(esDemand12), testHelpers, w)))
})

# 21-region data in the format of the fleet ES demand
esDemand21 <- data.table::CJ(region = regions21, period = c(2020, 2030), technology = c("BEV", "Liquids"))
esDemand21[, value := seq_len(.N) * 10]
esDemand21[, unit := "billion pkm/yr"]
esDemand21[, variable := "ES"]

test_that("unweighted aggregation sums the 21-region values", {
  out <- toolAggregateRegions21to12(data.table::copy(esDemand21), testHelpers)

  expected <- merge(esDemand21, regionMap, by.x = "region", by.y = "regionCode21")
  expected <- expected[, .(value = sum(value)), by = c("regionCode12", "period", "technology", "unit", "variable")]
  setnames(expected, "regionCode12", "region")
  setcolorder(expected, names(out))
  expect_equal(out[order(region, period, technology)], expected[order(region, period, technology)],
               ignore_attr = TRUE)
})

test_that("weighted aggregation gives the weighted average", {
  capCost21 <- data.table::copy(esDemand21)[, `:=`(value = seq_len(.N) * 0.1, unit = "US$2017/pkm",
                                                   variable = "Capital costs")]
  esWeight <- data.table::copy(esDemand21)[, c("unit", "variable") := NULL]

  out <- toolAggregateRegions21to12(data.table::copy(capCost21), testHelpers, data.table::copy(esWeight))

  expected <- merge(capCost21, esWeight, by = c("region", "period", "technology"))
  expected <- merge(expected, regionMap, by.x = "region", by.y = "regionCode21")
  expected <- expected[, .(value = sum(value.x * value.y) / sum(value.y)),
                       by = c("regionCode12", "period", "technology", "unit", "variable")]
  setnames(expected, "regionCode12", "region")
  setcolorder(expected, names(out))
  expect_equal(out[order(region, period, technology)], expected[order(region, period, technology)],
               ignore_attr = TRUE)
})

test_that("weighted disaggregation followed by aggregation returns the 12-region input", {
  out21 <- toolDisaggregateRegions12to21(data.table::copy(esDemand12), testHelpers, data.table::copy(weights21))
  out <- toolAggregateRegions21to12(out21, testHelpers)
  expect_equal(out[order(region, period, sector)], esDemand12[order(region, period, sector)],
               ignore_attr = TRUE)
})

test_that("weighted aggregation fails if a weight is NA or a region is missing in the weights", {
  esWeight <- data.table::copy(esDemand21)[, c("unit", "variable") := NULL]
  esWeightNA <- data.table::copy(esWeight)[region == "DEU" & period == 2030, value := NA]
  expect_error(suppressWarnings(toolAggregateRegions21to12(data.table::copy(esDemand21), testHelpers, esWeightNA)))
  esWeightMissing <- esWeight[region != "DEU"]
  expect_error(suppressWarnings(toolAggregateRegions21to12(data.table::copy(esDemand21), testHelpers, esWeightMissing)))
})
