test_that("stock calculation works", {
  load(test_path("testCalculateFleetComposition.Rdata"))
  output <- toolCalculateFleetComposition(ESdemandFVsalesLevel,
                                          vehDepreciationFactors,
                                          vehSalesAndModeShares,
                                          annualMileage,
                                          loadFactor,
                                          helpers)
  testOutput <- output$fleetVehNumbers[technology == "BEV" & period <= 2030]
  expect_equal(testOutput,
               exampleOutput,
               tolerance = 1e-5,
               ignore_attr = TRUE)
})
