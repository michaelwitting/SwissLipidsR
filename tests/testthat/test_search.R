test_that("basic searching", {

  # peform search with single result
  results <- swissLipidsSearch("Phosphatidate (36:2)")

  expect_equal(nrow(results), 1)
  expect_equal(results$entity_id, "SLM:000055865")

  # perform search with multiple results
  results <- swissLipidsSearch("Glc")

  expect_equal(nrow(results), 148)

  # perform search with no result
  results <- swissLipidsSearch("abc")

  expect_equal(nrow(results), 0)

})

test_that("advanced search", {

  # search for name
  results <- swissLipidsAdvancedSearch(name = "PC(34:2)")
  expect_equal(nrow(results), 1)
  expect_equal(results$classification_level, "Species")

  # search for formula
  results <- swissLipidsAdvancedSearch(formula = "C39H76NO8P")
  expect_equal(nrow(results), 82)

  # search by mass
  results <- swissLipidsAdvancedSearch(mz = 410.243,
                                       adduct = "MassExact",
                                       massErrorRate = 0.001)
  expect_equal(nrow(results), 13)

  # search by InChIKey
  results <- swissLipidsAdvancedSearch(inchikey = "WTJKGGKOPKCXLL-VYOBOKEXSA-N")
  expect_equal(nrow(results), 1)

})

test_that("children search", {

  # serach for LysoPCs (multiple results)
  results <- swissLipidsGetChildren("SLM:000000352")
  expect_equal(nrow(results), 84)

  # single results
  results <- swissLipidsGetChildren("SLM:000055318")
  expect_equal(nrow(results), 1)

  # no result
  results <- swissLipidsGetChildren("SLM:000000651")


})
