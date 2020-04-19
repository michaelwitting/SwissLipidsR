test_that("basic searching", {

  results <- swissLipidsSearch("Phosphatidate (36:2)")

  # test correct length of list
  expect_equal(length(results), 7)

  # check for correct ids
  expect_equal(results$entity_id, "SLM:000055865")

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

})
