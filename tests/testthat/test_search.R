test_that("basic searching", {

  results <- swissLipidsSearch("Phosphatidate (36:2)")

  # test correct length of list
  expect_equal(length(results), 7)

  # check for correct ids
  expect_equal(results$entity_id, "SLM:000055865")

})
