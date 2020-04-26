test_that("correct mapping of results", {

  from <- "SwissLipids"
  to <- "LipidMaps"
  ids <- c("SLM:000048885", "SLM:000000651")

  results <- swissLipidsMapping(from, to, ids)

  # test correct length of list
  expect_equal(nrow(results), 2)

  # check for correct ids
  expect_equal(results$to[[1]]$id, "LMGP01030010")
  expect_equal(results$to[[2]]$id, "LMGP01010005")

})
