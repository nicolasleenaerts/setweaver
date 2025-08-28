# Test the pairmi function
test_that("probstat works", {
  # Load pairs
  pairmiresult = pairmi(misimdata[,2:6])
  # Check if results match predifined result
  expect_equal(pairmiresult, readRDS(test_path("fixtures", "pairmiresult.rds")))
})
