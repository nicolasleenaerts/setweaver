# Test the probstat function
test_that("probstat works", {
  # Load pairs
  pairmiresult = readRDS(test_path("fixtures", "pairmiresult.rds"))
  # Run probstat
  probstatresult = probstat(misimdata$y,pairmiresult$expanded.data,nfolds = 5)
  # See if the result is a dataframe
  expect_s3_class(probstatresult, "data.frame")
  # Check if there is no missing data
  expect_equal(base::length(base::which(stats::complete.cases(probstatresult)==F)), 0)
  # Check if results match predifined result
  expect_equal(probstatresult, readRDS(test_path("fixtures", "probstatresult.rds")))
})
