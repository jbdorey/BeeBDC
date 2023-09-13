beesRaw <- BeeBDC::beesRaw


# Test the expected results
testthat::test_that("beesRaw expected number of columns", {
  testthat::expect_equal(ncol(beesRaw), 90)
})
testthat::test_that("beesRaw expected number of rows", {
  testthat::expect_equal(nrow(beesRaw), 100)
})


# Test classes
testthat::test_that("beesRaw expected class", {
  testthat::expect_type(beesRaw, "list")
})
testthat::test_that("beesRaw expected class", {
  testthat::expect_equal(attributes(beesRaw)$class, c("spec_tbl_df","tbl_df","tbl","data.frame" ))
})
