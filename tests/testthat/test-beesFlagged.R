beesFlagged <- BeeBDC::beesFlagged


# Test the expected results
testthat::test_that("beesFlagged expected number of columns", {
  testthat::expect_equal(ncol(beesFlagged), 124)
})
testthat::test_that("beesFlagged expected number of rows", {
  testthat::expect_equal(nrow(beesFlagged), 100)
})


# Test classes
testthat::test_that("beesFlagged expected class", {
  testthat::expect_type(beesFlagged, "list")
})
testthat::test_that("beesFlagged expected class", {
  testthat::expect_equal(attributes(beesFlagged)$class, c("spec_tbl_df","tbl_df","tbl","data.frame" ))
})
