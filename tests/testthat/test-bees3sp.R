
bees3sp <- BeeBDC::bees3sp


# Test the expected results
testthat::test_that("bees3sp expected number of columns", {
  testthat::expect_equal(ncol(bees3sp), 124)
})
testthat::test_that("bees3sp expected number of species", {
  testthat::expect_equal(length(unique(bees3sp$scientificName)), 3)
})


# Test classes
testthat::test_that("bees3sp expected class", {
  testthat::expect_type(bees3sp, "list")
})
testthat::test_that("bees3sp expected class", {
  testthat::expect_equal(attributes(bees3sp)$class, c("spec_tbl_df","tbl_df","tbl","data.frame" ))
})


