
# load in the dataset
data("beesChecklist")


# Test the expected results
testthat::test_that("beesChecklist expected number of columns", {
  testthat::expect_equal(ncol(beesChecklist), 23)
})

# Test classes
testthat::test_that("beesChecklist expected class", {
  testthat::expect_type(beesChecklist, "list")
})
testthat::test_that("beesChecklist expected class", {
  testthat::expect_equal(attributes(beesChecklist)$class, c("spec_tbl_df","tbl_df","tbl","data.frame" ))
})

