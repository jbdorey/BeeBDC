
# load in the dataset
data("beesChecklist")


# Test the expected results
testthat::test_that("beesChecklist expected number of columns", {
  testthat::expect_equal(ncol(beesChecklist), 30)
})
testthat::test_that("beesChecklist expect that number of unique ids == number of overall rows", {
  testthat::expect_equal(length(unique(beesChecklist$id)), nrow(beesChecklist))
})
testthat::test_that("beesChecklist expect that number of 0[accid] == accepted[taxostatus]", {
  testthat::expect_equal(sum(beesChecklist$accid == 0), 
                         sum(beesChecklist$taxonomic_status == "accepted"))
})

# Test classes
testthat::test_that("beesChecklist expected class", {
  testthat::expect_type(beesChecklist, "list")
})
testthat::test_that("beesChecklist expected class", {
  testthat::expect_equal(attributes(beesChecklist)$class, c("spec_tbl_df","tbl_df","tbl","data.frame" ))
})

