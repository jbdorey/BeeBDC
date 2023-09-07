  # load in the dataset
data("beesTaxonomy")


# Test the expected results
testthat::test_that("beesTaxonomy expected number of columns", {
  testthat::expect_equal(ncol(beesTaxonomy), 23)
})
testthat::test_that("beesTaxonomy expect that number of unique ids == number of overall rows", {
  testthat::expect_equal(length(unique(beesTaxonomy$id)), nrow(beesTaxonomy))
})
testthat::test_that("beesTaxonomy expect that number of 0[accid] == accepted[taxostatus]", {
  testthat::expect_equal(sum(beesTaxonomy$accid == 0), 
                         sum(beesTaxonomy$taxonomic_status == "accepted"))
})

  # Test classes
testthat::test_that("beesTaxonomy expected class", {
  testthat::expect_type(beesTaxonomy, "list")
})
testthat::test_that("beesTaxonomy expected class", {
  testthat::expect_equal(attributes(beesTaxonomy)$class, c("tbl_df","tbl","data.frame" ))
})

