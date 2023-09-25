  # load in the test dataset
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()


# Test the expected results
testthat::test_that("testTaxonomy expected number of columns", {
  testthat::expect_equal(ncol(testTaxonomy), 24)
})
testthat::test_that("testTaxonomy expect that number of unique ids == number of overall rows", {
  testthat::expect_equal(length(unique(testTaxonomy$id)), nrow(testTaxonomy))
})
testthat::test_that("testTaxonomy expect that number of 0[accid] == accepted[taxostatus]", {
  testthat::expect_equal(sum(testTaxonomy$accid == 0), 
                         sum(testTaxonomy$taxonomic_status == "accepted"))
})

  # Test classes
testthat::test_that("testTaxonomy expected class", {
  testthat::expect_type(testTaxonomy, "list")
})
testthat::test_that("testTaxonomy expected class", {
  testthat::expect_equal(attributes(testTaxonomy)$class, c("spec_tbl_df", "tbl_df","tbl","data.frame" ))
})



  # TEST the full data

taxonomyFile <- NULL
# load in the full dataset
taxonomyFile <- BeeBDC::beesTaxonomy()


if(!is.null(taxonomyFile)){
# Test the expected results
testthat::test_that("beesTaxonomy expected number of columns", {
  testthat::expect_equal(ncol(taxonomyFile), 24)
})
testthat::test_that("beesTaxonomy expect that number of unique ids == number of overall rows", {
  testthat::expect_equal(length(unique(taxonomyFile$id)), nrow(taxonomyFile))
})
testthat::test_that("beesTaxonomy expect that number of 0[accid] == accepted[taxostatus]", {
  testthat::expect_equal(sum(taxonomyFile$accid == 0), 
                         sum(taxonomyFile$taxonomic_status == "accepted"))
})

# Test classes
testthat::test_that("beesTaxonomy expected class", {
  testthat::expect_type(taxonomyFile, "list")
})
testthat::test_that("beesTaxonomy expected class", {
  testthat::expect_equal(attributes(taxonomyFile)$class, c("spec_tbl_df", "tbl_df","tbl","data.frame" ))
})

} #END !is.null
