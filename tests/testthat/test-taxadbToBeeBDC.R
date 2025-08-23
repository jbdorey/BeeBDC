requireNamespace("dplyr")
requireNamespace("BeeBDC")

# If rnaturalearthdata is present, run tests
if(requireNamespace("taxadb")){

  requireNamespace("taxadb")

  # Run the taxadbToBeeBDC function using the example
 ApisTaxonomy <- BeeBDC::taxadbToBeeBDC(name = "Apis",
                                        rank = "Genus",
                                        provider = "gbif",
                                        version = "22.12",
                                        removeEmptyNames = TRUE,
                                        outPath = tempdir(),
                                        fileName = "TEST_out.csv",
                                          # Must be NULL to avoid deprecation warning
                                        overwrite = NULL, lines = NULL)
 


# Test the expected results
# Test classes
testthat::test_that("taxadbToBeeBDC expected class", {
  testthat::expect_type(ApisTaxonomy, "list")
})
testthat::test_that("taxadbToBeeBDC expected class", {
  testthat::expect_equal(attributes(ApisTaxonomy)$class, c("tbl_df","tbl","data.frame"))
})

# Check directory that the plot was saved
testthat::test_that("taxadbToBeeBDC plot saved?", {
  testthat::expect_true(any(stringr::str_detect(list.files(tempdir()), "TEST_out.csv")))
})

# Check the number of columns in the dataset
testthat::test_that("taxadbToBeeBDC expected number of columns", {
  testthat::expect_equal(length(colnames(ApisTaxonomy)), 29)
})


} # END if require

