requireNamespace("dplyr")
requireNamespace("tibble")
requireNamespace("magrittr")

  # Load a test dataset
data("beesRaw")


  # Run the function
testOut <- BeeDC::HarmoniseR(
  path = tempdir(), #The path to a folder that the output can be saved
  SynList = BeeDC::beesTaxonomy, # The formatted taxonomy file
  occurrences = beesRaw,
  speciesColumn = "scientificName",
  rm_names_clean = TRUE)


# Test the expected results
testthat::test_that("HarmoniseR results unsuccessfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == FALSE), 4)
})
testthat::test_that("HarmoniseR results successfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == TRUE), 96)
})


# Test classes
testthat::test_that("HarmoniseR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("HarmoniseR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("tbl_df","tbl" ,"data.frame"))
})


