requireNamespace("dplyr")

  # Load a test dataset
data("beesRaw")


  # Run the function
testOut <- BeeBDC::harmoniseR(
  path = tempdir(), #The path to a folder that the output can be saved
  taxonomy = BeeBDC::beesTaxonomy, # The formatted taxonomy file
  data = beesRaw,
  speciesColumn = "scientificName",
  rm_names_clean = TRUE)


# Test the expected results
testthat::test_that("harmoniseR results unsuccessfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == FALSE), 4)
})
testthat::test_that("harmoniseR results successfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == TRUE), 96)
})


# Test classes
testthat::test_that("harmoniseR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("harmoniseR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("tbl_df","tbl" ,"data.frame"))
})


