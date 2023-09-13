requireNamespace("dplyr")

  # Load a test dataset
beesRaw <- BeeBDC::beesRaw

  # Load the small testTaxonomy file
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()


  # Run the function
testOut <- harmoniseR(
  path = tempdir(), #The path to a folder that the output can be saved
  taxonomy = testTaxonomy, # The formatted taxonomy file
  data = beesRaw,
  speciesColumn = "scientificName",
  rm_names_clean = TRUE,
  stepSize = 50,
  mc.cores = 1)


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


