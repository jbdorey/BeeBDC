requireNamespace("dplyr")

  # Load a test dataset
beesRaw <- BeeBDC::beesRaw

  # Load the small testTaxonomy file
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()


#### 1.0 Run normal function ####
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
  testthat::expect_equal(sum(testOut$.invalidName == FALSE), 9)
})
testthat::test_that("harmoniseR results successfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == TRUE), 91)
})


# Test classes
testthat::test_that("harmoniseR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("harmoniseR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("tbl_df","tbl" ,"data.frame"))
})



#### 2.0 Run checkVerbatim function ####
# Run the function
testOut <- harmoniseR(
  path = tempdir(), #The path to a folder that the output can be saved
  taxonomy = testTaxonomy, # The formatted taxonomy file
  data = beesRaw %>% 
      # Add some extra data to test checkVerbatim = TRUE
    dplyr::bind_rows(
      dplyr::tibble(database_id = c("verb112312", "verb212312"),
                    scientificName = c("Perdita Sandhouse, 1937","Perdita Sandhouse, 1937"),
                    verbatimScientificName = c("Perdita ignota", "Perdita ignota"),
                    family = c("Andrenidae", "Andrenidae"),
                    genus = c("Perdita","Perdita")
                    )
    ),
  speciesColumn = "scientificName",
  rm_names_clean = TRUE,
  checkVerbatim = TRUE,
  stepSize = 50,
  mc.cores = 1)


# Test the expected results
testthat::test_that("harmoniseR results unsuccessfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == FALSE), 9)
})
testthat::test_that("harmoniseR results successfuly matched", {
  testthat::expect_equal(sum(testOut$.invalidName == TRUE), 93)
})


# Test classes
testthat::test_that("harmoniseR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("harmoniseR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("tbl_df","tbl" ,"data.frame"))
})
