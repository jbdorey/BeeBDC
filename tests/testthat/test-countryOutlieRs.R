requireNamespace("dplyr")
requireNamespace("readr")

# Load in the test checklist data
system.file("extdata", "testChecklist.rda", package="BeeBDC") |>
  load()
# Load in the test dataset
beesFlagged <- BeeBDC::beesFlagged

  # Input a potentially difficult location on the map to test into an NA lat/lon slot
beesFlagged$decimalLatitude[[1]] <-  31.887646484374983
beesFlagged$decimalLongitude[[1]] <- 78.719726562500085 
beesFlagged$decimalLatitude[[2]] <-  78.719726562500085
beesFlagged$decimalLongitude[[2]] <- 31.887646484374983 

testOut <- BeeBDC::countryOutlieRs(
    # Speed up operation by providing only the relevant entries in the testChecklist
  checklist = testChecklist,
  data = beesFlagged %>% 
    dplyr::select(!tidyselect::any_of(c("countryMatch", ".countryOutlier","iso_a3"))),
  keepAdjacentCountry = FALSE,
  # running without a larger buffer to speed up tests
  pointBuffer = NULL,
  # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
  # Smaller numbers will result in much longer calculation times. 
  # We have not attempted a scale of 10.
  scale = 50)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("countryOutlieRs results TRUE/passed", {
  testthat::expect_equal(sum(testOut$.countryOutlier == TRUE, na.rm = TRUE), 74)
})
testthat::test_that("countryOutlieRs results FALSE/failed", {
  testthat::expect_equal(sum(testOut$.countryOutlier == FALSE, na.rm = TRUE), 5)
})
testthat::test_that("countryOutlieRs results NA/could not assess", {
  testthat::expect_equal(sum(is.na(testOut$.countryOutlier)), 21)
})

  # Test format
testthat::test_that("countryOutlieRs expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("countryOutlieRs results NA/could not assess", {
  testthat::expect_true( all(stringr::str_detect(attributes(testOut)$class, 
                                             c("tbl_df","tbl","data.frame"))))
})

# Expected number of columns matches input data
testthat::test_that("countryOutlieRs number of columns", {
    # Remove the three columns that will be updated by this function and then test that they are
      # added back in.
  testthat::expect_equal(ncol(beesFlagged %>% 
                                dplyr::select(!tidyselect::any_of(c("countryMatch", 
                                                                    ".countryOutlier",
                                                                    "iso_a3")))) + 3,
                         ncol(testOut))
})



