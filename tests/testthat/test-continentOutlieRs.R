requireNamespace("dplyr")
requireNamespace("readr")


  # If rnaturalearthdata is present, run tests
if(requireNamespace("rnaturalearthdata")){

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
beesFlagged$decimalLatitude[[3]] <-  18.846970
beesFlagged$decimalLongitude[[3]] <- 18.846970 
beesFlagged$decimalLatitude[[4]] <-  30.328259
beesFlagged$decimalLongitude[[4]] <- 101.838181 
 
testOut <- continentOutlieRs(
  # Speed up operation by providing only the relevant entries in the testChecklist
  checklist = testChecklist,
  data = beesFlagged %>% 
    dplyr::select(!tidyselect::any_of(c("continentyMatch", ".continentOutlier"))),
  keepAdjacentContinent = FALSE,
  # running without a larger buffer to speed up tests
  pointBuffer = NULL,
  # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
  # Smaller numbers will result in much longer calculation times. 
  # We have not attempted a scale of 10.
  scale = 50)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("continentOutlieRs results TRUE/passed", {
  testthat::expect_equal(sum(testOut$.continentOutlier == TRUE, na.rm = TRUE), 74)
})
testthat::test_that("continentOutlieRs results FALSE/failed", {
  testthat::expect_equal(sum(testOut$.continentOutlier == FALSE, na.rm = TRUE), 5)
})
testthat::test_that("continentOutlieRs results NA/could not assess", {
  testthat::expect_equal(sum(is.na(testOut$.continentOutlier)), 21)
})

# Test format
testthat::test_that("continentOutlieRs expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("continentOutlieRs results NA/could not assess", {
  testthat::expect_true( all(stringr::str_detect(attributes(testOut)$class, 
                                                 c("tbl_df","tbl","data.frame"))))
})

# Expected number of columns matches input data
testthat::test_that("continentOutlieRs number of columns", {
  # Remove the three columns that will be updated by this function and then test that they are
  # added back in.
  testthat::expect_equal(ncol(beesFlagged %>% 
                                dplyr::select(!tidyselect::any_of(c("continentMatch", 
                                                                    ".continentOutlier")))) + 2,
                         ncol(testOut))
})

} # END if require

