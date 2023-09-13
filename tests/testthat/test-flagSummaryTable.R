requireNamespace("dplyr")


  # Read in the test data
beesFlagged <- BeeBDC::beesFlagged

  # Run the function
flagTibble <- flagSummaryTable(data = beesFlagged,
                              column = "scientificName",
                              outPath = tempdir(),
                              fileName = "/filterTable.csv")

   # Get the number of 100% fails and 0% fails
fails100 <- sum(flagTibble$percentFailed == 100)
fails0 <- sum(flagTibble$percentFailed == 0)

# Test class
testthat::test_that("flagSummaryTable expected class", {
  testthat::expect_type(flagTibble, "list")
})
  # Test specific class
testthat::test_that("flagSummaryTable expected class", {
  testthat::expect_true(any(stringr::str_detect(attributes(flagTibble)$class, 
                                                "tbl_df")))
})

# Test 100% fails size
testthat::test_that("flagSummaryTable 100% failed size test", {
  testthat::expect_equal(fails100, 63)
})
testthat::test_that("flagSummaryTable 0% failed size test", {
  testthat::expect_equal(fails0, 15)
})

# Test number of filtering columns
testthat::test_that("flagSummaryTable test the number of flag columns in and out are equal, minus two. Excluding '.summary'", {
  testthat::expect_equal(colnames(beesFlagged %>% 
                                    dplyr::select(tidyselect::starts_with(".")) %>%
                                    dplyr::select(!tidyselect::any_of(".summary"))) %>%
                           length - 2, 
                         colnames(flagTibble %>% 
                                    dplyr::select(tidyselect::starts_with(".")) %>%
                                    dplyr::select(!tidyselect::any_of(".summary"))) %>%
                           length)
})




