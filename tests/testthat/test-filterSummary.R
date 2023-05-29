


  # Read in the test data
data("beesFlagged")

  # Run the function
filterTibble <- filterSummary(data = beesFlagged,
                              column = "scientificName",
                              outpath = paste0(tempdir(), "/filterTable.csv"))

   # Get the number of 100% fails and 0% fails
fails100 <- sum(filterTibble$percentFailed == 100)
fails0 <- sum(filterTibble$percentFailed == 0)

# Test class
testthat::test_that("filterSummary expected class", {
  testthat::expect_type(filterTibble, "list")
})
  # Test specific class
testthat::test_that("filterSummary expected class", {
  testthat::expect_true(any(stringr::str_detect(attributes(filterTibble)$class, 
                                                "tbl_df")))
})

# Test 100% fails size
testthat::test_that("filterSummary 100% failed size test", {
  testthat::expect_equal(fails100, 67)
})
testthat::test_that("filterSummary 0% failed size test", {
  testthat::expect_equal(fails0, 15)
})

# Test number of filtering columns
testthat::test_that("filterSummary test the numbero of flag columns in and out are equal. Excluding '.summary'", {
  testthat::expect_equal(colnames(beesFlagged %>% 
                                    dplyr::select(tidyselect::starts_with(".")) %>%
                                    dplyr::select(!tidyselect::any_of(".summary"))) %>%
                           length, 
                         colnames(filterTibble %>% 
                                    dplyr::select(tidyselect::starts_with(".")) %>%
                                    dplyr::select(!tidyselect::any_of(".summary"))) %>%
                           length)
})




