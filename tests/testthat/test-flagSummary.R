requireNamespace("dplyr")


  # Read in the test data
data("beesFlagged")

  # Run the function
flagTibble <- flagSummary(data = beesFlagged,
                              column = "scientificName",
                              outPath = paste0(tempdir(), "/filterTable.csv"))

   # Get the number of 100% fails and 0% fails
fails100 <- sum(flagTibble$percentFailed == 100)
fails0 <- sum(flagTibble$percentFailed == 0)

# Test class
testthat::test_that("flagSummary expected class", {
  testthat::expect_type(flagTibble, "list")
})
  # Test specific class
testthat::test_that("flagSummary expected class", {
  testthat::expect_true(any(stringr::str_detect(attributes(flagTibble)$class, 
                                                "tbl_df")))
})

# Test 100% fails size
testthat::test_that("flagSummary 100% failed size test", {
  testthat::expect_equal(fails100, 67)
})
testthat::test_that("flagSummary 0% failed size test", {
  testthat::expect_equal(fails0, 15)
})

# Test number of filtering columns
testthat::test_that("flagSummary test the numbero of flag columns in and out are equal. Excluding '.summary'", {
  testthat::expect_equal(colnames(beesFlagged %>% 
                                    dplyr::select(tidyselect::starts_with(".")) %>%
                                    dplyr::select(!tidyselect::any_of(".summary"))) %>%
                           length, 
                         colnames(flagTibble %>% 
                                    dplyr::select(tidyselect::starts_with(".")) %>%
                                    dplyr::select(!tidyselect::any_of(".summary"))) %>%
                           length)
})




