requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("BeeDC")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
              ~Collection, ~ID_project,                    ~Species, ~Longitude, ~Latitude, ~Year,
                  "CECON",          1L, "Agapostemon_atrocaeruleus",    -89.439,    16.357, 2006L,
                  "CECON",          2L,     "Agapostemon_leunculus",    -89.377,     14.62, 2003L,
                  "CECON",          3L,     "Agapostemon_leunculus",    -90.211,    15.266, 2011L,
                  "CECON",          4L,  "Agapostemon_melliventris",     -90.06,    14.926, 2004L,
                  "CECON",          5L,     "Anthidium_maculifrons",    -91.492,    15.332, 2009L,
                  "CECON",          6L,     "Anthidium_maculifrons",    -89.363,    14.607, 2003L
              )

# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_EcoS(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_EcoS results columns TRUE", {
  testthat::expect_equal(resultsT, 14)
})

testthat::test_that("readr_EcoS results columns FALSE", {
  testthat::expect_equal(resultsF, 0)
})

testthat::test_that("readr_EcoS expected class", {
  testthat::expect_type(testOut1, "list")
})
