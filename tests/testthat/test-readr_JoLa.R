requireNamespace("readr")
requireNamespace("BeeDC")
requireNamespace("dplyr")
requireNamespace("openxlsx")

library(dplyr) ## could not use %>% without loading as library

testData1 <- dplyr::tribble(
      ~Species,    ~Latitude1,     ~Longitude1, ~`Start.Date.(Year)`,
   "angelicum",         33.62,         -117.93,                1920L,
   "angelicum",         33.97,         -117.32,                1924L,
   "angelicum",         33.97,         -117.32,                1925L
   )

testData2 <- dplyr::tribble(
                  ~Species,    ~Latitude1,     ~Longitude1, ~`Start.Date.(Year)`,
               "angelicum",         33.97,         -118.44,                1981L,
               "angelicum",         34.15,         -118.14,                2000L,
               "angelicum",         34.15,         -118.14,                2000L
               )


testData <- list(testData1, testData2)

# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.xlsx", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                     sheetName = c("pre-1950", "post-1950"))

testOut1 <- BeeDC::readr_BeeDC(dataset = "JoLa",
                               path = tempdir(),
                               inFile = "testData.xlsx",
                               outFile = "testDataOut.csv",
                               sheet = c("pre-1950", "post-1950"),
                               dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")



# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)
rows <- nrow(testData1) + nrow(testData2)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_JoLa results columns TRUE", {
  testthat::expect_equal(resultsT, 13)
})
testthat::test_that("readr_JoLa results columns FALSE", {
  testthat::expect_equal(resultsF, 1)
})
testthat::test_that("readr_JoLa correct number of rows", {
   testthat::expect_equal(nrow(testOut1), rows)
})

testthat::test_that("readr_JoLa expected class", {
  testthat::expect_type(testOut1, "list")
})
