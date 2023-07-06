requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("openxlsx")
requireNamespace("dplyr")


testData <- tibble::tribble(
  ~'Specimen Number', ~`Farmers (names redacted)`, ~'Date Blue vane traps were active', ~'Date Range for collection', ~'Sampling Round', ~'Treatment type', ~'Trap number',          ~County, ~State, ~'GPS Coordinates of Traps',         ~Genus, ~`Species (if available)`, ~`other (example coleoptera)`, ~`notes (example hymenoptera-ant)`, ~comment,
             6376L,                          3L,                                NA,              "05/24-06/01",              1L,        "Grazed",         "S7", "Kit Carson Co.",   "CO",      "39.210N, -102.884W", "Species",                     "135",                            NA,                                 NA,       NA,
             6051L,                          3L,                                NA,              "05/24-06/01",              1L,      "Ungrazed",         "S6", "Kit Carson Co.",   "CO",      "39.210N, -102.883W", "Species",                     "165",                            NA,                                 NA,       NA,
             3291L,                          2L,                                NA,              "06/21-06/28",              2L,        "Grazed",         "T4",    "Perkins Co.",   "NE",      "40.799N, -101.944W",  "Species",             "secret",                            NA,                                 NA,       NA,
             3457L,                          2L,                                NA,              "06/21-06/28",              2L,      "Ungrazed",         "T8",    "Perkins Co.",   "NE",      "40.799N, -101.936W",  "Species",             "secret",                            NA,                                 NA,       NA,
              103L,                          1L,                                NA,              "05/24-06/01",              1L,        "Fallow",         "F4",       "Weld Co.",   "CO",      "40.503N, -103.901W",   "Species",               "secret",                            NA,                                 NA,       NA
  )


# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.xslx", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName="Sheet1")

testOut1 <- BeeDC::readr_EaCO(path = paste0(tempdir()),
                              inFile = "/testData.xlsx",
                              outFile = "testDataOut.csv",
                              dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")



# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_EaCO results columns TRUE", {
  testthat::expect_equal(resultsT, 18)
})
testthat::test_that("readr_EaCO results columns FALSE", {
  testthat::expect_equal(resultsF, 0)
})

testthat::test_that("readr_EaCO expected class", {
  testthat::expect_type(testOut1, "list")
})



