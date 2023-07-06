requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("BeeDC")
requireNamespace("openxlsx")
requireNamespace("dplyr")
library(dplyr) ## could not use %>% without loading as library


# data below is FAKE
testData <- tibble::tribble(
                                               ~ID, ~institutionCode,             ~Det, ~Number, ~Male, ~Female, ~Collection_method,          ~Collection_date,  ~Collector,        ~Order,    ~Suborder,        ~Family,     ~Subfamily,        ~Tribe,         ~Genus,         ~subgenus, ~sp_group,      ~species, ~subspecies,            ~author,                             ~whole_sci_name, ~Country,     ~State,         ~County_Parish,                        ~Location,                 ~Lat,               ~Long,
              "discoverlife fake1 USGS-DRO. fake1",           "USGS",      "S. Droege",      1L,    0L,      1L,              "Net", "2019-08-07 00:00:00 UTC", "J. Fowler", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73,
              "discoverlife fake2 USGS-DRO. fake2",           "USGS",      "S. Droege",      1L,    1L,      0L,              "Net", "2019-09-06 00:00:00 UTC", "J. Fowler", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73,
              "discoverlife fake3 USGS-DRO. fake3",           "USGS", "S. Droege 2019",      1L,    1L,      0L,              "Net", "2019-04-18 00:00:00 UTC", "S. Droege", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73,
              "discoverlife fake4 USGS-DRO. fake4",           "USGS",      "S. Droege",      1L,    1L,      0L,              "Net", "2016-03-25 00:00:00 UTC", "S. Droege", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73
              )

# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.xslx", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"))


testOut1 <- BeeDC::readr_KP(path = paste0(tempdir()),
                              inFile = "/testData.xlsx",
                              outFile = "testDataOut.csv",
                              dataLicense = "All rights reserved")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_KP results columns TRUE", {
  testthat::expect_equal(resultsT, 29)
})

testthat::test_that("readr_KP results columns FALSE", {
  testthat::expect_equal(resultsF, 3)
})

testthat::test_that("readr_KP expected class", {
  testthat::expect_type(testOut1, "list")
})
