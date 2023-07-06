requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("BeeDC")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
              ~eventID, ~occurrenceID,      ~basisOfRecord,  ~eventDate,   ~Kingdom,        ~Order, ~Family_or_grp,      ~Tribe,         ~Genus, ~Species, ~Morphospecies, ~adult, ~sex,    ~Collector,          ~Determiner, ~individualCount, ~samplingProtocol, ~samplingEffort, ~sampleSizeValue, ~sampleSizeUnit, ~decimalLatitude, ~decimalLongitude, ~geodeticDatum, ~countryCode,                   ~country,
               "Be2:b",        "EML1", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Andrenidae",          NA,             NA,       NA,            38L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               1L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
               "Be2:b",        "EML2", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Andrenidae",          NA,             NA,       NA,            77L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               2L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
               "Be2:b",        "EML3", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",       "Apidae",          NA,             NA,       NA,            15L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               2L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
               "Be2:b",        "EML4", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Halictidae", "Halictini",             NA,       NA,            39L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               1L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
               "Be2:b",        "EML5", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Halictidae",          NA,  "Agapostemon",       NA,            13L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               1L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
               "Be2:b",        "EML6", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Halictidae",          NA, "Lasioglossum",       NA,            37L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               4L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America"
              )


# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_Lic(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_Lic results columns TRUE", {
  testthat::expect_equal(resultsT, 27)
})

testthat::test_that("readr_Lic results columns FALSE", {
  testthat::expect_equal(resultsF, 8)
})

testthat::test_that("readr_Lic expected class", {
  testthat::expect_type(testOut1, "list")
})
