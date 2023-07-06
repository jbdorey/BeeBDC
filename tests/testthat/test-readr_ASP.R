requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("BeeDC")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
              ~catalogNumber,        ~family,     ~subfamily,       ~Tribe,         ~genus,      ~subgenus, ~Morphospecies, ~specificEpithet,   ~locality, ~Successional_Stage, ~decimalLatitude, ~decimalLongitude, ~coordinateUncertaintyInMeters, ~elevation, ~eventTime, ~samplingProtocol,   ~eventDate, ~sex, ~associatedTaxa,      ~continent,         ~recordedBy,
                          2L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Nananthidium",        "Msp 1",            "m16", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,     "p.m.",               "J",     "3/6/97",  "H",              NA, "South America", "Allan Smith-Pardo",
                          8L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Anthodioctes",        "Msp 2",      "mapirense", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,       "m.",               "J",  "23/9/1997",  "H",              NA, "South America", "Allan Smith-Pardo",
                          9L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Anthodioctes",        "Msp 2",      "mapirense", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,       "m.",               "J", "31/10/1997",  "H",              NA, "South America", "Allan Smith-Pardo",
                         10L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Anthodioctes",        "Msp 2",      "mapirense", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,     "p.m.",               "J",  "22/8/1997",  "H",              NA, "South America", "Allan Smith-Pardo"
              )


# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))


testOut1 <- BeeDC::readr_ASP(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_ASP results columns TRUE", {
  testthat::expect_equal(resultsT, 26)
})

testthat::test_that("readr_ASP results columns FALSE", {
  testthat::expect_equal(resultsF, 4)
})

testthat::test_that("readr_ASP expected class", {
  testthat::expect_type(testOut1, "list")
})
