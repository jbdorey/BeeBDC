requireNamespace("readr")
requireNamespace("BeeDC")
requireNamespace("dplyr")
requireNamespace("openxlsx")

library(dplyr) ## could not use %>% without loading as library

testData <- dplyr::tribble(
     ~Type,          ~Country, ~Muninciplaity, ~Gender,                                    ~Site,   ~Latitud, ~Longitude,     ~elevation,          ~date,               ~recordedby, ~catalogNumber, ~Collection, ~othercatalognumber, ~AssociatedTaxa,  ~taxonremarks,      ~Family,         ~Genus,      ~species, ~Citation,
        NA,                NA,   "Tamaulipas",    "1?", "10 km W of Antiguo Morelos, Highway 80",   "22.555",  "-99.167", "elev. 470 m.",  "6 Jul. 1990",         "Ilan Yarom leg.", "PALASEMC0026",      "SEMC",                  NA,              NA, "Lyucatanense", "Halictidae", "Lasioglossum", "yucatanense",        NA,
        NA,        "Campeche",             NA,    "1?",              "10 mi. north of HopelchÈn",    "19.89",   "-89.84",             NA, "17 Apr. 1962",       "L. A. Stange leg.",  "PALAUCD0001",      "UCDC",                  NA,              NA,     "Lpaxtoni", "Halictidae", "Lasioglossum",     "paxtoni",        NA,
        NA, "San Luis Potosi",             NA,    "1?",                 "12 km NW of El Naranjo", "22.59∞ N", "99.38∞ W", "elev. 400 m.",  "5 Jul. 1990", "Robert L. Minckley leg.", "PALASEMC0009",      "SEMC",                  NA,              NA, "Lyucatanense", "Halictidae", "Lasioglossum", "yucatanense",        NA
     )


# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.csv", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_BeeDC(dataset = "PALA",
                               path = tempdir(),
                               inFile = "testData.csv",
                               outFile = "testDataOut.csv",
                               dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_PALA results columns TRUE", {
  testthat::expect_equal(resultsT, 26)
})
testthat::test_that("readr_PALA results columns FALSE", {
  testthat::expect_equal(resultsF, 3)
})

testthat::test_that("readr_PALA expected class", {
  testthat::expect_type(testOut1, "list")
})
