requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("BeeDC")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
              ~catalogNumber,      ~Phylum, ~higherClassification,        ~Order,      ~family,        ~genus, ~specificEpithet,        ~scientificName, ~scientificNameAuthorship,     ~sex,         ~associatedTaxa,    ~identifiedBy, ~dateIdentified, ~recordedBy,   ~eventDate,      ~continent,    ~country,           ~stateProvince, ~county,                                              ~locality, ~decimalLatitude, ~decimalLongitude,             ~collectionID,       ~basisOfRecord,
              "RLMC00005185", "Arthropoda",             "Insecta", "Hymenoptera", "Colletidae",   "Cadeguala",     "albopilosa", "Cadeguala albopilosa",         "(Spinola, 1851)",   "Male",                      NA,       "L Packer",              NA,  "L Packer", "31/10/2001", "South America",     "Chile", "Region de la Araucania",      NA, "Parque Nacional Nahuelbuta, Piedra del Aguilla trail",         -37.8251,           -73.037, "University of Rochester", "preserved specimen",
              "RLMC00005186", "Arthropoda",             "Insecta", "Hymenoptera", "Colletidae",   "Cadeguala",     "albopilosa", "Cadeguala albopilosa",         "(Spinola, 1851)", "Female", "Aristotelia chilensis",       "L Packer",              NA, "A-I Gavel",     "2/7/05", "South America", "Argentina",   "Provincia del Chubut",      NA,                                "INTA Trevelin, site 2",     -43.09925833,         -71.54235, "University of Rochester", "preserved specimen",
              "RLMC00005187", "Arthropoda",             "Insecta", "Hymenoptera", "Andrenidae", "Spinoliella",         "rozeni",   "Spinoliella rozeni",        "Toro & Ruz, 1972",   "Male",                      NA, "AH Smith-Pardo",              NA,  "L Packer", "13/10/2001", "South America",     "Chile",                "Atacama",      NA,             "Tres Playitas, 10km north of Huasco Bajo",         -28.4066,          -71.1891, "University of Rochester", "preserved specimen",
              "RLMC00005188", "Arthropoda",             "Insecta", "Hymenoptera", "Andrenidae", "Spinoliella",         "rozeni",   "Spinoliella rozeni",        "Toro & Ruz, 1972", "Female",                      NA, "AH Smith-Pardo",              NA,  "L Packer", "13/11/1997", "South America",     "Chile",                "Atacama",      NA,                                             "Chanaral",         -26.3328,          -70.6347, "University of Rochester", "preserved specimen"
              )


# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))


testOut1 <- BeeDC::readr_BMin(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_BMin results columns TRUE", {
  testthat::expect_equal(resultsT, 28)
})

testthat::test_that("readr_BMin results columns FALSE", {
  testthat::expect_equal(resultsF, 2)
})

testthat::test_that("readr_BMin expected class", {
  testthat::expect_type(testOut1, "list")
})
