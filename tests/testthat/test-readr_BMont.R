requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("tidyverse")
library(dplyr) ## could not use %>% without loading as library - namespace is inadequate, apparently


testData <- tibble::tribble(
                                                             ~occurence_lsid, ~language,      ~basisOfRecord, ~catalogNumber,                                  ~scientificNameID,     ~scientificName,     ~sex, ~lifeStage, ~organismQuantity,                                                       ~institutionCode,                                                                                 ~fieldNotes,                                ~locality, ~decimalLatitude, ~decimalLongitude,    ~GPS_device,        ~country, ~stateProvince,   ~county, ~municipality,                                                          ~georeferenceRemarks, ~VerbatimEventDate, ~eventDate, ~eventTime, ~recordedBy, ~samplingProtocol, ~fieldNumber,              ~identifiedBy, ~dateIdentified, ~typeStatus,   ~kingdom,      ~phylum,     ~class,        ~order,  ~family,   ~genus, ~specificEpithet, ~infraspecificEpithet, ~habitat,       ~associatedTaxa,                                            ~source,              ~modified,
              "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014085",      "en", "PreservedSpecimen",  "MTEC 014085", "urn:lsid:biosci.ohio-state.edu:osuc_names:169432",  "Bombus sylvicola", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                    "[MONT. Powell Co. Flint Crk. Mts. 8500ft-9500ft, 28 July 1961 B Vogel]", "Flint Creek Mountains, Granite Co., MT",          46.3624,         -113.1077, "Google Earth", "United States",      "Montana", "Granite",            NA, "Label says Powell Co., but the Flint Creek Range is actually in Granite Co.",        "28-Jul-61",  "7/28/61",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",      "sylvicola",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014085", "2015-11-06T12:23:32Z",
              "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014086",      "en", "PreservedSpecimen",  "MTEC 014086", "urn:lsid:biosci.ohio-state.edu:osuc_names:169432",  "Bombus sylvicola", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                    "[MONT. Powell Co. Flint Crk. Mts. 8500ft-9500ft, 28 July 1961 B Vogel]", "Flint Creek Mountains, Granite Co., MT",          46.3624,         -113.1077, "Google Earth", "United States",      "Montana", "Granite",            NA, "Label says Powell Co., but the Flint Creek Range is actually in Granite Co.",        "28-Jul-61",  "7/28/61",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",      "sylvicola",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014086", "2015-11-06T12:23:32Z",
              "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014087",      "en", "PreservedSpecimen",  "MTEC 014087", "urn:lsid:biosci.ohio-state.edu:osuc_names:169432",  "Bombus sylvicola", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                                  "[MONT. Carbon Co. Beartooth Plateau, 9 Jly 1963 B Vogel]",      "Beartooth Plateau, Carbon Co., MT",          45.0214,         -109.5779, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,         "9-Jul-63",   "7/9/63",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",      "sylvicola",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014087", "2015-09-28T11:15:16Z",
              "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014088",      "en", "PreservedSpecimen",  "MTEC 014088", "urn:lsid:biosci.ohio-state.edu:osuc_names:128375", "Bombus flavifrons", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                                    "[MONT: Carbon Co. E. Rosebud L. 20 June 1962 B. Vogel]",        "E. Rosebud Lake, Carbon Co., MT",          45.2002,         -109.6412, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,        "20-Jun-62",  "6/20/62",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",     "flavifrons",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014088", "2015-09-28T11:15:17Z",
              "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014089",      "en", "PreservedSpecimen",  "MTEC 014089", "urn:lsid:biosci.ohio-state.edu:osuc_names:128375", "Bombus flavifrons", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)", "[MONT: Carbon Co. E. Rosebud L. 20 June 1962 B. Vogel] [on Penstemon confertas] confertus",        "E. Rosebud Lake, Carbon Co., MT",          45.2002,         -109.6412, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,        "20-Jun-62",  "6/20/62",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",     "flavifrons",                    NA,       NA, "Penstemon confertus", "http://hol.osu.edu/spmInfo.html?id=MTEC%20014089", "2015-09-28T11:15:17Z",
              "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014090",      "en", "PreservedSpecimen",  "MTEC 014090", "urn:lsid:biosci.ohio-state.edu:osuc_names:128375", "Bombus flavifrons", "female",   "worker",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)", "[MONT: Carbon Co. E. Rosebud L. 20 June 1962 B. Vogel] [on Penstemon confertas] confertus",        "E. Rosebud Lake, Carbon Co., MT",          45.2002,         -109.6412, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,        "20-Jun-62",  "6/20/62",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",     "flavifrons",                    NA,       NA, "Penstemon confertus", "http://hol.osu.edu/spmInfo.html?id=MTEC%20014090", "2015-09-28T11:15:18Z"
              )

# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_BMont(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")



# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_BMont results columns TRUE", {
  testthat::expect_equal(resultsT, 33)
})
testthat::test_that("readr_BMont results columns FALSE", {
  testthat::expect_equal(resultsF, 13)
})

testthat::test_that("readr_BMont expected class", {
  testthat::expect_type(testOut1, "list")
})
