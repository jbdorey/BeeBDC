requireNamespace("readr")
requireNamespace("tibble")


testData <- tibble::tribble(
                                ~organismName, ~individualCount,  ~county, ~stateProvince,                             ~locale, ~observationDate, ~collectionMethod,            ~references, ~institutionCode, ~latitude, ~longitude, ~georeferenceMethod, ~georeferenceMethodNotes,  ~georeferencedBy, ~determiner, ~lifeStage,                                                 ~Data.Source.Link, ~Notes,
              "Agapostemon_angelicus/texanus",               2L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "3/17/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
                   "Agapostemon_melliventris",               2L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "3/17/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
                           "Andrena_alamonis",               2L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "3/17/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
                      "Agapostemon_angelicus",               6L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "4/12/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
                    "Agapostemon_coloradinus",               1L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "4/12/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA
              )


# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_SMC(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")



# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
  # tibbles are a special case of lists)
testthat::test_that("readr_BBD results columns TRUE", {
  testthat::expect_equal(resultsT, 20)
})
testthat::test_that("readr_BBD results columns FALSE", {
  testthat::expect_equal(resultsF, 8)
})

testthat::test_that("readr_BBD expected class", {
  testthat::expect_type(testOut1, "list")
})



