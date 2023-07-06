requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("openxlsx")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
                ~studyID,           ~siteID, ~year,     ~date,                           ~animalID, ~abundance, ~abundanceMethod, ~samplingMethod, ~numCensus, ~samplingIntensity,                                                                                                                                                                                   ~censusType,     ~fieldDist,  ~flowering, ~decimalLatitude, ~decimalLongitude,                ~studyLocation, ~habitatType,                ~siteDescription,
                "Ball01", "Bastrop Gardens", 2013L, "2013/5/22",             "Agapostemon angelicus",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
                "Ball01", "Bastrop Gardens", 2013L, "2013/5/22",               "Agapostemon texanus",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
                "Ball01", "Bastrop Gardens", 2013L, "2013/5/22",            "Anthophora californica",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
                "Ball01", "Bastrop Gardens", 2013L, "2013/5/22",                "Ceratina shinnersi",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
                "Ball01", "Bastrop Gardens", 2013L, "2013/5/22",                  "Ceratina strenua",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
                "Ball01", "Bastrop Gardens", 2013L, "2013/5/22", "Lasioglossum (Dialictus) sp.TX-14",        12L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden"
              ) 
  # Because readr_Bal ignores the first row of data, get the columns names and add them again 
  # to the data to be saved (and eventually ignored).
testDataCols <-colnames(testData)
  # Convert into a data.frame instead of a tibble to relax the requirements on colmn classes matching
testData <- testData %>% 
  as.data.frame(stringsAsFactors = FALSE) 
  # Re-bind the column names to the first row of data.
testData <- testDataCols %>%
  rbind(testData)

# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.xslx", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName = "animal_data")

testOut1 <- BeeDC::readr_Bal(path = paste0(tempdir()),
                             inFile = "/testData.xlsx",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_Bal results columns TRUE", {
  testthat::expect_equal(resultsT, 19)
})

testthat::test_that("readr_Bal results columns FALSE", {
  testthat::expect_equal(resultsF, 8)
})

testthat::test_that("readr_Bal expected class", {
  testthat::expect_type(testOut1, "list")
})


