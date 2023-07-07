requireNamespace("readr")
requireNamespace("BeeDC")
requireNamespace("dplyr")
requireNamespace("openxlsx")

library(dplyr) ## could not use %>% without loading as library

testData <- dplyr::tribble(
            ~fam,        ~genus,            ~sp,                   ~species, ~sex,         ~locality,    ~munic, ~state,  ~g,  ~m,    ~s,          ~y,   ~G,  ~M,    ~S,           ~x, ~elev,         ~ecoregion,        ~veget, ~day, ~month, ~year, ~institutionCode,
    "Halictidae", "Agapostemon", "melliventris", "Agapostemon melliventris",  "M",       "El Sauzal",  "Mulegé",  "BCS", 27L, 10L,  22.3, 27.17286111, 112L, 52L,   5.7,   -112.86825,  132L,  "Vizcaíno Desert",       "oasis",  13L,  "jul", 2017L,         "CIBNOR",
    "Halictidae", "Agapostemon",    "mexicanus",    "Agapostemon mexicanus",  "M", "Cd. Insurgentes", "Comondú",  "BCS", 25L,  8L, 28.97, 25.14138056, 111L, 48L, 40.03, -111.8111194,   60L, "Magdalena plains", "xeric scrub",   3L,  "jun", 2017L,         "CIBNOR",
    "Halictidae", "Agapostemon", "melliventris", "Agapostemon melliventris",  "M",       "El Sauzal",  "Mulegé",  "BCS", 27L, 10L,  22.3, 27.17286111, 112L, 52L,   5.7,   -112.86825,  132L,  "Vizcaíno Desert",       "oasis",  13L,  "jul", 2017L,         "CIBNOR"
    )


# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.xlsx", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                     sheetName = "Sheet1")

testOut1 <- BeeDC::readr_BeeDC(dataset = "Arm",
                               path = tempdir(),
                               inFile = "testData.xlsx",
                               outFile = "testDataOut.csv",
                               sheet = "Sheet1",
                               dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_Arm results columns TRUE", {
  testthat::expect_equal(resultsT, 25)
})
testthat::test_that("readr_Arm results columns FALSE", {
  testthat::expect_equal(resultsF, 0)
})

testthat::test_that("readr_Arm expected class", {
  testthat::expect_type(testOut1, "list")
})
