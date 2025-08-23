requireNamespace("dplyr")
library(dplyr) # couldn't use %>% without this


# make some test data - this is derived from GBIF data but it is HEAVILY EDITED and USELESS
testData <- dplyr::tribble( # even step coordinates (different for lat/long)
  ~database_id, ~datasetName,  ~occurrenceStatus,          ~individualCount,
  "fake SCAN1", "fakeDataset",               "PRESENT",                1L,
  "fake SCAN2", "fakeDataset",               "PRESENT",                1L,
  "fake SCAN3", "fakeDataset",               "PRESENT",                1L,
  "fake SCAN4", "fakeDataset",                "ABSENT",                1L,
  "fake SCAN5", "fakeDataset",                "ABSENT",                1L,
  "fake SCAN6", "fakeDataset",                "ABSENT",                1L,
  "fake SCAN7", "fakeDataset",               "PRESENT",                1L,
  "fake SCAN8", "fakeDataset",                "ABSENT",                1L,
  "fake SCAN9", "fakeDataset",                  "ABSENT",                1L,
  "fake SCAN10", "fakeDataset",               "ABSENT",                1L,
  "fake SCAN11", "newDataset",               "PRESENT",                1L,
  "fake SCAN12", "newDataset",               "PRESENT",                1L,
  "fake SCAN13", "newDataset",                  "ABSENT",                1L,
  "fake SCAN14", "fakeDataset",            "PRESENT",                1L,
  "fake SCAN15", "fakeDataset",            "PRESENT",                1L,
  "fake SCAN16", "fakeDataset",            "PRESENT",                1L,
  "fake SCAN17", "fakeDataset",            "PRESENT",                1L,
  "fake SCAN18", "fakeDataset",             "ABSENT",                1L,
  "fake SCAN19", "fakeDataset",             "ABSENT",                1L,
  "fake SCAN20", "fakeDataset",             "ABSENT",                  1L,
  "fake SCAN21", "fakeDataset",             "ABSENT",                1L,
  "fake SCAN22", "fakeDataset",             "ABSENT",               1L,
  "fake SCAN23", "fakeDataset",            "PRESENT",               1L,
  "fake SCAN24", "fakeDataset",            "PRESENT",               0L,
  "fake SCAN25", "fakeDataset",            "PRESENT",               1L,
  "fake SCAN26", "fakeDataset",             "ABSENT",               1L,
  # no matching,
  "fake SCAN27", "fakeDataset",           "ABSENT",                 1L,
  "fake SCAN28", "fakeDataset",           "ABSENT",                 1L,
  "fake SCAN29", "fakeDataset",           "ABSENT",                 1L, 
  "fake SCAN30", "fakeDataset",           "ABSENT",                  1L,
  "fake SCAN31", "fakeDataset",           "ABSENT",                1L,
  # Too few decimals            
  "fake SCAN32", "newDataset",            "PRESENT",                1L,
  "fake SCAN33", "newDataset",            "PRESENT",                0L,
  "fake SCAN34", "newDataset",            "PRESENT",                1L,
  "fake SCAN35", "newDataset",            "PRESENT",                1L
)



# Run the function
testOut <- BeeBDC::flagAbsent(data = testData, PresAbs = "occurrenceStatus")


# test number of TRUE and FALSE values in the flag column, .occurrenceAbsent
resultsT <- length(testOut$.occurrenceAbsent[testOut$.occurrenceAbsent == TRUE])
resultsF <- length(testOut$.occurrenceAbsent[testOut$.occurrenceAbsent == FALSE])

testthat::test_that("flagAbsent column .occurrenceAbsent results TRUE", {
  testthat::expect_equal(resultsT, 15)
})

testthat::test_that("flagAbsent column .occurrenceAbsent results FALSE", {
  testthat::expect_equal(resultsF, 20)
})


# test the order of the TRUE and FALSE values in the flag column, .occurrenceAbsent
correct <- c(TRUE , TRUE,  TRUE, FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE,  TRUE, 
             FALSE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, FALSE,
             FALSE, FALSE,  TRUE, FALSE,  TRUE, FALSE, FALSE, FALSE, 
             FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE,  TRUE)

testthat::test_that("flagAbsent column .occurrenceAbsent results correct series", {
  testthat::expect_equal(correct, testOut$.occurrenceAbsent)
})

