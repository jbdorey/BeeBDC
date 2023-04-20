
testData <- readr::read_csv(system.file("extdata", "input_files/ColTestData.xlsx", package = "BeeDC"))


testData <- BeeDC::readr_Col(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = system.file("extdata", "input_files/ColTestData.xlsx", package = "BeeDC"),
                      outFile = system.file("extdata", "input_files/jbd_Col_Data.csv", package = "BeeDC"),
                      sheet = "Spanish headers",
                      dataLicense = "All rights reserved")


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
