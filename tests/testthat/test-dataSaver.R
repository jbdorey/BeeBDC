requireNamespace("readr")
requireNamespace("BeeBDC")
requireNamespace("dplyr")
requireNamespace("emld")

library(dplyr) ## could not use %>% without loading as library


## make some data
testData <- dplyr::tribble(
  ~catalog_number, ~pollinator_family, ~pollinator_genus, ~pollinator_species, ~collection_method, ~collector_number, ~day_collected, ~month_collected, ~year_collected,                   ~location_description, ~location_name,    ~habitat, ~latitude,  ~longitude, ~basis_of_record,
  "SFU7052435916",       "Halictidae",    "Secret",             "sp. 1",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU705917",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU7052346919",       "Halictidae",    "Secret",       "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU7052645920",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU705645921",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen"
)

  # Create a test attributes file
 testAttributes <- dplyr::tribble(
    ~dataSource, ~alternateIdentifier,              ~title,     ~pubDate,   ~dateStamp,           ~doi,                                          ~downloadLink, ~abstract,               ~citations,                                                     ~downloadCitation,                                                                          ~rights,
    "USGS_data",       "Not provided", "USGS_DRO database", "19/11/2022", "19/11/2022", "Not provided", "Not provided, contact Sam Droege at sdroege@usgs.gov",   "Empty", "Citations not provided", "Sam Droege. (2022-11-19). United States Geological Survey bee data.", "Rights are not provided. Please seek permission for data use from Same Droege."
    )
  # Add the attributes to the test data
 attributes(testData)$dataSource <- testAttributes

 # clear the temp directory - CAUTION THIS WILL COMPLETELY REMOVE ALL TEMP FILES FOR R SESSION
 file.remove(file.info(list.files(tempdir(), full.names = T, 
                                  recursive = TRUE)) %>% rownames())
## test the R file save method 
# write file
BeeBDC::dataSaver(path = tempdir(), 
                 save_type = "R_file", 
                 occurrences = testData)

# read files
list.files(tempdir(), full.names = T, pattern = "out_file",
           recursive = TRUE)

# Get a list of folders from the temp directory
folderList <- list.dirs(tempdir(), full.names = T, recursive = TRUE)

# Check that the "out_file" folder exists
testthat::test_that("dataSaver check that the out_file was created", {
  testthat::expect_true(any(stringr::str_detect(folderList, "out_file")))
})

# Get the paths for the files in that folder
testOutFiles <- file.info(list.files(paste(tempdir(), "out_file", sep = "/"), full.names = T, 
                             recursive = TRUE)) %>% rownames()

# Check the number of files == 1 (single .rds file)
testthat::test_that("dataSaver check that there is one file", {
  testthat::expect_true(length(testOutFiles) == 1)
})

testthat::test_that("dataSaver check that the one file is an .rds file", {
  testthat::expect_true(stringr::str_count(testOutFiles, "\\.rds") %>% sum() == 1)
})

# Test the output itself
testOut <- readRDS(testOutFiles[stringr::str_detect(testOutFiles,
                                                    "BeeData_[0-9]+-[0-9]+-[0-9]+")])

testthat::test_that("dataSaver RDS expected class", {
  testthat::expect_type(testOut, "list")
})

testOutData <- testOut[[1]]

testthat::test_that("read in occurrence data same as original data because no columns removed", {
  testthat::expect_equal(testData, testOutData)
})



## test the CSV save method
# clear the temp directory - CAUTION THIS WILL COMPLETELY REMOVE ALL TEMP FILES FOR R SESSION
file.remove(file.info(list.files(tempdir(), full.names = T, 
                                 recursive = TRUE)) %>% rownames())

# save the data
BeeBDC::dataSaver(path = tempdir(), 
                 save_type = "CSV_file", 
                 occurrences = testData)

# Get the paths for the files in that folder
testOutFiles <- file.info(list.files(paste(tempdir(), "out_file", sep = "/"), full.names = T, 
                                     recursive = TRUE)) %>% rownames()

# Check the file types
testthat::test_that("dataSaver check that there are two .csv files produced", {
  testthat::expect_true(stringr::str_count(testOutFiles, "\\.csv") %>% sum() == 2)
})

# Test the output itself
testOut2 <- readr::read_csv(file = testOutFiles[stringr::str_detect(testOutFiles, 
                                                                    "BeeData_combined")])

testthat::test_that("dataSaver csv expected class", {
  testthat::expect_type(testOut2, "list")
})

testthat::test_that("read in occurrence data same as original data because no columns removed", {
  testthat::expect_equal(testData, testOut2, ignore_attr = TRUE)
})

testOut3 <- readr::read_csv(file = testOutFiles[stringr::str_detect(testOutFiles, 
                                                                    "BeeData_attributes")])

testthat::test_that("dataSaver csv expected class", {
  testthat::expect_type(testOut3, "list")
})

testthat::test_that("read in occurrence data same as original data because no columns removed", {
  testthat::expect_equal(testAttributes, testOut3, ignore_attr = TRUE)
})

