requireNamespace("readr")
requireNamespace("BeeDC")
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
 testAttributes <- tibble::tribble(
    ~dataSource, ~alternateIdentifier,              ~title,     ~pubDate,   ~dateStamp,           ~doi,                                          ~downloadLink, ~abstract,               ~citations,                                                     ~downloadCitation,                                                                          ~rights,
    "USGS_data",       "Not provided", "USGS_DRO database", "19/11/2022", "19/11/2022", "Not provided", "Not provided, contact Sam Droege at sdroege@usgs.gov",   "Empty", "Citations not provided", "Sam Droege. (2022-11-19). United States Geological Survey bee data.", "Rights are not provided. Please seek permission for data use from Same Droege."
    )
  # Add the attributes to the test data
 attributes(testData)$dataSource <- testAttributes


## write file
#BeeDC::dataSaver(path = tempdir(), save_type = "R_file", occurrences = testData)
BeeDC::dataSaver(path = tempdir(), 
                 save_type = "CSV_file", 
                 occurrences = testData)



## read files
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
# Check the number of files == 4
testthat::test_that("dataSaver check that there are four files", {
  testthat::expect_true(length(testOutFiles) == 4)
})

# Check the file types
testthat::test_that("dataSaver check that there are two .csv files produced", {
  testthat::expect_true(stringr::str_count(testOutFiles, "\\.csv") %>% sum() == 2)
})
testthat::test_that("dataSaver check that there are two .rds files produced", {
  testthat::expect_true(stringr::str_count(testOutFiles, "\\.rds") %>% sum() == 2)
})


# Read in the .csv file that has the string "BeeData_combined"
testOut2 <- readr::read_csv(file = testOutFiles[stringr::str_detect(testOutFiles, 
                                                                    "BeeData_combined")])


## check that read in data is of the correct class
testthat::test_that("dataSaver data same as original testData", {
  testthat::expect_equal(attributes(testOut2)$class, 
                         c("spec_tbl_df", "tbl_df", "tbl", "data.frame" ))
})

  # I HAVE NOT LOOKED FROM HERE
testthat::test_that("dataSaver csv expected class", {
  testthat::expect_type(testOut2, "list")
})

testthat::test_that("dataSaver RDS expected class", {
  testthat::expect_type(testOut2, "list")
})

