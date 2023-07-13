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


## write file
#BeeDC::data_saver(path = tempdir(), save_type = "R_file", occurrences = testData)
BeeDC::data_saver(path = tempdir(), save_type = "CSV_file", occurrences = testData)



## read files
#testOut1 <- readRDS(file = paste0(tempdir(), "/BeeData_2023-07-10.rds")) # this line will have to be changed depending on when data written 
# - cannot specify full file name using data_saver for R files
testOut2 <- readr::read_csv(file = paste0(tempdir(), "BeeData_combined_2023-07-10.csv"))


## check that the fake data and the new datafile are identical
testthat::test_that("data_saver data same as original testData", {
  testthat::expect_equal(testData, testOut2)
})

testthat::test_that("data_saver csv expected class", {
  testthat::expect_type(testOut2, "list")
})

testthat::test_that("data_saver RDS expected class", {
  testthat::expect_type(testOut2, "list")
})

