requireNamespace("readr")
requireNamespace("BeeBDC")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


## fake data
testData <- dplyr::tribble(
  ~catalog_number, ~pollinator_family, ~pollinator_genus, ~pollinator_species, ~collection_method, ~collector_number, ~day_collected, ~month_collected, ~year_collected,                   ~location_description, ~location_name,    ~habitat, ~latitude,  ~longitude, ~basis_of_record,
  "SFU7052435916",       "Halictidae",    "Secret",             "sp. 1",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU705917",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU7052346919",       "Halictidae",    "Secret",       "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU7052645920",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU705645921",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen"
)


## write file
readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))


## find file 
testOut <- BeeBDC::fileFinder(path = paste0(tempdir()), fileName = "testData.csv")


## test
testthat::test_that("fileFinder expected class", {
  testthat::expect_type(testOut, "character")
})

