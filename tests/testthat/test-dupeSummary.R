requireNamespace("dplyr")


testData <- dplyr::tibble(
  database_id = c("fakeID1","fakeID2", "fakeID2", "fakeID4","fakeID5", "fakeID6", "fakeID7",
                  "fakeID8","fakeID9", "fakeID10","fakeID11","fakeID12"),
  decimalLatitude = c(1.111, 2.111, 3.111, 4.111, 5.111, 6.111, 6.111, 6.111, 7.111, 7.111, 7.111, 7.111),
  decimalLongitude = c(10.111, 11.111, 12.111, 13.111, 14.111, 15.111, 15.111, 16.111, 16.111, 16.111, 17.111, 18.111),
  scientificName = c("a", "b", "c", "d", "e", "f", "f", "f", "g", "g", "h", "g"),      
  eventDate = c("1988-10-13 00:00:00 UTC", "1930-06-06 00:00:00 UTC", "1994-01-01 00:00:00 UTC",
                "1987-08-06 00:00:00 UTC", "1995-03-31 00:00:00 UTC", "2000-05-01 00:00:00 UTC",
                "1998-01-01 00:00:00 UTC", "1994-01-01 00:00:00 UTC","1994-01-01 00:00:00 UTC",
                 NA,"1994-01-01 00:00:00 UTC",
                "1972-04-28 00:00:00 UTC"),
  recordedBy = c("bee-er1, bee-er2", "bee-er3", "bee-er4", "bee-er5", "bee-er6","bee-er6",
                 "bee-er6", "bee-er6", "bee-er6", "bee-er7", "bee-er1", "bee-er2"),
  catalogNumber = c("beeCode0001","beeCode0002","beeCode0003","beeCode0004","beeCode0006","beeCode0006",
                    "beeCode0006","beeCode0006","beeCode0006","be01","boe6","beeCode0006"),
  otherCatalogNumbers = c("otherBeeCode0001","otherBeeCode0002","otherBeeCode0003","otherBeeCode0004",
                          "otherBeeCode0006","otherBeeCode0006",
                          "otherBeeCode0007","otherBeeCode0007","otherBeeCode0001","otherBeeCode0045",
                          "otherBeeCode0678","otherBeeCode194"),
  institutionCode = c("beeHouse","beeHouse","beeHouse","beeHouse","beeHouse","waspHouse","waspHouse",
                      "waspHouse","waspHouse","beeHouse","beeHouse","beeHouse"),     
  gbifID = c("asdflgjkh11","asdflgjkh11",NA_character_,NA_character_,NA_character_,NA_character_,
             NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
  occurrenceID = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
                   NA_character_,NA_character_,"duplicatecode12345","duplicatecode12345",NA_character_,NA_character_),
  recordId = c(NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,
               NA_character_,NA_character_,NA_character_,NA_character_,NA_character_,NA_character_),
  id = c("a","b","c","d","e","f","g","h","i","j","k","l"),
  dataSource = c("GBIF","GBIF","GBIF","GBIF","GBIF","GBIF","GBIF","GBIF","GBIF","GBIF","GBIF","GBIF")
)


check_time <- BeeBDC::dupeSummary(
  data = testData,
  path = tempdir(),
  # options are "ID","collectionInfo", or "both"
  duplicatedBy = "collectionInfo", 
  # The columns to generate completeness info from (and to sort by completness)
  completeness_cols = c("decimalLatitude",  "decimalLongitude",
                        "scientificName", "eventDate"),
  # idColumns = c("gbifID", "occurrenceID", "recordId","id"),
  # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
  collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                     "recordedBy"),
  # The columns to combine, one-by-one with the collectionCols
  collectInfoColumns = c("catalogNumber", "otherCatalogNumbers"),
  # Custom comparisons — as a list of columns to compare
  # RAW custom comparisons do not use the character and number thresholds
  CustomComparisonsRAW = dplyr::lst(c("catalogNumber", "institutionCode", "scientificName")),
  # Other custom comparisons use the character and number thresholds
  CustomComparisons = dplyr::lst(c("gbifID", "scientificName"),
                                  c("occurrenceID", "scientificName"),
                                  c("recordId", "scientificName"),
                                  c("id", "scientificName")),
  # The order in which you want to KEEP duplicated based on data source
  # try unique(check_time$dataSource)
  sourceOrder = c("CAES", "Gai", "Ecd","BMont", "BMin", "EPEL", "ASP", "KP", "EcoS", "EaCO",
                  "FSCA", "Bal", "SMC", "Lic", "Arm",
                  "USGS", "ALA", "GBIF","SCAN","iDigBio"),
  # Prefix ordering is done using the database_id prefix, not the dataSource prefix.
  prefixOrder = c("Paige", "Dorey"),
  characterThreshold = 2,
  numberThreshold = 3,
  numberOnlyThreshold = 5
)

  # Get counts of the number of expected kept duplicates and duplicats
test_keptDuplicates <-  sum(check_time$duplicateStatus == "Kept duplicate")
test_duplicates <- sum(check_time$duplicateStatus == "Duplicate")

# Test duplicate numbers
testthat::test_that("dupeSummary kept duplicates", {
  testthat::expect_equal(test_keptDuplicates, 2)
})
testthat::test_that("dupeSummary duplicates", {
  testthat::expect_equal(test_duplicates, 3)
})
# Test concordance with TRUE/FALSE and number of duplicates
testthat::test_that("dupeSummary duplicates", {
  testthat::expect_equal(test_duplicates, sum(check_time$.duplicates == FALSE))
})

# Test class
testthat::test_that("dupeSummary expected class - simple", {
  testthat::expect_type(check_time, "list")
})
testthat::test_that("dupeSummary expected class - complex", {
  testthat::expect_true(any(stringr::str_detect(attributes(check_time)$class, "tbl_df|tbl")))
})



