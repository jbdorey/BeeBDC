requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("BeeDC")
library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
    ~institutionCode,	~Collection.Code,	~collection.var,	     ~catalogNumber,	   ~Other.Catalog.Number,	~sex,	~identifiedBy,	~dateIdentified,	~recordedBy,	~SpecimenLocation,	~BeeNonbee,	~order,	       ~family,	   ~genus,      	~subgenus,	          ~species,	     ~subspecies,	     ~scientificName,            	    ~associatedTaxa,	~locality,	          ~country,	~eventDate,	~SampleRound,	~SiteStatus,	~samplingProtocol,	~eventTime,	~EndTime,	~TempStart,	~TempEnd,	~WindStart,	~WindEnd,	~SkyStart,	~SkyEnd,	~Site,	~siteLocality,	~countryCode,	~stateProvince,	~county,	~decimalLatitude,	~decimalLongitude,	~year,	       ~syd,	       ~verbatimIdentification,	                   ~SiteStatusBACI,	~ypr,	~syd.veg,        	~associatedTaxa2,	~basisOfRecord,
      "EMEC-UTB",       "ENT",            "kremen-baci",   "M2006SR3DQU_30015", 30015L,                 NA, "C. Kremen",      "7/6/06",        "C. Kremen",    "UBC",             "bee",  "Hymenoptera",  "Apidae",   "Bombus",          NA,              "californicus",      NA,          "Bombus californicus",            "Rubus discolor",  "North America",  "United States", "7/6/06",   3L,         "control",       "net",        "10:41:00", "12:36:00",    29.2,    31.6,      0,           1,      "clear",  "clear",     "DQU",  "C5a",            "US",  "California",  "Yolo",              38.33687, -121.53191,            2006L,   "DQU;2006;187",    "Bombus californicus",                        "control",      0L,   "DQU;2006;187",  "Rubus discolor",  "preserved specimen",
       "EMEC-UTB",      "ENT",           "kremen-baci",  "M2006SR3DQU_30016",   30016L,                 NA, "C. Kremen",     "7/6/06",         "C. Kremen",   "UBC",              "bee", "Hymenoptera",     "Apidae",   "Bombus",         NA,          "californicus",        NA,          "Bombus californicus",            "Rubus discolor", "North America",    "United States",  "7/6/06",   3L,       "control",        "net",        "10:41:00",  "12:36:00",   29.2,    31.6,       0,          1,      "clear", "clear",    "DQU", "C5a",               "US", "California", "Yolo",                38.33687,    -121.53191,         2006L,    "DQU;2006;187",    "Bombus californicus",                     "control",        0L,    "DQU;2006;187", "Rubus discolor", "preserved specimen",
      "EMEC-UTB",     "ENT",             "kremen-baci", "M2007SR2Barger_007",   20398L,                "f", "J. Gibbs",     "7/2/08",          "K. Ullmann", "ESSIG",              "bee", "Hymenoptera", "Halictidae", "Lasioglossum",  "(Dialictus)", "diversopunctatum",    NA,          "Lasioglossum diversopunctatum",   "Brassica sp.", "North America",      "United States", "6/20/07",   2L,      "control",        "net",         "9:49:00",  "11:45:00",    21.4,    24.4,     1.2,        2.6,     "clear", "clear", "Barger",  "H1",               "US", "California", "Yolo",                38.34575,    -121.49834,         2007L, "Barger;2007;171", "Lasioglossum (Dialictus) diversopunctatum", "hedgerow",        0L, "Barger;2007;171",   "Brassica sp.", "preserved specimen",
       "EMEC-UTB",    "ENT",             "kremen-baci", "M2007SR2Barger_008",   20399L,                 "f",  "J. Gibbs",     "7/2/08",         "K. Ullmann", "ESSIG",             "bee", "Hymenoptera", "Halictidae", "Lasioglossum", "(Dialictus)", "diversopunctatum",     NA,        "Lasioglossum diversopunctatum",      "Brassica sp.", "North America",    "United States", "6/20/07",   2L,       "control",         "net",       "9:49:00",  "11:45:00",    21.4,    24.4,     1.2,          2.6,    "clear", "clear", "Barger",  "H1",               "US", "California", "Yolo",                38.34575,    -121.49834,         2007L, "Barger;2007;171", "Lasioglossum (Dialictus) diversopunctatum", "hedgerow",       0L, "Barger;2007;171",   "Brassica sp.", "preserved specimen"   
)

# need to change column names, which need spaces for function to work
colnames(testData) <- c("institutionCode", "Collection Code", "collection.var", "catalogNumber", "Other Catalog Number", "sex",	"identifiedBy",	"dateIdentified",	"recordedBy",	"SpecimenLocation",	"BeeNonbee", "order", "family", "genus", "subgenus", "species", "subspecies", "scientificName", "associatedTaxa", "locality", "country", "eventDate", "SampleRound", "SiteStatus", "samplingProtocol", "eventTime",	"EndTime", "TempStart", "TempEnd", "WindStart", "WindEnd", "SkyStart", "SkyEnd", "Site", "siteLocality", "countryCode", "stateProvince", "county", "decimalLatitude", "decimalLongitude", "year", "syd", "verbatimIdentification", "SiteStatusBACI", "ypr", "syd.veg", "associatedTaxa2",	"basisOfRecord")
 

# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_Gai(path = paste0(tempdir()),
                              inFile = "/testData.csv",
                              outFile = "testDataOut.csv",
                              dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_Gai results columns TRUE", {
  testthat::expect_equal(resultsT, 33)
})

testthat::test_that("readr_Gai results columns FALSE", {
  testthat::expect_equal(resultsF, 21)
})

testthat::test_that("readr_Gai expected class", {
  testthat::expect_type(testOut1, "list")
})
