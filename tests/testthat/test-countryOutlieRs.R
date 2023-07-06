requireNamespace("dplyr")
requireNamespace("readr")

# Load in the checklis data
data("beesChecklist")
# Load in the test dataset
data("beesFlagged")

  # Input a potentially difficult loction on the map to test into an NA lat/lon slot
beesFlagged$decimalLatitude[[1]] <-  31.887646484374983
beesFlagged$decimalLongitude[[1]] <- 78.719726562500085 
beesFlagged$decimalLatitude[[2]] <-  78.719726562500085
beesFlagged$decimalLongitude[[2]] <- 31.887646484374983 

testOut <- BeeDC::countryOutlieRs(checklist = beesChecklist,
                                      occData = beesFlagged,
                                      keepAdjacentCountry = TRUE,
                                      pointBuffer = 0.05,
                                      # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                                      # Smaller numbers will result in much longer calculation times. 
                                      # We have not attempted a scale of 10.
                                      rnearthScale = 50)
# A list of failed species-country combinations and their numbers can be output here
testOut %>%
  dplyr::filter(.countryOutlier == FALSE) %>%
  dplyr::select(database_id, scientificName, country) %>%
  dplyr::group_by(scientificName) %>% 
  dplyr::mutate(count_scientificName = n()) %>%
  distinct(scientificName, country, .keep_all = TRUE) %>% 
  readr::write_csv(paste(tempdir(), "03_space_failedCountryChecklist.csv",
                         sep = "/"))



