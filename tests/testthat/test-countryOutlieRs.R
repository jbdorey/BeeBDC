

# Load in the checklis data
data("beesChecklist")
# Load in the test dataset
data("beesFlagged")

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
  readr::write_csv(paste(OutPath_Intermediate, "03_space_failedCountryChecklist.csv",
                         sep = "/"))



