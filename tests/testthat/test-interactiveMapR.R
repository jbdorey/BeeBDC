


# Load in a test dataset
data("bees3sp")


interactiveMapR(
  # occurrence data — start with entire dataset, filter down to these species
  database = bees3sp,
  # Directory where to save files
  dir = paste0(tempdir(), "/interactiveMaps"),
  # lat long columns
  longitude = "decimalLongitude",
  latitude = "decimalLatitude",
  # Occurrence dataset column with species names
  speciesColumn = "scientificName",
  # Which species to map — a character vector of names or "ALL"
  # Note: "ALL" is defined AFTER filtering for country
  speciesList = "ALL",
  countryList = NULL, # studyArea
  # Point jitter to see stacked points — jitters an amount in decimal degrees
  jitterValue = 0.01,
  # If TRUE, it will only map the .summary column. Otherwise, it will map .summary
  # which will be over-written by countryOutliers and manualOutliers
  onlySummary = FALSE,
  TrueAlwaysTop = TRUE,
  # TRUE, FALSE, countryOutlier, customOutlier
  pointColours = c("blue", "darkred","#ff7f00", "black")
)



