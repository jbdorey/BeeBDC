requireNamespace("xml2")
requireNamespace("dplyr")

# If rnaturalearthdata is present, run tests
if(requireNamespace("leaflet")){


# Load in a test dataset
bees3sp <- BeeBDC::bees3sp


interactiveMapR(
  # occurrence data — start with entire dataset, filter down to these species
  data = bees3sp,
  # Directory where to save files
  outPath = paste0(tempdir(), "/interactiveMaps"),
  # lat long columns
  lon = "decimalLongitude",
  lat = "decimalLatitude",
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


# Check directory that the plot was saved
testthat::test_that("interactiveMapR plot saved?", {
  testthat::expect_true(any(stringr::str_detect(list.files(tempdir()), "interactiveMaps")))
})
# Check directory that the plot was saved
testthat::test_that("interactiveMapR plot saved?", {
  testthat::expect_equal(length(list.files(paste0(tempdir(), "/interactiveMaps"))), 3)
})

  # Find the .html files that were made
directories <- list.files(paste0(tempdir(), "/interactiveMaps"), 
                         full.names = TRUE)
  # Read in the html files for each species
A_tyleri <- xml2::read_html(directories[stringr::str_detect(directories, "tyleri")])
C_rhodopus <- xml2::read_html(directories[stringr::str_detect(directories, "rhodopus")])
P_octomaculata <- xml2::read_html(directories[stringr::str_detect(directories, "octomaculata")])

# Check directory that the plot was saved
testthat::test_that("interactiveMapR detect Agapostemon tyleri Cockerell, 1917", {
  testthat::expect_true(stringr::str_detect(A_tyleri %>% as.character(), 
                                            "Agapostemon tyleri Cockerell, 1917"))
})
testthat::test_that("interactiveMapR detect Centris rhodopus Cockerell, 1897", {
  testthat::expect_true(stringr::str_detect(C_rhodopus %>% as.character(), 
                                            "Centris rhodopus Cockerell, 1897"))
})
testthat::test_that("interactiveMapR detect Perdita octomaculata (Say, 1824)", {
  testthat::expect_true(stringr::str_detect(P_octomaculata %>% as.character(), 
                                            "Perdita octomaculata \\(Say, 1824\\)"))
})


# Test class
testthat::test_that("interactiveMapR expected class", {
  testthat::expect_equal(attributes(A_tyleri)$class, c("xml_document", "xml_node" ))
})


} # END if require namespace


