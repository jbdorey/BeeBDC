requireNamespace("readr")
requireNamespace("openxlsx")
requireNamespace("tibble")
requireNamespace("BeeDC")
requireNamespace("dplyr")

library(dplyr) ## could not use %>% without loading as library


testData <- tibble::tribble(
                ~Catalog.Number, ~`Collectors/First.Name`, ~`Collectors/Last.Name`, ~Count, ~Sex, ~Stage, ~Reproductive.Condition, ~Behavior,             ~Name, ~Alt.Cat.Number, ~Associated.Taxa, ~Associated.Ocurrence, ~Method,  ~Start.Date,    ~End.Date, ~`Start.Date.(Year)`, ~`Start.Date.(Month)`, ~`Start.Date.(Day)`, ~Start.Time, ~End.Time, ~Verbatim.Date, ~Habitat,      ~Continent,   ~Country,         ~State,                      ~County,                                               ~Locality.Name, ~Min.Elevation, ~Max.Elevation, ~Locality.and.Habitat.Notes,   ~Latitude1, ~Latitude2,     ~Lat1text,    ~Longitude1, ~Longitude2,     ~Long1text,               ~Full.Name,   ~Kingdom,        ~Order,  ~Family,    ~Subfamily,         ~Genus,      ~Species, ~Subspecies,     ~Species.Author, ~Type.Status, ~Qualifier, ~`Determiner/Last.Name`, ~`Determiner/First.Name`,
              "MPUJ_ENT0046822",                       NA,                      NA,     1L,   NA,     NA,                      NA,        NA, "Montado en seco",              NA,               NA,                    NA,      NA, "29-08-1987",           NA,                1987L,                    8L,                 29L,          NA,        NA,             NA,       NA, "South America", "Colombia",       "Tolima", "San Sebastián de Mariquita",                                                  "Mariquita",             NA,             NA,                          NA,     5.198894,         NA, "5.198894° N",      -74.89295,          NA,  "74.89295° W",   "Xylocopa aeneipennis", "Animalia", "Hymenoptera", "Apidae", "Xylocopinae",     "Xylocopa", "aeneipennis",          NA,    "(DeGeer, 1773)",           NA,         NA,               "Zanella",               "Fernando",
              "MPUJ_ENT0046821",                      "N",                 "Novoa",     1L,   NA,     NA,                      NA,        NA, "Montado en seco",              NA,               NA,                    NA,      NA, "27-03-2009",           NA,                2009L,                    3L,                 27L,          NA,        NA,             NA,       NA, "South America", "Colombia",        "Chocó",                     "Acandi",                      "Capurganá, Jardín Botánico del Darién",             90,             90,                          NA,     8.639755,         NA, "8.639755° N",     -77.350424,          NA, "77.350424° W",   "Xylocopa aeneipennis", "Animalia", "Hymenoptera", "Apidae", "Xylocopinae",     "Xylocopa", "aeneipennis",          NA,    "(DeGeer, 1773)",           NA,         NA,        "Guevara Farias",        "Diego Alexander",
              "MPUJ_ENT0089543",                      "R",                "Ovalle",     1L,   NA,     NA,                      NA,        NA, "Montado en seco",              NA,               NA,                    NA,      NA, "15-02-1986",           NA,                1986L,                    2L,                 15L,          NA,        NA,             NA,       NA, "South America", "Colombia", "Bogotá, D.C.",               "Bogotá, D.C.",                    "Las Villas, alrededores de la plazoleta",           2680,             NA,                          NA,           NA,         NA,            NA,             NA,          NA,             NA,      "Thygater aethiops", "Animalia", "Hymenoptera", "Apidae",      "Apinae",     "Thygater",    "aethiops",          NA,     "(Smith, 1854)",           NA,         NA,        "Guevara Farias",        "Diego Alexander",
              "MPUJ_ENT0089540",                      "R",                "Ovalle",     1L,   NA,     NA,                      NA,        NA, "Montado en seco",              NA,               NA,                    NA,      NA, "15-02-1986",           NA,                1986L,                    2L,                 15L,          NA,        NA,             NA,       NA, "South America", "Colombia", "Bogotá, D.C.",               "Bogotá, D.C.",                    "Las Villas, alrededores de la plazoleta",           2680,             NA,                          NA,           NA,         NA,            NA,             NA,          NA,             NA,      "Thygater aethiops", "Animalia", "Hymenoptera", "Apidae",      "Apinae",     "Thygater",    "aethiops",          NA,     "(Smith, 1854)",           NA,         NA,        "Guevara Farias",        "Diego Alexander",
              "MPUJ_ENT0036978",                      "M",                "Nariño",     1L,   NA,     NA,                      NA,        NA, "Montado en seco",              NA,               NA,                    NA,      NA, "07-03-2016", "11-03-2016",                2016L,                    3L,                  7L,          NA,        NA,             NA,       NA, "South America", "Colombia",       "Boyacá",                "Santa María", "sendero Hyca Quye, aproximadamente 5.5km NW de Santa María",            900,            900,                          NA,      4.89811,         NA,  "4.89811° N",      -73.29344,          NA,  "73.29344° W",    "Paratrigona anduzei", "Animalia", "Hymenoptera", "Apidae",      "Apinae",  "Paratrigona",     "anduzei",          NA,   "(Schwarz, 1943)",           NA,         NA,        "Guevara Farias",        "Diego Alexander",
              "MPUJ_ENT0072731",                "Rodrigo",                "García",     1L,   NA,     NA,                      NA,        NA, "Montado en seco",              NA,               NA,                    NA,      NA, "01-12-2021", "03-12-2021",                2021L,                   12L,                  1L,          NA,        NA,             NA,       NA, "South America", "Colombia",       "Caldas",                     "Samana",                                        "Vereda Monte Cristo",            850,             NA,                          NA,     5.503694,         NA, "5.503694° N",      -75.00061,          NA,  "75.00061° W", "Tetragonisca angustula", "Animalia", "Hymenoptera", "Apidae",      "Apinae", "Tetragonisca",   "angustula",          NA, "(Latreille, 1811)",           NA,         NA,        "Guevara Farias",        "Diego Alexander"
              )

# need to change column names, which need spaces for function to work
colnames(testData) <- c("Catalog Number", "Collectors/First Name", "Collectors/Last Name", "Count", "Sex", "Stage", "Reproductive Condition", "Behavior", "Name", "Alt Cat Number", "Associated Taxa", "Associated Ocurrence", "Method", "Start Date", "End Date", "Start Date (Year)", "Start Date (Month)", "Start Date (Day)", "Start Time", "End Time", "Verbatim Date", "Habitat", "Continent", "Country", "State", "County", "Locality Name", "Min Elevation", "Max Elevation", "Locality and Habitat Notes", "Latitude1", "Latitude2", "Lat1text", "Longitude1", "Longitude2", "Long1text", "Full Name", "Kingdom", "Order", "Family", "Subfamily", "Genus", "Species", "Subspecies", "Species Author", "Type Status", "Qualifier", "Determiner/Last Name", "Determiner/First Name")

# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.xlsx", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName = "Sheet1")

testOut1 <- BeeDC::readr_MPUJ(path = paste0(tempdir()),
                             inFile = "/testData.xlsx",
                             outFile = "testDataOut.csv",
                             sheet = "Sheet1", 
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_MPUJ results columns TRUE", {
  testthat::expect_equal(resultsT, 39)
})

testthat::test_that("readr_MPUJ results columns FALSE", {
  testthat::expect_equal(resultsF, 19)
})

testthat::test_that("readr_MPUJ expected class", {
  testthat::expect_type(testOut1, "list")
})
