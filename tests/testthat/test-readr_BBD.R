requireNamespace("readr")
requireNamespace("tibble")


testData <- tibble::tribble(
  ~CodeBBdatabase_curated, ~'Scientific name corrected', ~Native.to.Brazil, ~Family_Proposed.in.Moure.Catalogue,      ~Family,   ~SubFamily,        ~Tribe,            ~Codigo, ~institutioncode, ~Day, ~Month, ~Year, ~Date_precision, ~Country,           ~State, ~Latitude_dec.degrees, ~Longitude_dec.degrees, ~Precision.of.coord.meters, ~NotasLatLong, ~NotesOnLocality,                     ~Locality.original, ~Spcslink.county, ~Spcslink.continentocean,               ~Collector, ~Collection,                                                                                                                                                                                     ~Source,         ~Sex,                                                                                                                                                                                    ~Project,        ~Det_By, ~Spcslink.linha_numero, ~Spcslink.datelastmodified, ~Spcslink.collectioncode, ~Spcslink.basisofrecord, ~Spcslink.identifiedby, ~Spcslink.yearidentified, ~Spcslink.barcode,
             "BBD_498851",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,               NA,   NA,     NA,    NA,              NA, "Brazil", "Santa Catarina",          -27.14783261,           -52.34069428,    "municipality centroid",            NA,               NA,                                     NA,               NA,                       NA,  "Moure's Bee Catalogue",          NA,                                                                                                                                                                                          NA,           NA,                                                                                                                                                                                          NA,             NA,                     NA,                         NA,                       NA,                      NA,                     NA,                       NA,                NA,
           "BBD_00082426",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,             "KU",   NA,    11L, 1939L,              NA, "Brazil",      "Sao Paulo",             -23.53377,              -46.69189,                         NA,            NA,               NA,                            "Sao Paulo",               NA,            "Neotropical",                "unknown",   "KU-SEMC",                                                                                                                                                             "SpeciesLink 22/12/2020 17:23h",           NA,                                                                                                                                                                                          NA,             NA,                270288L,                40933.39514,                 100623465863.0,     "PreservedSpecimen",           "Ruz, Luisa",                       NA,                NA,
           "BBD_00082419",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,             "KU",  26L,     2L, 1954L,              NA, "Brazil", "Santa Catarina",           -27.1833333,            -52.3833333,                         NA,            NA,               NA,                        "Nova Teutonia",               NA,            "Neotropical",        "Plaumann, Fritz",   "KU-SEMC",                                                                                                                                                             "SpeciesLink 22/12/2020 17:23h",           NA,                                                                                                                                                                                          NA,             NA,                270340L,                40933.39375,                 1006265758.0,     "PreservedSpecimen",                     NA,                       NA,                NA,
           "BBD_00043536",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini", "AMNHBEE 00028690",               NA,   1L,     1L, 1970L,              NA, "BRAZIL",   "Minas Gerais",             -20.13333,                  -43.5,                         NA,            NA,               NA, "Serra do Caraca (Saof Santa Barbara)",               NA,                       NA,         "F. M. Oliveira",      "AMNH", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "Adult Male", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "J. S. Ascher",                     NA,                         NA,                       NA,                      NA,                     NA,                       NA,                NA,
           "BBD_00082342",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,             "KU",   NA,    11L, 1951L,              NA, "Brazil", "Santa Catarina",           -27.1833333,            -52.3833333,                         NA,            NA,               NA,                        "Nova Teutonia",               NA,            "Neotropical",            "Plaumann, L",   "KU-SEMC",                                                                                                                                                             "SpeciesLink 22/12/2020 17:23h",           NA,                                                                                                                                                                                          NA,             NA,                270388L,                40933.39375,                 100245624566694.0,     "PreservedSpecimen",                     NA,                       NA,                NA
  )


# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_BBD(path = paste0(tempdir()),
                      inFile = "/testData.csv",
                      outFile = "testDataOut.csv",
                      dataLicense = "All rights reserved")


  # Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_BBD results columns TRUE", {
  testthat::expect_equal(resultsT, 27)
})
testthat::test_that("readr_BBD results columns FALSE", {
  testthat::expect_equal(resultsF, 15)
})

testthat::test_that("readr_BBD expected class", {
  testthat::expect_type(testOut1, "list")
})

