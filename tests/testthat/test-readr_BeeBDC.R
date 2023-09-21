  # Test each of the readr_functions

  # Load required packages
requireNamespace("readr")
requireNamespace("BeeBDC")
requireNamespace("dplyr")
requireNamespace("openxlsx")

library(dplyr) ## could not use %>% without loading as library

#### 1.0 EPEL ####

testData <- dplyr::tribble(
  ~catalog_number, ~pollinator_family, ~pollinator_genus, ~pollinator_species, ~collection_method, ~collector_number, ~day_collected, ~month_collected, ~year_collected,                   ~location_description, ~location_name,    ~habitat, ~latitude,  ~longitude, ~basis_of_record,
  "SFU7052435916",       "Halictidae",    "Secret",             "sp. 1",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU705917",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU7052346919",       "Halictidae",    "Secret",       "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU7052645920",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
  "SFU705645921",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen"
)

# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.csv", recursive = TRUE))
unlink(rownames(testDataPath))

# Save a temporary version of these data
readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeBDC::readr_BeeBDC(dataset = "EPEL",
                               path = tempdir(),
                              inFile = "testData.csv",
                              outFile = "testDataOut.csv",
                              dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_EPEL results columns TRUE", {
  testthat::expect_equal(resultsT, 21)
})
testthat::test_that("readr_EPEL results columns FALSE", {
  testthat::expect_equal(resultsF, 0)
})

testthat::test_that("readr_EPEL expected class", {
  testthat::expect_type(testOut1, "list")
})




#### 2.0 ASP ####
    testData <- dplyr::tribble(
      ~catalogNumber,        ~family,     ~subfamily,       ~Tribe,         ~genus,      ~subgenus, ~Morphospecies, ~specificEpithet,   ~locality, ~Successional_Stage, ~decimalLatitude, ~decimalLongitude, ~coordinateUncertaintyInMeters, ~elevation, ~eventTime, ~samplingProtocol,   ~eventDate, ~sex, ~associatedTaxa,      ~continent,         ~recordedBy,
      2L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Nananthidium",        "Msp 1",            "m16", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,     "p.m.",               "J",     "3/6/97",  "H",              NA, "South America", "Allan Smith-Pardo",
      8L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Anthodioctes",        "Msp 2",      "mapirense", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,       "m.",               "J",  "23/9/1997",  "H",              NA, "South America", "Allan Smith-Pardo",
      9L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Anthodioctes",        "Msp 2",      "mapirense", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,       "m.",               "J", "31/10/1997",  "H",              NA, "South America", "Allan Smith-Pardo",
      10L, "Megachilidae", "Megachilinae", "Anthidiini", "Anthodioctes", "Anthodioctes",        "Msp 2",      "mapirense", "Calandria",                "RA",            6.773,          -75.1036,                           100L,      1019L,     "p.m.",               "J",  "22/8/1997",  "H",              NA, "South America", "Allan Smith-Pardo"
    )
    
# Be sure that the testData is not already in tempdir
testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                     pattern = "testData.csv", recursive = TRUE))
unlink(rownames(testDataPath))

    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "ASP",
                                   path = tempdir(),
                                   inFile = "testData.csv",
                                   outFile = "testDataOut.csv",
                                   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_ASP results columns TRUE", {
      testthat::expect_equal(resultsT, 26)
    })
    
    testthat::test_that("readr_ASP results columns FALSE", {
      testthat::expect_equal(resultsF, 4)
    })
    
    testthat::test_that("readr_ASP expected class", {
      testthat::expect_type(testOut1, "list")
    })



#### 3.0 BMin ####
    
    library(dplyr) ## could not use %>% without loading as library
    
    
    testData <- dplyr::tribble(
      ~catalogNumber,      ~Phylum, ~higherClassification,        ~Order,      ~family,        ~genus, ~specificEpithet,        ~scientificName, ~scientificNameAuthorship,     ~sex,         ~associatedTaxa,    ~identifiedBy, ~dateIdentified, ~recordedBy,   ~eventDate,      ~continent,    ~country,           ~stateProvince, ~county,                                              ~locality, ~decimalLatitude, ~decimalLongitude,             ~collectionID,       ~basisOfRecord,
      "RLMC00005185", "Arthropoda",             "Insecta", "Hymenoptera", "Colletidae",   "Cadeguala",     "albopilosa", "Cadeguala albopilosa",         "(Spinola, 1851)",   "Male",                      NA,       "L Packer",              NA,  "L Packer", "31/10/2001", "South America",     "Chile", "Region de la Araucania",      NA, "Parque Nacional Nahuelbuta, Piedra del Aguilla trail",         -37.8251,           -73.037, "University of Rochester", "preserved specimen",
      "RLMC00005186", "Arthropoda",             "Insecta", "Hymenoptera", "Colletidae",   "Cadeguala",     "albopilosa", "Cadeguala albopilosa",         "(Spinola, 1851)", "Female", "Aristotelia chilensis",       "L Packer",              NA, "A-I Gavel",     "2/7/05", "South America", "Argentina",   "Provincia del Chubut",      NA,                                "INTA Trevelin, site 2",     -43.09925833,         -71.54235, "University of Rochester", "preserved specimen",
      "RLMC00005187", "Arthropoda",             "Insecta", "Hymenoptera", "Andrenidae", "Spinoliella",         "rozeni",   "Spinoliella rozeni",        "Toro & Ruz, 1972",   "Male",                      NA, "AH Smith-Pardo",              NA,  "L Packer", "13/10/2001", "South America",     "Chile",                "Atacama",      NA,             "Tres Playitas, 10km north of Huasco Bajo",         -28.4066,          -71.1891, "University of Rochester", "preserved specimen",
      "RLMC00005188", "Arthropoda",             "Insecta", "Hymenoptera", "Andrenidae", "Spinoliella",         "rozeni",   "Spinoliella rozeni",        "Toro & Ruz, 1972", "Female",                      NA, "AH Smith-Pardo",              NA,  "L Packer", "13/11/1997", "South America",     "Chile",                "Atacama",      NA,                                             "Chanaral",         -26.3328,          -70.6347, "University of Rochester", "preserved specimen"
    )
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "BMin",
                                   path = tempdir(),
                                  inFile = "testData.csv",
                                  outFile = "testDataOut.csv",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_BMin results columns TRUE", {
      testthat::expect_equal(resultsT, 28)
    })
    
    testthat::test_that("readr_BMin results columns FALSE", {
      testthat::expect_equal(resultsF, 2)
    })
    
    testthat::test_that("readr_BMin expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
#### 4.0 BMont ####
    
    testData <- dplyr::tribble(
      ~occurence_lsid, ~language,      ~basisOfRecord, ~catalogNumber,                                  ~scientificNameID,     ~scientificName,     ~sex, ~lifeStage, ~organismQuantity,                                                       ~institutionCode,                                                                                 ~fieldNotes,                                ~locality, ~decimalLatitude, ~decimalLongitude,    ~GPS_device,        ~country, ~stateProvince,   ~county, ~municipality,                                                          ~georeferenceRemarks, ~VerbatimEventDate, ~eventDate, ~eventTime, ~recordedBy, ~samplingProtocol, ~fieldNumber,              ~identifiedBy, ~dateIdentified, ~typeStatus,   ~kingdom,      ~phylum,     ~class,        ~order,  ~family,   ~genus, ~specificEpithet, ~infraspecificEpithet, ~habitat,       ~associatedTaxa,                                            ~source,              ~modified,
      "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014085",      "en", "PreservedSpecimen",  "MTEC 014085", "urn:lsid:biosci.ohio-state.edu:osuc_names:169432",  "Bombus sylvicola", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                    "[MONT. Powell Co. Flint Crk. Mts. 8500ft-9500ft, 28 July 1961 B Vogel]", "Flint Creek Mountains, Granite Co., MT",          46.3624,         -113.1077, "Google Earth", "United States",      "Montana", "Granite",            NA, "Label says Powell Co., but the Flint Creek Range is actually in Granite Co.",        "28-Jul-61",  "7/28/61",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",      "sylvicola",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014085", "2015-11-06T12:23:32Z",
      "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014086",      "en", "PreservedSpecimen",  "MTEC 014086", "urn:lsid:biosci.ohio-state.edu:osuc_names:169432",  "Bombus sylvicola", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                    "[MONT. Powell Co. Flint Crk. Mts. 8500ft-9500ft, 28 July 1961 B Vogel]", "Flint Creek Mountains, Granite Co., MT",          46.3624,         -113.1077, "Google Earth", "United States",      "Montana", "Granite",            NA, "Label says Powell Co., but the Flint Creek Range is actually in Granite Co.",        "28-Jul-61",  "7/28/61",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",      "sylvicola",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014086", "2015-11-06T12:23:32Z",
      "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014087",      "en", "PreservedSpecimen",  "MTEC 014087", "urn:lsid:biosci.ohio-state.edu:osuc_names:169432",  "Bombus sylvicola", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                                  "[MONT. Carbon Co. Beartooth Plateau, 9 Jly 1963 B Vogel]",      "Beartooth Plateau, Carbon Co., MT",          45.0214,         -109.5779, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,         "9-Jul-63",   "7/9/63",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",      "sylvicola",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014087", "2015-09-28T11:15:16Z",
      "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014088",      "en", "PreservedSpecimen",  "MTEC 014088", "urn:lsid:biosci.ohio-state.edu:osuc_names:128375", "Bombus flavifrons", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)",                                    "[MONT: Carbon Co. E. Rosebud L. 20 June 1962 B. Vogel]",        "E. Rosebud Lake, Carbon Co., MT",          45.2002,         -109.6412, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,        "20-Jun-62",  "6/20/62",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",     "flavifrons",                    NA,       NA,                    NA, "http://hol.osu.edu/spmInfo.html?id=MTEC%20014088", "2015-09-28T11:15:17Z",
      "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014089",      "en", "PreservedSpecimen",  "MTEC 014089", "urn:lsid:biosci.ohio-state.edu:osuc_names:128375", "Bombus flavifrons", "female",    "queen",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)", "[MONT: Carbon Co. E. Rosebud L. 20 June 1962 B. Vogel] [on Penstemon confertas] confertus",        "E. Rosebud Lake, Carbon Co., MT",          45.2002,         -109.6412, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,        "20-Jun-62",  "6/20/62",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",     "flavifrons",                    NA,       NA, "Penstemon confertus", "http://hol.osu.edu/spmInfo.html?id=MTEC%20014089", "2015-09-28T11:15:17Z",
      "urn:lsid:biosci.ohio-state.edu:osuc_occurrences:MTEC__014090",      "en", "PreservedSpecimen",  "MTEC 014090", "urn:lsid:biosci.ohio-state.edu:osuc_names:128375", "Bombus flavifrons", "female",   "worker",                1L, "University of Colorado Museum of Natural History, Boulder, CO (UCMC)", "[MONT: Carbon Co. E. Rosebud L. 20 June 1962 B. Vogel] [on Penstemon confertas] confertus",        "E. Rosebud Lake, Carbon Co., MT",          45.2002,         -109.6412, "Google Earth", "United States",      "Montana",  "Carbon",            NA,                                                                            NA,        "20-Jun-62",  "6/20/62",         NA, "Vogel, B.",  "none specified",           NA, "Thorp, R. W. (Robbin W.)",           1966L,          NA, "Animalia", "Arthropoda", "Hexapoda", "Hymenoptera", "Apidae", "Bombus",     "flavifrons",                    NA,       NA, "Penstemon confertus", "http://hol.osu.edu/spmInfo.html?id=MTEC%20014090", "2015-09-28T11:15:18Z"
    )
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "BMont",
                                   path = tempdir(),
                                   inFile = "testData.csv",
                                   outFile = "testDataOut.csv",
                                   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_BMont results columns TRUE", {
      testthat::expect_equal(resultsT, 33)
    })
    testthat::test_that("readr_BMont results columns FALSE", {
      testthat::expect_equal(resultsF, 13)
    })
    
    testthat::test_that("readr_BMont expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
#### 5.0 Ecd  ####
    
    
    testData <- dplyr::tribble(
      ~id, ~institutionCode, ~collectionCode, ~ownerInstitutionCode,     ~basisOfRecord,                          ~occurrenceID, ~catalogNumber, ~otherCatalogNumbers,                                                                                                         ~higherClassification,   ~kingdom,      ~phylum,    ~class,        ~order,  ~family,        ~scientificName, ~taxonID, ~scientificNameAuthorship,     ~genus, ~subgenus, ~specificEpithet, ~verbatimTaxonRank, ~infraspecificEpithet, ~taxonRank, ~identifiedBy, ~dateIdentified, ~identificationReferences, ~identificationRemarks, ~taxonRemarks, ~identificationQualifier, ~typeStatus,          ~recordedBy, ~associatedCollectors, ~recordNumber, ~eventDate, ~year, ~month, ~day, ~startDayOfYear, ~endDayOfYear, ~verbatimEventDate, ~occurrenceRemarks,                           ~habitat, ~substrate, ~verbatimAttributes, ~fieldNumber, ~eventID, ~informationWithheld, ~dataGeneralizations, ~dynamicProperties, ~associatedOccurrences, ~associatedSequences, ~associatedTaxa, ~reproductiveCondition, ~establishmentMeans, ~cultivationStatus, ~lifeStage, ~sex, ~individualCount, ~preparations,        ~country, ~stateProvince,    ~county, ~municipality,                 ~locality, ~locationRemarks, ~localitySecurity, ~localitySecurityReason, ~decimalLatitude, ~decimalLongitude, ~geodeticDatum, ~coordinateUncertaintyInMeters, ~verbatimCoordinates, ~georeferencedBy, ~georeferenceProtocol, ~georeferenceSources, ~georeferenceVerificationStatus, ~georeferenceRemarks, ~minimumElevationInMeters, ~maximumElevationInMeters, ~minimumDepthInMeters, ~maximumDepthInMeters, ~verbatimDepth, ~verbatimElevation, ~disposition, ~language, ~recordEnteredBy,       ~modified, ~`sourcePrimaryKey-dbpk`, ~collID,                                       ~recordID,                                                                         ~references,
      637436L,            "ASU",   "BIO386-2020",                    NA, "HumanObservation", "bd5eea21-0995-45e6-b626-7a3cb361d878",             NA,                   NA,                   "Animalia|Arthropoda|Hexapoda|Insecta|Pterygota|Neoptera|Hymenoptera|Apocrita|Aculeata|Apoidea|Apidae|Apis", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",       "Apis mellifera",  235783L,          "Linnaeus, 1758",     "Apis",        NA,      "mellifera",                 NA,                    NA,  "Species",            NA,              NA,                        NA,                     NA,            NA,                       NA,          NA, "Melkonoff, Natalie",                    NA,            NA,  "9/26/20", 2020L,     9L,  26L,            270L,            NA,                 NA,                 NA, "On Asclepias angustifolia flower",         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 0L,         NA,   NA,               NA,            NA, "United States",      "Arizona", "Maricopa",            NA, "Desert Botanical Garden",               NA,                0L,                      NA,        33.460551,       -111.942522,        "WGS84",                            50L,                   NA,               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/26/20 16:14",                       NA,     23L, "urn:uuid:bd5eea21-0995-45e6-b626-7a3cb361d878", "https://serv.biokic.asu.edu/ecdysis/collections/individual/index.php?occid=637436",
      637531L,            "ASU",   "BIO386-2020",                    NA, "HumanObservation", "a19fa123-6f6f-4c2a-92c7-f2d8409873b1",             NA,                   NA,  "Animalia|Arthropoda|Hexapoda|Insecta|Pterygota|Neoptera|Hymenoptera|Apocrita|Aculeata|Apoidea|Apidae|Apinae|Bombini|Bombus", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Bombus vosneskenskii",  603453L,       "Radoszkowski 1862",   "Bombus",        NA,  "vosneskenskii",                 NA,                    NA,  "Species",            NA,              NA,                        NA,                     NA,            NA,                       NA,          NA,          "Ma, Janey",                    NA,            NA,  "9/21/20", 2020L,     9L,  21L,            265L,            NA,                 NA,      "Hymenoptera",                                 NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 0L,         NA,   NA,               NA,            NA, "United States",           "AZ", "Maricopa",            NA,                "Glendale",               NA,                0L,                      NA,          33.5771,          112.1906,             NA,                            50L,                   NA,               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/26/20 22:36",                       NA,     23L, "urn:uuid:a19fa123-6f6f-4c2a-92c7-f2d8409873b1", "https://serv.biokic.asu.edu/ecdysis/collections/individual/index.php?occid=637531",
      637539L,            "ASU",   "BIO386-2020",                    NA, "HumanObservation", "1d420150-ce0f-4510-bdf9-e65125621e43",             NA,                   NA, "Animalia|Arthropoda|Hexapoda|Insecta|Pterygota|Neoptera|Hymenoptera|Apocrita|Aculeata|Apoidea|Apidae|Xylocopinae|Xylocopini", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",             "Xylocopa",  204218L,         "Latreille, 1802", "Xylocopa",        NA,               NA,                 NA,                    NA,    "Genus",            NA,              NA,                        NA,                     NA,            NA,                       NA,          NA,          "Ma, Janey",                    NA,            NA,  "9/21/20", 2020L,     9L,  21L,            265L,            NA,                 NA,    "Hymenopterans",                                 NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 0L,         NA,   NA,               NA,            NA, "United States",      "Arizona", "Maricopa",            NA,                "Glendale",               NA,                0L,                      NA,          33.5771,           112.186,             NA,                            50L,                   NA,               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/26/20 22:51",                       NA,     23L, "urn:uuid:1d420150-ce0f-4510-bdf9-e65125621e43", "https://serv.biokic.asu.edu/ecdysis/collections/individual/index.php?occid=637539",
      637542L,            "ASU",   "BIO386-2020",                    NA, "HumanObservation", "c56b011d-70f7-4ee8-b0b9-f37a32fb5cd4",             NA,                   NA,                               "Animalia|Arthropoda|Hexapoda|Insecta|Pterygota|Neoptera|Hymenoptera|Apocrita|Aculeata|Apoidea", "Animalia", "Arthropoda", "Insecta", "Hymenoptera",       NA,         "Megachilidae",   91998L,                        NA,         NA,        NA,               NA,                 NA,                    NA,   "Family",            NA,              NA,                        NA,                     NA,            NA,                       NA,          NA,          "Ma, Janey",                    NA,            NA,  "9/24/20", 2020L,     9L,  24L,            268L,            NA,                 NA,      "Hymenoptera",                                 NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 0L,         NA,   NA,               NA,            NA, "United States",           "AZ", "Maricopa",            NA,                "Glendale",               NA,                0L,                      NA,          33.5771,           112.186,             NA,                            50L,                   NA,               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/26/20 22:53",                       NA,     23L, "urn:uuid:c56b011d-70f7-4ee8-b0b9-f37a32fb5cd4", "https://serv.biokic.asu.edu/ecdysis/collections/individual/index.php?occid=637542"
    )
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Ecd",
                                   path = tempdir(),
                                 inFile = "testData.csv",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_Ecd results columns TRUE", {
      testthat::expect_equal(resultsT, 53)
    })
    
    testthat::test_that("readr_Ecd results columns FALSE", {
      testthat::expect_equal(resultsF, 43)
    })
    
    testthat::test_that("readr_Ecd expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
#### 6.0 Gai ####
    
    testData <- dplyr::tribble(
      ~institutionCode,	~Collection.Code,	~collection.var,	     ~catalogNumber,	   ~Other.Catalog.Number,	~sex,	~identifiedBy,	~dateIdentified,	~recordedBy,	~SpecimenLocation,	~BeeNonbee,	~order,	       ~family,	   ~genus,      	~subgenus,	          ~species,	     ~subspecies,	     ~scientificName,            	    ~associatedTaxa,	~locality,	          ~country,	~eventDate,	~SampleRound,	~SiteStatus,	~samplingProtocol,	~eventTime,	~EndTime,	~TempStart,	~TempEnd,	~WindStart,	~WindEnd,	~SkyStart,	~SkyEnd,	~Site,	~siteLocality,	~countryCode,	~stateProvince,	~county,	~decimalLatitude,	~decimalLongitude,	~year,	       ~syd,	       ~verbatimIdentification,	                   ~SiteStatusBACI,	~ypr,	~syd.veg,        	~associatedTaxa2,	~basisOfRecord,
      "EMEC-UTB",       "ENT",            "kremen-baci",   "M2006SR3DQU_30015", 30015L,                 NA, "C. Kremen",      "7/6/06",        "C. Kremen",    "UBC",             "bee",  "Hymenoptera",  "Apidae",   "Bombus",          NA,              "californicus",      NA,          "Bombus californicus",            "Rubus discolor",  "North America",  "United States", "7/6/06",   3L,         "control",       "net",        "10:41:00", "12:36:00",    29.2,    31.6,      0,           1,      "clear",  "clear",     "DQU",  "C5a",            "US",  "California",  "Yolo",              38.33687, -121.53191,            2006L,   "DQU;2006;187",    "Bombus californicus",                        "control",      0L,   "DQU;2006;187",  "Rubus discolor",  "preserved specimen",
      "EMEC-UTB",      "ENT",           "kremen-baci",  "M2006SR3DQU_30016",   30016L,                 NA, "C. Kremen",     "7/6/06",         "C. Kremen",   "UBC",              "bee", "Hymenoptera",     "Apidae",   "Bombus",         NA,          "californicus",        NA,          "Bombus californicus",            "Rubus discolor", "North America",    "United States",  "7/6/06",   3L,       "control",        "net",        "10:41:00",  "12:36:00",   29.2,    31.6,       0,          1,      "clear", "clear",    "DQU", "C5a",               "US", "California", "Yolo",                38.33687,    -121.53191,         2006L,    "DQU;2006;187",    "Bombus californicus",                     "control",        0L,    "DQU;2006;187", "Rubus discolor", "preserved specimen",
      "EMEC-UTB",     "ENT",             "kremen-baci", "M2007SR2Barger_007",   20398L,                "f", "J. Gibbs",     "7/2/08",          "K. Ullmann", "ESSIG",              "bee", "Hymenoptera", "Halictidae", "Lasioglossum",  "(Dialictus)", "diversopunctatum",    NA,          "Lasioglossum diversopunctatum",   "Brassica sp.", "North America",      "United States", "6/20/07",   2L,      "control",        "net",         "9:49:00",  "11:45:00",    21.4,    24.4,     1.2,        2.6,     "clear", "clear", "Barger",  "H1",               "US", "California", "Yolo",                38.34575,    -121.49834,         2007L, "Barger;2007;171", "Lasioglossum (Dialictus) diversopunctatum", "hedgerow",        0L, "Barger;2007;171",   "Brassica sp.", "preserved specimen",
      "EMEC-UTB",    "ENT",             "kremen-baci", "M2007SR2Barger_008",   20399L,                 "f",  "J. Gibbs",     "7/2/08",         "K. Ullmann", "ESSIG",             "bee", "Hymenoptera", "Halictidae", "Lasioglossum", "(Dialictus)", "diversopunctatum",     NA,        "Lasioglossum diversopunctatum",      "Brassica sp.", "North America",    "United States", "6/20/07",   2L,       "control",         "net",       "9:49:00",  "11:45:00",    21.4,    24.4,     1.2,          2.6,    "clear", "clear", "Barger",  "H1",               "US", "California", "Yolo",                38.34575,    -121.49834,         2007L, "Barger;2007;171", "Lasioglossum (Dialictus) diversopunctatum", "hedgerow",       0L, "Barger;2007;171",   "Brassica sp.", "preserved specimen"   
    )
    
    # need to change column names, which need spaces for function to work
    colnames(testData) <- c("institutionCode", "Collection Code", "collection.var", "catalogNumber", "Other Catalog Number", "sex",	"identifiedBy",	"dateIdentified",	"recordedBy",	"SpecimenLocation",	"BeeNonbee", "order", "family", "genus", "subgenus", "species", "subspecies", "scientificName", "associatedTaxa", "locality", "country", "eventDate", "SampleRound", "SiteStatus", "samplingProtocol", "eventTime",	"EndTime", "TempStart", "TempEnd", "WindStart", "WindEnd", "SkyStart", "SkyEnd", "Site", "siteLocality", "countryCode", "stateProvince", "county", "decimalLatitude", "decimalLongitude", "year", "syd", "verbatimIdentification", "SiteStatusBACI", "ypr", "syd.veg", "associatedTaxa2",	"basisOfRecord")
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Gai",
                                   path = tempdir(),
                                 inFile = "testData.csv",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
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
    



#### 7.0 CAES  ####
    
    testData <- dplyr::tribble(
      ~PBIUSI,    ~Genus, ~species, ~Country,   ~State_Prov, ~Sec_Subdiv,                            ~Locality,     ~Lat,      ~Lon, ~Start_Date, ~End_Date,        ~Collector,                ~Sex, ~Inst_Code,                                                                                                                                                                                    ~Project,  ~Type,         ~Det_By, ~Det_Date, ~Det_History,    ~Loc_Notes, ~Accuracy, ~Coll_Method, ~Spec_Notes, ~Host_Family, ~Host_Genus, ~Host_species, ~Pres_Method, ~Spec_Count, ~Lat_Lon_Method, ~Lat_Lon_Accuracy, ~Elev_m, ~Elev_f, ~Elev_Det, ~Trip_Code, ~Dissections, ~Illustrations, ~Measurements, ~Photos, ~SEM, ~DNA, ~OrigUSI,      ~Family,   ~Subfamily,      ~Tribe, ~Macro_Habitat, ~Micro_Habitat, ~Host_subspecies, ~Host_Author, ~Host_Common_Name,    ~Host_Relation, ~Host_Location, ~Host_Emergence_Date, ~Host_with_Specimen,
      "AMNH_BEE 000266235443", "Secret", "species",    "USA", "Connecticut", "Fairfield",                         "New Canaan", 41.14666, -73.49472, 22786,        NA,      "M. Statham",      "Adult Female",     "AMNH", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None",  "J. S. Ascher",     2007L,           NA, "GNIS coord.",        NA,    "Netting",          NA,           NA,          NA,            NA,     "Pinned",          1L,     "Gazetteer",                NA,     97L,    318L, "Unknown",         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO",
      "AMNH_BEE 00026254675", "Secret", "species",    "USA", "Connecticut", "Fairfield",            "Danbury, I-84 at exit 2", 41.39247, -73.52731,  38451,        NA,    "J. S. Ascher",        "Adult Male",     "AMNH", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None",  "J. S. Ascher",     2007L,           NA,            NA,        NA,    "Netting",          NA, "Salicaceae",     "Salix",        "spp.",     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA, "associated with",             NA,                   NA,                "NO",
      "UCMS_ENT 00022347759", "Secret", "species",    "USA", "Connecticut",   "Tolland", "Coventry Twp., near Eagleville Dam", 41.76916, -72.30444, 26414,        NA, "L. R. Schechter", "Adult sex unknown",      "GSC", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None", "W. E. LaBerge",        NA,           NA,            NA,        NA,    "Unknown",          NA,           NA,          NA,            NA,     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO",
      "UCMS_ENT 00023457760", "Secret", "species",    "USA", "Connecticut",   "Tolland",        "Mansfield Twp., Gurleyville", 41.81416,   -72.255, 26044,        NA,     "G. I. Stage", "Adult sex unknown",      "GSC", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None", "W. E. LaBerge",        NA,           NA,            NA,        NA,    "Unknown",          NA,           NA,          NA,            NA,     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO",
      "UCMS_ENT 002345027762", "Secret", "species",    "USA", "Connecticut",   "Tolland", "Coventry Twp., near Eagleville Dam", 41.76916, -72.30444, 26417,        NA,    "J. R. Gordon", "Adult sex unknown",      "GSC", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None", "W. E. LaBerge",        NA,           NA,            NA,        NA,    "Unknown",          NA,           NA,          NA,            NA,     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO"
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                         sheetName = "Sheet1")
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "CAES",
                                     path = tempdir(),
                                  inFile = "testData.xlsx",
                                  outFile = "testDataOut.csv",
                                  sheet = "Sheet1",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_BBD results columns TRUE", {
      testthat::expect_equal(resultsT, 31)
    })
    testthat::test_that("readr_BBD results columns FALSE", {
      testthat::expect_equal(resultsF, 16)
    })
    
    testthat::test_that("readr_BBD expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    

#### 9.0 KP  ####
    
    # data below is FAKE
    testData <- dplyr::tribble(
      ~ID, ~institutionCode,             ~Det, ~Number, ~Male, ~Female, ~Collection_method,          ~Collection_date,  ~Collector,        ~Order,    ~Suborder,        ~Family,     ~Subfamily,        ~Tribe,         ~Genus,         ~subgenus, ~sp_group,      ~species, ~subspecies,            ~author,                             ~whole_sci_name, ~Country,     ~State,         ~County_Parish,                        ~Location,                 ~Lat,               ~Long,
      "discoverlife fake1 USGS-DRO. fake1",           "USGS",      "S. Droege",      1L,    0L,      1L,              "Net", "2019-08-07 00:00:00 UTC", "J. Fowler", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73,
      "discoverlife fake2 USGS-DRO. fake2",           "USGS",      "S. Droege",      1L,    1L,      0L,              "Net", "2019-09-06 00:00:00 UTC", "J. Fowler", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73,
      "discoverlife fake3 USGS-DRO. fake3",           "USGS", "S. Droege 2019",      1L,    1L,      0L,              "Net", "2019-04-18 00:00:00 UTC", "S. Droege", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73,
      "discoverlife fake4 USGS-DRO. fake4",           "USGS",      "S. Droege",      1L,    1L,      0L,              "Net", "2016-03-25 00:00:00 UTC", "S. Droege", "Hymenoptera", "Anthophila",       "Apidae",             NA,            NA,         "Apis",                NA,        NA,   "mellifera",          NA,   "Linnaeus, 1758",             "Apis mellifera Linnaeus, 1758",    "USA",       "NY",            "Kings Co.",           "Brooklyn Bridge Park",              "40.69",                 -73
    )
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"))
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "KP",
                                   path = tempdir(),
                                inFile = "testData.xlsx",
                                outFile = "testDataOut.csv",
                                dataLicense = "All rights reserved")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_KP results columns TRUE", {
      testthat::expect_equal(resultsT, 29)
    })
    
    testthat::test_that("readr_KP results columns FALSE", {
      testthat::expect_equal(resultsF, 2)
    })
    
    testthat::test_that("readr_KP expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
#### 11.0 EcoS  ####
    
    testData <- dplyr::tribble(
      ~Collection, ~ID_project,                    ~Species, ~Longitude, ~Latitude, ~Year,
      "CECON",          1L, "Agapostemon_atrocaeruleus",    -89.439,    16.357, 2006L,
      "CECON",          2L,     "Agapostemon_leunculus",    -89.377,     14.62, 2003L,
      "CECON",          3L,     "Agapostemon_leunculus",    -90.211,    15.266, 2011L,
      "CECON",          4L,  "Agapostemon_melliventris",     -90.06,    14.926, 2004L,
      "CECON",          5L,     "Anthidium_maculifrons",    -91.492,    15.332, 2009L,
      "CECON",          6L,     "Anthidium_maculifrons",    -89.363,    14.607, 2003L
    )
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "EcoS",
                                  path = tempdir(),
                                  inFile = "testData.csv",
                                  outFile = "testDataOut.csv",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_EcoS results columns TRUE", {
      testthat::expect_equal(resultsT, 14)
    })
    
    testthat::test_that("readr_EcoS results columns FALSE", {
      testthat::expect_equal(resultsF, 0)
    })
    
    testthat::test_that("readr_EcoS expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
#### 12.0 GeoL  ####
    
    testData <- dplyr::tribble(
      ~database_id,                              ~scientificName,      ~family,   ~subfamily,          ~genus, ~subgenus, ~subspecies,                     ~species, ~specificEpithet, ~infraspecificEpithet, ~acceptedNameUsage, ~taxonRank, ~scientificNameAuthorship,      ~continent,        ~country, ~country_suggested, ~countryCode, ~stateProvince,         ~county, ~municipality,                                                                   ~locality, ~island,                                             ~license,                                                                                                     ~issue,             ~eventDate, ~day, ~month, ~year,       ~basisOfRecord,            ~type, ~occurrenceStatus, ~recordNumber,                      ~recordedBy, ~eventID, ~samplingProtocol, ~samplingEffort, ~individualCount,                  ~catalogNumber, ~rightsHolder, ~institutionCode, ~decimallatitude, ~decimallongitude, ~datasetID, ~datasetName, ~otherCatalogNumbers,                                                          ~occurrenceID, ~coreid,                                       ~recordId,                          ~collectionID,                             ~verbatimScientificName,      ~verbatimEventDate,           ~sex,                                          ~rights, ~occurrenceYear,           ~id,       ~dataSource,                 ~names_clean,                     ~verbatim_scientificName, ~geolocate_Latitude, ~geolocate_Longitude, ~geolocate_UncertaintyRadiusMeters, ~geolocate_Score, ~geolocate_Precision,                  ~geolocate_ParsePattern, ~geolocate_locFieldUsed, ~geolocate_NumResults,
      "Ecd_data_20840",          "Agapostemon texanus Cresson, 1872", "Halictidae", "Halictinae",   "Agapostemon",        NA,          NA,        "Agapostemon texanus",        "texanus",                    NA,                 NA,  "Species",           "Cresson, 1872",              NA, "United states",    "United states",         "US",   "California", "Santa Barbara",            NA,                                   "University of California, Santa Barbara",      NA, "https://creativecommons.org/licenses/by-nc-sa/4.0/",                                                                                                         NA, "1984-05-17T00:00:00Z",  17L,     5L, 1984L,  "PreservedSpecimen",               NA,                NA,            NA,                               NA,       NA,                NA,              NA,               1L,              "UCSB-IZC00005603",            NA,           "UCSB",               NA,                NA,         NA,           NA,                   NA,                                 "3de7811e-1169-4fe9-a451-59ba9c653449",      NA,                                              NA,                                     NA,                                                  NA,             "17/5/1984",         "Male",                                               NA,              NA, "UCSB_828348",  "Ecd_Anthophila",                           NA,          "Agapostemon texanus Cresson, 1872",           34.416323,          -119.846392,                                84L,             100L,               "High", "UNIVERSITY OF CALIFORNIA SANTA BARBARA",              "locality",                   15L,
      "Dorey_data_761973",       "Perdita trifasciata Timberlake, 1953", "Andrenidae", "Panurginae",       "Perdita",        NA,          NA,        "Perdita trifasciata",    "trifasciata",                    NA,                 NA,  "SPECIES",        "Timberlake, 1953",              NA, "United states",    "United states",         "US",   "New mexico",              NA,            NA,                                                         "Cottonwood Spring",      NA,                                            "CC0_1_0",                                                         "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT", "1990-09-03T00:00:00Z",   3L,     9L, 1990L,  "MATERIAL_CITATION",               NA,         "PRESENT",            NA, "T. L. Griswold & Walnut Canyon",       NA,                NA,              NA,               1L,                              NA,            NA,               NA,               NA,                NA,         NA,           NA,                   NA, "03DA51566E5A3810FF43FF7FFDD46C19.mc.3B1BEA1D6E583810FC56FF08FC936808",      NA,                                              NA,                                     NA,                    "Perdita trifasciata Timberlake",                      NA,       "FEMALE",                                               NA,              NA,            NA, "GBIF_Andrenidae",        "Perdita trifasciata",       "Perdita trifasciata Timberlake, 1953",           36.876388,           -108.39286,                                90L,              92L,               "High",                      "COTTONWOOD SPRING",              "locality",                   33L,
      "Dorey_data_13971366", "Augochlorella pomoniella (Cockerell, 1915)", "Halictidae", "Halictinae", "Augochlorella",        NA,          NA,   "Augochlorella pomoniella",     "pomoniella",                    NA,                 NA,         NA,       "(Cockerell, 1915)",              NA, "United States",    "United States",         "US",   "California",          "Inyo",            NA,                   "17 mi E Big Pine, Death Valley Road, pinon juniper zone",      NA,                                                   NA,                                                                                                         NA, "1992-06-21T00:00:00Z",  21L,     6L, 1992L,  "PreservedSpecimen",               NA,                NA,            NA,        "M.E. Irwin, D.K. Yeates",       NA,                NA,              NA,               1L, "INHS Insect Collection 352770",            NA,           "INHS",               NA,                NA,         NA,           NA,                   NA,                                                              "3247430",      NA, "urn:uuid:ab6083b7-ad01-4537-a7c4-e1cff7f2172c", "d93d943b-2390-4e2c-9050-11a9ec9a2a96",                                                  NA, "1992-06-21/1992-06-24",       "Female", "http://creativecommons.org/licenses/by-nc/3.0/",              NA,    "58444644", "SCAN_Halictidae",                           NA, "Augochlorella pomoniella (Cockerell, 1915)",           37.142698,          -118.123715,                                90L,             100L,               "High",     "DEATH VALLEY RD AT DEATH VALLEY RD",              "locality",                    2L,
      "CAES_data_4284",       "Andrena wellesleyana Robertson, 1897", "Andrenidae", "Andreninae",       "Andrena",        NA,          NA,       "Andrena wellesleyana",   "wellesleyana",                    NA,                 NA,         NA,         "Robertson, 1897",              NA, "United states",    "United states",         "US",  "Connecticut",     "New Haven",            NA, "North Haven, 0.5 km NNW of interchange 12 (US Highway 5) on Interstate 91",      NA, "https://creativecommons.org/licenses/by-nc-sa/4.0/",                                                                                                         NA, "1998-03-31T00:00:00Z",  31L,     3L, 1998L,                   NA,               NA,                NA,            NA,                 "Chris T. Maier",       NA,         "Netting",              NA,               1L,             "UCMS_ENT 00076127",            NA,           "CAES",               NA,                NA,         NA,           NA,                   NA,                                                                     NA,      NA,                                              NA,                                     NA, "Andrena (Parandrena) wellesleyana Robertson, 1910",                      NA, "Adult Female",                                               NA,              NA,            NA, "CAES_Anthophila",       "Andrena wellesleyana",       "Andrena wellesleyana Robertson, 1897",           41.403421,           -72.853785,                               170L,              89L,               "High",         "Distance NNW of INTERCHANGE 12",              "locality",                    4L,
      "CAES_data_4285",       "Andrena wellesleyana Robertson, 1897", "Andrenidae", "Andreninae",       "Andrena",        NA,          NA,       "Andrena wellesleyana",   "wellesleyana",                    NA,                 NA,         NA,         "Robertson, 1897",              NA, "United states",    "United states",         "US",  "Connecticut",     "New Haven",            NA, "North Haven, 0.5 km NNW of interchange 12 (US Highway 5) on Interstate 91",      NA, "https://creativecommons.org/licenses/by-nc-sa/4.0/",                                                                                                         NA, "1997-04-21T00:00:00Z",  21L,     4L, 1997L,                   NA,               NA,                NA,            NA,                 "Chris T. Maier",       NA,         "Netting",              NA,               1L,             "UCMS_ENT 00076128",            NA,           "CAES",               NA,                NA,         NA,           NA,                   NA,                                                                     NA,      NA,                                              NA,                                     NA, "Andrena (Parandrena) wellesleyana Robertson, 1911",                      NA, "Adult Female",                                               NA,              NA,            NA, "CAES_Anthophila",       "Andrena wellesleyana",       "Andrena wellesleyana Robertson, 1897",           41.403421,           -72.853785,                               170L,              89L,               "High",         "Distance NNW of INTERCHANGE 12",              "locality",                    4L,
      "Dorey_data_1010149", "Acamptopoeum submetallicum (Spinola, 1851)", "Andrenidae", "Panurginae",  "Acamptopoeum",        NA,          NA, "Acamptopoeum submetallicum",  "submetallicum",                    NA,                 NA,  "SPECIES",         "(Spinola, 1851)", "SOUTH_AMERICA",         "Chile",            "Chile",         "CL",        "Maule",         "Talca",            NA,                                                                 "Rio Claro",      NA,                                       "CC_BY_NC_4_0", "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT;INSTITUTION_MATCH_FUZZY;INSTITUTION_COLLECTION_MISMATCH", "1980-11-28T00:00:00Z",  28L,    11L, 1980L, "PRESERVED_SPECIMEN", "PhysicalObject",         "PRESENT",            NA,                               NA,       NA,                NA,              NA,               1L,                          "1291",            NA,           "PUCV",               NA,                NA,         NA,           NA,                   NA,                                                       "PUCV:ENTO:1291",      NA,                                              NA,                                     NA,                        "Acamptopoeum submetallicum",                      NA,       "FEMALE",                                               NA,              NA,            NA, "GBIF_Andrenidae", "Acamptopoeum submetallicum", "Acamptopoeum submetallicum (Spinola, 1851)",          -34.997203,           -70.818563,                               210L,              84L,               "High",                              "RIO CLARO",              "locality",                    4L
    )
    
    testData2 <- dplyr::tribble(
      ~database_id,                            ~scientificname,      ~family,   ~subfamily,         ~genus, ~subgenus, ~infraspecificepithet,         ~specificepithet, ~`_specificepithet`, ~`_infraspecificepithet`, ~acceptednameusage, ~taxonrank, ~scientificnameauthorship, ~unnamed_column_1, ~continent,        ~country, ~country_suggested, ~countrycode,  ~stateprovince,  ~county, ~municipality,                    ~locality, ~island,    ~license,                                                                  ~issue,             ~eventdate, ~day, ~month, ~year,       ~basisofrecord, ~type, ~occurrencestatus, ~recordnumber,   ~recordedby, ~eventid, ~samplingprotocol, ~samplingeffort, ~individualcount,             ~catalognumber, ~rightsholder, ~institutioncode, ~unnamed_column_2, ~decimallatitude, ~decimallongitude, ~unnamed_column_3, ~datasetid, ~datasetname, ~othercatalognumbers,                          ~occurrenceid, ~coreid,                                       ~recordid,                          ~collectionid,                        ~verbatimscientificname, ~verbatimeventdate,     ~sex,                                          ~rights, ~unnamed_column_4, ~occurrenceyear,       ~id,       ~datasource, ~unnamed_column_5,      ~names_clean,                   ~verbatim_scientificname, ~bels_match_country, ~bels_interpreted_countrycode,                  ~bels_matchwithcoords,              ~bels_matchverbatimcoords,                  ~bels_matchsanscoords, ~bels_decimallatitude, ~bels_decimallongitude, ~bels_geodeticdatum, ~bels_coordinateuncertaintyinmeters,   ~bels_georeferencedby, ~bels_georeferenceddate,                                                                        ~bels_georeferenceprotocol,       ~bels_georeferencesources, ~bels_georeferenceremarks, ~bels_georeference_score, ~bels_georeference_source, ~bels_best_of_n_georeferences,              ~bels_match_type,
      "Dorey_data_7221776",              "Augochlora pura (Say, 1837)", "Halictidae", "Halictinae",   "Augochlora",        NA,                    NA,        "Augochlora pura",              "pura",                       NA,                 NA,  "SPECIES",             "(Say, 1837)",                NA,         NA, "United states",    "United states",         "US", "West Virginia",       NA,            NA, "Coopers Rock State Forest,",      NA, "CC_BY_4_0", "TAXON_MATCH_HIGHERRANK;INSTITUTION_MATCH_FUZZY;COLLECTION_MATCH_FUZZY", "1987-08-17T00:00:00Z",  17L,     8L, 1987L, "PRESERVED_SPECIMEN",    NA,         "PRESENT",            NA,            NA,       NA,                NA,              NA,               NA, "Insect Collection 358298",            NA,           "INHS",                NA,               NA,                NA,        436516747L,         NA,           NA,                   NA,                                     NA,      NA,                                              NA,                                     NA, "Augochlora (Augochlora) pura pura (Say 1837)",                 NA,       NA,                                               NA,                NA,              NA,        NA, "GBIF_Halictidae",                NA, "Augochlora pura",              "Augochlora pura (Say, 1837)",                "US",                          "US", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest",             39.664554,             -79.773401,         "epsg:4326",                                  6L,                      NA,                      NA,                                                                                                NA,                              NA,                        NA,                       0L,                    "GBIF",                            2L, "match using verbatim coords",
      "Dorey_data_7242038",              "Augochlora pura (Say, 1837)", "Halictidae", "Halictinae",   "Augochlora",        NA,                    NA,        "Augochlora pura",              "pura",                       NA,                 NA,  "SPECIES",             "(Say, 1837)",                NA,         NA, "United states",    "United states",         "US", "West Virginia",       NA,            NA, "Coopers Rock State Forest,",      NA, "CC_BY_4_0", "TAXON_MATCH_HIGHERRANK;INSTITUTION_MATCH_FUZZY;COLLECTION_MATCH_FUZZY", "1987-08-17T00:00:00Z",  17L,     8L, 1987L, "PRESERVED_SPECIMEN",    NA,         "PRESENT",            NA,            NA,       NA,                NA,              NA,               NA, "Insect Collection 358299",            NA,           "INHS",                NA,               NA,                NA,        436516760L,         NA,           NA,                   NA,                                     NA,      NA,                                              NA,                                     NA, "Augochlora (Augochlora) pura pura (Say 1837)",                 NA,       NA,                                               NA,                NA,              NA,        NA, "GBIF_Halictidae",                NA, "Augochlora pura",              "Augochlora pura (Say, 1837)",                "US",                          "US", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest",             39.664554,             -79.773401,         "epsg:4326",                                  6L,                      NA,                      NA,                                                                                                NA,                              NA,                        NA,                       0L,                    "GBIF",                            2L, "match using verbatim coords",
      "Dorey_data_7220088",              "Augochlora pura (Say, 1837)", "Halictidae", "Halictinae",   "Augochlora",        NA,                    NA,        "Augochlora pura",              "pura",                       NA,                 NA,  "SPECIES",             "(Say, 1837)",                NA,         NA, "United states",    "United states",         "US", "West Virginia",       NA,            NA, "Coopers Rock State Forest,",      NA, "CC_BY_4_0", "TAXON_MATCH_HIGHERRANK;INSTITUTION_MATCH_FUZZY;COLLECTION_MATCH_FUZZY", "1987-08-17T00:00:00Z",  17L,     8L, 1987L, "PRESERVED_SPECIMEN",    NA,         "PRESENT",            NA,            NA,       NA,                NA,              NA,               NA, "Insect Collection 358300",            NA,           "INHS",                NA,               NA,                NA,        436516763L,         NA,           NA,                   NA,                                     NA,      NA,                                              NA,                                     NA, "Augochlora (Augochlora) pura pura (Say 1837)",                 NA,       NA,                                               NA,                NA,              NA,        NA, "GBIF_Halictidae",                NA, "Augochlora pura",              "Augochlora pura (Say, 1837)",                "US",                          "US", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest",             39.664554,             -79.773401,         "epsg:4326",                                  6L,                      NA,                      NA,                                                                                                NA,                              NA,                        NA,                       0L,                    "GBIF",                            2L, "match using verbatim coords",
      "Dorey_data_7236978",              "Augochlora pura (Say, 1837)", "Halictidae", "Halictinae",   "Augochlora",        NA,                    NA,        "Augochlora pura",              "pura",                       NA,                 NA,  "SPECIES",             "(Say, 1837)",                NA,         NA, "United states",    "United states",         "US", "West Virginia",       NA,            NA, "Coopers Rock State Forest,",      NA, "CC_BY_4_0", "TAXON_MATCH_HIGHERRANK;INSTITUTION_MATCH_FUZZY;COLLECTION_MATCH_FUZZY", "1987-08-17T00:00:00Z",  17L,     8L, 1987L, "PRESERVED_SPECIMEN",    NA,         "PRESENT",            NA,            NA,       NA,                NA,              NA,               NA, "Insect Collection 358301",            NA,           "INHS",                NA,               NA,                NA,        436516766L,         NA,           NA,                   NA,                                     NA,      NA,                                              NA,                                     NA, "Augochlora (Augochlora) pura pura (Say 1837)",                 NA,       NA,                                               NA,                NA,              NA,        NA, "GBIF_Halictidae",                NA, "Augochlora pura",              "Augochlora pura (Say, 1837)",                "US",                          "US", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest", "uswestvirginiacoopersrockstateforest",             39.664554,             -79.773401,         "epsg:4326",                                  6L,                      NA,                      NA,                                                                                                NA,                              NA,                        NA,                       0L,                    "GBIF",                            2L, "match using verbatim coords",
      "Dorey_data_15031296", "Lasioglossum sisymbrii (Cockerell, 1895)", "Halictidae", "Halictinae", "Lasioglossum",        NA,                    NA, "Lasioglossum sisymbrii",         "sisymbrii",                       NA,                 NA,         NA,       "(Cockerell, 1895)",                NA,         NA, "United states",    "United states",         "US",       "Arizona", "Mohave",            NA,                     "Topock",      NA,          NA,                                                                      NA, "1972-04-08T00:00:00Z",   8L,     4L, 1972L,  "PreservedSpecimen",    NA,                NA,            NA, "B. Apperson",       NA,                NA,              NA,               1L,                "HYM 95971",         "MNA",            "MNA",                NA,               NA,                NA,                NA,         NA,           NA,                   NA, "1154c20d-211a-47ba-8320-784b8e320e6f",      NA, "urn:uuid:ada6a97d-7d8e-4765-a607-289b4abcaa39", "48b700e0-fcdb-4196-b642-70fd7b8a71ef",                                             NA,         "8/4/1972", "Female", "http://creativecommons.org/licenses/by-nc/4.0/",                NA,              NA, 57139867L, "SCAN_Halictidae",                NA,                NA, "Lasioglossum sisymbrii (Cockerell, 1895)",                "US",                          "US",                "usarizonamohavetopock",                "usarizonamohavetopock",                "usarizonamohavetopock",            34.7183075,           -114.4871902,         "epsg:4326",                                 14L, "Gary W. Shugart (PSM)",              "3/8/2003", "MaNIS/HerpNET/ORNIS Georeferencing Guidelines, Guide to Best Practices for Georeferencing 2006.", "AZ GNIS downloaded March 2003",                        NA,                      30L,                   "ORNIS",                            1L, "match using verbatim coords"
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    # First create a workbook
    wb <- openxlsx::createWorkbook()
    # Add sheets to it
    openxlsx::addWorksheet(wb, "GEOLOCATE HIGH")
    openxlsx::addWorksheet(wb, "BELS High")
    # Fill those sheets with data
    openxlsx::writeData(wb, "GEOLOCATE HIGH", testData)
    openxlsx::writeData(wb, "BELS High", testData2)
    # Save to file
    openxlsx::saveWorkbook(wb, file = paste0(tempdir(), "/testData.xlsx"), overwrite = TRUE)
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "GeoL",
                                   path = tempdir(),
                                  inFile = "testData.xlsx",
                                  outFile = "testDataOut.csv",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_GeoL results columns TRUE", {
      testthat::expect_equal(resultsT, 59)
    })
    testthat::test_that("readr_GeoL results columns FALSE", {
      testthat::expect_equal(resultsF, 1)
    })
    
    testthat::test_that("readr_GeoL expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    
    
    
#### 13.0 EaCO  ####
    
    testData <- dplyr::tribble(
      ~'Specimen Number', ~`Farmers (names redacted)`, ~'Date Blue vane traps were active', ~'Date Range for collection', ~'Sampling Round', ~'Treatment type', ~'Trap number',          ~County, ~State, ~'GPS Coordinates of Traps',         ~Genus, ~`Species (if available)`, ~`other (example coleoptera)`, ~`notes (example hymenoptera-ant)`, ~comment,
      6376L,                          3L,                                NA,              "05/24-06/01",              1L,        "Grazed",         "S7", "Kit Carson Co.",   "CO",      "39.210N, -102.884W", "Species",                     "135",                            NA,                                 NA,       NA,
      6051L,                          3L,                                NA,              "05/24-06/01",              1L,      "Ungrazed",         "S6", "Kit Carson Co.",   "CO",      "39.210N, -102.883W", "Species",                     "165",                            NA,                                 NA,       NA,
      3291L,                          2L,                                NA,              "06/21-06/28",              2L,        "Grazed",         "T4",    "Perkins Co.",   "NE",      "40.799N, -101.944W",  "Species",             "secret",                            NA,                                 NA,       NA,
      3457L,                          2L,                                NA,              "06/21-06/28",              2L,      "Ungrazed",         "T8",    "Perkins Co.",   "NE",      "40.799N, -101.936W",  "Species",             "secret",                            NA,                                 NA,       NA,
      103L,                          1L,                                NA,              "05/24-06/01",              1L,        "Fallow",         "F4",       "Weld Co.",   "CO",      "40.503N, -103.901W",   "Species",               "secret",                            NA,                                 NA,       NA
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xslx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName="Sheet1")
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "EaCo",
                                   path = tempdir(),
                                  inFile = "testData.xlsx",
                                  outFile = "testDataOut.csv",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_EaCO results columns TRUE", {
      testthat::expect_equal(resultsT, 18)
    })
    testthat::test_that("readr_EaCO results columns FALSE", {
      testthat::expect_equal(resultsF, 0)
    })
    
    testthat::test_that("readr_EaCO expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    
    
#### 14.0 MABC  ####
    
    testData <- dplyr::tribble(
      ~Ejemplar, ~'Fecha colecta',    ~País, ~`Estado/Provincia`, ~Municipio,       ~Localidad,            ~'Sitio Colecta', ~'Código sitio', ~'Metodo colecta', ~Hora, ~'Coordenadas Lat', ~'Coordenadas Long', ~Altitud, ~'Datos georeferenciación',         ~Colector,   ~Identificador,     ~Familia,      ~Subfamilia,          ~Tribu,      ~Genero, ~Subgenero,      ~Especie, ~Subespecie,       ~'Nombre especie', ~'Código especie', ~Sexo,
      "MABC-04-0000001",   "10/10/1982", "México",           "Jalisco",         NA,        "Chamela", "Estacion Biológica UNAM",            NA,              NA,    2,               NA,                NA,       NA,                       NA, "Stephen Bullock", "R. R. Snelling", "Colletidae", "Diphaglossinae", "Dissoglossini", "Secret",         NA,     "species",          NA,  "Secret species",              NA,   "M",
      "MABC-04-0000002",   "23/10/1982", "México",           "Jalisco",         NA,        "Chamela", "Estacion Biológica UNAM",            NA,              NA,    7,               NA,                NA,       NA,                       NA, "Stephen Bullock", "R. R. Snelling", "Colletidae", "Diphaglossinae", "Dissoglossini", "Secret",         NA,     "species",          NA,  "Secret species",              NA,   "H",
      "MABC-04-0000003",    "8/11/2003", "México",   "Baja California", "Ensenada", "Nuevo Rosarito",          "Nuevo Rosarito",       "BCENR",              NA,    NA,           28.634,          -114.017,       NA,             "Originales", "Stephen Bullock", "Terry Griswold", "Colletidae",     "Colletinae",     "Colletini",   "Secret",         NA, "species",          NA, "Secret species",        "COLALB",   "M",
      "MABC-04-0000004",    "8/11/2003", "México",   "Baja California", "Ensenada", "Nuevo Rosarito",          "Nuevo Rosarito",       "BCENR",              NA,    NA,           28.634,          -114.017,       NA,             "Originales", "Stephen Bullock", "Terry Griswold", "Colletidae",     "Colletinae",     "Colletini",   "Secret",         NA, "species",          NA, "Secret species",        "COLALB",   "M",
      "MABC-04-0000005",    "8/11/2003", "México",   "Baja California", "Ensenada", "Nuevo Rosarito",          "Nuevo Rosarito",       "BCENR",              NA,    NA,           28.634,          -114.017,       NA,             "Originales", "Stephen Bullock", "Terry Griswold", "Colletidae",     "Colletinae",     "Colletini",   "Secret",         NA, "species",          NA, "Secret species",        "COLALB",   "M"
    )
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName="Hoja1")
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "MABC",
                                   path = tempdir(),
                                  inFile = "testData.xlsx",
                                  outFile = "testDataOut.csv",
                                  sheet = "Hoja1",
                                  dataLicense = "All rights reserved")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    testthat::test_that("readr_MABC results columns TRUE", {
      testthat::expect_equal(resultsT, 27)
    })
    testthat::test_that("readr_MABC results columns FALSE", {
      testthat::expect_equal(resultsF, 0)
    })
    
    testthat::test_that("readr_MABC expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    
    
#### 15.0 Col  ####
    
    # this is FAKE DATA BECAUSE THE REAL DATA IS UNDER RESTRCTIVE LICENSE
    testData <- dplyr::tribble(
      ~Código.de.Barras, ~institutionCode, ~`Colectores.[Aggregated]`, ~Colectores.asociados, ~Fecha.colección.inicial,  ~Ano, ~mes, ~dia,        ~Orden, ~Familia,         ~Género,    ~Especie,            ~Especie.Author, ~Tipo, ~`Determinador.[Formatted]`, ~Fecha.determinación,            ~País,        ~Departamento, ~Municipio, ~Corregimiento.Departamental,                                              ~Localidad, ~Latitud.georref..dec., ~Longitud.georref..dec.,          ~Nombre.Completo,
      "ICN_butfake1",            "ICN",              "anonymous a",                    NA,             "01/01/2010", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,                          NA,                   NA, "ESTADOS UNIDOS",            "ARIZONA",   "COCHISE", "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera",
      "ICN_butfake2",            "ICN",              "anonymous b",                    NA,             "01/01/2010", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,               "anonymous e",                   NA, "ESTADOS UNIDOS",            "ARIZONA",   "COCHISE", "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera",
      "ICN_butfake3",            "ICN",              "anonymous c",                    NA,             "01/01/2010", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,                          NA,                   NA, "ESTADOS UNIDOS",            "ARIZONA",  "COCHISE",  "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera",
      "ICN_butfake4",            "ICN",              "anonymous d",                    NA,            "01/01/20101", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,               "anonymous f",                   NA, "ESTADOS UNIDOS",            "ARIZONA",  "COCHISE",  "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera"
    )
    
    # need to change column names, function requires spaces to work 
    colnames(testData) <- c("Código de Barras", "institutionCode", "Colectores [Aggregated]", "Colectores asociados", "Fecha colección inicial", "Ano", "mes", "dia", "Orden", "Familia", "Género", "Especie", "Especie Author", "Tipo", "Determinador [Formatted]", "Fecha determinación", "País", "Departamento", "Municipio", "Corregimiento Departamental", "Localidad", "Latitud georref. dec.", "Longitud georref. dec.", "Nombre Completo")
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName = "Spanish headers")
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Col",
                                   path = tempdir(),
                                 inFile = "testData.xlsx",
                                 outFile = "testDataOut.csv",
                                 sheet = "Spanish headers",
                                 dataLicense = "All rights reserved")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_Col results columns TRUE", {
      testthat::expect_equal(resultsT, 27)
    })
    
    testthat::test_that("readr_Col results columns FALSE", {
      testthat::expect_equal(resultsF, 3)
    })
    
    testthat::test_that("readr_Col expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
#### 16.0 FSCA  ####
    
    testData <- dplyr::tribble(
      ~id, ~institutionCode, ~collectionCode, ~ownerInstitutionCode,      ~basisOfRecord, ~occurrenceID,  ~catalogNumber, ~otherCatalogNumbers,                                                                                      ~higherClassification,   ~kingdom,      ~phylum,    ~class,        ~order,  ~family,       ~scientificName, ~taxonID, ~scientificNameAuthorship,      ~genus, ~subgenus, ~specificEpithet, ~verbatimTaxonRank, ~infraspecificEpithet, ~taxonRank,             ~identifiedBy, ~dateIdentified, ~identificationReferences, ~identificationRemarks, ~taxonRemarks, ~identificationQualifier, ~typeStatus,                    ~recordedBy, ~associatedCollectors, ~recordNumber, ~eventDate, ~year, ~month, ~day, ~startDayOfYear, ~endDayOfYear, ~verbatimEventDate,                                                                                                                             ~occurrenceRemarks, ~habitat, ~substrate, ~verbatimAttributes, ~fieldNumber, ~eventID, ~informationWithheld, ~dataGeneralizations, ~dynamicProperties, ~associatedOccurrences, ~associatedSequences, ~associatedTaxa, ~reproductiveCondition, ~establishmentMeans, ~cultivationStatus, ~lifeStage,     ~sex, ~individualCount, ~preparations,        ~country, ~stateProvince,   ~county, ~municipality,                                                                             ~locality, ~locationRemarks, ~localitySecurity, ~localitySecurityReason, ~decimalLatitude, ~decimalLongitude, ~geodeticDatum, ~coordinateUncertaintyInMeters,                  ~verbatimCoordinates, ~georeferencedBy, ~georeferenceProtocol, ~georeferenceSources, ~georeferenceVerificationStatus, ~georeferenceRemarks, ~minimumElevationInMeters, ~maximumElevationInMeters, ~minimumDepthInMeters, ~maximumDepthInMeters, ~verbatimDepth, ~verbatimElevation, ~disposition, ~language, ~recordEnteredBy,       ~modified, ~`sourcePrimaryKey-dbpk`, ~collID,                                       ~recordID,                                                                         ~references,
      2109207L,           "FSCA",          "FSCA",                    NA, "PreservedSpecimen",            NA, "FSCA 00008737",                   NA, "Organism|Animalia|Arthropoda|Insecta|Hymenoptera|Apoidea|Anthophila|Apidae|Apinae|Anthophorini|Habropoda", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Habropoda laboriosa",   11043L,       "(Fabricius, 1804)", "Habropoda",        NA,      "laboriosa",                 NA,                    NA,  "Species", "Wiley, J. R. (James R.)",        "3/1/99",                        NA,  "Habropoda laboriosa",            NA,                       NA,          NA,    "Stange, L. A. (Lionel A.)",                    NA,            NA,  "3/25/91", 1991L,     3L,  25L,             84L,            NA,                 NA, "[Florida Alachua Co: Gainesvile Doyle Conner Bld, March 25, 1991 L.A. Stange col.] [Habropoda laboriosa (Fabricius) 1804 det. J. Wiley 1999]",       NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 NA,    "adult", "Female",               NA,            NA, "United States",      "Florida", "Alachua",            NA, "Doyle Conner Bldg. (32601), Dept. of Agriculture, Gainesville, Alachua Co., Florida",               NA,                0L,                      NA,         29.63438,         -82.37143,             NA,                             NA,                 "29.63438, -82.37143",               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/12/22 14:58",          "FSCA 00008737",     14L, "urn:uuid:b08fcc6e-ba5d-437a-ba80-0ab32bfd0269", "https://library.big-bee.net/portal/collections/individual/index.php?occid=2109207",
      2109208L,           "FSCA",          "FSCA",                    NA, "PreservedSpecimen",            NA, "FSCA 00008738",                   NA, "Organism|Animalia|Arthropoda|Insecta|Hymenoptera|Apoidea|Anthophila|Apidae|Apinae|Anthophorini|Habropoda", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Habropoda laboriosa",   11043L,       "(Fabricius, 1804)", "Habropoda",        NA,      "laboriosa",                 NA,                    NA,  "Species",               "Wiley, J.",        "3/1/99",                        NA,  "Habropoda laboriosa",            NA,                       NA,          NA, "Weems Jr., H. V. (Howard V.)",                    NA,            NA,  "3/26/84", 1984L,     3L,  26L,             86L,            NA,                 NA, "[Florida Alachua Co., 8 miles W of Gainesville of FL 26 -111-1984 H.V. Weems, Jr.] [Habropoda laboriosa (Fabricius) 1804 det. J. Wiley 1999]",       NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 NA,    "adult",   "Male",               NA,            NA, "United States",      "Florida", "Alachua",            NA,                                             "8 mi. W of Gainesville, Alachua Co., FL",               NA,                0L,                      NA,      29.66733333,      -82.53688889,             NA,                             NA, "29.6673333333333, -82.5368888888889",               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/12/22 14:58",          "FSCA 00008738",     14L, "urn:uuid:342bc796-8184-4f59-a8b2-1c68a9f67456", "https://library.big-bee.net/portal/collections/individual/index.php?occid=2109208",
      2109209L,           "FSCA",          "FSCA",                    NA, "PreservedSpecimen",            NA, "FSCA 00008739",                   NA, "Organism|Animalia|Arthropoda|Insecta|Hymenoptera|Apoidea|Anthophila|Apidae|Apinae|Anthophorini|Habropoda", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Habropoda laboriosa",   11043L,       "(Fabricius, 1804)", "Habropoda",        NA,      "laboriosa",                 NA,                    NA,  "Species",               "Wiley, J.",        "3/1/99",                        NA,  "Habropoda laboriosa",            NA,                       NA,          NA,                             NA,                    NA,            NA,  "3/17/85", 1985L,     3L,  17L,             76L,            NA,                 NA,                          "[Florida: Gainesville UF Campus Mtrap Bamila 17-III-1985] [Habropoda laboriosa (Fabricius) 1804 det. J. Wiley 1999]",       NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 NA,    "adult",   "Male",               NA,            NA, "United States",      "Florida", "Alachua",            NA,                                      "Gainesville, University of FL, Alachua Co., FL",               NA,                0L,                      NA,         29.65163,         -82.32483,             NA,                             NA,                 "29.65163, -82.32483",               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/12/22 14:58",          "FSCA 00008739",     14L, "urn:uuid:af32f90c-2763-4a21-8f90-e93588a80c0f", "https://library.big-bee.net/portal/collections/individual/index.php?occid=2109209",
      2109210L,           "FSCA",          "FSCA",                    NA, "PreservedSpecimen",            NA, "FSCA 00008740",                   NA, "Organism|Animalia|Arthropoda|Insecta|Hymenoptera|Apoidea|Anthophila|Apidae|Apinae|Anthophorini|Habropoda", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Habropoda laboriosa",   11043L,       "(Fabricius, 1804)", "Habropoda",        NA,      "laboriosa",                 NA,                    NA,  "Species",               "Wiley, J.",        "3/1/99",                        NA,  "Habropoda laboriosa",            NA,                       NA,          NA,                             NA,                    NA,            NA,  "3/17/85", 1985L,     3L,  17L,             76L,            NA,                 NA,                          "[Florida: Gainesville UF Campus Mtrap Bamila 17-III-1985] [Habropoda laboriosa (Fabricius) 1804 det. J. Wiley 1999]",       NA,         NA,                  NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,                 NA,    "adult",   "Male",               NA,            NA, "United States",      "Florida", "Alachua",            NA,                                      "Gainesville, University of FL, Alachua Co., FL",               NA,                0L,                      NA,         29.65163,         -82.32483,             NA,                             NA,                 "29.65163, -82.32483",               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "9/12/22 14:58",          "FSCA 00008740",     14L, "urn:uuid:ca52acc1-7026-46cd-8089-add7f4388734", "https://library.big-bee.net/portal/collections/individual/index.php?occid=2109210"
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "FSCA",
                                   path = tempdir(),
                                  inFile = "testData.csv",
                                  outFile = "testDataOut.csv",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_FSCA results columns TRUE", {
      testthat::expect_equal(resultsT, 52)
    })
    
    testthat::test_that("readr_FSCA results columns FALSE", {
      testthat::expect_equal(resultsF, 0)
    })
    
    testthat::test_that("readr_FSCA expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
#### 17.0 SMC  ####
    
    testData <- dplyr::tribble(
      ~organismName, ~individualCount,  ~county, ~stateProvince,                             ~locale, ~observationDate, ~collectionMethod,            ~references, ~institutionCode, ~latitude, ~longitude, ~georeferenceMethod, ~georeferenceMethodNotes,  ~georeferencedBy, ~determiner, ~lifeStage,                                                 ~Data.Source.Link, ~Notes,
      "Agapostemon_angelicus/texanus",               2L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "3/17/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
      "Agapostemon_melliventris",               2L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "3/17/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
      "Andrena_alamonis",               2L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "3/17/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
      "Agapostemon_angelicus",               6L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "4/12/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA,
      "Agapostemon_coloradinus",               1L, "Bailey",        "Texas", "Muleshoe National Wildlife Refuge",      "4/12/2013",       "pan_traps", "Auerbach et al, 2019",            "TTU",  33.96377,  -102.7508,       "Digital Map",           "Google Earth", "Shannon Collins",          NA,    "Adult", "https://academic.oup.com/ee/article/48/4/968/5494819?login=true",     NA
    )
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "SMC",
                                   path = tempdir(),
                                 inFile = "testData.csv",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_SMC results columns TRUE", {
      testthat::expect_equal(resultsT, 20)
    })
    testthat::test_that("readr_SMC results columns FALSE", {
      testthat::expect_equal(resultsF, 8)
    })
    
    testthat::test_that("readr_SMC expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    
#### 18.0 Bal  ####
    
    testData <- dplyr::tribble(
      ~studyID,           ~siteID, ~year,     ~date,                           ~animalID, ~abundance, ~abundanceMethod, ~samplingMethod, ~numCensus, ~samplingIntensity,                                                                                                                                                                                   ~censusType,     ~fieldDist,  ~flowering, ~decimalLatitude, ~decimalLongitude,                ~studyLocation, ~habitatType,                ~siteDescription,
      "Ball01", "Bastrop Gardens", 2013L, "22/5/2013",             "Agapostemon angelicus",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
      "Ball01", "Bastrop Gardens", 2013L, "22/5/2013",               "Agapostemon texanus",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
      "Ball01", "Bastrop Gardens", 2013L, "22/5/2013",            "Anthophora californica",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
      "Ball01", "Bastrop Gardens", 2013L, "22/5/2013",                "Ceratina shinnersi",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
      "Ball01", "Bastrop Gardens", 2013L, "22/5/2013",                  "Ceratina strenua",         1L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden",
      "Ball01", "Bastrop Gardens", 2013L, "22/5/2013", "Lasioglossum (Dialictus) sp.TX-14",        12L,      "Abundance",      "Pan Trap",         1L,                24L, "50 pan traps (painted blue, yellow or left white as in LeBuhn et al) placed in x formation 1 m apart from center of the 50 m2 plot, vegetation measured as described for blue vane trapped.", "not measured", "flowering",        30.157397,        -97.491703, "Bastrop, Bastrop County, TX",           NA, "Agriculture. Community garden"
    ) 
    # Because readr_Bal ignores the first row of data, get the columns names and add them again 
    # to the data to be saved (and eventually ignored).
    testDataCols <-colnames(testData)
    # Convert into a data.frame instead of a tibble to relax the requirements on colmn classes matching
    testData <- testData %>% 
      as.data.frame(stringsAsFactors = FALSE) 
    # Re-bind the column names to the first row of data.
    testData <- testDataCols %>%
      rbind(testData)
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName = "animal_data")
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Bal",
                                   path = tempdir(),
                                 inFile = "testData.xlsx",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
                                 sheet = "animal_data")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_Bal results columns TRUE", {
      testthat::expect_equal(resultsT, 19)
    })
    
    testthat::test_that("readr_Bal results columns FALSE", {
      testthat::expect_equal(resultsF, 8)
    })
    
    testthat::test_that("readr_Bal expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    
#### 19.0 Lic  ####
    
    testData <- dplyr::tribble(
      ~eventID, ~occurrenceID,      ~basisOfRecord,  ~eventDate,   ~Kingdom,        ~Order, ~Family_or_grp,      ~Tribe,         ~Genus, ~Species, ~Morphospecies, ~adult, ~sex,    ~Collector,          ~Determiner, ~individualCount, ~samplingProtocol, ~samplingEffort, ~sampleSizeValue, ~sampleSizeUnit, ~decimalLatitude, ~decimalLongitude, ~geodeticDatum, ~countryCode,                   ~country,
      "Be2:b",        "EML1", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Andrenidae",          NA,             NA,       NA,            38L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               1L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
      "Be2:b",        "EML2", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Andrenidae",          NA,             NA,       NA,            77L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               2L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
      "Be2:b",        "EML3", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",       "Apidae",          NA,             NA,       NA,            15L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               2L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
      "Be2:b",        "EML4", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Halictidae", "Halictini",             NA,       NA,            39L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               1L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
      "Be2:b",        "EML5", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Halictidae",          NA,  "Agapostemon",       NA,            13L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               1L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America",
      "Be2:b",        "EML6", "PreservedSpecimen", "24/6/2014", "Animalia", "Hymenoptera",   "Halictidae",          NA, "Lasioglossum",       NA,            37L,   TRUE,   NA, "L. Rafferty", "Ivan Milosavljevic",               4L,   "blue pan trap",      "24 hours",               2L,         "traps",            46.52,           -116.87,        "WGS84",         "US", "United States of America"
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Lic",
                                   path = tempdir(),
                                 inFile = "testData.csv",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_Lic results columns TRUE", {
      testthat::expect_equal(resultsT, 27)
    })
    
    testthat::test_that("readr_Lic results columns FALSE", {
      testthat::expect_equal(resultsF, 8)
    })
    
    testthat::test_that("readr_Lic expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
#### 20.0 Arm  ####
    
    testData <- dplyr::tribble(
      ~fam,        ~genus,            ~sp,                   ~species, ~sex,         ~locality,    ~munic, ~state,  ~g,  ~m,    ~s,          ~y,   ~G,  ~M,    ~S,           ~x, ~elev,         ~ecoregion,        ~veget, ~day, ~month, ~year, ~institutionCode,
      "Halictidae", "Agapostemon", "melliventris", "Agapostemon melliventris",  "M",       "El Sauzal",  "Mulegé",  "BCS", 27L, 10L,  22.3, 27.17286111, 112L, 52L,   5.7,   -112.86825,  132L,  "Vizcaíno Desert",       "oasis",  13L,  "jul", 2017L,         "CIBNOR",
      "Halictidae", "Agapostemon",    "mexicanus",    "Agapostemon mexicanus",  "M", "Cd. Insurgentes", "Comondú",  "BCS", 25L,  8L, 28.97, 25.14138056, 111L, 48L, 40.03, -111.8111194,   60L, "Magdalena plains", "xeric scrub",   3L,  "jun", 2017L,         "CIBNOR",
      "Halictidae", "Agapostemon", "melliventris", "Agapostemon melliventris",  "M",       "El Sauzal",  "Mulegé",  "BCS", 27L, 10L,  22.3, 27.17286111, 112L, 52L,   5.7,   -112.86825,  132L,  "Vizcaíno Desert",       "oasis",  13L,  "jul", 2017L,         "CIBNOR"
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                         sheetName = "Sheet1")
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Arm",
                                   path = tempdir(),
                                   inFile = "testData.xlsx",
                                   outFile = "testDataOut.csv",
                                   sheet = "Sheet1",
                                   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_Arm results columns TRUE", {
      testthat::expect_equal(resultsT, 25)
    })
    testthat::test_that("readr_Arm results columns FALSE", {
      testthat::expect_equal(resultsF, 0)
    })
    
    testthat::test_that("readr_Arm expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
    
#### 21.0 Dorey  ####
    
    testData <- dplyr::tribble(
      ~basisOfRecord, ~catalogNumber, ~recordNumber,   ~locationID,      ~family,  ~subfamily,         ~genus, ~specificEpithet,           ~scientificName, ~identifiedBy,    ~sex, ~individualCount, ~decimalLatitude, ~decimalLongitude, ~elevationInMeters, ~stateOrProvince,        ~locality,   ~eventDate, ~eventTime, ~recordedBy,    ~country,                                                                                                                                                                                                                                                                                                               ~fieldNotes, ~samplingEffort,            ~associatedTaxa, ~Plant.ID.by, ~Plant.ID.using,                                                                                                                                                                                ~references, ~coordinateUncertaintyInMeters, ~institutionCode,                                                                                                                                                                     ~bibliographicCitation,
      "preservedSpecimen",             NA, "18JDEC450-A", "JBD_Site365", "Colletidae", "Hylaeinae", "Pharohylaeus",     "lactiferus", "Pharohylaeus lactiferus",    "JB Dorey",     "F",               1L,        -17.26785,         145.49384,               868L,            "QLD", "Hallorans Hill",     "3/2/19",     "8:55",  "JB Dorey", "Australia",                                                                                                                                                                                                                                                    "Sight-swept off vegetation on rainforests edge. Mostly overcast ~24C",              NA, "Brachychiton acerifolius",   "JB Dorey",       "Keybase", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",                            10L,          "Dorey", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",
      "preservedSpecimen",             NA, "18JDEC459-A", "JBD_Site365", "Colletidae", "Hylaeinae", "Pharohylaeus",     "lactiferus", "Pharohylaeus lactiferus",    "JB Dorey",    "2M",               2L,        -17.26761,         145.49474,               864L,            "QLD", "Hallorans Hill",     "5/2/19",    "12:12",  "JB Dorey", "Australia",                                                                                                                             "General sweep off Stenocarpus sinuatus 6-7 m up in the canopy of rainforest (probably regenerated around a remnant core). Partly sunny to raining. Other floral resources presented ~26C +i",    "15 minutes",     "Stenocarpus sinuatus",   "JB Dorey",       "Keybase", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",                            10L,          "Dorey", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",
      "preservedSpecimen",             NA,   "18JDEC460", "JBD_Site365", "Colletidae", "Hylaeinae", "Pharohylaeus",     "lactiferus", "Pharohylaeus lactiferus",    "JB Dorey",    "2M",               2L,        -17.26761,         145.49474,               864L,            "QLD", "Hallorans Hill",     "5/2/19",    "14:30",  "JB Dorey", "Australia",                                                                                                                                           "General sweep off Stenocarpus sinuatus 6-7 m up in the canopy of rainforest (probably regenerated around a remnant core). Overcast. Other floral resources presented  ~26C +i",    "17 minutes",     "Stenocarpus sinuatus",   "JB Dorey",       "Keybase", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",                            10L,          "Dorey", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",
      "preservedSpecimen",             NA, "18JDEC497-A", "JBD_Site365", "Colletidae", "Hylaeinae", "Pharohylaeus",     "lactiferus", "Pharohylaeus lactiferus",    "JB Dorey",     "M",               1L,        -17.26761,         145.49474,               864L,            "QLD", "Hallorans Hill",     "3/2/19",    "11:01",  "JB Dorey", "Australia",              "General/sight swept off Stenocarpus sinuatus. Males of Pharohylaeus frequently seen patrolling flowers ~slowly flying around each flower in what appeared to be a circuit. They would sometimes alight on leaves. No females were seen (or were at least caught) despite male activity. Hot and sunny ~30C",    "52 minutes",     "Stenocarpus sinuatus",   "JB Dorey",       "Keybase", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",                            10L,          "Dorey", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",
      "preservedSpecimen",             NA,    "19JDEC60", "JBD_Site380", "Colletidae", "Hylaeinae", "Pharohylaeus",     "lactiferus", "Pharohylaeus lactiferus",    "JB Dorey", "4M 1F",               5L,        -16.81119,         145.63464,               351L,            "QLD",        "Kuranda", "20/11/2019",    "11:53",  "JB Dorey", "Australia",                                                                                                                                                  "Tall flame tree (~13 m high) on street surrounded by rainforest. Several females and males observed. Bees seen almost immediately upon arrival. Sunny and warm ~30C +i",    "14 minutes", "Brachychiton acerifolius",   "JB Dorey",       "Keybase", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",                            10L,          "Dorey", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",
      "preservedSpecimen",             NA,    "19JDEC61", "JBD_Site365", "Colletidae", "Hylaeinae", "Pharohylaeus",     "lactiferus", "Pharohylaeus lactiferus",    "JB Dorey", "1M 2F",               3L,        -17.26804,         145.49175,               853L,            "QLD",       "Atherton", "21/11/2019",    "10:30",  "JB Dorey", "Australia", "General sweep of flame tree near rainforest, but on a street. Males and females seen in flowers, but Hylaeinae have pollen covering their backs. Sunny and hot ~27C +i. Additionally: Lasioglossum sp. female, Tetragonula female, Meroglossa itamuca males and females, 3 Hylaeus (Gnathoprosopis) albonitens females.",    "14 minutes", "Brachychiton acerifolius",   "JB Dorey",       "Keybase", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180.",                            10L,          "Dorey", "DOREY, J. B. 2021. Missing for almost 100 years: the rare and potentially threatened bee Pharohylaeus lactiferus (Hymenoptera, Colltidae). Journal of Hymenoptera Research, 81, 165-180."
    )
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "Dor",
                                 path = tempdir(),
                                 inFile = "/testData.csv",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_Dor results columns TRUE", {
      testthat::expect_equal(resultsT, 34)
    })
    
    testthat::test_that("readr_Dor results columns FALSE", {
      testthat::expect_equal(resultsF, 4)
    })
    
    testthat::test_that("readr_Dor expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    
    
#### 22.0 MEPB ####
    
    testData <- dplyr::tribble(
      ~id,            ~type, ~institutionID, ~collectionID, ~institutionCode,                              ~collectionCode,          ~datasetName,      ~basisOfRecord,          ~occurrenceID, ~catalogNumber, ~occurrenceRemarks, ~recordedBy, ~organismQuantity, ~organismQuantityType, ~sex, ~lifeStage, ~preparations, ~associatedTaxa, ~previousIdentifications, ~samplingProtocol, ~eventDate, ~year, ~month, ~day, ~verbatimEventDate,            ~habitat, ~continent,   ~country, ~countryCode, ~stateProvince,          ~county,                                        ~locality,                    ~verbatimLocality, ~verbatimElevation, ~minimumElevationInMeters, ~maximumElevationInMeters, ~verbatimCoordinates, ~verbatimLatitude, ~verbatimLongitude, ~decimalLatitude, ~decimalLongitude, ~geodeticDatum, ~coordinateUncertaintyInMeters,               ~georeferencedBy, ~georeferencedDate,                                                                                                                                                                                                                                                                                                                                                                              ~georeferenceProtocol,                                                                                                                                                                                                                                                                                                                                    ~georeferenceSources,                                                                                                                                                                                 ~georeferenceRemarks, ~identifiedBy, ~dateIdentified, ~identificationRemarks, ~identificationQualifier, ~scientificName,   ~kingdom,      ~phylum,    ~class,        ~order,  ~family,     ~genus, ~subgenus, ~specificEpithet, ~infraspecificEpithet, ~taxonRank, ~verbatimTaxonRank, ~scientificNameAuthorship, ~taxonomicStatus,
      "COMFENALCO:MEPB:955", "Objeto fÃ.sico",  "890900842-6",     "RNC:147",     "COMFENALCO", "Museo EntomolÃ³gico Piedras Blancas (MEPB)", "ColecciÃ³n INDERENA", "PreservedSpecimen",  "COMFENALCO:MEPB:955",           955L,                 NA, "P Cardona",                1L,          "Individuos",   NA,   "Adulto",        "Seco",              NA,                 "Apidae",                NA,     "2002", 2002L,     NA,   NA,             "2002",          "Hotel***",       "SA", "Colombia",         "CO",     "QuindÃ.o",       "Quimbaya",           "Vereda Kerman, Km 7 al Parque Panaca", "Vereda Kerman, Km 7, Parque Panaca",      "1339 - 1339",                     1182L,                     1182L,                   NA,                NA,                 NA,          4.60781,         -75.82079,        "WGS84",                            671, "Daniel Suarez (SiB Colombia)",           "4/1/17", "Escobar D, Jojoa LM, DÃ.az SR, Rudas E, AlbarracÃ.n RD, RamÃ.rez C, GÃ³mez JY, LÃ³pez CR, Saavedra J, Ortiz R, (2016). GeorreferenciaciÃ³n de localidades: Una guÃ.a de referencia para colecciones biolÃ³gicas. Instituto de InvestigaciÃ³n de Recursos BiolÃ³gicos Alexander von Humboldt â€“ Instituto de Ciencias Naturales, Universidad Nacional de Colombia. BogotÃ¡ D.C., Colombia. 144 p", "Plancha 224 del Instituto GeogrÃ¡fico AgustÃ.n Codazzi - IGAC. Base cartogrÃ¡fica oficial integrada.Escala 1:100.000. CodificaciÃ³n de la divisiÃ³n polÃ.tico administrativa de Colombia(Divipola) - DANE. NASA Land Processes Distributed Active Archive Center (LP DAAC). ASTER Global DEM. EOSDIS/Reverb ECHO. https://reverb.echo.nasa.gov. 2011.",                                         "Nivel 2. Se valida la localidad. Se valida la geografÃ.a superior. No reporta coordenadas. Incertidumbre por extensiÃ³n de la localidad y escala del mapa.",            NA,              NA,                     NA,                       NA,        "Apidae", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",         NA,        NA,               NA,                    NA,  "Familia",              "sp.",                        NA,               NA,
      "COMFENALCO:MEPB:7534", "Objeto fÃ.sico",  "890900842-6",     "RNC:147",     "COMFENALCO", "Museo EntomolÃ³gico Piedras Blancas (MEPB)",           "DonaciÃ³n", "PreservedSpecimen", "COMFENALCO:MEPB:7534",          7534L,                 NA, "P Cardona",                1L,          "Individuos",   NA,   "Adulto",        "Seco",              NA,                 "Apidae",  "Captura manual",     "2002", 2002L,     NA,   NA,             "2002", "Intradomicilio***",       "SA", "Colombia",         "CO",     "QuindÃ.o",       "Quimbaya", "Parque Panaca, Km 7 de la vereda Kerman, hotel",  "Panaca, Km 7 Vereda Kerman, Hotel",                 NA,                     1182L,                     1182L,                   NA,                NA,                 NA,          4.60948,          -75.8252,        "WGS84",                            296, "Daniel Suarez (SiB Colombia)",           "4/1/17", "Escobar D, Jojoa LM, DÃ.az SR, Rudas E, AlbarracÃ.n RD, RamÃ.rez C, GÃ³mez JY, LÃ³pez CR, Saavedra J, Ortiz R, (2016). GeorreferenciaciÃ³n de localidades: Una guÃ.a de referencia para colecciones biolÃ³gicas. Instituto de InvestigaciÃ³n de Recursos BiolÃ³gicos Alexander von Humboldt â€“ Instituto de Ciencias Naturales, Universidad Nacional de Colombia. BogotÃ¡ D.C., Colombia. 144 p", "Plancha 224 del Instituto GeogrÃ¡fico AgustÃ.n Codazzi - IGAC. Base cartogrÃ¡fica oficial integrada.Escala 1:100.000. CodificaciÃ³n de la divisiÃ³n polÃ.tico administrativa de Colombia(Divipola) - DANE. NASA Land Processes Distributed Active Archive Center (LP DAAC). ASTER Global DEM. EOSDIS/Reverb ECHO. https://reverb.echo.nasa.gov. 2011.",                                         "Nivel 2. Se valida la localidad. Se valida la geografÃ.a superior. No reporta coordenadas. Incertidumbre por extensiÃ³n de la localidad y escala del mapa.",            NA,              NA,                     NA,                       NA,        "Apidae", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",         NA,        NA,               NA,                    NA,  "Familia",              "sp.",                        NA,               NA,
      "COMFENALCO:MEPB:1175", "Objeto fÃ.sico",  "890900842-6",     "RNC:147",     "COMFENALCO", "Museo EntomolÃ³gico Piedras Blancas (MEPB)", "ColecciÃ³n INDERENA", "PreservedSpecimen", "COMFENALCO:MEPB:1175",          1175L,                 NA,          NA,                1L,          "Individuos",   NA,   "Adulto",        "Seco",              NA,               "Xylocopa",                NA,   "7/1/83", 1983L,     7L,   1L,           "7/1/83",                  NA,       "SA", "Colombia",         "CO",    "Antioquia",          "Urrao",                                      "Sin Datos",                                   NA,                 NA,                     2250L,                     2250L,                   NA,                NA,                 NA,      6.337984698,       -76.2756657,        "WGS84",                      426237168,             "Ricardo Ortiz G.",          "6/12/16", "Escobar D, Jojoa LM, DÃ.az SR, Rudas E, AlbarracÃ.n RD, RamÃ.rez C, GÃ³mez JY, LÃ³pez CR, Saavedra J, Ortiz R, (2016). GeorreferenciaciÃ³n de localidades: Una guÃ.a de referencia para colecciones biolÃ³gicas. Instituto de InvestigaciÃ³n de Recursos BiolÃ³gicos Alexander von Humboldt â€“ Instituto de Ciencias Naturales, Universidad Nacional de Colombia. BogotÃ¡ D.C., Colombia. 144 p",   "Instituto GeogrÃ¡fico AgustÃ.n Codazzi - IGAC. Base cartogrÃ¡fica oficial integrada. Plancha 145. Escala 1:100.000. CodificaciÃ³n de la divisiÃ³n polÃ.tico administrativa de Colombia(Divipola) - DANE. NASA Land Processes Distributed Active Archive Center (LP DAAC). ASTER Global DEM. EOSDIS/Reverb ECHO. https://reverb.echo.nasa.gov. 2011.", "Nivel 4. Georreferenciado en el centroide del municipio. La incertidumbre corresponde a la distancia desde el centroide hasta el lÃ.mite mas alejado del municipio mas la incertidumbre por escala",            NA,              NA,                     NA,                       NA,      "Xylocopa", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Xylocopa",        NA,               NA,                    NA,  "GÃ©nero",              "sp.",         "Latreille, 1802",       "Aceptado",
      "COMFENALCO:MEPB:7531", "Objeto fÃ.sico",  "890900842-6",     "RNC:147",     "COMFENALCO", "Museo EntomolÃ³gico Piedras Blancas (MEPB)",           "DonaciÃ³n", "PreservedSpecimen", "COMFENALCO:MEPB:7531",          7531L,                 NA, "A Acevedo",                1L,          "Individuos",   NA,   "Adulto",        "Seco",              NA,                 "Apidae",            "Jama",   "3/1/84", 1984L,     3L,   1L,           "3/1/84",                  NA,       "SA", "Colombia",         "CO",    "Antioquia", "Puerto Triunfo",                                      "Sin Datos",                                   NA,                 NA,                      167L,                      167L,                   NA,                NA,                 NA,      5.953695157,      -74.68633006,        "WGS84",                      180120452,             "Ricardo Ortiz G.",          "6/12/16", "Escobar D, Jojoa LM, DÃ.az SR, Rudas E, AlbarracÃ.n RD, RamÃ.rez C, GÃ³mez JY, LÃ³pez CR, Saavedra J, Ortiz R, (2016). GeorreferenciaciÃ³n de localidades: Una guÃ.a de referencia para colecciones biolÃ³gicas. Instituto de InvestigaciÃ³n de Recursos BiolÃ³gicos Alexander von Humboldt â€“ Instituto de Ciencias Naturales, Universidad Nacional de Colombia. BogotÃ¡ D.C., Colombia. 144 p",   "Instituto GeogrÃ¡fico AgustÃ.n Codazzi - IGAC. Base cartogrÃ¡fica oficial integrada. Plancha 168. Escala 1:100.000. CodificaciÃ³n de la divisiÃ³n polÃ.tico administrativa de Colombia(Divipola) - DANE. NASA Land Processes Distributed Active Archive Center (LP DAAC). ASTER Global DEM. EOSDIS/Reverb ECHO. https://reverb.echo.nasa.gov. 2011.", "Nivel 4. Georreferenciado en el centroide del municipio. La incertidumbre corresponde a la distancia desde el centroide hasta el lÃ.mite mas alejado del municipio mas la incertidumbre por escala",            NA,              NA,                     NA,                       NA,        "Apidae", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",         NA,        NA,               NA,                    NA,  "Familia",              "sp.",                        NA,               NA
    ) %>%
      dplyr::mutate(year = year %>% as.double(),
                    month = month %>% as.double(),
                    day = day %>% as.double())
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                         sheetName = "PiedrasBalncas Bees Data")
    
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "MEPB",
                                   path = tempdir(),
                                  inFile = "testData.xlsx",
                                  outFile = "testDataOut.csv",
                                  sheet = "PiedrasBalncas Bees Data",
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_MEPB results columns TRUE", {
      testthat::expect_equal(resultsT, 46)
    })
    
    testthat::test_that("readr_MEPB results columns FALSE", {
      testthat::expect_equal(resultsF, 24)
    })
    
    testthat::test_that("readr_MEPB expected class", {
      testthat::expect_type(testOut1, "list")
    })
    




#### 23.0 Brazil ####
    
    testData <- dplyr::tribble(
      ~CodeBBdatabase_curated, ~'Scientific name corrected', ~Native.to.Brazil, ~Family_Proposed.in.Moure.Catalogue,      ~Family,   ~SubFamily,        ~Tribe,            ~Codigo, ~institutioncode, ~Day, ~Month, ~Year, ~Date_precision, ~Country,           ~State, ~Latitude_dec.degrees, ~Longitude_dec.degrees, ~Precision.of.coord.meters, ~NotasLatLong, ~NotesOnLocality,                     ~Locality.original, ~Spcslink.county, ~Spcslink.continentocean,               ~Collector, ~Collection,                                                                                                                                                                                     ~Source,         ~Sex,                                                                                                                                                                                    ~Project,        ~Det_By, ~Spcslink.linha_numero, ~Spcslink.datelastmodified, ~Spcslink.collectioncode, ~Spcslink.basisofrecord, ~Spcslink.identifiedby, ~Spcslink.yearidentified, ~Spcslink.barcode,
      "BBD_498851",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,               NA,   NA,     NA,    NA,              NA, "Brazil", "Santa Catarina",          -27.14783261,           -52.34069428,    "municipality centroid",            NA,               NA,                                     NA,               NA,                       NA,  "Moure's Bee Catalogue",          NA,                                                                                                                                                                                          NA,           NA,                                                                                                                                                                                          NA,             NA,                     NA,                         NA,                       NA,                      NA,                     NA,                       NA,                NA,
      "BBD_00082426",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,             "KU",   NA,    11L, 1939L,              NA, "Brazil",      "Sao Paulo",             -23.53377,              -46.69189,                         NA,            NA,               NA,                            "Sao Paulo",               NA,            "Neotropical",                "unknown",   "KU-SEMC",                                                                                                                                                             "SpeciesLink 22/12/2020 17:23h",           NA,                                                                                                                                                                                          NA,             NA,                270288L,                40933.39514,                 100623465863.0,     "PreservedSpecimen",           "Ruz, Luisa",                       NA,                NA,
      "BBD_00082419",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,             "KU",  26L,     2L, 1954L,              NA, "Brazil", "Santa Catarina",           -27.1833333,            -52.3833333,                         NA,            NA,               NA,                        "Nova Teutonia",               NA,            "Neotropical",        "Plaumann, Fritz",   "KU-SEMC",                                                                                                                                                             "SpeciesLink 22/12/2020 17:23h",           NA,                                                                                                                                                                                          NA,             NA,                270340L,                40933.39375,                 1006265758.0,     "PreservedSpecimen",                     NA,                       NA,                NA,
      "BBD_00043536",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini", "AMNHBEE 00028690",               NA,   1L,     1L, 1970L,              NA, "BRAZIL",   "Minas Gerais",             -20.13333,                  -43.5,                         NA,            NA,               NA, "Serra do Caraca (Saof Santa Barbara)",               NA,                       NA,         "F. M. Oliveira",      "AMNH", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "Adult Male", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "J. S. Ascher",                     NA,                         NA,                       NA,                      NA,                     NA,                       NA,                NA,
      "BBD_00082342",      "Secret species",             "yes",                            "Apidae", "Andrenidae", "Andreninae", "Calliopsini",                 NA,             "KU",   NA,    11L, 1951L,              NA, "Brazil", "Santa Catarina",           -27.1833333,            -52.3833333,                         NA,            NA,               NA,                        "Nova Teutonia",               NA,            "Neotropical",            "Plaumann, L",   "KU-SEMC",                                                                                                                                                             "SpeciesLink 22/12/2020 17:23h",           NA,                                                                                                                                                                                          NA,             NA,                270388L,                40933.39375,                 100245624566694.0,     "PreservedSpecimen",                     NA,                       NA,                NA
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "BBD",
                                   path = tempdir(),
                                 inFile = "testData.csv",
                                 outFile = "testDataOut.csv",
                                 dataLicense = "All rights reserved")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
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
    
    




#### 24.0 MPUJ ####
    
    testData <- dplyr::tribble(
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
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "MPUJ",
                                   path = tempdir(),
                                  inFile = "testData.xlsx",
                                  outFile = "testDataOut.csv",
                                  sheet = "Sheet1", 
                                  dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (ColTypeR()[[1]] %>% names()) == FALSE)
    
    
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
    



#### 25.0 STRI ####
    
    testData <- dplyr::tribble(
      ~Catalognumber, ~institutionCode, ~collectionCode, ~ownerInstitutionCode,                          ~collectionID,      ~basisOfRecord,                          ~occurrenceID, ~catalogNumber, ~otherCatalogNumbers,                                                                                           ~higherClassification,   ~kingdom,      ~phylum,    ~class,        ~order,  ~family,       ~scientificName, ~taxonID, ~scientificNameAuthorship,     ~genus, ~subgenus, ~specificEpithet, ~verbatimTaxonRank, ~infraspecificEpithet, ~taxonRank,  ~identifiedBy, ~dateIdentified, ~identificationReferences, ~identificationRemarks, ~taxonRemarks, ~identificationQualifier, ~typeStatus,      ~recordedBy, ~recordNumber, ~eventDate, ~year, ~month, ~day, ~startDayOfYear, ~endDayOfYear, ~verbatimEventDate,                                                                                                ~occurrenceRemarks, ~habitat, ~fieldNumber, ~eventID, ~informationWithheld, ~dataGeneralizations, ~dynamicProperties, ~associatedOccurrences, ~associatedSequences, ~associatedTaxa, ~reproductiveCondition, ~establishmentMeans, ~lifeStage, ~sex, ~individualCount, ~preparations, ~locationID, ~continent, ~waterBody, ~islandGroup, ~island,        ~country,     ~stateProvince, ~county, ~municipality,        ~locality, ~locationRemarks, ~decimalLatitude, ~decimalLongitude, ~geodeticDatum, ~coordinateUncertaintyInMeters,  ~verbatimCoordinates, ~georeferencedBy, ~georeferenceProtocol, ~georeferenceSources, ~georeferenceVerificationStatus, ~georeferenceRemarks, ~minimumElevationInMeters, ~maximumElevationInMeters, ~minimumDepthInMeters, ~maximumDepthInMeters, ~verbatimDepth, ~verbatimElevation, ~disposition, ~language, ~recordEnteredBy,      ~modified,                                             ~rights, ~rightsHolder, ~accessRights,                              ~recordID,                                                                  ~references,
      911208L,      "Field-Obs",    "Entomology",                    NA, "6e16d724-0be0-4101-a230-b841ebd1f0a5", "PreservedSpecimen", "48739ca8-e401-43c2-b52a-137ad191d726",             NA,                   NA, "Organism|Animalia|Arthropoda|Insecta|Pterygota|Hymenoptera|Apocrita|Apoidea|Apidae|Apinae|Euglossini|Euglossa", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae", "Euglossa allosticta",   48972L,             "Moure, 1969", "Euglossa",        NA,     "allosticta",                 NA,                    NA,  "Species", "David Roubik",              NA,                        NA,                     NA,            NA,                       NA,          NA,   "David Roubik",            NA,   "2/2/06", 2006L,     2L,   2L,             33L,            NA,                 NA,                                   "Euglossa allosticta; Euglossa allosticta, Face. Taken with Automontage system",       NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,         NA,   NA,               NA,            NA,          NA,         NA,         NA,           NA,      NA, "French Guiana",                 NA,      NA,            NA,               NA,               NA,               NA,                NA,             NA,                             NA,                    NA,               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "1/8/15 11:31", "http://creativecommons.org/publicdomain/zero/1.0/",            NA,            NA, "62e76208-6975-43ae-b805-5f02766fc9ce", "https://panamabiota.org/stri/collections/individual/index.php?occid=911208",
      911091L,      "Field-Obs",    "Entomology",                    NA, "6e16d724-0be0-4101-a230-b841ebd1f0a5", "PreservedSpecimen", "a4e323b1-f79d-4826-86ba-26c00c7ba7ea",             NA,                   NA,  "Organism|Animalia|Arthropoda|Insecta|Pterygota|Hymenoptera|Apocrita|Apoidea|Apidae|Apinae|Meliponini|Trigona", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",    "Trigona amalthea",   55214L,         "(Olivier, 1789)",  "Trigona",        NA,       "amalthea",                 NA,                    NA,  "Species",             NA,              NA,                        NA,                     NA,            NA,                       NA,          NA, "Miriam Arrueta",            NA,         NA,    NA,     NA,   NA,              NA,            NA,                 NA,  "Orchid Bee: Trigona amalthea; David Roubik Cerro Brewster Orchid Bee collection: Trigona amalthea, Dorsal view",       NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,         NA,   NA,               NA,            NA,          NA,         NA,         NA,           NA,      NA,        "PANAMA",         "SAN BLAS",      NA,            NA, "CERRO BREWSTER",               NA,         9.370558,        -79.247071,             NA,                             NA, "17 692500E 1036300N",               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "1/8/15 11:31", "http://creativecommons.org/publicdomain/zero/1.0/",            NA,            NA, "2c2a5b65-de7d-4ceb-9445-ada9fcfbb7ad", "https://panamabiota.org/stri/collections/individual/index.php?occid=911091",
      911092L,      "Field-Obs",    "Entomology",                    NA, "6e16d724-0be0-4101-a230-b841ebd1f0a5", "PreservedSpecimen", "9e7a88c2-09be-4b9c-b59b-f458ea424800",             NA,                   NA,  "Organism|Animalia|Arthropoda|Insecta|Pterygota|Hymenoptera|Apocrita|Apoidea|Apidae|Apinae|Meliponini|Trigona", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Apidae",    "Trigona amalthea",   55214L,         "(Olivier, 1789)",  "Trigona",        NA,       "amalthea",                 NA,                    NA,  "Species",             NA,              NA,                        NA,                     NA,            NA,                       NA,          NA, "Miriam Arrueta",            NA,         NA,    NA,     NA,   NA,              NA,            NA,                 NA, "Orchid Bee: Trigona amalthea; David Roubik Cerro Brewster Orchid Bee collection: Trigona amalthea, Oblique view",       NA,           NA,       NA,                   NA,                   NA,                 NA,                     NA,                   NA,              NA,                     NA,                  NA,         NA,   NA,               NA,            NA,          NA,         NA,         NA,           NA,      NA,        "PANAMA", "SAN BLAS (KUNA Y",      NA,            NA, "CERRO BREWSTER",               NA,         9.370558,        -79.247071,             NA,                             NA, "17 692500E 1036300N",               NA,                    NA,                   NA,                              NA,                   NA,                        NA,                        NA,                    NA,                    NA,             NA,                 NA,           NA,        NA,               NA, "1/8/15 11:31", "http://creativecommons.org/publicdomain/zero/1.0/",            NA,            NA, "29b950c2-4d7b-423b-aba1-ea96ccba523b", "https://panamabiota.org/stri/collections/individual/index.php?occid=911092"
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "STRI",
                                   path = tempdir(),
                                   inFile = "testData.csv",
                                   outFile = "testDataOut.csv",
                                   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_STRI results columns TRUE", {
      testthat::expect_equal(resultsT, 60)
    })
    testthat::test_that("readr_STRI results columns FALSE", {
      testthat::expect_equal(resultsF, 39)
    })
    
    testthat::test_that("readr_STRI expected class", {
      testthat::expect_type(testOut1, "list")
    })
    



#### 26.0 PALA ####
    
    testData <- dplyr::tribble(
      ~Type,          ~Country, ~Muninciplaity, ~Gender,                                    ~Site,   ~Latitud, ~Longitude,     ~elevation,          ~date,               ~recordedby, ~catalogNumber, ~Collection, ~othercatalognumber, ~AssociatedTaxa,  ~taxonremarks,      ~Family,         ~Genus,      ~species, ~Citation,
      NA,                NA,   "Tamaulipas",    "1?", "10 km W of Antiguo Morelos, Highway 80",   "22.555",  "-99.167", "elev. 470 m.",  "6 Jul. 1990",         "Ilan Yarom leg.", "PALASEMC0026",      "SEMC",                  NA,              NA, "Lyucatanense", "Halictidae", "Lasioglossum", "yucatanense",        NA,
      NA,        "Campeche",             NA,    "1?",              "10 mi. north of HopelchÈn",    "19.89",   "-89.84",             NA, "17 Apr. 1962",       "L. A. Stange leg.",  "PALAUCD0001",      "UCDC",                  NA,              NA,     "Lpaxtoni", "Halictidae", "Lasioglossum",     "paxtoni",        NA,
      NA, "San Luis Potosi",             NA,    "1?",                 "12 km NW of El Naranjo", "22.59∞ N", "99.38∞ W", "elev. 400 m.",  "5 Jul. 1990", "Robert L. Minckley leg.", "PALASEMC0009",      "SEMC",                  NA,              NA, "Lyucatanense", "Halictidae", "Lasioglossum", "yucatanense",        NA
    )
    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.csv", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    readr::write_excel_csv(testData, paste0(tempdir(), "/testData.csv"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "PALA",
                                   path = tempdir(),
                                   inFile = "testData.csv",
                                   outFile = "testDataOut.csv",
                                   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == FALSE)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_PALA results columns TRUE", {
      testthat::expect_equal(resultsT, 26)
    })
    testthat::test_that("readr_PALA results columns FALSE", {
      testthat::expect_equal(resultsF, 3)
    })
    
    testthat::test_that("readr_PALA expected class", {
      testthat::expect_type(testOut1, "list")
    })
    
    



#### 27.0 JoLa ####
    
    testData1 <- dplyr::tribble(
      ~Species,    ~Latitude1,     ~Longitude1, ~`Start.Date.(Year)`,
      "angelicum",         33.62,         -117.93,                1920L,
      "angelicum",         33.97,         -117.32,                1924L,
      "angelicum",         33.97,         -117.32,                1925L
    )
    
    testData2 <- dplyr::tribble(
      ~Species,    ~Latitude1,     ~Longitude1, ~`Start.Date.(Year)`,
      "angelicum",         33.97,         -118.44,                1981L,
      "angelicum",         34.15,         -118.14,                2000L,
      "angelicum",         34.15,         -118.14,                2000L
    )
    
    
    testData <- list(testData1, testData2)
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                         sheetName = c("pre-1950", "post-1950"))
    
    testOut1 <- BeeBDC::readr_BeeBDC(dataset = "JoLa",
                                   path = tempdir(),
                                   inFile = "testData.xlsx",
                                   outFile = "testDataOut.csv",
                                   sheet = c("pre-1950", "post-1950"),
                                   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == FALSE)
    rows <- nrow(testData1) + nrow(testData2)
    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_JoLa results columns TRUE", {
      testthat::expect_equal(resultsT, 13)
    })
    testthat::test_that("readr_JoLa results columns FALSE", {
      testthat::expect_equal(resultsF, 1)
    })
    testthat::test_that("readr_JoLa correct number of rows", {
      testthat::expect_equal(nrow(testOut1), rows)
    })
    
    testthat::test_that("readr_JoLa expected class", {
      testthat::expect_type(testOut1, "list")
    })
    


    
    #### 28.0 VicWam ####
      # Make the test data
    testData <- dplyr::tribble(
                   ~REGNO, ~Reg_DoreyExtension, ~OLDNO, ~COLLNUM, ~INSTITUTE, ~CATBY,      ~DATENT, ~CHECKEDBY, ~DATMOD, ~on_loan, ~LOAN_NO, ~cabinet, ~drawer,    ~CLASS,        ~ORDER, ~SUPERFAMILY,      ~FAMILY, ~SUBFAMILY, ~TRIBE, ~GENUS, ~SUBGENUS, ~SPECIES, ~SUBSPECIES, ~DTMNDBY, ~DTMNDDT, ~IDMETHOD, ~LIFEHISTORY,              ~SEX, ~NAMEQUALIFIER, ~SPCMTYPE, ~designator, ~DESIGNATORYEAR, ~SPECNUM, ~MULTIPLEMOUNTS, ~PREVIOUSID, ~verbiage,    ~country,            ~STATE, ~ISLANDGR, ~ISLAND,                 ~SITE,       ~NEAREST, ~DISTANCE, ~DIST_UNIT, ~DIRECTION_TX, ~DIRECTION, ~LATITUDE, ~LONGITUDE,    ~LATDEC,    ~LONGDEC,  ~PLACEDERIVATION,                        ~PLACEACCURACY, ~ALTITUDEM, ~ALTITUDEF, ~ALTITUDEDERIVATION, ~ALTITUDEACCURACY,        ~COLLTOR, ~COLLMETH,        ~DTFR, ~DTTO, ~STORAGE, ~quarantine, ~QUARANTINEINFO, ~reared, ~REARINGINFO, ~MICROHABITAT, ~MACROHABITAT, ~HOSTCOMMON,     ~LABELFAMILY,  ~LABELGENUS, ~LABELSPECIES, ~CENSUSGENUS,         ~CENSUSSPECIES,          ~REMARKS, ~voucher, ~VOUCHERINFO,                                                                                         ~NOTES, ~JD_Genus,            ~JD_Species,      ~JD_Genus_Species,             ~JD_PlantGS,     ~JD_Family, ~Bee_GS_asALA, ~JD_PlantGenus_APNI,
                   23798L,         "WAM 23798",     NA,       NA,      "WAM",  "BPH", "07/07/2010",         NA,      NA,       NA,       NA,       NA,      NA, "Insecta", "Hymenoptera",    "Apoidea", "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA, "curated",      "adult",                NA,             NA,        NA,          NA,              NA,       1L,              NA,          NA,        NA, "AUSTRALIA",              "SA",        NA,      NA,                    NA,      "Hartley",        4L,       "mi",        "East",         90, "35°12`S", "139°05`E",    "-35.2",  "139.0833", "AUSLIG Database",                  "within 2 km radius",         NA,         NA,                  NA,                NA, "Houston, T.F.",        NA, "10/01/1971",    NA, "pinned",          NA,              NA,      NA,           NA,            NA,            NA,          NA,               NA, "Eucalyptus",            NA,           NA,                     NA,         "on host",       NA,           NA,                                              "donation from Elizabeth Exley (UQ) 21 June 2004",        NA,                     NA,                     NA,            "Eucalyptus",    "Myrtaceae",            NA,                  NA,
                   24021L,         "WAM 24021",     NA,       NA,      "WAM",  "BPH", "08/07/2010",         NA,      NA,       NA,       NA,       NA,      NA, "Insecta", "Hymenoptera",    "Apoidea", "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA, "curated",      "adult", "unsexed/unknown",             NA,        NA,          NA,              NA,       1L,              NA,          NA,        NA, "AUSTRALIA",              "SA",        NA,      NA,                    NA,      "Minnipa",       48L,       "km",         "NNE",       22.5, "32°25`S", "135°09`E", "-32.4166",    "135.15", "AUSLIG Database", "within 1 km radius (nearest minute)",         NA,         NA,                  NA,                NA, "Houston, T.F.",        NA, "18/10/1973",    NA, "pinned",          NA,              NA,      NA,           NA,            NA,            NA,          NA,               NA, "Eucalyptus",            NA,           NA,                     NA,         "on host",       NA,           NA,                                              "donation from Elizabeth Exley (UQ) 21 June 2004",        NA,                     NA,                     NA,            "Eucalyptus",    "Myrtaceae",            NA,                  NA,
                   24034L,         "WAM 24034",     NA,       NA,      "WAM",  "BPH", "08/07/2010",         NA,      NA,       NA,       NA,       NA,      NA, "Insecta", "Hymenoptera",    "Apoidea", "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA, "curated",      "adult",                NA,             NA,        NA,          NA,              NA,       1L,              NA,          NA,        NA, "AUSTRALIA",              "SA",        NA,      NA,                    NA,      "Hartley",        4L,       "mi",        "East",         90, "35°12`S", "139°05`E",    "-35.2",  "139.0833", "AUSLIG Database",                  "within 2 km radius",         NA,         NA,                  NA,                NA, "Houston, T.F.",        NA, "10/01/1971",    NA, "pinned",          NA,              NA,      NA,           NA,            NA,            NA,          NA,               NA, "Eucalyptus",            NA,           NA,                     NA,         "on host",       NA,           NA,                                              "donation from Elizabeth Exley (UQ) 21 June 2004",        NA,                     NA,                     NA,            "Eucalyptus",    "Myrtaceae",            NA,                  NA,
                   82259L,         "WAM 82259",     NA,       NA,      "WAM",  "BPH", "08/07/2010",         NA,      NA,       NA,       NA,       NA,      NA, "Insecta", "Hymenoptera",    "Apoidea", "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA, "curated",      "adult",                NA,             NA,        NA,          NA,              NA,       1L,              NA,          NA,        NA, "AUSTRALIA",              "SA",        NA,      NA,                    NA,  "Oodla Wirra",        6L,       "mi",          "NE",         45, "32°49`S", "139°08`E", "-32.8166",  "139.1333", "AUSLIG Database",                  "within 2 km radius",         NA,         NA,                  NA,                NA, "Houston, T.F.",        NA, "17/12/1969",    NA, "pinned",          NA,              NA,      NA,           NA,            NA,            NA,          NA,               NA, "Eucalyptus",            NA,           NA,                     NA,         "on host",       NA,           NA, "donation from Elizabeth Exley (UQ) 21 June 2004; 82259 to 82260 double-mounted onto same pin",        NA,                     NA,                     NA,            "Eucalyptus",    "Myrtaceae",            NA,                  NA,
                   82260L,         "WAM 82260",     NA,       NA,      "WAM",  "BPH", "08/07/2010",         NA,      NA,       NA,       NA,       NA,      NA, "Insecta", "Hymenoptera",    "Apoidea", "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA, "curated",      "adult",                NA,             NA,        NA,          NA,              NA,       1L,              NA,          NA,        NA, "AUSTRALIA",              "SA",        NA,      NA,                    NA,  "Oodla Wirra",        6L,       "mi",          "NE",         45, "32°49`S", "139°08`E", "-32.8166",  "139.1333", "AUSLIG Database",                  "within 2 km radius",         NA,         NA,                  NA,                NA, "Houston, T.F.",        NA, "17/12/1969",    NA, "pinned",          NA,              NA,      NA,           NA,            NA,            NA,          NA,               NA, "Eucalyptus",            NA,           NA,                     NA,         "on host",       NA,           NA, "donation from Elizabeth Exley (UQ) 21 June 2004; 82259 to 82260 double-mounted onto same pin",        NA,                     NA,                     NA,            "Eucalyptus",    "Myrtaceae",            NA,                  NA,
                   82261L,         "WAM 82261",     NA,       NA,      "WAM",  "BPH", "08/07/2010",         NA,      NA,       NA,       NA,       NA,      NA, "Insecta", "Hymenoptera",    "Apoidea", "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA, "curated",      "adult",                NA,             NA,        NA,          NA,              NA,       1L,              NA,          NA,        NA, "AUSTRALIA",              "SA",        NA,      NA,                    NA,  "Oodla Wirra",        6L,       "mi",          "NE",         45, "32°49`S", "139°08`E", "-32.8166",  "139.1333", "AUSLIG Database",                  "within 2 km radius",         NA,         NA,                  NA,                NA, "Houston, T.F.",        NA, "17/12/1969",    NA, "pinned",          NA,              NA,      NA,           NA,            NA,            NA,          NA,               NA, "Eucalyptus",            NA,           NA,                     NA,         "on host",       NA,           NA,                                              "donation from Elizabeth Exley (UQ) 21 June 2004",        NA,                     NA,                     NA,            "Eucalyptus",    "Myrtaceae",            NA,                  NA,
                   41780L,         "HYM 41780",     NA,       NA,      "Vic",  "HYM",           NA,         NA,      NA,       NA,       NA,       NA,      NA,        NA, "Hymenoptera",           NA, "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA,        NA,           NA,         "Unknown",             NA,        NA,          NA,              NA,       NA,              NA,          NA,        NA,          NA, "South Australia",        NA,      NA, "1 km S Port Germein", "Port Germein",        NA,         NA,            NA,         NA,        NA,         NA, "33 05  S", "138 00  E",                NA,                                    NA,         NA,         NA,                  NA,                NA,   "Walker, K L",        NA, "24/10/1990",    NA,       NA,          NA,              NA,      NA,           NA,            NA,            NA,          NA, "Zygophyllaceae",   "Nitraria", "billardieri",           NA, "Nitraria billardieri", "EntoSex = adult",       NA,           NA,                                                                                             NA,        NA, "Nitraria billardieri", "Nitraria billardieri", "Nitraria billardierei", "Nitrariaceae",            NA,          "Nitraria",
                   41774L,         "HYM 41774",     NA,       NA,      "Vic",  "HYM",           NA,         NA,      NA,       NA,       NA,       NA,      NA,        NA, "Hymenoptera",           NA, "Colletidae",         NA,     NA,     NA,        NA,       NA,          NA,       NA,       NA,        NA,           NA,         "Unknown",             NA,        NA,          NA,              NA,       NA,              NA,          NA,        NA,          NA, "South Australia",        NA,      NA, "1 km S Port Germein", "Port Germein",        NA,         NA,            NA,         NA,        NA,         NA, "33 05  S", "138 00  E",                NA,                                    NA,         NA,         NA,                  NA,                NA,   "Walker, K L",        NA, "24/10/1990",    NA,       NA,          NA,              NA,      NA,           NA,            NA,            NA,          NA, "Zygophyllaceae",   "Nitraria", "billardieri",           NA, "Nitraria billardieri", "EntoSex = adult",       NA,           NA,                                                                                             NA,        NA, "Nitraria billardieri", "Nitraria billardieri", "Nitraria billardierei", "Nitrariaceae",            NA,          "Nitraria"
                   )

    
    
    # Be sure that the testData is not already in tempdir
    testDataPath <- file.info(list.files(tempdir(), full.names = T, 
                                         pattern = "testData.xlsx", recursive = TRUE))
    unlink(rownames(testDataPath))
    
    # Save a temporary version of these data
    openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"),
                         sheetName = "Combined")
    
    suppressWarnings(
    testOut1 <- readr_BeeBDC(dataset = "VicWam",
                                     path = tempdir(),
                                     inFile = "testData.xlsx",
                                     outFile = "testDataOut.csv",
                                     sheet = "Combined",
                                     dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    )
    
    
    # Get a count of TRUE and FALSE column name matches
    resultsT <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == TRUE)
    resultsF <- sum(colnames(testOut1) %in% (BeeBDC::ColTypeR()[[1]] %>% names()) == FALSE)

    
    # Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
    # tibbles are a special case of lists)
    testthat::test_that("readr_VicWam results columns TRUE", {
      testthat::expect_equal(resultsT, 37)
    })
    testthat::test_that("readr_VicWam results columns FALSE", {
      testthat::expect_equal(resultsF, 62)
    })
    testthat::test_that("readr_VicWam correct number of rows", {
      testthat::expect_equal(nrow(testOut1), nrow(testData))
    })
    
    testthat::test_that("readr_VicWam expected class", {
      testthat::expect_type(testOut1, "list")
    })





