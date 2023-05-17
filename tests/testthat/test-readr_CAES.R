requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("xlsx")
requireNamespace("tribble")


testData <- tibble::tribble(
              ~PBIUSI,    ~Genus, ~species, ~Country,   ~State_Prov, ~Sec_Subdiv,                            ~Locality,     ~Lat,      ~Lon, ~Start_Date, ~End_Date,        ~Collector,                ~Sex, ~Inst_Code,                                                                                                                                                                                    ~Project,  ~Type,         ~Det_By, ~Det_Date, ~Det_History,    ~Loc_Notes, ~Accuracy, ~Coll_Method, ~Spec_Notes, ~Host_Family, ~Host_Genus, ~Host_species, ~Pres_Method, ~Spec_Count, ~Lat_Lon_Method, ~Lat_Lon_Accuracy, ~Elev_m, ~Elev_f, ~Elev_Det, ~Trip_Code, ~Dissections, ~Illustrations, ~Measurements, ~Photos, ~SEM, ~DNA, ~OrigUSI,      ~Family,   ~Subfamily,      ~Tribe, ~Macro_Habitat, ~Micro_Habitat, ~Host_subspecies, ~Host_Author, ~Host_Common_Name,    ~Host_Relation, ~Host_Location, ~Host_Emergence_Date, ~Host_with_Specimen,
  "AMNH_BEE 000266235443", "Secret", "species",    "USA", "Connecticut", "Fairfield",                         "New Canaan", 41.14666, -73.49472, 22786,        NA,      "M. Statham",      "Adult Female",     "AMNH", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None",  "J. S. Ascher",     2007L,           NA, "GNIS coord.",        NA,    "Netting",          NA,           NA,          NA,            NA,     "Pinned",          1L,     "Gazetteer",                NA,     97L,    318L, "Unknown",         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO",
  "AMNH_BEE 00026254675", "Secret", "species",    "USA", "Connecticut", "Fairfield",            "Danbury, I-84 at exit 2", 41.39247, -73.52731,  38451,        NA,    "J. S. Ascher",        "Adult Male",     "AMNH", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None",  "J. S. Ascher",     2007L,           NA,            NA,        NA,    "Netting",          NA, "Salicaceae",     "Salix",        "spp.",     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA, "associated with",             NA,                   NA,                "NO",
  "UCMS_ENT 00022347759", "Secret", "species",    "USA", "Connecticut",   "Tolland", "Coventry Twp., near Eagleville Dam", 41.76916, -72.30444, 26414,        NA, "L. R. Schechter", "Adult sex unknown",      "GSC", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None", "W. E. LaBerge",        NA,           NA,            NA,        NA,    "Unknown",          NA,           NA,          NA,            NA,     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO",
  "UCMS_ENT 00023457760", "Secret", "species",    "USA", "Connecticut",   "Tolland",        "Mansfield Twp., Gurleyville", 41.81416,   -72.255, 26044,        NA,     "G. I. Stage", "Adult sex unknown",      "GSC", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None", "W. E. LaBerge",        NA,           NA,            NA,        NA,    "Unknown",          NA,           NA,          NA,            NA,     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO",
  "UCMS_ENT 002345027762", "Secret", "species",    "USA", "Connecticut",   "Tolland", "Coventry Twp., near Eagleville Dam", 41.76916, -72.30444, 26417,        NA,    "J. R. Gordon", "Adult sex unknown",      "GSC", "Data citation: Digital Bee Collections Network, 2014 (and updates). Version: <download date>. National Science Foundation grant DBI 0956388; Contact: Ascher, John S. (dbsajs@nus.edu.sg)", "None", "W. E. LaBerge",        NA,           NA,            NA,        NA,    "Unknown",          NA,           NA,          NA,            NA,     "Pinned",          1L,              NA,                NA,      NA,      NA,        NA,         NA,         "NO",           "NO",          "NO",    "NO", "NO",   NA,       NA, "Andrenidae", "Andreninae", "Andrenini",             NA,             NA,               NA,           NA,                NA,                NA,             NA,                   NA,                "NO"
  )


# Save a temporary version of these data
xlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName="Sheet1")

testOut1 <- BeeDC::readr_CAES(path = paste0(tempdir()),
                             inFile = "/testData.xlsx",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")



# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_BBD results columns TRUE", {
  testthat::expect_equal(resultsT, 31)
})
testthat::test_that("readr_BBD results columns FALSE", {
  testthat::expect_equal(resultsF, 17)
})

testthat::test_that("readr_BBD expected class", {
  testthat::expect_type(testOut1, "list")
})



