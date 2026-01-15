requireNamespace("dplyr")
library(dplyr)
library(lubridate)
library(stringr)

data = testData <- dplyr::tribble(
  ~database_id,               ~fieldNotes,                                                 ~eventDate, ~year, ~month, ~day,        ~verbatimEventDate, ~locality,                                ~locationRemarks, 
  "fake_SCAN01",                       "",                                                         "",    NA,     NA,   NA,                        "",   "Davis",      "coordinates obtained from Label; 6/28/05", 
  "fake_SCAN02",                       "",                                                         "",    NA,     NA,   NA,                        "",   "Davis", "coordinates obtained from Label: 3 march 2022", 
  "fake_SCAN03",            "28 Jun 2005",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN04",                       "",                                                         "",  2022,      2,    1,                        "",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN05",                       "",                                                         "",    NA,     NA,   NA, "28 Jun 2005/29 Jun 2005",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN06",                       "",                                                         "", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN07",                       "",                                                         "",    NA,     NA,   NA,              "28 IX 2005",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN08",                       "",                                                         "",    NA,     NA,   NA,                  "I 2022",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN09",                       "",                                                         "",    NA,     NA,   NA,                    "2022",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN10",                       "",                                               "2005-05-05",    NA,     NA,   NA,                    "2022",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN11",                       "",                                  "28 Jun 2005/29 Jun 2005",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN12",                       "",                                       "15-17 OCTOBER 1976",    NA,     NA,   NA,                    "2022",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN13",                       "",                            "28 September - 4 October 2005",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN14",                       "",      "2019-07-29T14:00:00+02:00/2019-07-29T15:00:00+02:00",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN15",                       "",                                  "07 to 21 September 2009",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN16",                       "",                              "Aug 30 1921 to Sept. 5 1921",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN17",                       "",                          "2 October 2003 - 3 October 2003",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN18",                       "",                                 "1940-06-26 to 1940-06-28",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN19",                       "", "startDateTime:201908301000xx; EndDateTime:201908301000xx",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN20",                       "",                                 "9.viii.2000/11.viii.2000",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN21",                       "",                                               "05-05-2005",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN22",                       "",                                               "05-28-2020",    NA,     NA,   NA,                    "    ",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN23",                       "",                                                 "06/28/05",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label", 
  "fake_SCAN24", "01.vii.2000/13.xi.2001",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label",
  "fake_SCAN25",            "III-05-2020",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label",
  "fake_SCAN26",           "XIII-10-2020",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label",
  "fake_SCAN27",               "5/6/2020",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label",
  "fake_SCAN28",  "15/6/2020 - 15/7/2020",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label",
  "fake_SCAN29",                 "Mar-22",                                                         "",    NA,     NA,   NA,                        "",   "Davis",               "coordinates obtained from Label"
  )


testOut <- dateFindR(data = testData,
                        # Years above this are removed (from the recovered dates only)
                        maxYear = lubridate::year(Sys.Date()),
                        # Years below this are removed (from the recovered dates only)
                        minYear = 1700) %>%
  dplyr::select(database_id, eventDate, fieldNotes, locationRemarks, verbatimEventDate, 
                day, month, year, startDayOfYear, endDayOfYear) %>%
  dplyr::arrange(desc(database_id))


# Test the expected results
testthat::test_that("dateFindR results successfully matched", {
  testthat::expect_equal(sum(complete.cases(testOut$eventDate) ), 27)
})
testthat::test_that("dateFindR results unsuccessfully matched", {
  testthat::expect_equal(sum(is.na(testOut$eventDate) ), 2)
})

testthat::test_that("dateFindR output dates match", {
  testthat::expect_equal(testOut %>% dplyr::arrange(database_id) %>% dplyr::pull(eventDate),
                         c("2005-06-28 00:00:00 UTC", "2022-03-03 00:00:00 UTC", "2005-06-28 00:00:00 UTC", "2022-02-01 00:00:00 UTC",
                           "2005-06-28 00:00:00 UTC", "2005-06-28 00:00:00 UTC", "2005-09-28 00:00:00 UTC", "2022-01-01 00:00:00 UTC",
                           NA                       , "2005-05-05 00:00:00 UTC", "2005-06-29 00:00:00 UTC", "1976-10-17 00:00:00 UTC",
                           "2005-10-04 00:00:00 UTC", "2019-07-29 13:00:00 UTC", "2009-09-21 00:00:00 UTC", "1921-09-05 00:00:00 UTC",
                           "2003-10-03 00:00:00 UTC", "1940-06-28 00:00:00 UTC", "2019-08-30 10:00:00 UTC", "2000-08-11 00:00:00 UTC",
                           "2005-05-20 05:00:00 UTC", "2020-05-28 00:00:00 UTC", "2005-06-28 00:00:00 UTC", "2000-07-01 00:00:00 UTC",
                           "2020-03-05 00:00:00 UTC",                        NA, "2020-01-01 00:00:00 UTC", "2020-06-15 00:00:00 UTC",
                           "2022-03-01 00:00:00 UTC") %>%
                           lubridate::ymd_hms(quiet = TRUE))
})

  # Test classes
testthat::test_that("dateFindR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("dateFindR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("tbl_df","tbl" ,"data.frame"))
})




troubleRows <- dplyr::tribble(
           ~database_id,                                                                                                                                          ~fieldNotes,                                                                                                     ~locality,                                ~eventDate,                     ~eventTime, ~startDayOfYear, ~endDayOfYear, ~day, ~month, ~year,   ~verbatimEventDate, ~locationRemarks,
   "NC_checklist_32750",                                                                                                                 "lot_id: 6994; preparation: pointed",        "\"GSMNP Oconaluftee residential area, 35°10'3\"\"N 83°18'16\"\"W, 2039ft, Old Field, Swain Co., NC\"",                     "2004-07-14T00:00:00",                             NA,              NA,            NA,  14L,     7L, 2004L,          "14-Jul-04",               NA,
    "NC_checklist_3666",   "[Savanna at Burgaw, N. C.] [T. B. Mitchell Oct. 23, 1957] [Hellanthus] [Andrena accepta Vier. ♀ det. Mitch. '62] [Andrena (Pterandrena) accepta]",                                      "Savanna, Burgaw, Pender Co., NC, Pender, North Carolina, United States",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA,
   "NC_checklist_45856",                                              "C. Dasch Collection [Mt. Mitchell, N.C. 5500 ft.; added 12-JUL-2002] [Aug 17 1970; added 12-JUL-2002]",                                                                      "Mount Mitchell, 5500ft, Yancey Co., NC",                               "8/17/1970",                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA,
   "NC_checklist_45975",                                              "C. Dasch Collection [Coccino Cape Hatteras NC.; added 12-JUL-2002] [Aug. 24, 1971; added 12-JUL-2002]",                                                                        "Cape Hatteras, Coccino, Dare Co., NC",                               "8/24/1971",                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA,
   "NC_checklist_45007",                                                                                                 "[Mt. Mitchell N.C.5500 ft. Aug. 13, 1970 C. Dasch]",                                                                      "Mount Mitchell, 5500ft, Yancey Co., NC",                               "8/13/1970",                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA,
   "NC_checklist_87482",                                                                                                                                                   NA,                                                                                              "Southern Pines",                               "4/14/2010",                             NA,            104L,            NA,  14L,     4L, 1910L,          "4/14/2010",               NA,
   "NC_checklist_49805",                                                                                     "[Balsam, N.C.; added 12-JUL-2002] [VI-1-56; added 12-JUL-2002]",                                                                                     "Balsam, Jackson Co., NC",                                "6/1/1956",                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA,
   "NC_checklist_49033",                                              "C. Dasch Collection [Mt. Mitchell, N.C. 5500 ft.; added 12-JUL-2002] [Aug 13 1970; added 12-JUL-2002]",                                                                      "Mount Mitchell, 5500ft, Yancey Co., NC",                               "8/13/1970",                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA,
   "NC_checklist_46230",                                                                                                                                                   NA,                                                                                              "Southern Pines",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA,          "31-Jul-18",               NA,
   "NC_checklist_44059",                                                                                                                                                   NA,                                                              "Smokemont, Great Smoky Mountains National Park",                     "1835-08-01T00:00:00",                             NA,            243L,          243L,   NA,     8L, 1835L,           "Aug. '35",               NA,
  "NC_checklist_100075",                                                                                                                                                   NA,                                                                                                    "Savannah",                              "1954-00-00",                             NA,              NA,            NA,  10L,    13L, 1954L,         "1954-13-10",               NA,
   "NC_checklist_28295",                                                                                                                                                   NA,                                                                                   "Valley of Black Mountains",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA,          "6/20/1906",               NA,
   "NC_checklist_26803",                                                                                                                                                   NA,                                                                                              "Southern Pines",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA,          "6/13/1918",               NA,
   "NC_checklist_18034",                                                                                                                                                   NA,                                                                                                     "Raleigh",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA,     "mid April 1921",               NA,
   "NC_checklist_16325",                                                                                                                                                   NA,                                                                                                     "Raleigh",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA, "mid September 1921",               NA,
   "NC_checklist_36870",                                                                                                                 "lot_id: 6710; preparation: pointed", "\"GSMNP The Purchase, nr. house, 35°35'8\"\"N 83°4'23\"\"W, 1945ft, cut lawn/forest edge, Haywood Co., NC\"",                     "2003-06-27T00:00:00",                             NA,              NA,            NA,  27L,     6L, 2003L,          "27-Jun-03",               NA,
   "NC_checklist_27278",                                                                                                                                                   NA,                                                                              "Kitty Hawk, 703 Kitty Hawk Rd.", "2015-08-14T16:00:00/2015-08-15T16:00:00",                             NA,              NA,            NA,   NA,     NA,    NA,          "7/30/2011",               NA,
   "NC_checklist_58897",                                                                                                                                                   NA,                                                                                                    "Havelock",                   "2015-08-14/2015-08-15",                             NA,              NA,            NA,   NA,     4L,    9L,   "10/27/2015 11:56",               NA,
    "NC_checklist_3249",                                                        "[N CAROLINA: Carteret Co. Fort Macon State Park Sweep Netting 20-X-2005 Coll: Randy Newman]",                            "Fort Macon State Park, Carteret Co., NC, Carteret, North Carolina, United States",                                        NA,                             NA,              NA,            NA,   NA,     NA,    NA,                   NA,               NA
  ) %>%
  dplyr::select(database_id, eventDate, fieldNotes, locationRemarks, verbatimEventDate, locality,
                day, month, year, startDayOfYear, endDayOfYear)


testOut2 <- dateFindR(data = troubleRows,
                             # Years above this are removed (from the recovered dates only)
                             maxYear = lubridate::year(Sys.Date()),
                             # Years below this are removed (from the recovered dates only)
                             minYear = 1700) %>%
  dplyr::arrange(desc(database_id))



# Test the expected results
testthat::test_that("dateFindR results successfuly matched", {
  testthat::expect_equal(sum(complete.cases(testOut2$eventDate) ), 18)
})
testthat::test_that("dateFindR results unsuccessfuly matched", {
  testthat::expect_equal(sum(is.na(testOut2$eventDate) ), 1)
})

testthat::test_that("dateFindR output dates match", {
  testthat::expect_equal(testOut2$eventDate ,
                         c("1910-04-14 00:00:00 UTC", "2015-08-15 00:00:00 UTC", "2002-07-12 00:00:00 UTC", "2002-07-12 00:00:00 UTC",
                           "2018-07-31 00:00:00 UTC", "2002-07-12 00:00:00 UTC", "2002-07-12 00:00:00 UTC", "1970-08-13 00:00:00 UTC",
                           "1835-08-01 00:00:00 UTC", "2003-06-27 00:00:00 UTC", "2023-10-01 00:00:00 UTC", "2004-07-14 00:00:00 UTC",
                           "2005-10-20 00:00:00 UTC", "1906-06-20 00:00:00 UTC", "2015-08-15 16:00:00 UTC", "1918-06-13 00:00:00 UTC",
                           "1921-04-01 00:00:00 UTC", "1921-09-01 00:00:00 UTC", NA  ) %>%
                           lubridate::ymd_hms(quiet = TRUE))
})

# Test classes
testthat::test_that("dateFindR expected class", {
  testthat::expect_type(testOut2, "list")
})
testthat::test_that("dateFindR expected class", {
  testthat::expect_equal(attributes(testOut2)$class, c("tbl_df","tbl" ,"data.frame"))
})
