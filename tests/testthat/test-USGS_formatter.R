


  # Copy in some of the USGS dataset
testData <- "ID.$name$sex$DeterminedBy$DeterminedWhen$WhoScanned$DateScanned$SpeciesNotes$DateEntered$COLLECTION.db$gmt$ip$latitude$longitude$accuracy$elevation$country$state$county$city$site$position$time1$time2$days$who$email$how0$how1$how2$how3$how4$habitat$field_note$note
USGS_DRO000001$Lasioglossum imitatum$f$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$\n
USGS_DRO000002$Lasioglossum imitatum$f$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$\n
USGS_DRO000003$Augochlorella aurata$f$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$\n
USGS_DRO000004$Augochlorella aurata$f$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$\n
USGS_DRO000005$Halictus confusus$f$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$\n
USGS_DRO000006$Nomada maculata$m$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$\n
USGS_DRO000007$Lasioglossum pilosum$f$Sam Droege$2005-01-01$Unknown$1900-01-01$$2017-01-27 09:35:35.770000000$USGS_DRO337$20081107112006$134.192.250.22$35.5917$-83.0602$3$1530$USA$North Carolina$Swain$Great Smoky Mountains$Purchase Knob$23$200204021030xx$200204021630xx$$Harold W. Ikerd$sdroege@usgs.gov$pan trap$5$bowl 6.0oz$yellow-uv$soap dawn$Field$Field no blooms$"


writeLines(testData, paste0(tempdir(), "/19-Nov-22_USGS_DRO_flat.txt"))

testOut <- BeeBDC::USGS_formatter(path = tempdir(), 
                                 pubDate = "19-11-2022")



# Test the expected results
testthat::test_that("USGS_formatter test length of occurrence data", {
  testthat::expect_equal(nrow(testOut$USGS_data), 7)
})
testthat::test_that("USGS_formatter test length of EML data", {
  testthat::expect_equal(nrow(testOut$EML_attributes$Source_tibble), 1)
})
testthat::test_that("USGS_formatter test length of list", {
  testthat::expect_equal(length(testOut), 2)
})


# Test classes
testthat::test_that("USGS_formatter expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("USGS_formatter expected class", {
  testthat::expect_equal(attributes(testOut$USGS_data)$class, c("tbl_df","tbl" ,"data.frame"))
})
testthat::test_that("USGS_formatter expected attribute names", {
  testthat::expect_equal(attributes(testOut$EML_attributes)$names, c("source_eml","Source_tibble"))
})


