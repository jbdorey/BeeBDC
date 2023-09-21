
requireNamespace("dplyr")
requireNamespace("lubridate")
requireNamespace("mgsub")

# Download the datasets
# load in the small test dataset in the background
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()
# Rename the file
beesTaxonomy <- testTaxonomy
rm(testTaxonomy)

# load in the small test dataset in the background
system.file("extdata", "testChecklist.rda", package="BeeBDC") |>
  load()
# Rename the file
beesChecklist <- testChecklist
rm(testChecklist)


if(!is.null(beesTaxonomy) & !is.null(beesChecklist)){
  # Run example queries
# Single entry example
 testQuerySing <- BeeBDCQuery(
   beeName = "Nomia maneei",
   searchChecklist = TRUE,
   printAllSynonyms = TRUE,
   beesTaxonomy = beesTaxonomy,
   beesChecklist = beesChecklist)
 
   # Multiple entry example
 testQueryMult <- BeeBDCQuery(
   beeName = c("Nomia maneei", "Bombus hypnorum",
   "Ceratina nanula Cockerell, 1897"),
   searchChecklist = TRUE,
   printAllSynonyms = TRUE,
   beesTaxonomy = beesTaxonomy,
   beesChecklist = beesChecklist)
 
  # Simple without checklist
 testQuerySimple <- BeeBDCQuery(
   beeName = c("Nomia maneei", "Bombus hypnorum",
               "Ceratina nanula Cockerell, 1897"),
   searchChecklist = FALSE,
   printAllSynonyms = TRUE,
   beesTaxonomy = beesTaxonomy,
   beesChecklist = beesChecklist)

 
 # Test class
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_type(testQuerySing, "list")
 })
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_type(testQueryMult, "list")
 })
  # Test that elements are tibbles
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_true(any(stringr::str_detect(attributes(testQuerySing$taxonomyReport)$class, 
                                                 "tbl_df")))
 })
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_true(any(stringr::str_detect(attributes(testQueryMult$taxonomyReport)$class, 
                                                 "tbl_df")))
 })
 
 # Test output lengths
 testthat::test_that("BeeBDCQuery list size", {
   testthat::expect_equal(length(testQuerySing), 4)
 })
 testthat::test_that("BeeBDCQuery list size", {
   testthat::expect_equal(length(testQueryMult), 4)
 })
 testthat::test_that("BeeBDCQuery list size", {
   testthat::expect_equal(length(testQuerySimple), 3)
 })
 
 }
