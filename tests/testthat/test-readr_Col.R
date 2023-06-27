requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("openxlsx")
requireNamespace("BeeDC")
library(dplyr) ## could not use %>% without loading as library


# this is FAKE DATA BECAUSE THE REAL DATA IS UNDER RESTRCTIVE LICENSE
testData <- tibble::tribble(
              ~Código.de.Barras, ~institutionCode, ~`Colectores.[Aggregated]`, ~Colectores.asociados, ~Fecha.colección.inicial,  ~Ano, ~mes, ~dia,        ~Orden, ~Familia,         ~Género,    ~Especie,            ~Especie.Author, ~Tipo, ~`Determinador.[Formatted]`, ~Fecha.determinación,            ~País,        ~Departamento, ~Municipio, ~Corregimiento.Departamental,                                              ~Localidad, ~Latitud.georref..dec., ~Longitud.georref..dec.,          ~Nombre.Completo,
                 "ICN_butfake1",            "ICN",              "anonymous a",                    NA,             "01/01/2010", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,                          NA,                   NA, "ESTADOS UNIDOS",            "ARIZONA",   "COCHISE", "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera",
                 "ICN_butfake2",            "ICN",              "anonymous b",                    NA,             "01/01/2010", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,               "anonymous e",                   NA, "ESTADOS UNIDOS",            "ARIZONA",   "COCHISE", "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera",
                 "ICN_butfake3",            "ICN",              "anonymous c",                    NA,             "01/01/2010", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,                          NA,                   NA, "ESTADOS UNIDOS",            "ARIZONA",  "COCHISE",  "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera",
                 "ICN_butfake4",            "ICN",              "anonymous d",                    NA,            "01/01/20101", 2010L, "01", "01", "Hymenoptera", "Apidae",          "Apis", "mellifera",           "Linneaus, 1758",    NA,               "anonymous f",                   NA, "ESTADOS UNIDOS",            "ARIZONA",  "COCHISE",  "1 milla above nion saddle",      "USA. Arizona. Cochise. 1 milla above nion saddle",             32.1139688,            -109.9211756,          "Apis mellifera"
              )

# need to change column names, function requires spaces to work 
colnames(testData) <- c("Código de Barras", "institutionCode", "Colectores [Aggregated]", "Colectores asociados", "Fecha colección inicial", "Ano", "mes", "dia", "Orden", "Familia", "Género", "Especie", "Especie Author", "Tipo", "Determinador [Formatted]", "Fecha determinación", "País", "Departamento", "Municipio", "Corregimiento Departamental", "Localidad", "Latitud georref. dec.", "Longitud georref. dec.", "Nombre Completo")

# Save a temporary version of these data
openxlsx::write.xlsx(testData, paste0(tempdir(), "/testData.xlsx"), sheetName = "Spanish headers")


testOut1 <- BeeDC::readr_Col(path = paste0(tempdir()),
                             inFile = "/testData.xlsx",
                             outFile = "testDataOut.csv",
                             sheet = "Spanish headers",
                             dataLicense = "All rights reserved")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


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
