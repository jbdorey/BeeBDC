requireNamespace("readr")
requireNamespace("tibble")
requireNamespace("openxlsx")
requireNamespace("dplyr")


testData <- tibble::tribble(
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

testOut1 <- BeeDC::readr_MABC(path = paste0(tempdir()),
                              inFile = "/testData.xlsx",
                              outFile = "testDataOut.csv",
                              sheet = "Hoja1",
                              dataLicense = "All rights reserved")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)


testthat::test_that("readr_MABC results columns TRUE", {
  testthat::expect_equal(resultsT, 27)
})
testthat::test_that("readr_MABC results columns FALSE", {
  testthat::expect_equal(resultsF, 0)
})

testthat::test_that("readr_MABC expected class", {
  testthat::expect_type(testOut1, "list")
})



