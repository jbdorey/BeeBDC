  # These functions were written by James B Dorey beginning on the 17th of June 2022 to read in,
    # Format and save various datasets.
  # For questions, please email jbdorey[at]me.com


#### x.0 readr_BeeBDC function ####
#' A wrapper for all of the data readr_functions
#' 
#' Read in a variety of data files that are specific to certain smaller data providers. 
#' There is an internal readr function for each dataset and each one of these functions is called
#' by readr_BeeBDC. While these functions are internal, they are displayed in the documentation of 
#' readr_BeeBDC for clarity.
#' 
#' This function wraps several internal readr functions. Users may call
#' readr_BeeBDC and select the dataset name to import a certain dataset. These datasets include:
#' 
#' Excel (.xlsx) formatted datasets: CAES, MABC, Col, Bal, MEPB, MUPJ, Arm, JoLa, and VicWam.
#' 
#' CSV (.csv) formatted datasets: EPEL, ASP, BMin, BMont, Ecd, Gai, KP, EcoS, GeoL, EaCo, FSCA, SMC,
#' Lic, Dor, BBD, STRI, and PALA
#' 
#' See Dorey et al. 2023 BeeBDC... for further details.
#'
#' @param dataset Character. The name of the dataset to be read in. For example readr_CAES can
#' be called using "readr_CAES" or "CAES". This is not caps sensitive.
#' @param path A character path. The path to the directory containing the data.
#' @param inFile Character or character path. The name of the file itself (can also be the 
#' remainder of a path including the file name).
#' @param outFile Character or character path. The name of the Darwin Core format file to be saved.
#' @param dataLicense Character. The license to accompany each record in the Darwin Core 'license' 
#' column.
#' @param sheet A character String. For those datasets read from an .xlsx format, provide the 
#' sheet name. 
#' NOTE: This will be ignored for .csv readr_ functions and required for .xlsx readr_ functions.
#'
#' @return A data frame that is in Darwin Core format.
#' @export
#' 
#' @importFrom readr read_csv write_excel_csv
#' @importFrom dplyr rename mutate select if_else  %>%
#' @importFrom lubridate ymd month
#' @importFrom stringr str_c
#' 
#'
#' @examples
#' \dontrun{
#' # An example using a .xlsx file
#'Arm_Data <- readr_BeeBDC(
#'     dataset = "Arm",
#'     path = paste0(tempdir(), "/Additional_Datasets"),
#'     inFile = "/InputDatasets/Bee database Armando_Final.xlsx",
#'     outFile = "jbd_Arm_Data.csv",
#'     sheet = "Sheet1",
#'     dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
#'     
#'     
#'     # An example using a .csv file
#'EPEL_Data <- readr_BeeBDC(
#'   dataset = "readr_EPEL",
#'   path = paste0(tempdir(), "/Additional_Datasets"),
#'   inFile = "/InputDatasets/bee_data_canada.csv",
#'   outFile = "jbd_EPEL_data.csv",
#'   dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
#' }
readr_BeeBDC <- function(
    dataset = NULL,
    path = NULL,
    inFile = NULL,
    outFile = NULL,
    dataLicense = NULL,
    sheet = NULL){
  
  #### x.0 Prep ####
  ##### x.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(dataset)){
    stop(" - Please provide an argument for dataset. This can be from any of the readr_ functions")
  }
  if(is.null(path)){
    stop(" - Please provide an argument for the path to a folder containing your file.")
  }
  if(is.null(inFile)){
    stop(" - Please provide an argument for the inFile name to find.")
  }
  if(is.null(outFile)){
    stop(" - Please provide an argument for outFile to save as.")
  }
  if(is.null(dataLicense)){
    stop(" - Please provide an argument for dataLicense.")
  }
  
  ##### x.2 Data types ####
  # Create the lists of potential data types for .xlsx or .csv inputs
  excelTypes <- c("CAES", "MABC", "Col", "Bal", "MEPB", "MPUJ", "Arm", "JoLa", "VicWam")
  csvTypes <- c("EPEL", "ASP", "BMin", "BMont", "Ecd", "Gai", "KP", "EcoS", "GeoL",
                "EaCo", "FSCA", "SMC", "Lic", "Dor", "BBD", "PALA", "STRI")
  
  # Wrong entry test
  if(!tolower(dataset) %in% tolower(c(paste0("readr_",csvTypes), csvTypes,
                              paste0("readr_",excelTypes), excelTypes)) ){
    stop("The dataset does not match a readr_BeeBDC function")
  }
  
  
  #### x.0 Choose function ####
  ##### x.1 Excel functions ####
  # EXCEL test
  if(tolower(dataset) %in% tolower(c(paste0("readr_",excelTypes), excelTypes)) ){
    message("A .xlsx data type was chosen...")
    # If no sheet is provided
    if(is.null(sheet)){
      stop(" - No sheet argument was provided. Please check for the sheet name to import.")
    }
    ###### a. CAES ####
    if(tolower(dataset) %in% tolower(c("readr_CAES", "CAES")) ){
      dataOut <- readr_CAES(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense,
                        sheet = sheet)}
    ##### b. MABC ####
    if(tolower(dataset) %in% tolower(c("readr_MABC", "MABC")) ){
      dataOut <- readr_MABC(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense,
                        sheet = sheet)}
    ##### c. Col ####
    if(tolower(dataset) %in% tolower(c("readr_Col", "Col")) ){
      dataOut <- readr_Col(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense,
                       sheet = sheet)}
    ##### d. Bal ####
    if(tolower(dataset) %in% tolower(c("readr_Bal", "Bal")) ){
      dataOut <- readr_Bal(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense,
                       sheet = sheet)}
    ##### e. MEPB ####
    if(tolower(dataset) %in% tolower(c("readr_MEPB", "MEPB")) ){
      dataOut <- readr_MEPB(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense,
                        sheet = sheet)}
    ##### f. MPUJ ####
    if(tolower(dataset) %in% tolower(c("readr_MPUJ", "MPUJ")) ){
      dataOut <- readr_MPUJ(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense,
                        sheet = sheet)}
    ##### f. Arm ####
    if(tolower(dataset) %in% tolower(c("readr_Arm", "Arm")) ){
      dataOut <- readr_Arm(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense,
                       sheet = sheet)}
    ##### g. JoLa ####
    if(tolower(dataset) %in% tolower(c("readr_JoLa", "JoLa")) ){
      dataOut <- readr_JoLa(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense,
                        sheet = sheet)}
    ##### h. VicWam ####
    if(tolower(dataset) %in% tolower(c("readr_VicWam", "VicWam")) ){
      dataOut <- readr_VicWam(path = path,
                            inFile = inFile,
                            outFile = outFile,
                            dataLicense = dataLicense,
                            sheet = sheet)}
  }# END Excel functions
  
  ##### x.2 CSV functions ####
  # CSV test
  if(tolower(dataset) %in% tolower(c(paste0("readr_",csvTypes), csvTypes)) ){
    message("A .csv data type was chosen...")
    
    ###### a. EPEL ####
    if(tolower(dataset) %in% tolower(c("readr_EPEL", "EPEL")) ){
      dataOut <- readr_EPEL(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### b. ASP ####
    if(tolower(dataset) %in% tolower(c("readr_ASP", "ASP")) ){
      dataOut <- readr_ASP(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### c. BMin ####
    if(tolower(dataset) %in% tolower(c("readr_BMin", "BMin")) ){
      dataOut <- readr_BMin(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### d. BMont ####
    if(tolower(dataset) %in% tolower(c("readr_BMont", "BMont")) ){
      dataOut <- readr_BMont(path = path,
                         inFile = inFile,
                         outFile = outFile,
                         dataLicense = dataLicense)}
    ##### e. Ecd ####
    if(tolower(dataset) %in% tolower(c("readr_Ecd", "Ecd")) ){
      dataOut <- readr_Ecd(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### f. Gai ####
    if(tolower(dataset) %in% tolower(c("readr_Gai", "Gai")) ){
      dataOut <- readr_Gai(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### g. KP ####
    if(tolower(dataset) %in% tolower(c("readr_KP", "KP")) ){
      dataOut <- readr_KP(path = path,
                      inFile = inFile,
                      outFile = outFile,
                      dataLicense = dataLicense)}
    ##### h. EcoS ####
    if(tolower(dataset) %in% tolower(c("readr_EcoS", "EcoS")) ){
      dataOut <- readr_EcoS(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### i. GeoL ####
    if(tolower(dataset) %in% tolower(c("readr_GeoL", "GeoL")) ){
      dataOut <- readr_GeoL(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### j. EaCO ####
    if(tolower(dataset) %in% tolower(c("readr_EaCO", "EaCo")) ){
      dataOut <- readr_EaCO(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### k. FSCA ####
    if(tolower(dataset) %in% tolower(c("readr_FSCA", "FSCA")) ){
      dataOut <- readr_FSCA(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### l. SMC ####
    if(tolower(dataset) %in% tolower(c("readr_SMC", "SMC")) ){
      dataOut <- readr_SMC(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### m. Lic ####
    if(tolower(dataset) %in% tolower(c("readr_Lic", "Lic")) ){
      dataOut <- readr_Lic(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### n. Dor ####
    if(tolower(dataset) %in% tolower(c("readr_Dor", "Dor")) ){
      dataOut <- readr_Dor(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### o. BBD ####
    if(tolower(dataset) %in% tolower(c("readr_BBD", "BBD")) ){
      dataOut <- readr_BBD(path = path,
                       inFile = inFile,
                       outFile = outFile,
                       dataLicense = dataLicense)}
    ##### p. STRI ####
    if(tolower(dataset) %in% tolower(c("readr_STRI", "STRI")) ){
      dataOut <- readr_STRI(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    ##### g. PALA ####
    if(tolower(dataset) %in% tolower(c("readr_PALA", "PALA")) ){
      dataOut <- readr_PALA(path = path,
                        inFile = inFile,
                        outFile = outFile,
                        dataLicense = dataLicense)}
    
    
    
  } #END csv
  
  return(dataOut)
} # END readr_BeeBDC





#### 1.0 EPEL ####
#' @rdname readr_BeeBDC
#' 
#' 
#'
readr_EPEL <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  catalog_number<-pollinator_family<-pollinator_genus<-pollinator_species<-collection_method<-
    day_collected<-month_collected<-year_collected<-location_description<-latitude<-longitude<-
    basis_of_record<-genus<-specificEpithet<-year<-day<-eventDate<-collector_number<-
    location_name<-habitat<-.<-catalogNumber <- month<-NULL
  
  #### 1.1 Prep ####
  requireNamespace("dplyr")
  requireNamespace("lubridate")

  #### 1.2 Read+ ####
  EPEL_Data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
  # Rename some columns to make the consistent with DarwinCore
  dplyr::rename(
    catalogNumber =	catalog_number,
    family =	pollinator_family,
    genus =	pollinator_genus,
    specificEpithet =	pollinator_species,
    samplingProtocol =	collection_method,
    day =	day_collected,
    month =	month_collected,
    year =	year_collected,
    locality =	location_description,
    decimalLatitude =	latitude,
    decimalLongitude =	longitude,
    basisOfRecord =	basis_of_record) %>%
  # Make new columns
  dplyr::mutate(
    # Merge to scientific name
    scientificName = paste(genus, specificEpithet),
    # Add data source
    dataSource = "EPEL_Anthophila",
    # Add eventDate
    eventDate = lubridate::ymd(paste(year, month, day, sep = "-"),
                               truncated = 2),
    month = lubridate::month(eventDate)) %>%
      # Add fieldNotes
    dplyr::mutate(
      fieldNotes = stringr::str_c(
        dplyr::if_else(!is.na(collector_number),
                       paste0("Collector_number: ", collector_number), ""),
        dplyr::if_else(!is.na(location_name),
                       paste0("location_name: ", location_name), ""),
        dplyr::if_else(!is.na(habitat),
                       paste0("habitat: ", habitat), ""),
        sep = "|") %>%
        # Remove extra bars "|".
        stringr::str_replace_all(pattern = "(\\|){2,9}",
                                 replacement = "\\|") %>%
        stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                                 replacement = "")) %>%
  # Remove these input columns
  dplyr::select(!c(collector_number, location_name, habitat)) %>%
  # add the database_id column
  dplyr::mutate(
    database_id = paste("EPEL_data_", 1:nrow(.), sep = ""),
    .before = catalogNumber) %>%
    dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
    dplyr::mutate(
      datasetName = "Elle Pollination Ecology Lab",
      datasetID = "EPEL",
      institutionCode = "SFU"
    )
  
  #### 1.3 Out ####
# Save the dataset
readr::write_excel_csv(EPEL_Data, file = paste(path, outFile, sep = "/"))
# Return data
return(EPEL_Data)
} # END readr_EPEL


#### 2.0 ASP ####
#' @rdname readr_BeeBDC
#' 
#' 
#'
readr_ASP <- function(path = NULL,
                         inFile = NULL,
                         outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  Tribe <- Morphospecies <- Successional_Stage <- genus <- specificEpithet <- NULL
  eventDate <- catalogNumber <- . <- elevation <- NULL
  #### 2.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 2.2 Read+ ####
ASP_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                            trim_ws = TRUE) %>%
    dplyr::mutate(verbatimElevation = elevation) %>%
  dplyr::mutate(
    # Add previousIdentifications
    previousIdentifications = stringr::str_c(
      dplyr::if_else(!is.na(Tribe),
                     paste0("Tribe: ", Tribe), ""),
      dplyr::if_else(!is.na(Morphospecies),
                     paste0("Morphospecies: ", Morphospecies), ""),
      sep = "|") %>%
      # Remove extra bars "|".
      stringr::str_replace_all(pattern = "(\\|){2,9}",
                               replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                               replacement = "")) %>%
  dplyr::mutate(
    # Add locationRemarks
    locationRemarks = paste0("Successional Stage:", Successional_Stage),
    # Add scientificName
    scientificName = paste(genus, specificEpithet),
    # Add dataSource
    dataSource = "ASP_Anthophila",
    # Add eventDate
    eventDate = lubridate::dmy(eventDate,
                               truncated = 2),
    # I'm sorry but this catalogNumber is useless. I'm going to edit it more-unique
    catalogNumber = dplyr::if_else(!is.na(catalogNumber),
                                   paste0("ASP_", catalogNumber), "")) %>%
  # add the database_id column
  dplyr::mutate(
    database_id = paste("ASP_data_", 1:nrow(.), sep = ""),
    .before = catalogNumber) %>%
  dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "Allan Smith-Pardo",
    datasetID = "ASP"
  )

  #### 2.3 Out ####
# Save the dataset
readr::write_excel_csv(ASP_data, file = paste(path, outFile, sep = "/"))
# Return data
return(ASP_data)
} # END readr_ASP


#### 3.0 BMin ####
 
 #' @rdname readr_BeeBDC
#' 
#' 
#'

readr_BMin <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  eventDate <- . <- catalogNumber <- NULL
  catalog_number <- pollinator_family <- pollinator_genus <- pollinator_species <- NULL
  collection_method <- day_collected <- month_collected <- year_collected <- NULL
  location_description <- latitude <- longitude <- basis_of_record <- genus <- NULL
  specificEpithet <- year <- day <- eventDate <- collector_number <- location_name <- NULL
  habitat <- . <- catalogNumber <- NULL
  
  
  #### 3.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  #### 3.2 Read+ ####
BMin_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                               trim_ws = TRUE) %>%
  dplyr::mutate(
    # Format eventDate
    eventDate = lubridate::dmy(eventDate,
                               truncated = 2, quiet = TRUE),
    dataSource = "BMin_Anthophila") %>%
  # add the database_id column
  dplyr::mutate(
    database_id = paste("BMin_data_", 1:nrow(.), sep = ""),
    .before = catalogNumber)  %>%
  dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "Robert Minckley",
    datasetID = "BMin",
    institutionCode = "University of Rochester"
  )
  #### 3.3 Out ####
# Save the dataset
readr::write_excel_csv(BMin_data, file = paste(path, outFile, sep = "/"))
# Return data
return(BMin_data)
} # END readr_BMin



#### 4.0 BMont ####
#' @rdname readr_BeeBDC
#' 
#' 
#'
readr_BMont <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  occurence_lsid <- fieldNotes <- GPS_device <- . <- catalogNumber <- eventDate <- dateTest <- NULL
  tempDate <- mdy <- VerbatimEventDate <- ymd<-NULL
  
  
  #### 4.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 4.2 Read+ ####
BMont_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
  dplyr::rename(
    occurrenceID = occurence_lsid,
    verbatimEventDate = VerbatimEventDate) %>%
  dplyr::mutate(
    # Add fieldNotes 
     fieldNotes = stringr::str_c(
      dplyr::if_else(!is.na(fieldNotes),
                     paste0("fieldNotes: ", fieldNotes), ""),
      dplyr::if_else(!is.na(GPS_device),
                     paste0("GPS_device: ", GPS_device), ""),
      sep = "|") %>%
       # Remove extra bars "|".
       stringr::str_replace_all(pattern = "(\\|){2,9}",
                                replacement = "\\|") %>%
       stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                                replacement = "")) %>%
    dplyr::mutate(dataSource = "BMont_Anthophila") %>%
  # add the database_id column
  dplyr::mutate(
    database_id = paste("BMont_data_", 1:nrow(.), sep = ""),
    .before = catalogNumber) %>%
  # Find the date ranges
  dplyr::mutate(dateTest = dplyr::if_else(stringr::str_count(eventDate) > 11,
                                          stringr::str_replace(string = eventDate,
                                                               pattern = "/", replacement = ","), 
                                          "FALSE")) %>%
  # Append these to fieldNotes
  dplyr::mutate(fieldNotes = dplyr::if_else(dateTest != FALSE & !is.na(dateTest),
                                            paste0(fieldNotes,
                                                   "|dateRange: ", eventDate), fieldNotes)) %>%
  # Now remove those eventDates with a range and instead insert the start date
  dplyr::mutate(tempDate = dplyr::if_else(dateTest != FALSE & !is.na(dateTest),
                                          paste0(stringr::str_replace(eventDate,
                                                                      pattern = ".*/",
                                                                      replacement = "")), 
                                          eventDate)) %>%
  # Pick up dates of different formats and format together.
  dplyr::mutate(mdy = lubridate::mdy(tempDate, quiet = TRUE)) %>%
  dplyr::mutate(ymd = lubridate::ymd(tempDate, quiet = TRUE)) %>%
  tidyr::unite(col = eventDate,
               mdy, ymd, sep = "", na.rm = TRUE,
               remove = FALSE) %>% 
  # Remove working columns
  dplyr::select(!c(dateTest, tempDate, mdy, ymd)) %>%
  dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "Bombus Montana",
    datasetID = "BMon"
  )
  #### 4.3 Out ####
# Save the dataset
readr::write_excel_csv(BMont_data, file = paste(path, outFile, sep = "/"))
  # Return data
return(BMont_data)
} # END readr_BMont


#### 5.0 Ecd ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Ecd <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  year <- day <- institutionCode <- id <- . <- catalogNumber <- recordID <-month<- NULL
  
  #### 5.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 5.2 Read+ ####
Ecd_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                            trim_ws = TRUE) %>%
    dplyr::rename(recordId = recordID) %>%
  dplyr::mutate(
    # Format eventDate
    eventDate = lubridate::ymd(paste(year, month, day, sep = "-"),
                               truncated = 2, quiet = TRUE),
    dataSource = "Ecd_Anthophila",
    # I want to make sure id is unique...
    id = paste(institutionCode, id, sep = "_")) %>%
  # add the database_id column
  dplyr::mutate(
    database_id = paste("Ecd_data_", 1:nrow(.), sep = ""),
    .before = catalogNumber)  %>%
  dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "Ecdysis",
    datasetID = "Ecd"
  )

  #### 5.3 Out ####
# Save the dataset
readr::write_excel_csv(Ecd_data, file = paste(path, outFile, sep = "/"))
# Return data
return(Ecd_data)
} # END readr_Ecd

###### 6.0 Gai ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Gai <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  species<-subspecies<-SpecimenLocation<-eventTime<-EndTime<-TempStart<-TempEnd<-WindStart<-
    WindEnd<-SkyStart<-SkyEnd<-Site<-siteLocality<-syd<-eventDate<-.<-institutionCode <- NULL
  
  #### 6.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  #### 6.2 Read+ ####
Gai_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                            trim_ws = TRUE) %>%
  # Make columns DarwinCore-compatible
  dplyr::rename(
    collectionCode = 'Collection Code',
    otherCatalogNumbers = 'Other Catalog Number',
    specificEpithet = species,
    infraspecificEpithet = subspecies)  %>%
  # Add locationRemarks from a bunch of other columns
  dplyr::mutate(
    locationRemarks = stringr::str_c(
    dplyr::if_else(!is.na(SpecimenLocation), paste0("SpecimenLocation: ", SpecimenLocation), ""),
    dplyr::if_else(!is.na(eventTime),
                   paste0("StartTime: ", eventTime), ""),
    dplyr::if_else(!is.na(EndTime),
                   paste0("EndTime: ", EndTime), ""),
    dplyr::if_else(!is.na(TempStart),
                   paste0("TempStart: ", TempStart), ""),
    dplyr::if_else(!is.na(TempEnd),
                   paste0("TempEnd: ", TempEnd), ""),
    dplyr::if_else(!is.na(WindStart),
                   paste0("WindStart: ", WindStart), ""),
    dplyr::if_else(!is.na(WindEnd),
                   paste0("WindEnd: ", WindEnd), ""),
    dplyr::if_else(!is.na(SkyStart),
                   paste0("SkyStart: ", SkyStart), ""),
    dplyr::if_else(!is.na(SkyEnd),
                   paste0("SkyEnd: ", SkyEnd), ""),
    dplyr::if_else(!is.na(Site),
                   paste0("Site: ", Site), ""),
    dplyr::if_else(!is.na(siteLocality),
                   paste0("siteLocality: ", siteLocality), ""),
    dplyr::if_else(!is.na(syd),
                   paste0("syd: ", syd), ""),
    sep = "|") %>%
      # Remove extra bars "|".
      stringr::str_replace_all(pattern = "(\\|){2,9}",
                               replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                               replacement = "")) %>%
  dplyr::mutate(
    # Format eventDate
    eventDate = lubridate::mdy(eventDate,
                               truncated = 2, quiet = FALSE),
    dataSource = "Gai_Anthophila") %>%
  # add the database_id column
  dplyr::mutate(
    database_id = paste("Gai_data_", 1:nrow(.), sep = ""),
    .before = institutionCode)  %>%
  dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "Gaiarsa et al. 2021",
    datasetID = "Gai"
  )
  #### 6.3 Out ####
# Save the dataset
readr::write_excel_csv(Gai_data, file =  paste(path, outFile, sep = "/"))
# Return data
return(Gai_data)
} # END readr_Gai

#### 7.0 CAES ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_CAES <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = "Sheet1"){
  # locally bind variables to the function
  Tribe <- Morphospecies <- Successional_Stage <- genus <- specificEpithet <- NULL
  eventDate <- catalogNumber <- . <- NULL
  .<-PBIUSI<-Family<-Subfamily<-Genus<-species<-Country<-State_Prov<-Sec_Subdiv<-Locality<-Lat<-
    Lon<-Start_Date<-Collector<-Sex<-Inst_Code<-Det_By<-Det_Date<-Coll_Method<-Spec_Count<-
    Elev_m<-Trip_Code<-Project<-Det_History<-Tribe<-Host_Genus<-Host_Common_Name<-
    Host_Relation<-Host_Location<-Loc_Notes<-Lat_Lon_Method<-End_Date<-eventDate<-
    Elev_Det<-Macro_Habitat<-Micro_Habitat<-Pres_Method<-Spec_Notes<-genus<-specificEpithet<-
    Lat_Lon_Accuracy<-Host_species<-Host_Family<-catalogNumber <- NULL
  
    #### 7.1 Prep ####
    # This will load the requireNamespaced packages. These packages may still need to be installed to 
      # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  #### 7.2 Read+ ####
    # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
CAES_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"), sheet = sheet) %>%
  # Turn spaces into "_" in column names
  dplyr::rename_with(., ~ gsub(" ", "_", .x, fixed = TRUE)) %>%
  # Make columns DarwinCore-compatible
  dplyr::rename(
    catalogNumber = PBIUSI,
    family = Family,
    subfamily = Subfamily,
    genus = Genus,
    specificEpithet = species,
    country = Country,
    stateProvince = State_Prov,
    county = Sec_Subdiv,
    locality = Locality,
    decimalLatitude = Lat,
    decimalLongitude = Lon,
    eventDate = Start_Date,
    recordedBy = Collector,
    sex = Sex,
    institutionCode = Inst_Code,
    identifiedBy = Det_By,
    dateIdentified = Det_Date,
    samplingProtocol = Coll_Method,
    individualCount = Spec_Count,
    elevation = Elev_m,
    otherCatalogNumbers = Trip_Code,
    bibliographicCitation = Project,
    identificationRemarks = Det_History)  %>%
      # Add a bunch of columns from other columns
    dplyr::mutate(
    # Add previousIdentifications
    previousIdentifications = paste0(
        # ONLY do this IF there is something in the cell - is.na() finds "NA" values. the "!" reverses this to find stats::complete.cases only.
      dplyr::if_else(!is.na(Tribe), paste0("Tribe: ", Tribe),"")),
    # Add associatedTaxa
      # This will ONLY concatenate columns where they have a value.
    associatedTaxa = stringr::str_c(
      dplyr::if_else(!is.na(Host_Genus), paste0("Host_id: ", Host_Genus), ""),
      dplyr::if_else(!is.na(Host_Common_Name),
                     paste0("Host_commonName: ", Host_Common_Name), ""),
      dplyr::if_else(!is.na(Host_Relation),
                     paste0("Host_relation: ", Host_Relation), ""),
      dplyr::if_else(!is.na(Host_Location),
                     paste0("Host_location: ", Host_Location), ""),
      sep = "|") %>%
      # Remove extra bars "|".
      stringr::str_replace_all(pattern = "(\\|){2,9}",
                               replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                               replacement = "")) %>%
    # Do the same as the last mutate, but for fieldNotes
  dplyr::mutate(
    fieldNotes = stringr::str_c(
      dplyr::if_else(!is.na(Loc_Notes),
                     paste0("fieldNotes: ", Loc_Notes), ""),
      dplyr::if_else(!is.na(Lat_Lon_Method),
                     paste0("GPS_device: ", Lat_Lon_Method), ""),
      dplyr::if_else(!is.na(End_Date),
                     paste0("Sampling period: ", eventDate, " to ", End_Date), ""),
      dplyr::if_else(!is.na(Elev_Det),
                     paste0("Elevation_by: ", Elev_Det), ""),
      dplyr::if_else(!is.na(Macro_Habitat),
                     paste0("Macro_Habitat: ", Macro_Habitat), ""),
      dplyr::if_else(!is.na(Micro_Habitat),
                     paste0("Micro_Habitat: ", Micro_Habitat), ""),
      dplyr::if_else(!is.na(Pres_Method),
                     paste0("Preservation_method: ", Pres_Method), ""),
      dplyr::if_else(!is.na(Spec_Notes),
                     paste0("Specimen_notes: ", Spec_Notes), ""),
      sep = "|") %>%
      # Remove extra bars "|".
      stringr::str_replace_all(pattern = "(\\|){2,9}",
                               replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                               replacement = "")) %>%
        # Add scientificName
      dplyr::mutate(
        scientificName = stringr::str_c(
          dplyr::if_else(!is.na(genus),
                         genus, ""),
          dplyr::if_else(!is.na(specificEpithet),
                         specificEpithet, ""),
          sep = " ")) %>%
  # Get the coordinateUncertaintyInMeters by taking the UPPER uncertainty limit from the provided 
    # ranges. I.e., "100-1000" becomes "1000"
  dplyr::mutate(
    coordinateUncertaintyInMeters =  stringr::str_extract(
      Lat_Lon_Accuracy, pattern = "-.*|[0-9]+" ),
    # Format eventDate
    eventDate = lubridate::ymd(
        # First convert from silly excel numeric format to real dates...
      eventDate %>%
        as.numeric() %>%
        as.Date(., origin = "1899-12-30"),
      truncated = 2, quiet = FALSE), # 215 failed to parse. 
    dataSource = "CAES_Anthophila") %>%
  # Add the scientificNameAuthorship column using the specificEpithet column
      # dplyr::mutate(scientificNameAuthorship = stringr::str_replace(
      #   string = verbatimScientificName,
      #   pattern = paste0(".*", specificEpithet, " "),
      #   replacement = ""
      # )) %>%
  # Remove those now redundant columns
  dplyr::select(!c(Tribe, Host_Genus, Host_species, Host_Family, Host_Common_Name,
                   Host_Relation, Host_Location, Loc_Notes, Lat_Lon_Method,
                   End_Date, Elev_Det, Macro_Habitat, Micro_Habitat, Pres_Method, Spec_Notes,
                   Lat_Lon_Accuracy)) %>%
  # Remove double white-spaces
  apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
  # add the database_id column
  dplyr::mutate(
    database_id = paste("CAES_data_", 1:nrow(.), sep = ""),
    .before = catalogNumber)  %>%
  dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "Connecticut Agricultural Experiment Station",
    datasetID = "CAES"
  )

  #### 7.3 Out ####
# Save the dataset
readr::write_excel_csv(CAES_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
return(CAES_data)
} # END readr_CAES






#### 9.0 KP ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_KP <- function(path = NULL,
                     inFile = NULL,
                     outFile = NULL,
                     dataLicense = NULL){
  # locally bind variables to the function
  .<-ID<-Family<-Subfamily<-Genus<-Det<-Number<-Collection_method<-Collector<-Order<-Suborder<-
    subgenus<-species<-subspecies<-author<-whole_sci_name<-Country<-State<-County_Parish<-
    Location<-Lat<-Long<-decimalLatitude<-decimalLongitude<-Tribe<-sp_group<-Male<-Female<-
    genus<-specificEpithet<-infraspecificEpithet<-Collection_date<-catalogNumber <- NULL
  
  #### 9.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 9.2 Read+ ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
KP_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/")) %>%
    # Turn spaces into "_" in column names
    dplyr::rename_with(., ~ gsub(" ", "_", .x, fixed = TRUE)) %>%
    # Make columns DarwinCore-compatible
    dplyr::rename(
      catalogNumber = ID,
      family = Family,
      subfamily = Subfamily,
      genus = Genus,
      identifiedBy = Det,
      individualCount = Number,
      samplingProtocol = Collection_method,
      recordedBy = Collector,
      order = Order,
      suborder = Suborder,
      subgenus = subgenus,
      specificEpithet = species,
      infraspecificEpithet = subspecies,
      scientificNameAuthorship = author,
      verbatimScientificName = whole_sci_name,
      country = Country,
      stateProvince = State,
      county = County_Parish,
      locality = Location,
      decimalLatitude = Lat,
      decimalLongitude = Long)  %>%
    # round the coordinates to six decimal places
  dplyr::mutate(
    decimalLatitude = decimalLatitude %>% as.numeric() %>% base::round(digits = 6) %>%
      suppressWarnings(classes = "warning"),
    decimalLongitude = decimalLongitude %>% as.numeric() %>% base::round(digits = 6) %>%
      suppressWarnings(classes = "warning")) %>%
    # Add a bunch of columns from other columns
  dplyr::mutate(
    previousIdentifications = stringr::str_c(
      dplyr::if_else(!is.na(Tribe),
                     paste0("Tribe: ", Tribe), ""),
      dplyr::if_else(!is.na(sp_group),
                     paste0("sp_group: ", sp_group), ""),
      sep = "|") %>%
      # Remove extra bars "|".
      stringr::str_replace_all(pattern = "(\\|){2,9}",
                               replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                               replacement = "")) %>%
    # Do the same as the last mutate, but for fieldNotes
    dplyr::mutate(
      sex = stringr::str_c(
        dplyr::if_else(!is.na(Male) & Male != 0,
                       paste0(Male, " M"), ""),
        dplyr::if_else(!is.na(Female) & Female != 0,
                       paste0(Female, " F"), ""),
        sep = "|") %>%
        # Remove extra bars "|".
        # Remove extra bars "|".
        stringr::str_replace_all(pattern = "(\\|){2,9}",
                                 replacement = "\\|") %>%
        stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                                 replacement = "")) %>%
    # Format eventDate and add dataSource
    dplyr::mutate(
      # Create scientificName
      scientificName = stringr::str_c(
        dplyr::if_else(!is.na(genus),
                       paste0(genus), ""),
        dplyr::if_else(!is.na(specificEpithet),
                       paste0(specificEpithet), ""),
        dplyr::if_else(!is.na(infraspecificEpithet),
                       paste0(infraspecificEpithet), ""),
        sep = " ") %>% stringr::str_squish() %>% stringr::str_trim(side = "both"),
      # Format eventDate
      eventDate = lubridate::ymd_hms(Collection_date,
                                 truncated = 5, quiet = FALSE, tz = "UTC"), # 215 failed to parse. 
      dataSource = "KP_Anthophila") %>%
    # Remove those now redundant columns
    dplyr::select(!c(Male, Female, sp_group, Tribe, Collection_date)) %>%
    # Remove double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("KP_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
  # add the database_id column
  dplyr::mutate(
    datasetName = "USDA ARS Southeastern USA",
    datasetID = "KP"
  )
  
  #### 9.3 Out ####
  # Save the dataset
  readr::write_excel_csv(KP_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(KP_data)
} # END readr_KP










#### 11.0 EcoS ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_EcoS <- function(path = NULL,
                     inFile = NULL,
                     outFile = NULL,
                     dataLicense = NULL){
  # locally bind variables to the function
  Species <- . <- scientificName <- Latitude <- Longitude <- Year <- catalogNumber <- NULL
  Collection <- ID_project <- NULL
  
  #### 11.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 11.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  EcoS_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                                trim_ws = TRUE, guess_max = 33000) %>%
      # Add institution information
    dplyr::mutate(
      institutionCode = Collection,
      datasetName = Collection,
      catalogNumber = ID_project,
      otherCatalogNumbers = stringr::str_c(Collection, ID_project, sep = "_")) %>%
  # Add taxonomic information
    dplyr::mutate(
      scientificName = Species) %>%
      # Split genus and species names
    tidyr::separate(
      data = ., col = scientificName,
      into = c("genus", "specificEpithet"),
               sep = "_", remove = FALSE) %>%
      # Lat Lon
    dplyr::mutate(
      decimalLatitude = Latitude,
      decimalLongitude = Longitude) %>%
      # Year
    dplyr::mutate(
      year = Year) %>%
      # Add dataset information
    dplyr::mutate(dataSource = "EcoS_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("EcoS_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # keep only valid columns
    dplyr::select( tidyselect::any_of(names(ColTypeR()[[1]])))  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "EcoSur",
      datasetID = "EcoS"
    )
    
  
  #### 11.3 Out ####
  # Save the dataset
  readr::write_excel_csv(EcoS_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(EcoS_data)
} # END readr_EcoS








#### 12.0 GeoL ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_GeoL <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  # locally bind variables to the function
  geolocate_Latitude<-geolocate_Longitude<-geolocate_UncertaintyRadiusMeters<-database_id<-
    bels_decimallatitude<-bels_decimallongitude<-bels_coordinateuncertaintyinmeters<-datasource<-
    scientificname<-infraspecificepithet<-specificepithet<-acceptednameusage<-taxonrank<-
    scientificnameauthorship<-countrycode<-stateprovince<-eventdate<-basisofrecord<-
    occurrencestatus<-recordnumber<-recordedby<-eventid<-samplingprotocol<-samplingeffort<-
    individualcount<-catalognumber<-rightsholder<-institutioncode<-datasetname<-
    othercatalognumbers<-occurrenceid<-coreid<-recordid<-collectionid<-
    verbatimscientificname<-verbatimeventdate<-id <- . <- NULL
  rightsHolder <- continent <- type <- samplingProtocol <- NULL
  island <- municipality <- verbatimEventDate <- catalogNumber <- NULL
  
  #### 12.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 12.2 Read+ ####
    ###### a. GeoL_high ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
  GeoL_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                               sheet = "GEOLOCATE HIGH") %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
      # Convert GeoLocate columns into dwc columns
    dplyr::rename(
      decimalLatitude = geolocate_Latitude, decimalLongitude = geolocate_Longitude,
      coordinateUncertaintyInMeters = geolocate_UncertaintyRadiusMeters) %>%
    # keep only valid columns
    dplyr::select( tidyselect::any_of(names(ColTypeR()[[1]]))) %>%
      # Remove blanks
    tidyr::drop_na(database_id) %>%
      # Temporarily add an identifier column
    dplyr::mutate(
      tempSource = "GeoL",
      rightsHolder = rightsHolder  %>% as.character(),
      island = island %>% as.character(),
      municipality = municipality %>% as.character(),
      verbatimEventDate = verbatimEventDate %>% as.character()
    )
  # User output
  writeLines(paste0(
    " - We have read in ", 
    format(nrow(GeoL_data), big.mark = ","), " occurrence records from the 'GEOLOCATE HIGH' sheet." 
  ))
  
  ###### b. BELS_high ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
  BELS_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                  sheet = "BELS High") %>%
    # Convert GeoLocate columns into dwc columns
    dplyr::rename(
      decimalLatitude = bels_decimallatitude, 
      decimalLongitude = bels_decimallongitude,
      coordinateUncertaintyInMeters = bels_coordinateuncertaintyinmeters,
      dataSource = datasource,
      scientificName = scientificname,
      infraspecificEpithet = infraspecificepithet,
      specificEpithet = "_specificepithet",
      species = specificepithet,
      acceptedNameUsage = acceptednameusage,
      taxonRank = taxonrank,
      scientificNameAuthorship = scientificnameauthorship,
      countryCode = countrycode,
      stateProvince = stateprovince,
      eventDate = eventdate,
      basisOfRecord = basisofrecord,
      occurrenceStatus = occurrencestatus,
      recordNumber = recordnumber,
      recordedBy = recordedby,
      eventID = eventid,
      samplingProtocol = samplingprotocol,
      samplingEffort = samplingeffort,
      individualCount = individualcount,
      catalogNumber = catalognumber,
      rightsHolder = rightsholder,
      institutionCode = institutioncode,
      datasetName = datasetname,
      otherCatalogNumbers = othercatalognumbers,
      occurrenceID = occurrenceid,
      coreid = coreid,
      recordId = recordid,
      collectionID = collectionid,
      verbatimScientificName = verbatimscientificname,
      verbatimEventDate = verbatimeventdate,
      id = id) %>%
      # Correct some formatting
    dplyr::mutate(continent = continent %>% as.character(),
                  type = type %>% as.character(),
                  id = id %>% as.character(),
                  samplingProtocol = samplingProtocol %>% as.character(),
                  island = island %>% as.character(),
                  municipality = municipality %>% as.character(),
                  verbatimEventDate = verbatimEventDate %>% as.character()) %>%
    # keep only valid columns
    dplyr::select( tidyselect::any_of(names(ColTypeR()[[1]]))) %>%
    # Remove blanks
    tidyr::drop_na(database_id) %>%
    # Temporarily add an identifier column
    dplyr::mutate(
      tempSource = "Bels"
    )
  # User output
  writeLines(paste0(
    " - We have read in ", 
    format(nrow(BELS_data), big.mark = ","), " occurrence records from the 'BELS High' sheet." 
  ))
  
  ###### c. merge ####
  GeoL_data <- GeoL_data %>%
      # Remove data that occurs in BELS_data
    dplyr::filter(!database_id %in% BELS_data$database_id) %>%
      # Combine datasets 
    dplyr::bind_rows(BELS_data) %>%
    dplyr::select(!database_id) %>%
    # add the database_id column
    dplyr::mutate(
      database_id = paste("GEOL_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)
    
    # User output
  writeLines(paste0(
    " - We have kept ", 
    format(sum(GeoL_data$tempSource == "GeoL", na.rm = FALSE), big.mark = ","), 
    " occurrences from GeoLocate, and ",
    format(sum(GeoL_data$tempSource == "Bels", na.rm = FALSE), big.mark = ","),
    " records from BELS (",
    format(nrow(GeoL_data), big.mark = ","),
    " in total). BELS was given preference over GeoLocate"
  ))
    
  #### 12.3 Out ####
  # Save the dataset
  readr::write_excel_csv(GeoL_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(GeoL_data)
} # END readr_GeoL









#### 13.0 EaCO ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_EaCO <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  # locally bind variables to the function
  County<-State<-Genus<-genus<-species<-dateRange<-dateSet<-dateCollected<-treatment<-
    trapNumber<-samplingRound<-coordinates<-decimalLatitude<-decimalLongitude<-recordNumber<-
    .<-catalogNumber <- NULL
  
  #### 13.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 13.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  EaCO_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/")) %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
    # Rename columns
    dplyr::rename(
      recordNumber = 'Specimen Number',
      county = County,
      stateProvince = State,
      genus = Genus,
      species = 'Species (if available)',
      dateRange = 'Date Range for collection',
      treatment = 'Treatment type',
      trapNumber = 'Trap number',
      samplingRound = 'Sampling Round',
      coordinates = 'GPS Coordinates of Traps') %>%
    # Drop rows without species names or that aren't bees or aren't useable names
    dplyr::filter(!is.na(genus), !genus == "",
                  !is.na(species), !species == "",
                  !stringr::str_detect(species, pattern = "[0-9]"),
                  !stringr::str_detect(species, pattern = "/"),
                  !stringr::str_detect(tolower(species), pattern = "sp.")) %>%
      # Split dates
    tidyr::separate(
      col = dateRange, sep = "-",
      into = c("dateSet", "dateCollected")) %>%
      # Add year and convert to ymd format
    dplyr::mutate(
      dateSet = stringr::str_c(dateSet, "/2017") %>% lubridate::mdy(truncated = 2),
      dateCollected = stringr::str_c(dateCollected, "/2017") %>% lubridate::mdy(truncated = 2),
      year = 2017) %>%
      # Add scientificName
    dplyr::mutate(scientificName = stringr::str_c(genus, species, sep = " ")) %>%
      # Add field notes
    dplyr::mutate(
      fieldNotes = stringr::str_c(
        dplyr::if_else(!is.na(dateSet), paste0("dateSet: ", dateSet), ""),
        dplyr::if_else(!is.na(dateCollected),
                       paste0("dateCollected: ", dateCollected), ""),
        dplyr::if_else(!is.na(treatment),
                       paste0("treatmentType: ", treatment), ""),
        dplyr::if_else(!is.na(trapNumber),
                       paste0("trapNumber: ", trapNumber), ""),
        dplyr::if_else(!is.na(samplingRound),
                       paste0("samplingRound: ", samplingRound), ""),
        sep = "|") %>%
        # Remove extra bars "|".
        stringr::str_replace_all(pattern = "(\\|){2,9}",
                                 replacement = "\\|") %>%
        stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                                 replacement = "")) %>%
      # Add samplingEffort
    dplyr::mutate(
      samplingEffort = dateCollected - dateSet
    ) %>%
      # rescue coordinates
    tidyr::separate(
      col = coordinates,
      into = c("decimalLatitude", "decimalLongitude"),
      sep = ",") %>%
    dplyr::mutate(
      decimalLatitude = decimalLatitude %>% stringr::str_remove(pattern = "[a-zA-Z]"),
      decimalLongitude = decimalLongitude %>% stringr::str_remove(pattern = "[a-zA-Z]")) %>%
    dplyr::mutate(
      catalogNumber = stringr::str_c("EastColarado_",recordNumber, sep = "")) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "EaCO_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("EaCO_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # keep only valid columns
    dplyr::select( tidyselect::any_of(names(ColTypeR()[[1]])))  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Eastern Colorado (Arathi Seshadri)",
      datasetID = "EaCo",
      institutionCode = "USDA ARS"
    )
  
  
  #### 13.3 Out ####
  # Save the dataset
  readr::write_excel_csv(EaCO_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(EaCO_data)
} # END readr_EaCO








#### 14.0 MABC ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_MABC <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = "Hoja1"){
  # locally bind variables to the function
  genus <- specificEpithet <- collectionSite <- siteCode <- hour <- tribe <- eventDate <- NULL
  . <- catalogNumber <- NULL
  
  #### 14.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 14.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  MABC_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                  sheet = sheet) %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
    # Rename columns
    dplyr::rename(
      catalogNumber = 'Ejemplar',
      eventDate = 'Fecha colecta',
      country = paste0('Pa\u00eds'),
      stateProvince = 'Estado/Provincia',
      municipality = 'Municipio',
      locality = 'Localidad',
      samplingProtocol = 'Metodo colecta',
      decimalLatitude = 'Coordenadas Lat',
      decimalLongitude = 'Coordenadas Long',
      verbatimElevation = 'Altitud',
      georeferenceVerificationStatus = paste0('Datos georeferenciaci\u00f3n'),
      recordedBy = 'Colector',
      identifiedBy = 'Identificador',
      family = 'Familia',
      subfamily = 'Subfamilia',
      genus = 'Genero',
      subgenus = 'Subgenero',
      specificEpithet = 'Especie',
      infraspecificEpithet = 'Subespecie',
      species = 'Nombre especie',
      taxonID = paste0('C\u00f3digo especie'),
      sex = 'Sexo',
      # Non-standard fields
      tribe = 'Tribu',
      collectionSite = 'Sitio Colecta',
      siteCode = paste0('C\u00f3digo sitio'),
      hour = 'Hora') %>%
    # Add scientificName
    dplyr::mutate(scientificName = stringr::str_c(genus, specificEpithet, sep = " ")) %>%
    # Add field notes
    dplyr::mutate(
      fieldNotes = stringr::str_c(
        dplyr::if_else(!is.na(collectionSite), paste0("collectionSite: ", collectionSite), ""),
        dplyr::if_else(!is.na(siteCode),
                       paste0("siteCode: ", siteCode), ""),
        dplyr::if_else(!is.na(hour),
                       paste0("time: ", hour), ""),
        sep = "|") %>%
        # Remove extra bars "|".
        stringr::str_replace_all(pattern = "(\\|){2,9}",
                                 replacement = "\\|") %>%
        stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                                 replacement = "")) %>%
    # Add identificationRemarks
    dplyr::mutate(
      identificationRemarks = stringr::str_c(
        dplyr::if_else(!is.na(tribe), paste0("tribe: ", tribe), ""))) %>%
  # Add year and ensure ymd format
  dplyr::mutate(eventDate = lubridate::dmy(eventDate))  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "MABC_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("MABC_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # keep only valid columns
    dplyr::select( tidyselect::any_of(names(ColTypeR()[[1]])))  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "MABC",
      datasetID = "MABC"
    )
  
  
  #### 14.3 Out ####
  # Save the dataset
  readr::write_excel_csv(MABC_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(MABC_data)
} # END readr_MABC






#### 15.0 Col ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Col <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                      sheet = sheet){
  # locally bind variables to the function
  day <- year <- eventDateInitial <- eventDate <- month2 <- day2 <- eventDate2 <- NULL
  scientificName <- . <- catalogNumber <-month<- NULL
  
  #### 15.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 15.2 Read+ ####
    ###### a. Col_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  Col_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                 sheet = sheet) %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
      # Fix some special cases that already involve "." in the column names
    stats::setNames(., stringr::str_replace_all(colnames(.), "  ", ". ")) %>%
    stats::setNames(., stringr::str_replace_all(colnames(.), " $", ".")) %>%
    # Rename columns
    dplyr::rename(
      catalogNumber = paste0('C\u00f3digo de Barras'),
      recordedBy = 'Colectores [Aggregated]',
      recordedByID = 'Colectores asociados',
      eventDateInitial = paste0('Fecha colecci\u00f3n inicial'),
      order = 'Orden',
      family = 'Familia',
      genus = paste0('G\u00e9nero'),
      specificEpithet = 'Especie',
      scientificNameAuthorship = 'Especie Author',
      typeStatus = 'Tipo',
      identifiedBy = 'Determinador [Formatted]',
      dateIdentified = paste0('Fecha determinaci\u00f3n'),
      country = paste0('Pa\u00eds'),
      stateProvince = 'Departamento',
      municipality = 'Municipio',
      locationRemarks = 'Corregimiento Departamental',
      locality = 'Localidad',
      decimalLatitude = 'Latitud georref. dec.',
      decimalLongitude = 'Longitud georref. dec.',
      scientificName = 'Nombre Completo',
      day = "dia",
      month = "mes",
      year = "Ano"
      # Previous column names:
        # collectionID = 'Colecci\\u00f3n/Guid',
        # collectionCode = 'C\\u00f3digo',
        # occurrenceID = 'Collection Object/GUID',
        # identificationRemarks = 'Observaciones generales',
        # eventID = 'Evento de Recolecci\\u00f3n/Guid',
        # recordedByID = 'Numero de colector',
        # eventDateFinal = 'Fecha colecci\\u00f3n final',
        # class = 'Clase',
        # infraspecificEpithet = 'Subespecie',
        # namePublishedInID = 'Referencia original',
        # identificationID = 'Determinaciones/Guid',
      # identificationQualifier = 'Obs. Determinaci\\u00f3n',
        # locationID = 'LocalityID',
        # geodeticDatum = 'Datum geod\\u00e9sico',
        # coordinateUncertaintyInMeters = 'Precisi\\u00f3n coord. georref.',
        # minimumElevationInMeters = 'Elevaci\\u00f3n m\\u00ednima',
        # maximumElevationInMeters = 'Elevaci\\u00f3n m\\u00e1xima',
        # georeferenceProtocol = 'Protocolo de georreferenciaci\\u00f3n',
        # verbatimLatitude = 'Informaci\\u00f3n geogr\\u00e1fica/Latitude1 literal',
        # verbatimLongitude = 'Longitude1 literal',
        # locationRemarks = 'Observaciones ninf Geogr\\u00e1fica',
        # lifeStage = 'Estado de desarrollo',
        # habitat = 'H\\u00e1bitat',
        # continent = 'Continente',
        # tribe = 'Tribu',
        # subSpeciesAuthor = 'Subespecie Author'
    ) %>%
    # Fix some date issues:
      # a. replace 00 dates with "" to be picked u pby truncated
    dplyr::mutate(
      day = dplyr::if_else(day == "0" | day == "00",
                           "", day) %>%
        stringr::str_remove("to.*|^0") %>%
        as.numeric(na.rm = TRUE),
      month = dplyr::if_else(month == "0" | month == "00",
                           "", month) %>%
        stringr::str_remove("^0") %>%
        as.numeric(na.rm = TRUE),
      year = year %>% as.numeric()
    ) %>%
    dplyr::mutate(
      eventDate = lubridate::ymd(paste(year, month, day, sep = "/"), 
                                 truncated = 2),
      .after = eventDateInitial) %>%
    # b. Fix inverted day-month
  dplyr::mutate(
    day2 = dplyr::if_else(is.na(eventDate),
                         month,
                         1),
    month2 = dplyr::if_else(is.na(eventDate),
                          day,
                          1),
    eventDate2 = dplyr::if_else(is.na(eventDate),
      lubridate::ymd(paste(year, month2, day2, sep = "/"), 
                                truncated = 2),
      eventDate),
    .after = year
  ) %>%
    # Combine into final date values
    dplyr::mutate(
        # Take eventDate if it's not empty, and eventDate2 if it is empty
      eventDate = dplyr::if_else(is.na(eventDate),
                                 eventDate2,
                                 eventDate),
      day = dplyr::if_else(is.na(day),
                           day2,
                            day),
      month = dplyr::if_else(is.na(month),
                             month2,
                             month)) %>%
    dplyr::select(!c(eventDate2, day2, month2)) %>%
    # Remvoe extra spaces in subgenus
    dplyr::mutate(scientificName = scientificName %>%
                    stringr::str_replace(pattern = "\\( ",
                                         replacement = "\\(")) %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Col_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "Col_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Colombia - Diego Alexander Guevara Farias",
      datasetID = "Col"
    )
  
  
  #### 15.3 Out ####
  # Save the dataset
  readr::write_excel_csv(Col_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Col_data)
} # END readr_Col






#### 16.0 FSCA ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_FSCA <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  # locally bind variables to the function
  . <- catalogNumber <- recordID <- NULL
  
  #### 16.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 16.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  FSCA_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                                  trim_ws = TRUE) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "FSCA_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("FSCA_data_", 1:nrow(.), sep = ""),
      .before = 1)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # keep only valid columns
    dplyr::select( tidyselect::any_of(names(ColTypeR()[[1]]))) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Florida State Collection of Arthropods",
      datasetID = "FSCA",
      institutionCode = "FSCA"
    )
  
  
  #### 16.3 Out ####
  # Save the dataset
  readr::write_excel_csv(FSCA_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(FSCA_data)
} # END readr_FSCA




#### 17.0 SMC ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_SMC <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  collectionMethod <- locale <- latitude <- longitude <- organismName <- scientificName <- NULL
  observationDate <- eventDate <- . <- NULL
  
  #### 17.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  #### 17.2 Read+ ####
  SMC_Data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE, guess_max = 33000) %>%
    # Rename some columns to make format consistent with DarwinCore
    dplyr::rename(
      samplingProtocol = collectionMethod,
      locality = locale,
      decimalLatitude =	latitude,
      decimalLongitude = longitude,
      scientificName = organismName) %>%
    # Make new columns
    dplyr::mutate(
      # Remove underscore from scientificName strings
      scientificName = gsub("_", " ", scientificName),
      # Add basis of record
      basisOfRecord =	"specimen",
      # Add eventDate
      eventDate = lubridate::mdy(observationDate),
      # Parse eventDate into day, month, and year
      month = lubridate::month(eventDate),
      day = lubridate::day(eventDate),
      year = lubridate::year(eventDate)) %>%
    # Add dataSource information
    dplyr::mutate(dataSource = "SMC_Anthophila") %>%
    # add the database_id column
    dplyr::mutate(
      database_id = paste("SMC_data_", 1:nrow(.), sep = "")) %>%
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Texas SMC literature data",
      datasetID = "SMC"
    )
  # Save the dataset
  readr::write_excel_csv(SMC_Data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(SMC_Data)
} # END readr_SMC




#### 18.0 Bal ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Bal <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL,
                      sheet = "animal_data"){
  # locally bind variables to the function
  siteID<-year<-animalID<-abundance<-samplingMethod<-censusType<-decimalLatitude<-
    decimalLongitude<-studyLocation<-siteDescription<-studyID<-locationID<-.<-
    samplingIntensity<-eventDate<-catalogNumber <- NULL
  
  #### 18.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 18.2 Read+ ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
  Bal_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                 sheet = sheet, startRow = 2) %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
    # Make columns DarwinCore-compatible
    dplyr::rename(
      locationID = siteID,
      year = year,
      eventDate = date,
      scientificName = animalID,
      individualCount = abundance,
      samplingProtocol = samplingMethod,
      fieldNotes = censusType,
      decimalLatitude = decimalLatitude,
      decimalLongitude = decimalLongitude,
      locality = studyLocation,
      locationRemarks = siteDescription) %>%
    # Add some columns
    dplyr::mutate(
      catalogNumber = stringr::str_c(studyID, "_", locationID,"_", 1:nrow(.), sep = ""),
      samplingEffort = stringr::str_c(samplingIntensity, " hours")
    ) %>%
    # round the coordinates to six decimal places
    dplyr::mutate(
      decimalLatitude = decimalLatitude %>% as.numeric() %>% base::round(digits = 6) %>%
        suppressWarnings(classes = "warning"),
      decimalLongitude = decimalLongitude %>% as.numeric() %>% base::round(digits = 6) %>%
        suppressWarnings(classes = "warning")) %>%
    # Format eventDate and add dataSource
    # Format eventDate
    dplyr::mutate(
      eventDate = lubridate::dmy(eventDate,
                                 truncated = 2, quiet = FALSE),
      # Parse eventDate into day, month, and year
      month = lubridate::month(eventDate),
      day = lubridate::day(eventDate),
      year = lubridate::year(eventDate),
      dataSource = "Bal_Anthophila") %>%
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Bal_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Ballare et al. 2019",
      datasetID = "Bal"
    )
  
  #### 18.3 Out ####
  # Save the dataset
  readr::write_excel_csv(Bal_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Bal_data)
} # END readr_Bal







#### 19.0 Lic ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Lic <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  Kingdom<-Order<-Family_or_grp<-Genus<-Species<-sex<-Collector<-Determiner<-genus<-species<-
    occurrenceID<-eventID<-eventDate<-.<-catalogNumber<-family <- NULL
  
  #### 19.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 19.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  Lic_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE, guess_max = 33000) %>%
    dplyr::rename(
      kingdom = Kingdom,
      order = Order,
      family = Family_or_grp,
      genus = Genus,
      species = Species,
      sex = sex,
      recordedBy = Collector,
      identifiedBy = Determiner) %>%
    # Add taxonomic information
    dplyr::mutate(
      scientificName = stringr::str_c(
        dplyr::if_else(!is.na(genus),
                       paste0(genus), ""),
        dplyr::if_else(!is.na(species),
                       paste0(species), ""),
        sep = " ")) %>%
    # Make a catalogue number
    dplyr::mutate(
      catalogNumber = stringr::str_c(
        dplyr::if_else(!is.na(occurrenceID),
                       paste0(occurrenceID), ""),
        dplyr::if_else(!is.na(eventID),
                       paste0(eventID), ""),
        sep = "_")) %>%
    # Format eventDate
    dplyr::mutate(
      eventDate = lubridate::dmy(eventDate,
                                 truncated = 2, quiet = FALSE),
      # Parse eventDate into day, month, and year
      month = lubridate::month(eventDate),
      day = lubridate::day(eventDate),
      year = lubridate::year(eventDate)) %>% 
    # Add dataset information
    dplyr::mutate(dataSource = "Lic_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Lic_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Elinor Lichtenberg Canola Data",
      datasetID = "Lic"
    )
  
  # filter to bee families only
    Lic_data <- Lic_data %>%
      dplyr::filter(tolower(family) %in% 
                      tolower(c("Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae",
                                "Melittidae","Stenotritidae")))
  
  
  #### 19.3 Out ####
  # Save the dataset
  readr::write_excel_csv(Lic_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Lic_data)
} # END readr_Lic






#### 20.0 Arm ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Arm <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = "Sheet1"){
  # locally bind variables to the function
  fam<-genus<-sp<-species<-sex<-locality<-munic<-state<-y<-x<-elev<-specificEpithet<-ecoregion<-
    veget<-g<-m<-s<-G<-M<-S<-day<-year<-.<-family <- month<-NULL
  
  #### 20.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 20.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  Arm_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"), sheet = sheet) %>%
    # Make columns DarwinCore-compatible
    dplyr::rename(
      family = fam,
      genus = genus,
      specificEpithet = sp,
      species = species,
      sex = sex,
      locality = locality,
      municipality = munic,
      stateProvince = state,
      decimalLatitude = y,
      decimalLongitude = x,
      verbatimElevation = elev)  %>%
    # Add a bunch of columns from other columns
    dplyr::mutate(
      # Add scientificName
      # This will ONLY concatenate columns where they have a value.
      scientificName = stringr::str_c(
        dplyr::if_else(!is.na(genus), genus, ""),
        dplyr::if_else(!is.na(specificEpithet),
                       specificEpithet, ""),
        sep = " ") ) %>%
    # Do the same as the last mutate, but for fieldNotes
    dplyr::mutate(
      fieldNotes = stringr::str_c(
        dplyr::if_else(!is.na(ecoregion),
                       paste0("ecoregion: ", ecoregion), ""),
        dplyr::if_else(!is.na(veget),
                       paste0("vegetation: ", veget), ""),
        sep = "|") %>%
        # Remove extra bars "|".
        stringr::str_replace_all(pattern = "(\\|){2,9}",
                                 replacement = "\\|") %>%
        stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                                 replacement = "")) %>%
    # Add scientificName
    dplyr::mutate(
      verbatimLatitude = stringr::str_c(
        dplyr::if_else(!is.na(g),
                       as.character(g), ""),
        dplyr::if_else(!is.na(m),
                       as.character(m), ""),
        
        dplyr::if_else(!is.na(s),
                       as.character(s), ""),
        sep = " "),
      verbatimLongitude = stringr::str_c(
        dplyr::if_else(!is.na(G),
                       as.character(G), ""),
        dplyr::if_else(!is.na(M),
                       as.character(M), ""),
        dplyr::if_else(!is.na(S),
                       as.character(S), ""),
        sep = " ")) %>%
    # Get the coordinateUncertaintyInMeters by taking the UPPER uncertainty limit from the provided 
    # ranges. I.e., "100-1000" becomes "1000"
    dplyr::mutate(
      # Format eventDate
      eventDate = lubridate::dmy(paste(
        day, month, year, sep = "-"
      )), # 215 failed to parse. 
      dataSource = "Arm_Anthophila") %>%
    # Remove double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Arm_data_", 1:nrow(.), sep = ""),
      .before = family)  %>%
    dplyr::mutate(license = dataLicense) %>%
      # Remove spent columns
    dplyr::select(!tidyselect::any_of(c("veget", "ecoregion", "g", "m", "s", "G", "M", "S"))) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Armando Falcon-Brindis",
      datasetID = "Arm"
    )
  
  #### 20.3 Out ####
  # Save the dataset
  readr::write_excel_csv(Arm_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Arm_data)
} # END readr_Arm








#### 21.0 Dorey ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_Dor <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  . <- catalogNumber <- eventDate <- stateOrProvince <- NULL
  
  #### 21.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 21.2 Read+ ####
  Dor_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
    dplyr::rename(stateProvince = stateOrProvince) %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Dor_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "Dor_Anthophila") %>%
    # Format date
    dplyr::mutate(eventDate = eventDate %>% lubridate::dmy(., truncated = 2),
                  year = lubridate::year(eventDate),
                  month = lubridate::month(eventDate),
                  day = lubridate::day(eventDate)) %>%
    # Pick up dates of different formats and format together.
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "James B Dorey Bee Data",
      datasetID = "Dorey"
    )
  #### 21.3 Out ####
  # Save the dataset
  readr::write_excel_csv(Dor_data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(Dor_data)
} # END readr_Dor



#### 22.0 MEPB ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_MEPB <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = NULL){
  # locally bind variables to the function
  catalog_number<-pollinator_family<-pollinator_genus<-pollinator_species<-collection_method<-
    day_collected<-month_collected<-year_collected<-location_description<-latitude<-longitude<-
    basis_of_record<-genus<-specificEpithet<-year<-day<-eventDate<-collector_number<-
    location_name<-habitat<-.<-catalogNumber <- NULL
  
  #### 22.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 22.2 Read+ ####
  MEPB_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"), sheet = sheet)  %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
    # Fix broken encodings
    dplyr:: mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ stringr::str_replace_all(.,
                                 pattern = c("\\u00c3\\u00b3"="\\u00f3",  
                                             "\\u00c3\\u00a9"="\\u00e9", 
                                             "\\u00c3\\u00b1"="\\u00f1",
                                             "\\u00c2\\u00b0"="\\u00b0",
                                             "\\u00c3"="\\u00e1") 
        ))) %>%
    # add the database_id column
    dplyr::mutate(
      database_id = paste("MEPB_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "MEPB_Anthophila") %>%
    # Format date
    dplyr::mutate(eventDate = lubridate::ymd(stringr::str_c(year %>% as.numeric(), 
                                                            month %>% as.numeric(), 
                                                            day %>% as.numeric(), 
                                                            sep = "/"),
                                             truncated = 2)) %>%
    # Pick up dates of different formats and format together.
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetID = "MEPB"
    )
  #### 22.3 Out ####
  # Save the dataset
  readr::write_excel_csv(MEPB_data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(MEPB_data)
} # END readr_MEPB




#### 23.0 Brazil ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_BBD <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  # locally bind variables to the function
  . <- catalogNumber <- year <- day <- dateLastModified <- dateLastModified2 <- NULL
  identifiedBy <- Spcslink.identifiedby <-month <- NULL
  
  #### 23.1 Prep ####
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")


  
  #### 23.2 Read+ ####
  lookupCols <- c(verbatimScientificName = "Scientificname_ORIGINAL",
                  verbatimLatitude = "Lat_original",
                  verbatimLongitude = "Long_original")
  BBD_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
    # Rename columns
    dplyr::rename(
      tidyselect::any_of(lookupCols),
      id = "CodeBBdatabase_curated",
      scientificName = "Scientific name corrected",
      family = "Family",
      institutionCode = "institutioncode",
      # infraspecificEpithet = "Spcslink.subspecies",
      # scientificNameAuthorship = "Spcslink.scientificnameauthor",
      day = "Day",
      month = "Month",
      year = "Year",
      country = "Country",
      stateProvince = "State",
      decimalLatitude = "Latitude_dec.degrees",
      decimalLongitude = "Longitude_dec.degrees",
      coordinateUncertaintyInMeters = "Precision.of.coord.meters",
      verbatimLocality = "Locality.original",
      georeferenceRemarks = "NotasLatLong",
      county = "Spcslink.county", 
      recordedBy = "Collector",
      collectionCode = "Collection",
      references = "Source",
      sex = "Sex",
      identifiedBy = "Det_By",
      catalogNumber = "Codigo",
      otherCatalogNumbers = "Spcslink.collectioncode",
      dateIdentified = "Spcslink.yearidentified",
      dateLastModified = "Spcslink.datelastmodified",
      basisOfRecord = "Spcslink.basisofrecord",
      # occurrenceID = "Spcslink.collectioncode",
      eventRemarks = "NotesOnLocality") %>%
    #dplyr::mutate(
    #  decimalLatitude = decimalLatitude %>% stringr::str_replace("\\,", "\\."),
    #  decimalLongitude = decimalLongitude %>% stringr::str_replace("\\,", "\\."))
    # add the database_id column
    dplyr::mutate(
      database_id = paste("BBD_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber) %>%
    # Edit catalogNumber for AMNH
    dplyr::mutate(catalogNumber = stringr::str_replace(
      catalogNumber, pattern = "AMNHBEE", replacement = "AMNH_BEE"),
      catalogNumber = stringr::str_replace(
        catalogNumber, pattern = "^0\\.", replacement = ".")) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "BBD_Anthophila") %>%
    # Format date
    dplyr::mutate(eventDate = lubridate::ymd(paste0(
      year, month, day, sep = "/"), truncated = 2, quiet = TRUE)) %>%
    dplyr::mutate(dateLastModified2 = 
                    lubridate::dmy_hms(dateLastModified, truncated = 5, quiet = TRUE),
      .after = dateLastModified) %>%
    dplyr::mutate(dateLastModified2 = dplyr::if_else(is.na(dateLastModified2),
                                                         lubridate::ymd_hms(dateLastModified, truncated = 5, quiet = TRUE),
                                                     dateLastModified2)) %>%
    dplyr::mutate(dateLastModified2 = dplyr::if_else(is.na(dateLastModified2),
                                                   lubridate::mdy_hms(dateLastModified, truncated = 5, quiet = TRUE),
                                                   dateLastModified2)) %>%
    dplyr::mutate(dateLastModified2 = dplyr::if_else(is.na(dateLastModified2),
                                                   lubridate::ymd(dateLastModified, truncated = 2, quiet = TRUE),
                                                   dateLastModified2)) %>%
    dplyr::mutate(dateLastModified2 = dplyr::if_else(is.na(dateLastModified2),
                                                   as.Date(dateLastModified %>% as.numeric(na.rm = TRUE),
                                                           origin = "1899-12-30") %>%
                                                     lubridate::ymd_hms(., truncated = 5, quiet = TRUE),
                                                   dateLastModified2),
                                                   .after = dateLastModified) %>%
    dplyr::mutate(dateLastModified = dateLastModified2) %>%
    dplyr::select(!dateLastModified2) %>%
    # Pick up dates of different formats and format together.
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "BBD_Brazil",
      datasetID = "BBD"
    ) %>%
      # If identifiedBy is not filled where it should be, try the other column rpovided
    dplyr::mutate(identifiedBy = dplyr::if_else(is.na(identifiedBy),
                                                Spcslink.identifiedby,
                                                identifiedBy))
  
  
  #### 23.3 Out ####
  # Save the dataset
  readr::write_excel_csv(BBD_data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(BBD_data)
} # END readr_BBD




#### 24.0 MPUJ ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
readr_MPUJ <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL,
                      sheet = sheet){
  # locally bind variables to the function
  collector1stName <- collectorsLastName <- determined1stName <- determinedLastName <- NULL
  day <- year <- eventDate <- endDate <- . <- catalogNumber <- fieldNotes <- NULL
  `Start Date (Year)` <- `Start Date (Month)` <- `Start Date (Day)` <- month<-NULL
  
  #### 24.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  requireNamespace("lubridate")


  
  #### 24.2 Read+ ####
  ###### a. MPUJ_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  MPUJ_data <- openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                 sheet = sheet) %>%
      # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
    # Rename columns
    dplyr::rename(
      catalogNumber = 'Catalog Number',
      individualCount = "Count",
      otherCatalogNumbers = "Alt Cat Number",
      typeStatus = "Type Status",
      identificationQualifier = "Qualifier",
      sex = "Sex",
      samplingProtocol = "Method",
      habitat = "Habitat",
      continent = "Continent",
      country = "Country",
      stateProvince = "State",
      county = "County",
      locality = "Locality Name",
      minimumElevationInMeters = "Min Elevation",
      maximumElevationInMeters = "Max Elevation",
      fieldNotes = "Locality and Habitat Notes",
      decimalLongitude = "Latitude1",
      decimalLatitude = "Longitude1",
      verbatimLatitude = "Lat1text",
      verbatimLongitude = "Long1text",
      scientificName = "Full Name",
      kingdom = "Kingdom",
      order = "Order",
      family = "Family",
      subfamily = "Subfamily",
      genus = "Genus",
      specificEpithet = "Species",
      infraspecificEpithet = "Subspecies",
      scientificNameAuthorship = "Species Author",
      day = `Start Date (Day)`,
      month = `Start Date (Month)`,
      year = `Start Date (Year)`,
      associatedTaxa = "Associated Taxa",
      associatedOccurrences = "Associated Ocurrence",
      lifeStage = "Stage",
      collector1stName = 'Collectors/First Name',
      collectorsLastName = 'Collectors/Last Name',
      determined1stName = 'Determiner/First Name',
      determinedLastName = 'Determiner/Last Name',
      endDate = "End Date",
      verbatimEventDate = "Verbatim Date"
      ) %>%
    dplyr::mutate(year = year %>% as.numeric(),
                  month = month %>% as.numeric(),
                  day = day %>% as.numeric()) %>%
    dplyr::mutate(recordedBy = stringr::str_c(collector1stName, collectorsLastName,
                                              sep = " "),
                  identifiedBy = stringr::str_c(determined1stName, determinedLastName,
                                              sep = " ")) %>%
    dplyr::mutate(eventDate = lubridate::dmy(stringr::str_c(day, month, year, sep = "/"), 
                                             truncated = 2)) %>%
    dplyr::mutate(
      fieldNotes = 
        stringr::str_c(
          dplyr::if_else(!is.na(fieldNotes),
                         paste0("fieldNotes: ", fieldNotes), ""),
        dplyr::if_else(!is.na(eventDate),
                       paste0("startDate: ", eventDate), ""),
        dplyr::if_else(!is.na(endDate),
                       paste0("endDate: ", endDate), ""),
        sep = "|")
    ) %>%
    dplyr::mutate(basisOfRecord = "Preserved specimen") %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("MPUJ_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "MPUJ_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Colombia MPUJ - Diego Alexander Guevara Farias",
      datasetID = "MPUJ"
    )
  
  #### 24.3 Out ####
  # Save the dataset
  readr::write_excel_csv(MPUJ_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(MPUJ_data)
} # END readr_MPUJ



#### 25.0 STRI ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
#' 
readr_STRI <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  # locally bind variables to the function
  fieldNotes <- Catalognumber <- . <- day <- year <- catalogNumber <-  recordId <-month<- NULL
  
  #### 25.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  
  requireNamespace("lubridate")

  
  
  #### 25.2 Read+ ####
  ###### a. STRI_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  STRI_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                                  trim_ws = TRUE) %>% 
    dplyr::rename(recordId = "recordID") %>%
    # Rename columns
    dplyr::mutate(
      fieldNotes = stringr::str_c(
        dplyr::if_else(!is.na(Catalognumber),
                       paste0("secondary catalog #: ", Catalognumber), ""),
                       sep = "|"),
      species = "scientificName") %>%
      # Format dates
    dplyr::mutate(
        # If day is not recorded, set to first day of month
      day = dplyr::if_else(day == 0, 1, day),
      eventDate = lubridate::dmy(stringr::str_c(day, month, year, sep = "/"), 
                                             truncated = 2)) %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("STRI_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "STRI_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "STRI",
      datasetID = "STRI"
    )
  
  #### 25.3 Out ####
  # Save the dataset
  readr::write_excel_csv(STRI_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(STRI_data)
} # END readr_STRI




#### 26.0 PALA ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
#' 
readr_PALA <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  # locally bind variables to the function
  romanNumerals <- numeralConversion <- catalogNumber <- type <- country <- municipality <- NULL
  locality <- decimalLatitude <- decimalLongitude <- verbatimElevation <- verbatimEventDate <- NULL
  recordedBy <- collectionCode <- otherCatalogNumbers <- associatedTaxa <- taxonRemarks <- NULL
  family <- genus <- references <- specificEpithet <- scientificName <- . <- eventDate <- NULL
  
  #### 26.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  requireNamespace("lubridate")

  requireNamespace("mgsub")
  
  
  #### 26.2 Read+ ####
  ###### a. month strings ####
    # Prepare month strings to convert from roman numerals
  romanNumerals <- c("i","ii","iii","iv","v","vi","vii","viii","ix","x","xi","xii",
                     "I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")
  numeralConversion <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                         "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
  
  ###### b. PALA_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  PALA_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                               trim_ws = TRUE) %>%
  # Rename columns
    dplyr::rename(
      catalogNumber = "catalogNumber",
      type = "Type",
      country = "Country",
      municipality = "Muninciplaity",
      locality = "Site",
      decimalLatitude = "Latitud",
      decimalLongitude = "Longitude",
      verbatimElevation = "elevation",
      verbatimEventDate = "date",
      recordedBy = "recordedby",
      collectionCode = "Collection",
      otherCatalogNumbers = "othercatalognumber",
      associatedTaxa = "AssociatedTaxa",
      taxonRemarks = "taxonremarks",
      family = "Family",
      genus = "Genus",
      specificEpithet = "species",
      references = "Citation"
    ) %>%
      # Add in sciName
    dplyr::mutate(scientificName = stringr::str_c(genus, specificEpithet, sep = " ")) %>%
      # Format date
    dplyr::mutate(eventDate = verbatimEventDate %>%
                    mgsub::mgsub(
                      pattern = paste("[-/ \\.]", romanNumerals, "[ -/\\.]", sep = ""),
                      replacement = numeralConversion) %>%
                    lubridate::dmy(truncated = 2, quiet = TRUE),
                  .after = verbatimEventDate) %>% 
      # Add day, month, year
    dplyr::mutate(
      day = lubridate::day(eventDate),
      month = lubridate::month(eventDate),
      year = lubridate::year(eventDate),
      .after = eventDate
    ) %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("PALA_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "PALA_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "PALA",
      datasetID = "PALA"
    )
  
  #### 26.3 Out ####
  # Save the dataset
  readr::write_excel_csv(PALA_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(PALA_data)
} # END readr_PALA




#### 27.0 JoLa ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
#' 
readr_JoLa <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = c("pre-1950", "post-1950")){
  # locally bind variables to the function
  fieldNotes <- Catalognumber <-`Start Date (Year)` <- genus <- specificEpithet <-  NULL
  year <- . <- NULL
  
  #### 27.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  requireNamespace("lubridate")

  
  
  
  #### 27.2 Read+ ####
  ###### a. JoLa_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
    # Read in both sheets and bind them together
  JoLa_data <-  openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                   sheet = sheet[1]) %>% 
    dplyr::bind_rows(openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                        sheet = sheet[2])) %>%
    # Return spaces in column names to keep the consistent with file before renaming
    stats::setNames(., stringr::str_replace_all(colnames(.), "\\.", " ")) %>%
      # Rename the columns
    dplyr::rename(
      specificEpithet = "Species",
      decimalLatitude = "Latitude1",
      decimalLongitude = "Longitude1",
      year = `Start Date (Year)`
    ) %>%
    # Add in higher taxonomic information
    dplyr::mutate(
      genus = "Lasioglossum",
      family = "Halictidae",
      order = "Hymenoptera",
      scientificName = stringr::str_c(genus, specificEpithet, sep = " ")
    ) %>%
    dplyr::mutate(
      eventDate = lubridate::ymd(year, truncated = 2)
    ) %>%
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("JoLa_data_", 1:nrow(.), sep = ""),
      .before = 1)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "JoLa_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "JoLa",
      datasetID = "JoLa"
    )
  
  #### 27.3 Out ####
  # Save the dataset
  readr::write_excel_csv(JoLa_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(JoLa_data)
} # END readr_JoLa



#### 28.0 VicWam ####
#' @rdname readr_BeeBDC
#' 
#' 
#' 
#'
#' 
readr_VicWam <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = "Combined"){
  # locally bind variables to the function
  fieldNotes <- Catalognumber <- recordNumber <- genus <- specificEpithet <-  NULL
  year <- . <- VicWam_data <- otherCatalogNumbers <- institutionCode <- class <- order <- NULL
  infraspecificEpithet <- NULL
  NEAREST <- DISTANCE <- DIST_UNIT <- DIRECTION <- locality <- locality2 <- PLACEACCURACY <- 
    coordinateUncertaintyInMeters <- DTFR <- day <- month <- year <- eventDate <- eventDate2 <- 
    DTTO <- dayTO <- monthTO <- yearTO <- eventDateTO <- eventDateTO2 <- LABELFAMILY <- 
    LABELGENUS <- LABELSPECIES <- associatedTaxa <- stateProvince <- country <- dataSource <- 
    license <- decimalLatitude <- decimalLongitude <- lengthTest <- decimalLatitude2 <- 
    decimalLongitude2 <- NULL
  
  #### 28.1 Prep ####
  # This will load the requireNamespaced packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  
  
  #### 28.2 Read+ ####
  ###### a. JoLa_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  # Read in both sheets and bind them together
  VicWam_data <-  openxlsx::read.xlsx(paste(path, inFile, sep = "/"),
                                    sheet = sheet[1]) %>%
      # Start by renaming columns
    dplyr::rename(
      recordNumber = "Reg_DoreyExtension",
      otherCatalogNumbers = "COLLNUM",
      institutionCode = "INSTITUTE",
      class = "CLASS",
      order = "ORDER",
      "superfamily" = "SUPERFAMILY",
      "family" = "FAMILY",
      "subfamily" = "SUBFAMILY",
      "tribe" = "TRIBE",
      "genus" = "GENUS",
      "subgenus" = "SUBGENUS",
      "specificEpithet" = "SPECIES",
      "infraspecificEpithet" = "SUBSPECIES",
      "identifiedBy" = "DTMNDBY",
      "dateIdentified" = "DTMNDDT",
      "lifeStage" = "LIFEHISTORY",
      "sex" = "SEX",
      "identificationQualifier" = "NAMEQUALIFIER",
      "typeStatus" = "SPCMTYPE",
      "individualCount" = "SPECNUM",
      "stateProvince" = "STATE",
      "locality" = "SITE",
      "decimalLatitude" = "LATDEC",
      "decimalLongitude" = "LONGDEC",
      "recordedBy" = "COLLTOR",
      "basisOfRecord" = "STORAGE",
      "verbatimLatitude" = "LATITUDE",
      "verbatimLongitude" = "LONGITUDE",
      "occurrenceRemarks" = "NOTES"
    ) %>%
      # Create the scientificName
    dplyr::mutate(scientificName = stringr::str_c(
      dplyr::if_else(!is.na(genus),
                     paste0(genus), ""),
      dplyr::if_else(!is.na(specificEpithet),
                     paste0(specificEpithet), ""),
      dplyr::if_else(!is.na(infraspecificEpithet),
                     paste0(infraspecificEpithet), ""),
      sep = " "
    ) %>% stringr::str_squish()) %>%
      # modify locality
    dplyr::mutate(locality2 = stringr::str_c(dplyr::if_else(stats::complete.cases(NEAREST),
                                                             NEAREST, ""),
                                              dplyr::if_else(stats::complete.cases(DISTANCE),
                                                             as.character(DISTANCE), ""),
                                              dplyr::if_else(stats::complete.cases(DIST_UNIT),
                                                             DIST_UNIT, ""),
                                              dplyr::if_else(stats::complete.cases(DIRECTION),
                                                             paste0(DIRECTION, "\u00b0"), 
                                                             ""),
                                              sep = " ") %>% as.character() %>%
                    stringr::str_squish(),
                  .after = locality) %>%
    dplyr::mutate(locality = dplyr::if_else(is.na(locality),
                                            locality2, locality)) %>%
    dplyr::select(!locality2) %>%
    # Extract coordinateUncertaintyInMeters
    dplyr::mutate(
      coordinateUncertaintyInMeters = PLACEACCURACY %>% stringr::str_extract(
        "[0-9]+\\s[m(km)(mi)]+") %>% stringr::str_replace_all(c("km" = "000",
                                                                "m" = "")) %>%
        stringr::str_replace(" ","") %>% as.numeric(),
      .after = PLACEACCURACY
    ) %>%
      # Create eventDate
    dplyr::mutate(
      day = DTFR %>% stringr::str_extract("^[0-9]+/") %>% stringr::str_remove_all("/") %>%
        dplyr::if_else(. == "00", NA_character_, .),
      month = DTFR %>% stringr::str_extract("/[0-9]+/") %>% stringr::str_remove_all("/") %>%
        dplyr::if_else(. == "00", NA_character_, .),
      year = DTFR %>% stringr::str_extract("/[0-9]+$") %>% stringr::str_remove_all("/") %>%
        dplyr::if_else(. == "0000", NA_character_, .),
      eventDate = lubridate::dmy(stringr::str_c(day, month, year, sep = "/"), truncated = 2),
      eventDate2 = dplyr::if_else(!stringr::str_detect(DTFR, "[a-zA-Z]"),  # convert from silly excel numeric format to real dates...
        as.Date(as.numeric(DTFR), origin = "1899-12-30") %>% as.character(),
        DTFR) %>% lubridate::ymd(truncated = 2),
        # if eventDate is empty, use eventDate2
      eventDate = dplyr::if_else(is.na(eventDate),
                                 eventDate2, eventDate),
      .after = DTFR
    ) %>% dplyr::select(!eventDate2) %>%
      # Create the date to
    dplyr::mutate(
      dayTO = DTTO %>% stringr::str_extract("^[0-9]+/") %>% stringr::str_remove_all("/") %>%
        dplyr::if_else(. == "00", NA_character_, .),
      monthTO = DTTO %>% stringr::str_extract("/[0-9]+/") %>% stringr::str_remove_all("/") %>%
        dplyr::if_else(. == "00", NA_character_, .),
      yearTO = DTTO %>% stringr::str_extract("/[0-9]+$") %>% stringr::str_remove_all("/") %>%
        dplyr::if_else(. == "0000", NA_character_, .),
      eventDateTO = lubridate::dmy(stringr::str_c(dayTO, monthTO, yearTO, sep = "/"), truncated = 2),
      eventDateTO2 = dplyr::if_else(!stringr::str_detect(DTTO, "[a-zA-Z]"),  # convert from silly excel numeric format to real dates...
                                  as.Date(as.numeric(DTTO), origin = "1899-12-30") %>% as.character(),
                                  DTTO %>% as.character()) %>% lubridate::ymd(truncated = 2),
      # if eventDateTO is empty, use eventDateTO2
      eventDateTO = dplyr::if_else(is.na(eventDateTO),
                                   eventDateTO2, eventDateTO) %>% as.character(),
      eventDateTO = dplyr::if_else(is.na(eventDateTO),
                                   stringr::str_c(
                                     dplyr::if_else(stats::complete.cases(yearTO),
                                                    yearTO,""),
                                     dplyr::if_else(stats::complete.cases(monthTO),
                                                    monthTO,""),
                                     dplyr::if_else(stats::complete.cases(dayTO),
                                                    dayTO,""),
                                     sep = " ") %>% stringr::str_squish() %>% 
                                     stringr::str_replace(" ", "-"),
                                   eventDateTO),
      .after = DTTO
    ) %>% 
    dplyr::select(!tidyselect::any_of(c("eventDateTO2", "dayTO", "monthTO", "yearTO"))) %>%
      # Add field notes
    dplyr::mutate(
    fieldNotes = stringr::str_c(
      dplyr::if_else(!is.na(eventDate),
                     paste0("startDate: ", eventDate), ""),
      dplyr::if_else(!is.na(eventDateTO),
                     paste0("endDate: ", eventDateTO), ""),
      dplyr::if_else(!is.na(LABELFAMILY),
                     paste0("associatedFamily: ", LABELFAMILY), ""),
      dplyr::if_else(!is.na(LABELGENUS),
                     paste0("associatedGenus: ", LABELGENUS), ""),
      dplyr::if_else(!is.na(LABELSPECIES),
                     paste0("associatedSpecies: ", LABELSPECIES), ""),
      sep = "|")) %>% 
        # Add associatedTaxa
    dplyr::mutate(associatedTaxa = stringr::str_c(
      dplyr::if_else(!is.na(LABELGENUS),
                     paste0(LABELGENUS), ""),
      dplyr::if_else(!is.na(LABELSPECIES),
                     paste0(LABELSPECIES), ""),
      sep = " ") %>% stringr::str_squish()) %>%
        # Worst case, use family
      dplyr::mutate(associatedTaxa = stringr::str_c(
        dplyr::if_else(is.na(associatedTaxa),
                       paste0(LABELFAMILY), associatedTaxa)
      )) %>% 
      # Format country name
    dplyr::mutate(country = stringr::str_to_sentence(country)) %>%
    dplyr::mutate(stateProvince = stringr::str_replace_all(stateProvince,
                                   c("^ACT$" = "Australian Capital Territory",
                                     "New South wales" = "New South Wales",
                                     "^NSW" = "New South Wales",
                                     "^NT$" = "Northern Territory",
                                     "^Qld$" = "Queensland",
                                     "^SA$" = "South Australia",
                                     "^Tas$" = "Tasmania",
                                     "^Vic$" = "Victoria",
                                     "^WA$" = "Western Australia",
                                     "Western australia" = "Western Australia"))) %>% 
    # Remove any double white-spaces
    apply(., 2, stringr::str_squish) %>% dplyr::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("VicWam_data_", 1:nrow(.), sep = ""),
      .before = 1)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "VicWam_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "VicWam",
      datasetID = "VicWam"
    ) %>%
      # Format some lats and lons to decimal degrees
    # first, squish out extra spaces
    dplyr::mutate(
      decimalLatitude = decimalLatitude %>% stringr::str_squish(),
      decimalLongitude = decimalLongitude %>% stringr::str_squish(),
    ) %>%
      # make a column to test for the lat/lon length
    dplyr::mutate(lengthTest = dplyr::if_else(decimalLatitude %>% stringr::str_detect("S"),
                                              dplyr::if_else(stringr::str_count(decimalLatitude,
                                                                                "\\s") > 2,
                                                             "Long", "Short"),
                                              "NA"),
                  .before = decimalLatitude) %>%
    # Convert to DD
    # Long coordinates
    dplyr::mutate(decimalLatitude2 = dplyr::if_else(decimalLatitude %>% stringr::str_detect("S") &
                                                      lengthTest == "Long",
                                                    stringr::str_c(stringr::str_extract(decimalLatitude, "^[0-9]{2}") %>%
                                                                     as.numeric() +
                                                                     (stringr::str_extract(decimalLatitude,
                                                                                           "\\s[0-9]{2}\\s") %>% 
                                                                        as.numeric() / 60) +
                                                                     (stringr::str_extract(decimalLatitude, 
                                                                                           "[0-9]{2}\\sS") %>% 
                                                                        stringr::str_remove("S") %>% 
                                                                        as.numeric() / 3600)) %>%
                                                      as.character(),
                                                    decimalLatitude),
                  decimalLongitude2 = dplyr::if_else(decimalLongitude %>% stringr::str_detect("E") &
                                                       lengthTest == "Long",
                                                     stringr::str_c(stringr::str_extract(decimalLongitude, "^[0-9]+") %>%
                                                                      as.numeric() +
                                                                      (stringr::str_extract(decimalLongitude, 
                                                                                            "\\s[0-9]{2}\\s") %>%
                                                                         as.numeric() / 60) +
                                                                      (stringr::str_extract(decimalLongitude, 
                                                                                            "\\s[0-9]+\\sE") %>% 
                                                                         stringr::str_remove("E") %>% 
                                                                         as.numeric() / 3600)) %>%
                                                       as.character(),
                                                     decimalLongitude),
                  .after = decimalLatitude) %>%
    # Short coordinates
    dplyr::mutate(decimalLatitude2 = dplyr::if_else(decimalLatitude %>% stringr::str_detect("S") &
                                                      lengthTest == "Short",
                                                    stringr::str_c(stringr::str_extract(decimalLatitude, "^[0-9]{2}") %>%
                                                                     as.numeric() +
                                                                     (stringr::str_extract(decimalLatitude,
                                                                                           "\\s[0-9]{2}\\sS") %>% 
                                                                        stringr::str_remove("S") %>%
                                                                        as.numeric() / 60) ) %>%
                                                      as.character(),
                                                    decimalLatitude2) %>% as.numeric(),
                  decimalLongitude2 = dplyr::if_else(decimalLongitude %>% stringr::str_detect("E") &
                                                       lengthTest == "Short",
                                                     stringr::str_c(stringr::str_extract(decimalLongitude, "^[0-9]+") %>%
                                                                      as.numeric() +
                                                                      (stringr::str_extract(decimalLongitude, 
                                                                                            "\\s[0-9]{2}+\\sE") %>%
                                                                         stringr::str_remove("E") %>%
                                                                         as.numeric() / 60) ) %>%
                                                       as.character(),
                                                     decimalLongitude2) %>% as.numeric(),
                  .after = decimalLatitude) %>%
      # Change lat/lon to correct accuracy
    dplyr::mutate(
      decimalLatitude2 = dplyr::if_else(lengthTest == "Long",
                                        round(decimalLatitude2, digits = 2),
                                        dplyr::if_else(lengthTest == "Short",
                                                       round(decimalLatitude2, digits = 1),
                                                       decimalLatitude2)),
      decimalLongitude2 = dplyr::if_else(lengthTest == "Long",
                                         round(decimalLongitude2, digits = 2),
                                         dplyr::if_else(lengthTest == "Short",
                                                        round(decimalLongitude2, digits = 1),
                                                        decimalLongitude2))
    ) %>%
      # Add coordinateUncertaintyInMeters for these as well based on the length of coordinates
    dplyr::mutate(
      coordinateUncertaintyInMeters = coordinateUncertaintyInMeters %>% as.numeric(),
      coordinateUncertaintyInMeters = dplyr::if_else(
      is.na(coordinateUncertaintyInMeters),
      dplyr::if_else(lengthTest == "Long", 1000, 10000),
      coordinateUncertaintyInMeters
    )) %>%
      # Replace the lat/lon columns with the working columns
    dplyr::select(!c("decimalLongitude", "decimalLatitude")) %>%
    dplyr::rename(decimalLongitude = "decimalLongitude2",
                  decimalLatitude = "decimalLatitude2") %>%
    # All of Australia is in the southern hemisphere, correct these latitudes to negative
    dplyr::mutate(decimalLatitude = dplyr::if_else(
      stateProvince %in% c("Australian Capital Territory", "New South Wales",
                           "Northern Territory", "Queensland", "South Australia",
                           "Tasmania", "Victoria", "Western Australia", ""),
      abs(decimalLatitude) * - 1,
      decimalLatitude)) %>%
      # Do the same for longitude but hte entire dataset is in the eastern hemisphere
    dplyr::mutate(decimalLongitude = abs(decimalLongitude))
  
  #### 28.3 Out ####
  # Save the dataset
  readr::write_excel_csv(VicWam_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(VicWam_data)
} # END readr_VicWam

