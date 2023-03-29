  # These functinos were written by James B Dorey around the 17th of June 2022 to read in,
    # Format and save varioues datasets.
  # For questions, please email jbdorey@me.com

#### 1.0 EPEL ####
#' Reads specific data files into Darwin Core format
#' 
#' Data files are specific to various data providers and users may examine code function-by-function.
#' 
#' @param path The path to the directory containing the data.
#' @param inFile The name of the file itself (can also be the remainder of a path including the file name).
#' @param outFile The name of the Darwin Core format file to produce.
#' @param dataLicense The license to accompany each record in the Darwin Core 'license' column.
#'
#' @return A data frame that's in Darwin Core format.
#' @export
#' 
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr rename mutate select if_else
#' @importFrom lubridate ymd month
#' @importFrom stringr str_c
#' @importFrom mgsub mgsub
#'
#' @examples
#' \dontrun{
#' An example using a .csv file
#' EPEL_Data <- Ereadr_PEL(path = paste0(DataPath, "/Additional_Datasets"),
#'inFile = "/InputDatasets/bee_data_canada.csv",
#' outFile = "jbd_EPEL_data.csv",
#' dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
#' 
#' An example using a .xlsx file
#'     Arm_Data <- readr_Arm(path = paste0(DataPath, "/Additional_Datasets"),
#' inFile = "/InputDatasets/Bee database Armando_Final.xlsx",
#' outFile = "jbd_Arm_Data.csv",
#' sheet = "Sheet1",
#' dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
#' }
readr_EPEL <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 1.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
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
        # Remove extra bars.
        mgsub::mgsub(pattern = c("(\\|){2,9}"),
                     replacement = c("\\|")) %>%
        mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                     replacement = c("",""))) %>%
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
readr::write_csv(EPEL_Data, file = paste(path, outFile, sep = "/"))
# Return data
return(EPEL_Data)
} # END readr_EPEL


#### 2.0 ASP ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_ASP <- function(path = NULL,
                         inFile = NULL,
                         outFile = NULL,
                      dataLicense = NULL){
  #### 2.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 2.2 Read+ ####
ASP_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                            trim_ws = TRUE) %>%
  dplyr::mutate(
    # Add previousIdentifications
    previousIdentifications = stringr::str_c(
      dplyr::if_else(!is.na(Tribe),
                     paste0("Tribe: ", Tribe), ""),
      dplyr::if_else(!is.na(Morphospecies),
                     paste0("Morphospecies: ", Morphospecies), ""),
      sep = "|") %>%
      # Remove extra bars.
      mgsub::mgsub(pattern = c("(\\|){2,9}"),
                   replacement = c("\\|")) %>%
      mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                   replacement = c("",""))) %>%
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
    catalogNumber = paste0("ASP_", catalogNumber)) %>%
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
readr::write_csv(ASP_data, file = paste(path, outFile, sep = "/"))
# Return data
return(ASP_data)
} # END readr_ASP


#### 3.0 BMin ####
 
 #' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export

readr_BMin <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 3.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
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
readr::write_csv(BMin_data, file = paste(path, outFile, sep = "/"))
# Return data
return(BMin_data)
} # END readr_BMin



#### 4.0 BMont ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_BMont <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 4.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 4.2 Read+ ####
BMont_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
  dplyr::rename(
    occurrenceID = occurence_lsid) %>%
  dplyr::mutate(
    # Add fieldNotes 
     fieldNotes = stringr::str_c(
      dplyr::if_else(!is.na(fieldNotes),
                     paste0("fieldNotes: ", fieldNotes), ""),
      dplyr::if_else(!is.na(GPS_device),
                     paste0("GPS_device: ", GPS_device), ""),
      sep = "|") %>%
      # Remove extra bars.
      mgsub::mgsub(pattern = c("(\\|){2,9}"),
                   replacement = c("\\|")) %>%
      mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                   replacement = c("",""))) %>%
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
readr::write_csv(BMont_data, file = paste(path, outFile, sep = "/"))
  # Return data
return(BMont_data)
} # END readr_BMont


#### 5.0 Ecd ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Ecd <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                      dataLicense = NULL){
  #### 5.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 5.2 Read+ ####
Ecd_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                            trim_ws = TRUE) %>%
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
readr::write_csv(Ecd_data, file = paste(path, outFile, sep = "/"))
# Return data
return(Ecd_data)
} # END readr_Ecd

###### 6.0 Gai ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Gai <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                      dataLicense = NULL){
  #### 6.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
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
  # Remove extra bars.
  mgsub::mgsub(pattern = c("(\\|){2,9}"),
               replacement = c("\\|")) %>%
  mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
               replacement = c("",""))) %>% 
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
readr::write_csv(Gai_data, file =  paste(path, outFile, sep = "/"))
# Return data
return(Gai_data)
} # END readr_Gai

#### 7.0 CAES ####

#' Reads specific data files into Darwin Core format
#' 
#' Data files are specific to various data providers and users may examine code function-by-function.
#' 
#' @param path The path to the directory containing the data.
#' @param inFile The name of the file itself (can also be the remainder of a path including the file name).
#' @param outFile The name of the Darwin Core format file to produce.
#' @param dataLicense The license to accompany each record in the Darwin Core 'license' column.
#' @param sheet A character String. For those datasets read from an .xlsx format, provide the 
#' sheet name.
#'
#' @return A data frame that's in Darwin Core format.
#' @export
#' 
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr rename mutate select if_else
#' @importFrom lubridate ymd month
#' @importFrom stringr str_c
#' @importFrom mgsub mgsub
#'
#' @examples
#' An example using a .csv file
#' EPEL_Data <- readr_EPEL(path = paste0(DataPath, "/Additional_Datasets"),
#'inFile = "/InputDatasets/bee_data_canada.csv",
#' outFile = "jbd_EPEL_data.csv",
#' dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
#' 
#' An example using a .xlsx file
#'     Arm_Data <- readr_Arm(path = paste0(DataPath, "/Additional_Datasets"),
#' inFile = "/InputDatasets/Bee database Armando_Final.xlsx",
#' outFile = "jbd_Arm_Data.csv",
#' sheet = "Sheet1",
#' dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
readr_CAES <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = "Sheet1"){
    #### 7.1 Prep ####
    # This will load the required packages. These packages may still need to be installed to 
      # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 7.2 Read+ ####
    # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
CAES_data <- readxl::read_xlsx(paste(path, inFile, sep = ""), sheet = sheet,
                             trim_ws = TRUE, col_types = "text",
                             progress = readxl_progress()) %>%
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
        # ONLY do this IF there is something in the cell — is.na() finds "NA" values. the "!" reverses this to find complete.cases only.
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
      mgsub::mgsub(pattern = c("(\\|){2,9}"),
                   replacement = c("\\|")) %>%
      mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                   replacement = c("",""))) %>%
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
      # Remove extra bars.
      mgsub::mgsub(pattern = c("(\\|){2,9}"),
                   replacement = c("\\|")) %>%
      mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                   replacement = c("",""))) %>%
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
  apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
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
readr::write_csv(CAES_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
return(CAES_data)
} # END readr_CAES






#### 9.0 KP ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_KP <- function(path = NULL,
                     inFile = NULL,
                     outFile = NULL,
                     dataLicense = NULL){
  #### 9.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 9.2 Read+ ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
KP_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                               trim_ws = TRUE, guess_max = 33000) %>%
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
      # Remove extra bars.
      mgsub::mgsub(pattern = c("(\\|){2,9}"),
                   replacement = c("\\|")) %>%
      mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                   replacement = c("",""))) %>%
    # Do the same as the last mutate, but for fieldNotes
    dplyr::mutate(
      sex = stringr::str_c(
        dplyr::if_else(!is.na(Male) & Male != 0,
                       paste0(Male, " M"), ""),
        dplyr::if_else(!is.na(Female) & Female != 0,
                       paste0(Female, " F"), ""),
        sep = "|") %>%
        # Remove extra bars.
        mgsub::mgsub(pattern = c("(\\|){2,9}"),
                     replacement = c("\\|")) %>%
        mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                     replacement = c("",""))) %>%
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
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
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
  readr::write_csv(KP_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(KP_data)
} # END readr_KP






#### 10.0 INHS ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_INHS <- function(path = NULL,
                     inFile = NULL,
                     outFile = NULL,
                     dataLicense = NULL){
  #### 10.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 10.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  INHS_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                                trim_ws = TRUE, col_types = ColTypeR()) %>%
    # Format eventDate and add dataSource
    dplyr::mutate(
      dataSource = "INHS_Anthophila") %>%
    # add the database_id column
    dplyr::mutate(
      database_id = paste("INHS_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Illinois Natural History Survey",
      datasetID = "INHS",
      
    )
  
  #### 10.3 Out ####
  # Save the dataset
  readr::write_csv(INHS_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(INHS_data)
} # END readr_INHS





#### 11.0 EcoS ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_EcoS <- function(path = NULL,
                     inFile = NULL,
                     outFile = NULL,
                     dataLicense = NULL){
  #### 11.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
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
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
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
  readr::write_csv(EcoS_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(EcoS_data)
} # END readr_EcoS








#### 12.0 GeoL ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_GeoL <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  #### 12.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 12.2 Read+ ####
    ###### a. GeoL_high ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
  GeoL_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                               trim_ws = TRUE, 
                               sheet = "GEOLOCATE HIGH",
                               col_types = "text") %>%
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
      tempSource = "GeoL"
    )
  # User output
  writeLines(paste0(
    " — We have read in ", 
    format(nrow(GeoL_data), big.mark = ","), " occurrence records from the 'GEOLOCATE HIGH' sheet." 
  ))
  
  ###### b. BELS_high ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
  BELS_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                                  trim_ws = TRUE, 
                                  sheet = "BELS High",
                                  col_types = "text") %>%
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
    " — We have read in ", 
    format(nrow(BELS_data), big.mark = ","), " occurrence records from the 'BELS High' sheet." 
  ))
  
  ###### c. merge ####
  GeoL_data <- GeoL_data %>%
      # Remove data that occurs in BELS_data
    dplyr::filter(!database_id %in% BELS_data$database_id) %>%
      # Combine datasets 
    dplyr::bind_rows(BELS_data) # %>%
      # Annotate dataSource
    #dplyr::mutate(dataSource = stringr::str_c("GeoL", dataSource, sep = "_"))
    
    # User output
  writeLines(paste0(
    " — We have kept ", 
    format(sum(GeoL_data$tempSource == "GeoL", na.rm = FALSE), big.mark = ","), 
    " occurrences from GeoLocate, and ",
    format(sum(GeoL_data$tempSource == "Bels", na.rm = FALSE), big.mark = ","),
    " records from BELS (",
    format(nrow(GeoL_data), big.mark = ","),
    " in total). BELS was given preference over GeoLocate"
  ))
    
  #### 12.3 Out ####
  # Save the dataset
  readr::write_csv(GeoL_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(GeoL_data)
} # END readr_GeoL









#### 13.0 EaCO ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_EaCO <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  #### 13.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 13.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  EaCO_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                               trim_ws = TRUE, guess_max = 33000) %>%
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
        mgsub::mgsub(pattern = c("(\\|){2,9}"),
                     replacement = c("\\|")) %>%
        mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                     replacement = c("",""))) %>%
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
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
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
  readr::write_csv(EaCO_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(EaCO_data)
} # END readr_EaCO








#### 14.0 MABC ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_MABC <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  #### 14.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 14.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  MABC_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                                  trim_ws = TRUE,
                                  col_types = "text",
                                  sheet = "Hoja1") %>%
    # Rename columns
    dplyr::rename(
      catalogNumber = 'Ejemplar',
      eventDate = 'Fecha colecta',
      country = 'País',
      stateProvince = 'Estado/Provincia',
      municipality = 'Municipio',
      locality = 'Localidad',
      samplingProtocol = 'Metodo colecta',
      decimalLatitude = 'Coordenadas Lat',
      decimalLongitude = 'Coordenadas Long',
      verbatimElevation = 'Altitud',
      georeferenceVerificationStatus = 'Datos georeferenciación',
      recordedBy = 'Colector',
      identifiedBy = 'Identificador',
      family = 'Familia',
      subfamily = 'Subfamilia',
      genus = 'Genero',
      subgenus = 'Subgenero',
      specificEpithet = 'Especie',
      infraspecificEpithet = 'Subespecie',
      species = 'Nombre especie',
      taxonID = 'Código especie',
      sex = 'Sexo',
      # Non-standard fields
      tribe = 'Tribu',
      collectionSite = 'Sitio Colecta',
      siteCode = 'Código sitio',
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
        mgsub::mgsub(pattern = c("(\\|){2,9}"),
                     replacement = c("\\|")) %>%
        mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                     replacement = c("",""))) %>% 
    # Add identificationRemarks
    dplyr::mutate(
      identificationRemarks = stringr::str_c(
        dplyr::if_else(!is.na(tribe), paste0("tribe: ", tribe), ""))) %>%
  # Add year and ensure ymd format
  dplyr::mutate(eventDate = lubridate::ymd(eventDate))  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "MABC_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
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
  readr::write_csv(MABC_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(MABC_data)
} # END readr_MABC






#### 15.0 Col ####
#' @describeIn readr_CAES
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Col <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                      sheet = sheet){
  #### 15.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 15.2 Read+ ####
    ###### a. Col_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  Col_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                                 sheet = sheet,
                                  trim_ws = TRUE, 
                                 col_types = "text") %>%
    # Rename columns
    dplyr::rename(
      catalogNumber = 'Código de Barras',
      recordedBy = 'Colectores [Aggregated]',
      recordedByID = 'Colectores asociados',
      eventDateInitial = 'Fecha colección inicial',
      order = 'Orden',
      family = 'Familia',
      genus = 'Género',
      specificEpithet = 'Especie',
      scientificNameAuthorship = 'Especie Author',
      typeStatus = 'Tipo',
      identifiedBy = 'Determinador [Formatted]',
      dateIdentified = 'Fecha determinación',
      country = 'País',
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
        # collectionID = 'Colección/Guid',
        # collectionCode = 'Código',
        # occurrenceID = 'Collection Object/GUID',
        # identificationRemarks = 'Observaciones generales',
        # eventID = 'Evento de Recolección/Guid',
        # recordedByID = 'Numero de colector',
        # eventDateFinal = 'Fecha colección final',
        # class = 'Clase',
        # infraspecificEpithet = 'Subespecie',
        # namePublishedInID = 'Referencia original',
        # identificationID = 'Determinaciones/Guid',
      # identificationQualifier = 'Obs. Determinación',
        # locationID = 'LocalityID',
        # geodeticDatum = 'Datum geodésico',
        # coordinateUncertaintyInMeters = 'Precisión coord. georref.',
        # minimumElevationInMeters = 'Elevación mínima',
        # maximumElevationInMeters = 'Elevación máxima',
        # georeferenceProtocol = 'Protocolo de georreferenciación',
        # verbatimLatitude = 'Información geográfica/Latitude1 literal',
        # verbatimLongitude = 'Longitude1 literal',
        # locationRemarks = 'Observaciones ninf Geográfica',
        # lifeStage = 'Estado de desarrollo',
        # habitat = 'Hábitat',
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
        as.numeric(na.rm = TRUE)
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
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Col_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "Col_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Colombia — Diego Alexander Guevara Farias",
      datasetID = "Col"
    )
  
  
  #### 15.3 Out ####
  # Save the dataset
  readr::write_csv(Col_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Col_data)
} # END readr_Col






#### 16.0 FSCA ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_FSCA <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL){
  #### 16.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 16.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  FSCA_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                                  trim_ws = TRUE, col_types = ColTypeR()) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "FSCA_Anthophila") %>%
    # Remove any double white-spaces
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("FSCA_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
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
  readr::write_csv(FSCA_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(FSCA_data)
} # END readr_FSCA




#### 17.0 SMC ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_SMC <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 17.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
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
  readr::write_csv(SMC_Data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(SMC_Data)
} # END readr_SMC




#### 18.0 Bal ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Bal <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 18.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 18.2 Read+ ####
  # Reads in the .xlsx file, trims the white spaces, and formats the columns to the correct type
  Bal_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                                 trim_ws = TRUE, guess_max = 33000,
                                 sheet = "animal_data", skip = 1) %>%
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
      eventDate = lubridate::ymd(eventDate,
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
  readr::write_csv(Bal_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Bal_data)
} # END readr_Bal







#### 19.0 Lic ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Lic <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL,
                      beeFilter = TRUE){
  #### 19.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
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
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
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
  if(beeFilter == TRUE){
    Lic_data <- Lic_data %>%
      dplyr::filter(tolower(family) %in% 
                      tolower(c("Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae",
                                "Melittidae","Stenotritidae")))
  } # END beeFilter
  
  
  #### 19.3 Out ####
  # Save the dataset
  readr::write_csv(Lic_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Lic_data)
} # END readr_Lic






#### 20.0 Arm ####
#' @describeIn readr_CAES
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Arm <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = "Sheet1"){
  #### 20.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 20.2 Read+ ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  Arm_data <- readxl::read_xlsx(paste(path, inFile, sep = ""), sheet = sheet,
                                 trim_ws = TRUE, col_types = "text",
                                 progress = readxl_progress()) %>%
    # Make columns DarwinCore-compatible
    dplyr::rename(
      family = fam,
      genus = genus,
      specificEpithet = sp,
      species = species,
      sex = sex,
      localilty = locality,
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
        # Remove extra bars.
        mgsub::mgsub(pattern = c("(\\|){2,9}"),
                     replacement = c("\\|")) %>%
        mgsub::mgsub(pattern = c("(\\|$)+","(^\\|)+"),
                     replacement = c("",""))) %>%
    # Add scientificName
    dplyr::mutate(
      verbatimLatitude = stringr::str_c(
        dplyr::if_else(!is.na(g),
                       g, ""),
        dplyr::if_else(!is.na(m),
                       m, ""),
        
        dplyr::if_else(!is.na(s),
                       s, ""),
        sep = " "),
      verbatimLongitude = stringr::str_c(
        dplyr::if_else(!is.na(G),
                       G, ""),
        dplyr::if_else(!is.na(M),
                       M, ""),
        
        dplyr::if_else(!is.na(S),
                       S, ""),
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
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("Arm_data_", 1:nrow(.), sep = ""),
      .before = family)  %>%
    dplyr::mutate(license = dataLicense) %>%
      # Remove spent columns
    dplyr::select(!c(veget, ecoregion, g, m, s, G, M, S,)) %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Armando Falcon-Brindis",
      datasetID = "Arm"
    )
  
  #### 20.3 Out ####
  # Save the dataset
  readr::write_csv(Arm_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(Arm_data)
} # END readr_Arm








#### 21.0 Dorey ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_Dor <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 21.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 21.2 Read+ ####
  Dor_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
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
  readr::write_csv(Dor_data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(Dor_data)
} # END readr_Dor



#### 22.0 MEPB ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_MEPB <- function(path = NULL,
                       inFile = NULL,
                       outFile = NULL,
                       dataLicense = NULL,
                       sheet = NULL){
  #### 22.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 22.2 Read+ ####
  MEPB_data <- readxl::read_xlsx(paste(path, inFile, sep = ""), sheet = sheet,
                                 trim_ws = TRUE, col_types = "text",
                                 progress = readxl_progress())  %>%
    # Fix broken encodings
    mutate(
      across(
        .cols = everything(),
        .fns = ~ str_replace_all(.,
                                 pattern = c("Ã³"="ó",  
                                             "Ã©"="é", 
                                             "Ã±"="ñ",
                                             "Â°"="°",
                                             "Ã"="á") 
        ))) %>%
    # add the database_id column
    dplyr::mutate(
      database_id = paste("MEPB_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber) %>%
    # Add dataset information
    dplyr::mutate(dataSource = "MEPB_Anthophila") %>%
    # Format date
    dplyr::mutate(eventDate = lubridate::ymd(stringr::str_c(year, month, day, sep = "/"),
                                             truncated = 2)) %>%
    # Pick up dates of different formats and format together.
    dplyr::mutate(license = dataLicense) %>%
    # add the database_id column
    dplyr::mutate(
      datasetID = "MEPB"
    )
  #### 22.3 Out ####
  # Save the dataset
  readr::write_csv(MEPB_data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(MEPB_data)
} # END readr_MEPB




#### 23.0 Brazil ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_BBD <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL){
  #### 23.1 Prep ####
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 23.2 Read+ ####
  BBD_data <- readr::read_csv(paste(path, inFile, sep = "/"),
                              trim_ws = TRUE) %>%
    # Rename columns
    dplyr::rename(
      id = "CodeBBdatabase_curated",
      scientificName = "Scientific name corrected",
      verbatimScientificName = "Scientificname_ORIGINAL",
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
      verbatimLatitude = "Lat_original",
      verbatimLongitude = "Long_original",
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
  readr::write_csv(BBD_data, file = paste(path, outFile, sep = "/"))
  # Return data
  return(BBD_data)
} # END readr_BBD




#### 24.0 MPUJ ####
#' @describeIn readr_EPEL
#' 
#' Reads specific data files into Darwin Core format
#' 
#' @export
readr_MPUJ <- function(path = NULL,
                      inFile = NULL,
                      outFile = NULL,
                      dataLicense = NULL,
                      sheet = sheet){
  #### 24.1 Prep ####
  # This will load the required packages. These packages may still need to be installed to 
  # R using install.packages("dplyr")... etc.
  require(dplyr)
  require(readr)
  require(lubridate)
  require(stringr)
  require(mgsub)
  
  #### 24.2 Read+ ####
  ###### a. MPUJ_data ####
  # Reads in the .csv file, trims the white spaces, and formats the columns to the correct type
  MPUJ_data <- readxl::read_excel(paste(path, inFile, sep = "/"),
                                 sheet = sheet,
                                 trim_ws = TRUE, 
                                 col_types = "text") %>%
    # Rename columns
    dplyr::rename(
      catalogNumber = 'Catalog Number',
      individualCount = "Count",
      sex = "Sex",
      collectionCode = "Method",
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
      day = 'Start Date (Day)',
      month = 'Start Date (Month)',
      year = "Start Date (Year)",
      associatedTaxa = "Associated Taxa",
      lifeStage = "Stage",
      collector1stName = 'Collectors/First Name',
      collectorsLastName = 'Collectors/Last Name',
      determined1stName = 'Determiner/First Name',
      determinedLastName = 'Determiner/Last Name',
      endDate = "End Date"
      ) %>%
    dplyr::mutate(recordedBy = stringr::str_c(collector1stName, collectorsLastName,
                                              sep = " "),
                  identifiedBy = stringr::str_c(determined1stName, determinedLastName,
                                              sep = " ")) %>%
    dplyr::mutate(eventDate = lubridate::dmy(stringr::str_c(day, month, year, sep = "/"), 
                                             truncated = 2)) %>%
    dplyr::mutate(
      verbatimEventDate = stringr::str_c(
        dplyr::if_else(!is.na(eventDate),
                       paste0("startDate: ", eventDate), ""),
        dplyr::if_else(!is.na(endDate),
                       paste0("endDate: ", endDate), ""),
        sep = "|")
    ) %>%
    dplyr::mutate(basisOfRecord = "Preserved specimen") %>%
    # Remove any double white-spaces
    apply(., 2, str_squish) %>% tibble::as_tibble() %>% 
    # add the database_id column
    dplyr::mutate(
      database_id = paste("MPUJ_data_", 1:nrow(.), sep = ""),
      .before = catalogNumber)  %>%
    # Add dataset information
    dplyr::mutate(dataSource = "MPUJ_Anthophila") %>%
    dplyr::mutate(license = dataLicense)  %>%
    # add the database_id column
    dplyr::mutate(
      datasetName = "Colombia MPUJ — Diego Alexander Guevara Farias",
      datasetID = "MPUJ"
    )
  
  #### 24.3 Out ####
  # Save the dataset
  readr::write_csv(MPUJ_data, file = paste(path, outFile, sep = "/"))
  # Return the data from the function to the user
  return(MPUJ_data)
} # END readr_MPUJ

