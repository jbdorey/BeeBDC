#### 4. USGS formatter ####
#' Find, import, and format USGS data to Darwin Core
#' 
#' The function finds, imports, formats, and creates metadata for the USGS dataset.
#'
#' @param path A character path to a directory that contains the USGS data, which will be found using
#' [BeeBDC::fileFinder()]. The function will look for "USGS_DRO_flat".
#' @param pubDate Character. The publication date of the dataset to update the metadata and citation.
#'
#' @return Returns a list with the occurrence data, "USGS_data", and the EML data, "EML_attributes".
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' USGS_data <- USGS_formatter(path = DataPath, pubDate = "19-11-2022")
#' }
#' 
USGS_formatter <- function(
    path, 
    pubDate){
  # locally bind variables to the function
  . <-readr <- lubridate <- problems <- how0 <- how1 <- how2 <- how3 <- how4 <- days <- 
    field_note <- note <- SpeciesNotes <- SpeciesNotes<-DateEntered<-DateScanned<-ip<-position<-
    time1<-time2<- occurrenceID <- NULL
  
  #### require and checks ####
  # Load required packages
  requireNamespace("lubridate")
  requireNamespace("dplyr")
  # File name to search for
  USGS_fileName <- "USGS_DRO_flat"
  # Find the USGS data from the HomePath
  USGS_loc <- fileFinder(path, USGS_fileName)
  # Define ColsToKeep
  ColsToKeep <- ColTypeR()[[1]] %>% names()
  
  # Check if the data are present
  if(is.null(USGS_loc) == TRUE){ # If there are no data matching the name...
    stop(" - Oh dear, it looks like there are no USGS data in the HomePath. If you want to include such data, make sure that they exist.")
  }
  # Check if the ddate has been enetered
  if(exists("pubDate") == FALSE){ # If there are no data matching the name...
    stop(paste(" - Oh no! It looks like you have not provided pubDate = dd-mm-yyy. ",
               "Please include this as the date that Sam Droege passed on his data in the above format."))
  }
  
  #### Find and import ####
  if(grepl(pattern =  ".txt.gz", USGS_loc) == TRUE){ # If the zipped file is present
    writeLines("Only one zip file detected. Unzipping file to be read in.")
    # unzip the file
    R.utils::gunzip(
      # File to unzip
      fileName = USGS_loc, 
      # Where to put the extracted file
      destname = stringr::str_remove(USGS_loc, ".gz"),
      overwrite = FALSE,
      remove = FALSE) 
    USGS_loc <- stringr::str_remove(USGS_loc, ".gz")
    # User output
    writeLines(paste(" - Unzipped file to: ", USGS_loc))
  }
  # If already extracted, use the extracted file
  if(stringr::str_detect(pattern = USGS_fileName, 
                         # Select the string that matches only (avoids a warning but works without)
                         string = USGS_loc)){
    writeLines(paste(" - Reading in data file. This should not take too long.","\n",
                     "There may be some errors upon reading in depending on the state of the data.",
                     "\n", "One might consider reporting errors to Sam Droege to improve the dataset."))
    # Read in the data file using "$" as the delimiter 
    USGS_data <- readr::read_delim(USGS_loc[[1]],
                                   delim = "$")
    # Make a copy of the problems, if they exist
    USGS_problems <- readr::problems(USGS_data)
  }
  
  #### Metadata building ####
  Attributes_USGS <- dplyr::tibble(
    dataSource = "USGS_data",
    alternateIdentifier = "Not provided",
    title = "USGS_DRO database",
    pubDate = lubridate::dmy(pubDate),
    dateStamp = pubDate,
    doi = "Not provided",
    downloadLink = "Not provided, contact Sam Droege at sdroege@usgs.gov",
    abstract = dplyr::lst(
      "Note:  If you are getting data from Sam Droege, you are getting one of two file types. Either an Excel spreadsheet with data already extracted for you, or the entire data set which is compressed into a file called USGS_DRO_flat.gz.  The file USGS_DRO_flat.gz is how this file is usually shipped when we send the entire database.  If you get this file it is compressed and needs to be uncompressed with 7-zip, gzip, or something similar ...after unzipping it becomes a very large txt file which can be imported into a database program for manipulation and searching.  Data can also be extracted from BISON and GBIF and we assume the file structure below is reflected in their data also.
      Be sure to check for spelling errors...we do global checks only every year or two.
      Column Headings for USGS Bee Flat File (or an Excel Sheet if we send a subset)
      ID. - Unique 6 digit specimen number with the database identifier (USGS_DRO) in front
      name - Scientific name; ('Destroyed' = label destroyed without being used; 'NONBEE' = An insect that is not a bee, but was given a label; Blank = not yet identified, can indicate that specimen is waiting for identification, is lost, or was missed during data entry) 
      Sex - 'f' = female; 's' = male; 'u' = unknown
      DeterminedBy - Who did the identification of the specimen
      DeterminedWhen - Roughly when the identification was done
      WhoScanned - Who scanned or entered the data
      DateScanned - Date data were scanned
      SpeciesNotes - Any notes about the species such as changes in identification
      DateEntered - Date entered into the new database (done automatically)
      COLLECTION.db - Collection event number.  Up to 5 digit code preceded by database identifier (USGS_DRO) of the date, time, place of the collection event
      gmt - Greenwich Mean Time of I think the time in which the Collection Event Number was created
      Latitude - Latitude in decimal degrees
      Longitude - Longitude in decimal degrees
      accuracy - Degree of association of specimens to the Latitude/Longitude (1 = Specimens from the level of a country/state; 2 = specimens found in area of a county; 3 = specimens found in area of a park or refuge; 4 and 5 = Specimens located very close to coordinants
      elevation - Elevation of location (rarely used)
      country - Country where collection occurred
      state - State where collection occurred
      county - County where collection occurred
      city - Nearest city or the geographic unit (i.e. Park or refuge) where collected 
      site - A site name, number, or designation within a city/park
      position - similar to \"site\" but used less often
      time1 - Date/time collection started or traps put out (format is yyyymmddmmss)
      time2 - Date/time collection ended or traps picked up
      days - Number of trapping days (used inconsistently)
      who - The collector
      email - Almost always Sam Droege's OLD email address
      how0 - Technique used to capture bees
      how1 - Number of traps that remained full of trap fluid when traps picked up
      how2 - Bowl/ trap size
      how3 - Trap color (rarely used since mostly we use several colors)
      how4 - Trap liquid/soap
      habitat - Rarely used
      field_note - 'field_note' and 'note' used interchangeably to take notes about collection event
      note -  'field_note' and 'note' used interchangeably to take notes about collection event"
    ),
    citations = dplyr::lst("Citations not provided"),
    downloadCitation = paste("Sam Droege. (", 
                             lubridate::as_date(pubDate, format = "%m-%d-%y"),
                             "). United States Geological Survey bee data.",
                             sep = ""),
    rights = dplyr::lst("Rights are not provided. Please seek permission for data use from Same Droege.") 
  ) # END metadata
  # combine the input eml and the attributes tibble into a list for output from the function
  EML_attributes <- dplyr::lst("No_eml_from_USGS", Attributes_USGS)
  names(EML_attributes) <- c("source_eml","Source_tibble") 
  
  
  #### Format the data ####
  writeLines(" - Formatting the USGS dataset...")
  
  writeLines(" - Formatting the dateTime...")
  # Convert time1 and time2 to dateTime format
  # Convert time1
  USGS_data$time1 <- USGS_data$time1 %>% 
    lubridate::ymd_hms(., truncated = 5)   
  # Convert time2
  USGS_data$time2 <- USGS_data$time2 %>% 
    lubridate::ymd_hms(., truncated = 5)  
  
  writeLines(" - Creating samplingProtocol and samplingEffort columns...")
  # Create new columns with extra information that doesn't fit the established columns well
  # Merge all of the extra collection info
  USGS_data <- USGS_data %>% dplyr::mutate(
    samplingProtocol = stringr::str_c(
      dplyr::if_else(!is.na(how0),
                     paste0("Technique used: ", how0), ""),
      dplyr::if_else(!is.na(how1),
                     paste0("Bowls full upon collection: ", how1), ""),
      dplyr::if_else(!is.na(how2),
                     paste0("Sampling Bowl/trap size: ", how2), ""),
      dplyr::if_else(!is.na(how3),
                     paste0("Trap colour: ", how3), ""),
      dplyr::if_else(!is.na(how4),
                     paste0("Trap liquid: ", how4), ""),
      sep = "|") %>%
      # Remove extra bars.
      # Remove extra bars.
      stringr::str_replace_all(pattern = "(\\|){2,5}",
                               replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                               replacement = "")) %>%
      # Add in samplingEffort
    dplyr::mutate(
      samplingEffort = stringr::str_c(
        dplyr::if_else(!is.na(days),
                       paste0("sampling days: ", days), ""),
        sep = "")) %>%
      # Add in dataset information
    dplyr::mutate(
      datasetName = "USGS_DRO database",
      datasetID = "USGS_DRO",
      institutionCode = "USGS"
    )

  
  writeLines(" - Creating the fieldNotes and dataSource columns...")
  # Enter these extra data into a new column.
  USGS_data <- USGS_data %>% dplyr::mutate(
    fieldNotes = stringr::str_c(
      dplyr::if_else(!is.na(field_note),
                     paste0("field_note: ", field_note), ""),
      dplyr::if_else(!is.na(note),
                     paste0("note: ", note), ""),
      dplyr::if_else(!is.na(SpeciesNotes),
                     paste0("SpeciesNotes: ", SpeciesNotes), ""),
      dplyr::if_else(!is.na(DateEntered),
                     paste0("DateEntered: ", DateEntered), ""),
      dplyr::if_else(!is.na(DateScanned),
                     paste0("DateScanned: ", DateScanned), ""),
      dplyr::if_else(!is.na(ip),
                     paste0("ipAddress: ", ip), ""),
      dplyr::if_else(!is.na(position),
                     paste0("position: ", position), ""),
      dplyr::if_else(!is.na(time1),
                     paste0("time1: ", time1), ""),
      dplyr::if_else(!is.na(time2),
                     paste0("time2: ", time2), ""),
      sep = "|") %>%
    # Remove extra bars.
    stringr::str_replace_all(pattern = "(\\|){2,9}",
                 replacement = "\\|") %>%
      stringr::str_replace_all(pattern = "(\\|$)+|(^\\|)+",
                 replacement = "")) 
  # Trim white spaces
    # stringr::str_trim(side = "both")
  
  # Set the dataSource
  USGS_data$dataSource <- "USGS_data"

  writeLines(" - Renaming and selecting columns...")
  # These data must be formatted to match the other data sets.
  # that we created at the top of the R-script
  USGS_data <- USGS_data %>%  # The data frame to match with
    dplyr::rename("occurrenceID" = "ID.",
                  "scientificName" = "name",
                  "sex" = "sex",
                  "identifiedBy" = "DeterminedBy", 
                  "dateIdentified" = "DeterminedWhen", 
                  "eventID" = "COLLECTION.db", 
                  "eventTime" = "gmt", 
                  "decimalLatitude" = "latitude", 
                  "decimalLongitude" = "longitude", 
                  "coordinateUncertaintyInMeters" = "accuracy", 
                  "stateProvince" = "state", 
                  "municipality" = "city", 
                  "Location" = "site", 
                  "recordedBy" = "who", 
                  "eventDate" = "time1") %>%
    dplyr::mutate(
      id = occurrenceID,
      recordId = occurrenceID,
    ) %>%
    # select columns that match the following string
    dplyr::select( dplyr::matches(      
      # Use the carrot "^" to signify start of string and dollar sign "$" to signify end of 
      # string. Effectively, this will only return an exact match.
      paste("^",ColsToKeep,"$",sep="") ))  
  #### Save data ####
  # Check for and create outpath if needed
  outPath <- outFile_maker(path = path)
  # Notfiy user that occurrence file is being written
  writeLines( paste(" - Writing occurrence data file...", "\n",
                    "Number of rows (records): ", format(nrow(USGS_data), big.mark=",",scientific=FALSE), "\n",
                    "Written to file called ", paste("USGS_formatted_", Sys.Date(), ".csv", sep = ""),
                    " at location ", outPath,
                    sep = "")) 
  # Write the occurence file
  readr::write_excel_csv(USGS_data, paste(outPath, "/USGS_formatted_", Sys.Date(), ".csv", sep = ""))
  # Notify user that the .eml file is being written
  writeLines( paste(" - Writing attributes file...", "\n",
                    "Written to file called ", paste("USGS_attribute_files", Sys.Date(),".xml", sep="" ),
                    " at location ", outPath,
                    sep = "")) 
  # Format the attributes for exporting lists
  EML_attributes$Source_tibble$abstract <- EML_attributes$Source_tibble$abstract %>% unlist()
  EML_attributes$Source_tibble$citations <- EML_attributes$Source_tibble$citations %>% unlist()
  EML_attributes$Source_tibble$rights <- EML_attributes$Source_tibble$rights %>% unlist()
  # Write the attribute file
  readr::write_excel_csv(EML_attributes$Source_tibble, file = paste(outPath, 
                                                       "/USGS_attribute_files", 
                                                       Sys.Date(),".csv", sep="" ))
  # IF there were problems detected, write these to a .csv file and notify the user
  if(nrow(USGS_problems) > 0){
    writeLines(" - Problems detected with the tibble. Saving to a .csv file...")
    readr::write_excel_csv(USGS_problems, file = paste(outPath, "/USGS_problems", 
                                          Sys.Date(),".csv", sep="" ))
  }
  
  # Return end product and print completion note
  writeLines(paste(" - Fin.", sep = "\n"))
  return( dplyr::lst(USGS_data, EML_attributes) )
} # END USGS_import

