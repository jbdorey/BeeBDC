
#### importOccurrences ####
#' Imports the most-recent repoMerge data
#' 
#' Looks for and imports the most-recent version of the occurrence data created by the [BeeBDC::repoMerge()] 
#' function.
#'
#' @param path A directory as a character. The directory to recursively look in for the above data.
#' @param fileName Character. A String of text to look for the most-recent dataset. 
#' Default = "^BeeData_". Find faults by modifying [BeeBDC::fileFinder()]
#' and logic-checking the file that's found.
#'
#' @return A list with a data frame of merged occurrence records, "Data_WebDL", and a list of EML 
#' files contained in "eml_files". 
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' DataImp <- importOccurrences(path = DataPath)
#' }
importOccurrences <- function(path = path,
           fileName = "^BeeData_" #occurrence file name. If not provided, R will search to match "BeeData_"
           ){ #spatial reference system as epsg code
  . <- NULL
  # Load required packages
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  
  # if the fileName is not provided...
  if(!exists("fileName")){
    fileName = "^BeeData"
  }
  
  #### Find files ####
  # Find all of the previously-produced data files
  most_recent <- BeeBDC::fileFinder(path = path, fileName = fileName)
  
  # Return information to user
  writeLines(paste(" - Great, R has detected file(s), including... ", "\n",
                   paste(most_recent, collapse = "\n") ), sep = "")
  
  #### Detect format ####
    # Find the format of the most-recent files. This could potentially be .csv or .rds
    # TRUE IF .rds are present (that are not attribute files):
  (rdata_query <- any(most_recent[stringr::str_count(most_recent, pattern = "([aA]ttribute)|\\.rds") > 0] %>%
                        stringr::str_detect(., pattern = "([aA]ttribute)", negate = TRUE) == TRUE))
    # TRUE IF .csv data are present:
  (csv_query <- any(stringr::str_detect(most_recent, "([aA]ttributes)")) == TRUE &&
      any(stringr::str_detect(most_recent, ".*\\.csv{1}")) == TRUE)

  #### Both present ####
  # IF their is a complete .rds file among the most-recent files AND a .csv version...
  if(rdata_query == TRUE && csv_query == TRUE){
    writeLines(paste("\n", 
                     " - Oh boy, it looks like there are both .csv and .rds versions of your data!", 
                     "\n", "R will preferentially use the .rds file.", "\n",
                     "NOTE: the .rds file can be very slow to load"))
    # File to read:
    fileLoc <- most_recent[intersect(grep(".*\\.rds{1}", most_recent),
                                     grep("([aA]ttributes)", most_recent, invert = TRUE))]
    # Read in the .rds file
    writeLines(paste("Reading in ", fileLoc, "...", sep = ""))
   
    # Find the index of the string that matches and select that to read in
    occurDF <- fileLoc %>%
      readRDS()
  } #END IF both
  
  #### RData present ####
  # IF their is ONLY a complete .rds file among the most-recent files...
  if(rdata_query == TRUE && csv_query == FALSE){
    writeLines(paste(" - .rds export version found. Loading this file...", "\n",
                     "NOTE: the .rds file can be very slow to load"))
    # File to read:
    fileLoc <- most_recent[intersect(grep(".*\\.rds{1}", most_recent),
                                     grep("([aA]ttributes)", most_recent, invert = TRUE))]
    # Read in the .rds file
    writeLines(paste("Reading in ", 
                     fileLoc,
               "...", sep = ""))
    # Find the index of the string that matches and select that to read in
    occurDF <- as.character(fileLoc) %>%
      readRDS()
    writeLines("Completed reading in .rds file")
  } #END IF .rds

  #### CSV present ####
  # IF their is ONLY a complete .csv file among the most-recent files...
  if(csv_query == TRUE && rdata_query == FALSE){
    writeLines(paste(" - .csv exported version found. Loading this file..."))
    ColTypes <- ColTypeR()
    # Find the most-recent .csv occurrence file
    occurDF <- most_recent[intersect(grep(".*\\.csv", most_recent),
                                      grep("([aA]ttributes)", most_recent, invert = TRUE))] %>%
      read_csv(col_types = ColTypes)
    # Find the most-recent .rds attributes file
    attr_loc <- most_recent[stringr::str_which(most_recent, "(.*[aA]ttribute)(.*\\.rds)")] 
      # Check to see if the attributes file exists or not
    if(length(attr_loc) == 0){
      writeLines("No attribute file found... Please make sure that one exists to include the EML data")
    }else{
      writeLines(
        paste("Reading attribute file named ", attr_loc, "..." ))
      attr_file <- readRDS(attr_loc)
      }
    # Add the attributes file to the occurrence data file 
    attributes(occurDF) <- attr_file
    # Read in the EML file 
      # Find the folder that the attributes file is in.
    EML_home <- stringr::str_replace(attr_loc, pattern = "\\/[a-zA-Z0-9-_]+\\.rds$", "")
      # Find the .xml file in the same location as the attribute's folder
    EML_loc <- BeeBDC::fileFinder(path = EML_home, fileName = "eml.*\\.rds")
      # Read in the EML file
    EML_file <- readRDS(EML_loc)
  } #END IF .csv
  
  # Extract and save the data and the metadata based on their class
  for(i in 1:length(occurDF)){
      # If eml
    if(base::any(class(occurDF[[i]]) %in% c( "emld") )){
      eml_files <- occurDF[i]
    }
      # If tibble 
    if(base::any(class(occurDF[[i]]) %in% c("tbl_df", "data.frame", "tbl"))){
      Data_WebDL <- occurDF[i]
    }
  }

  
#### Return data ####
  # Re-combine the data and EML data
  Data_WebDL <- dplyr::lst(Data_WebDL, 
                          eml_files)
  
  # Return the Data_WebDL
  return(Data_WebDL)
  # Return end product and print completion note
  writeLines(paste(" - Fin.", sep = "\n"))
} # END data_importer


