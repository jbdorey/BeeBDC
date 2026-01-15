
#### Aux. functions ####

##### a. dataSaver ####
#' Simple function to save occurrence AND EML data as a list
#' 
#' Used at the end of 1.x in the example workflow in order to save the occurrence dataset and its associated eml metadata.
#'
#' @param path Character. The main file path to look for data in.
#' @param save_type Character. The file format in which to save occurrence and EML data. 
#' Either "R_file" or "CSV_file"
#' @param occurrences The occurrences to save as a data frame or tibble.
#' @param eml_files A list of the EML files.
#' @param file_prefix Character. A prefix for the resulting output file.
#'
#' @return This function saves both occurrence and EML data as a list when save_type = "R_File" or
#' as individual csv files when save_type = "CSV_file".
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' dataSaver(path = tempdir(),# The main path to look for data in
#' save_type = "CSV_file", # "R_file" OR "CSV_file"
#' occurrences = Complete_data$Data_WebDL, # The existing datasheet
#' eml_files = Complete_data$eml_files, # The existing EML files
#' file_prefix = "Fin_") # The prefix for the file name
#' }
#' 
dataSaver <- function(path = NULL,
                       save_type = NULL,
                       occurrences = NULL,
                       eml_files = NULL,
                       file_prefix = NULL){
  # locally bind variables to the function
  . <- countComplete <- NULL
  
  # Check if the save type is present AND valid
  if(exists("save_type") == FALSE){ # If there are no data matching the name...
    stop(" - Heck! You must choose a save_type for the data output. Either .rds or .csv files")
  } # END Missing save_type type
  if(sum(stringr::str_count(pattern = c("R_file","CSV_file"), string = save_type)) == 0 ){
    stop(" - Heck! Please choose a valid save_type for the data output. Either .rds (R_file) or .csv files (csv_files)")
  } # END invalid save_type
  # If there is no file prefix... name it "BeeData_"
  if(is.null(file_prefix) == TRUE){
    file_prefix <- "BeeData_"
  }else{
    file_prefix <- paste(file_prefix,"BeeData_", sep = "")
  }
  # Notify user if an out_file has been created and make one, if required.
  outPath <- outFile_maker(path = path)
  # Extract the attribute data
  occurrences_attributes <- attributes(occurrences)
  
  # Find the empty columns
  colTest <- occurrences %>% 
    dplyr::summarise(dplyr::across(tidyselect::everything(), ~ sum(complete.cases(.)))) 
  colKeeps <- dplyr::tibble(column = colnames(colTest),
                 countComplete = t(colTest)[,1]) %>%
    dplyr::filter(countComplete > 0)
  # Cols to remove:
  colRemoves <- dplyr::tibble(column = colnames(colTest),
         countComplete = t(colTest)[,1]) %>%
    dplyr::filter(countComplete == 0)
    # Discard empty columns
  occurrences <- occurrences %>% 
    dplyr::select(tidyselect::all_of(colKeeps$column))
  message(paste0(
    " - We have removed empty columns. This is standard, but as an FYI, these columns are: ",
    paste(colRemoves$column, collapse = ", ")
  ))
  

  #### R save ####
  # Save R data
  if(save_type == "R_file"){
    writeLines( paste(" - Writing occurrence, attribute, and EML data file in .rds format...", "\n",
                      "Number of records: ", format(nrow(occurrences), big.mark=",",scientific=FALSE), "\n",
                      "Number of attribute sources: ", format(nrow(occurrences_attributes$dataSource), 
                                                              big.mark=",",scientific=FALSE), "\n",
                      "The ", length(names(eml_files)), " eml sources are ", 
                      paste(names(eml_files), collapse = ", "), "\n",
                      "Writing to file called ", paste(file_prefix, Sys.Date(), ".rds", sep = ""),
                      " at location ", outPath,"...",
                      sep = ""))
    # Save all of these data into a .rds format
    list(occurrences, eml_files) %>%
      saveRDS(., file = paste(outPath, "/", file_prefix, Sys.Date(), ".rds", sep = ""))
  }
  
  #### csv save ####
  # Save csv files
  if(save_type == "CSV_file"){
    ##### Occ. file ####
    writeLines( paste(" - Writing occurrence data file in csv format...", "\n",
                      "Number of rows (records): ", format(nrow(occurrences), big.mark=",",scientific=FALSE), "\n",
                      "Writing to file called ", paste(file_prefix, "combined_", Sys.Date(), ".csv", sep = ""),
                      " at location ", outPath,"...",
                      sep = "")) 
    # Write the occurence file
    readr::write_excel_csv(occurrences, paste(outPath, "/", file_prefix, "combined_", Sys.Date(), ".csv", sep = ""))
    
    #### Attr. file ####
    # Notfiy user that attribute data are being written
    occurrences_attributes <- attributes(occurrences)
    writeLines( paste(" - Writing attribute data file in csv format...", "\n",
                      "Number of rows (sources): ", format(nrow(occurrences_attributes$dataSource), 
                                                           big.mark=",",scientific=FALSE), "\n",
                      "Written to file called ", paste(file_prefix, "attributes_", Sys.Date(), ".csv", 
                                                       sep = ""),
                      " at location ", outPath,
                      sep = "")) 
    
    #### DataSource file #####
    # Write the citations file
    # Write the occurrence file
    readr::write_excel_csv(occurrences_attributes$dataSource, paste(outPath, "/" ,file_prefix, "attributes_", 
                                                      Sys.Date(), ".csv", sep = ""))
    
    #### All attributes ####
    # Update the names of each list element to reflect their source and taxonomic coverage
    names(occurrences_attributes$dataSource$citations) <- paste(occurrences_attributes$dataSource$dataSource, "_citations", sep = "")
    names(occurrences_attributes$dataSource$rights) <- paste(occurrences_attributes$dataSource$dataSource, "_rights", sep = "")
    names(occurrences_attributes$dataSource$abstract) <- paste(occurrences_attributes$dataSource$dataSource, "_abstract", sep = "")
    
    #### EML file ####
    if(!is.null(eml_files)){
    # Notify user that the .eml file is being written
    writeLines( paste(" - Writing eml file in xml format...", "\n",
                      "The ", length(names(eml_files)), " eml sources are ", 
                      paste(names(eml_files), collapse = ", "), "\n",
                      "Written to file called ", paste("eml_files", Sys.Date(),".xml", sep="" ),
                      " at location ", outPath,
                      sep = "")) 
    # Write the compounded .eml file as a .rds file
    saveRDS(eml_files, file = paste(outPath, "/eml_files", Sys.Date(),".rds", sep="" ))
    
    # Write the .rds file with all attribute information - this file can then be read into R again later 
    occurrences_attributes %>%
      saveRDS(., file = paste(outPath, "/", file_prefix, "completeAttributes_", Sys.Date(), ".rds", sep = ""))
  }}
  # Print completion note
  writeLines(paste(" - dataSaver. Fin.", sep = "\n"))
} # END dataSaver



#### +++++ ####



##### b. Set strings ####
#Set up bee family list
Bee_Families <- c("Andrenidae","Apidae", "Colletidae","Halictidae","Megachilidae","Melittidae",
                  "Stenotritidae","andrenidae","apidae", "colletidae","halictidae","megachilidae",
                  "melittidae","stenotritidae")


##### c. outFile_maker ####
outFile_maker <- function(path = path, file2make = "out_file"){
  # Write user output...
  writeLines(" - Checking for existing out_file directory...")
  # Look for outfile
  outFileLoc <- file.info(list.files(path, full.names = T, 
                                     pattern = file2make,
                                     recursive = TRUE,
                                     include.dirs = TRUE)
  )
  # IF there is not outfile, create one.
  if(nrow(outFileLoc) == 0){
    writeLines(paste(" - No existing,", file2make, " directory found. Creating directory...", sep = ""))
    dir.create(path = paste(path, file2make, sep = "/"))
  } # END create outfile
  # IF there IS an outfile, create one.
  if(nrow(outFileLoc) != 0){
    writeLines(paste(" - Existing ", file2make, "directory found. Data will be saved here.", sep = ""))
  } # END create outfile
  return(paste(path, file2make, sep = "/"))
} # END outFile_maker



#### ++++++ ####
#### d. fileFinder ####
#' Finds files within a directory
#' 
#' A function which can be used to find files within a user-defined directory based on a 
#' user-provided character string.
#'
#' @param path A directory as character. The directory to recursively search.
#' @param fileName A character/regex string. The file name to find.
#'
#' @return Returns a directory to the most-recent file that matches the provide file. Using regex
#' can greatly improve specificity.
#' The function will also write into the console the file that it has found - it is worthwhile to
#'  check that this is the correct file to avoid complications down the line
#' 
#' @importFrom stats complete.cases
#' @importFrom dplyr desc  %>%
#' 
#' @export
#'
#' @examples
#' \donttest{
#' # load dplyr
#' library(dplyr)
#' 
#'  # Make the RootPath to the tempdir for this example
#'   RootPath <- tempdir()
#'   
#'  # Load the example data
#'  data("beesRaw", package = "BeeBDC")
#' 
#' # Save and example dataset to the temp dir
#'   readr::write_csv(beesRaw, file = paste0(RootPath, "/beesRaw.csv"))
#' 
#'  # Now go find it!
#' fileFinder(path = RootPath, fileName = "beesRaw")
#' # more specifically the .csv version
#' fileFinder(path = RootPath, fileName = "beesRaw.csv")
#' }
fileFinder <- function(path, fileName){
  # locally bind variables to the function
  . <- dates <- NULL
  
  # Find all of the previously-produced data files
  locations <- file.info(list.files(path, full.names = T, pattern = fileName,
                                    recursive = TRUE))
  # Check if the data are present
  if(nrow(locations) == 0){ # If there are no data matching the name...
    stop(paste0(" - Bugger it, R can't find the file that you're looking for :(\n",
                "Please check that it exists in the provided directory."))
  }
  ##### Date from name ####
  # Extract only the file name to find the date from...
  FileName_dates <- stringr::str_replace(rownames(locations), ".+/", "") %>%
    # Remove additional text
    stringr::str_replace(., "[a-zA-Z\\/\\_]+[a-zA-Z\\/\\_]+", "") %>%
    # Find those files with dates in their name in d-m-y format
    dplyr::tibble(
      # Extract the dates from the file path rownames and supress the warning from non-matches
      suppressWarnings(lubridate::dmy(.), classes = "warning")) %>%
    # Set the column names of this new tibble
    stats::setNames(c("Locs","dates"))
       # IF there are no dates, look for ymd format
     if(sum(complete.cases(FileName_dates$dates)) == 0){
       FileName_dates <- stringr::str_replace(rownames(locations), ".+/", "") %>%
         # Remove additional text
         stringr::str_replace(., "[a-zA-Z\\/\\_]+[a-zA-Z\\/\\_]+", "") %>%
         # Find those files with dates in their name in d-m-y format
         dplyr::tibble(
           # Extract the dates from the file path rownames and supress the warning from non-matches
           suppressWarnings(lubridate::ymd(.), classes = "warning")) %>%
         # Set the column names of this new tibble
         stats::setNames(c("Locs","dates"))
     }
  # Insert the full locations in the Locs column
  FileName_dates$Locs <- rownames(locations)
  # Sort from most- to least-recent files
  FileName_dates <- dplyr::arrange(FileName_dates, dplyr::desc(dates))
  
  ##### Date from ctime ####
  # IF there are NO dates in the file names, use the file's ctime
  if(sum(complete.cases(FileName_dates$dates)) == 0){
    writeLines(" - No dates in file name(s). Finding most-recent from file save time...")
    # Find the most-recent file
    maxTime <- max(locations$ctime)
    # Extract the correct rowname (path)
    most_recent <- rownames(locations)[stringr::str_which(string = locations$ctime,
                                                 pattern = as.character(maxTime))]
    
  }else{
    writeLines(" - Dates found in file name(s). Finding most-recent file from file name...")
    # Return the strings containing this date
    most_recent <- FileName_dates[1,1] 
  } # END else
  # User output text
  writeLines(paste(
    " - Found the following file(s):", "\n",
    most_recent
  ))
  # Return this file location
  return(most_recent[[1]])
} # END fileFinder

#### ++++++ ####
#### e. re-export pipe operator ####
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL



