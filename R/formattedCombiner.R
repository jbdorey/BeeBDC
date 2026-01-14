
#### 5. formatted combiner ####
# Currently works to combine the USGS data with the rest of the data when the string is as below.
# But, it should also work for any properly-formatted files
# strings = c("USGS_[a-zA-Z_]+[0-9]{4}-[0-9]{2}-[0-9]{2}")

#' Combine the formatted USGS data with the main dataset
#' 
#' Merges the Darwin Core version of the USGS dataset that was created using [BeeBDC::USGS_formatter()]
#' with the main dataset.
#'
#' @param path A directory as character. The directory to look in for the formatted USGS data. 
#' @param strings A regex string. The string to find the most-recent formatted USGS dataset.
#' @param existingOccurrences A data frame. The existing occurrence dataset.
#' @param existingEMLs An EML file. The existing EML data file to be appended.
#'
#' @return A list with the combined occurrence dataset and the updated EML file.
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' DataPath <- tempdir()
#' strings = c("USGS_DRO_flat_27-Apr-2022")
#'     # Combine the USGS data and the existing big dataset
#' Complete_data <- formattedCombiner(path = DataPath, 
#'                                     strings = strings, 
#'                                     # This should be the list-format with eml attached
#'                                     existingOccurrences = DataImp$Data_WebDL,
#'                                     existingEMLs = DataImp$eml_files) 
#'                                     }

formattedCombiner <- function(path, 
                               strings, 
                               existingOccurrences,
                               existingEMLs){
  # locally bind variables to the function
  . <- NULL
  
  requireNamespace("dplyr")
  requireNamespace("xml2")
  # Find all of the previously-produced data files
  BeeData_Locs <- file.info(list.files(path, full.names = T, pattern = strings,
                                       recursive = TRUE))
  # Check if the data are present
  if(nrow(BeeData_Locs) == 0){ # If there are no data matching the name...
    stop(" - Bugger it, R can't find any files produced by our package in the path provided :(")
  }
  # Find the most-recent files based on date in file name
  file_dates <- stringr::str_extract(rownames(BeeData_Locs), 
                                     pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    # Sort from most- to least-recent files
    sort(decreasing = TRUE) 
  # Return the strings containing this date
  most_recent <- stringr::str_subset(rownames(BeeData_Locs), 
                                     pattern = file_dates[1]) 
  # Return information to user
  writeLines(paste(" - Great, R has detected some files. These files include: ", "\n",
                   paste(most_recent, collapse = "\n") ), sep = "")
  
  #### CSV import ####
  # IF there is ONLY a complete .csv file among the most-recent files...
  if(any(stringr::str_detect(most_recent, paste(strings,".csv", sep = ""))) == TRUE &&
     all(stringr::str_detect(most_recent, paste(strings,".rds", sep = ""))) == FALSE){
    writeLines(paste(" - .csv export version found. Loading this file..."))
    ColTypes <- ColTypeR()
    # Find the most-recent .csv occurrence file
    # Find the file that deos NOT include "attribute" or "problems" in the string
    occ_file <- most_recent[stringr::str_which(most_recent, negate = TRUE,
                                               paste("attribute|problems", sep = ""))] %>%
      # Read in .csv file and supress warnings
      readr::read_csv(col_types = ColTypes) %>%
      suppressWarnings(., classes = "warning")
    # Check if attributes file is in .rds format or not and preferentially read this if present
    Rdata_test <- most_recent[stringr::str_which(most_recent, paste("attribute", sep = ""))] %>%
      stringr::str_detect(pattern = ".rds") 
    # Find the most-recent .rds attributes file
    if(Rdata_test == TRUE){
      # Find the file that does include "attribute in the name
      attr_file <- most_recent[stringr::str_which(most_recent, paste("attribute", sep = ""))] %>%
        base::readRDS()
    } # END .rds portion of csv
    # Find the most-recent .csv attributes file
    if(Rdata_test == FALSE){
      # Find the file that does include "attribute in the name
      attr_file <- most_recent[stringr::str_which(most_recent, paste("attribute", sep = ""))] %>%
        readr::read_csv(col_types = readr::cols_only(
          dataSource = "c", alternateIdentifier = "c", title = "c", pubDate = "D", dateStamp = "c",
          doi = "c", downloadLink = "c", abstract = "c", citations = "c", downloadCitation = "c",
          rights = "c"))
      # Turn the abstract, citations, and rights into lists
      attr_file$abstract <- dplyr::lst(attr_file$abstract)
      attr_file$citations <- dplyr::lst(attr_file$citations)
      attr_file$rights <- dplyr::lst(attr_file$rights)
    } # END .rds portion of csv
    # Look for a .xml file
    if(any(stringr::str_detect(most_recent,pattern = ".xml")) == TRUE){
      # Find and read in the .xml file
      xml_file <- most_recent[stringr::str_which(most_recent, paste(".xml", sep = ""))] %>%
        file() %>%
        xml2::read_xml()
    } # END xml look
  } #END IF .csv
  
  #### Merge ####
  # print user information
  writeLines( paste(" - Merging occurrence and attribute files.", "\n",
                    "Depending on file size, this could take some time...","\n",
                    sep = ""))
  ##### dataset and attrs ####
  # Merge the datasets
    # If the dataset appears as a list, make it not a list.
  if(any(class(existingOccurrences) %in% "list")){
    existingOccurrences <- existingOccurrences[[1]]
  }
  bound_data <- dplyr::bind_rows(existingOccurrences %>%
                                   # Convert column types
                                   readr::type_convert(col_types = ColTypeR()), 
                                 occ_file)
  # Extract the existing attributes file
  extAttr_file <-  attributes(existingOccurrences)
  # Bind the dataSource tables together
  extAttr_file$dataSource <- dplyr::bind_rows(extAttr_file$dataSource, attr_file)
  # Update this in the bound_data tibble 
  attr(existingOccurrences, "dataSource") <- extAttr_file$dataSource
  # Replace the existing data with the newly-bound data
  existingOccurrences <- bound_data
  

  ##### xmls ####
  # IF the xml file exists, append it to the eml_files 
  if(exists("xml_file") == TRUE){
    # append the new xml_file to the existing ones
    existingEMLs <- append(existingEMLs, xml_file)
  }
  
  # Combine these files back into a list
  existing_data <- list(existingOccurrences, existingEMLs) %>%
    setNames(c("Data_WebDL", "eml_files"))

  
  # Return end product and print completion note
  writeLines(paste(" - Fin.", sep = "\n"))
  # Return the outfile
  return(existing_data)
} # COMPLETE formattedCombiner


