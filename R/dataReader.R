#####  2.3 dataReader ####
#' @importFrom dplyr %>%
# Read in occurence data with the correct format to be merged
dataReader <-  function(path_i, home_path){
    # locally bind variables to the function
dplyr <- mgsub <- setNames <- . <- family <- day <- NULL
  
requireNamespace("dplyr", "mgsub")

  #Set up bee family list
  Bee_Families <- c("Andrenidae","Apidae", "Colletidae","Halictidae","Megachilidae","Melittidae",
                    "Stenotritidae","andrenidae","apidae", "colletidae","halictidae","megachilidae",
                    "melittidae","stenotritidae")  # Find the paths
  occ_paths <- repoFinder(path = path_i)
  # Get the column types
  ColTypes <- ColTypeR()
  # Get the columns to keep
  ColsToKeep <- names(ColTypes$cols)
  # Make an internal copy of the template for use in the loop as the template tibble
  data_template <- matrix(ncol = length(BeeBDC::ColTypeR()[[1]] %>% names()), nrow = 0) %>% as.data.frame() %>% 
    setNames(BeeBDC::ColTypeR()[[1]] %>% names()) %>% dplyr::tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  #### ALA data ####
  if(grepl("ALA_data", names(path_i)) == "TRUE"){
    # Import these data
    data_i <- readr::read_csv(path_i, col_names = TRUE,
                              # read in all columns as character for now
                              col_types = readr::cols(.default = readr::col_character()),
                              name_repair = "minimal") %>% 
      # Suppress warnings from read_csv
      suppressWarnings(., classes = "warning") %>% 
      # Select only the unique columns
      dplyr::select(unique(colnames(.), fromLast = TRUE))
    # Change column names to match other datasets
    # Remove dcterms: prefixes from some column names
    colnames(data_i) <- mgsub::mgsub(colnames(data_i), 
                                     c("dcterms:"),
                                     c(""))
    # Filter the columns to only those that we want to select, based on the ColsToKeep vector 
    # that we created at the top of the R-script
    if("recordID" %in% colnames(data_i)){
      data_i <- data_i %>%
        # The dataframe to match with
        dplyr::rename("id" = "recordID") 
    }
      
    data_i <- data_i %>%  
      # select columns that match the following string
      dplyr::select( dplyr::matches(      
        # Use the carrot "^" to signify start of string and dollar sign "$" to signify end of 
        # string. Effectivly, this will only return an exact match.
        paste("^",ColsToKeep,"$",sep="") ))  
    # Define the column types to match our standard
    data_i <- data_i %>% # readr::type_convert(data_i, col_types = cols(.default = "c")) %>%
        # Keep only the columns defined in ColTypeR
      dplyr::select(tidyselect::any_of(c(ColsToKeep))) %>%
      # Select only Bee families (in case this is needed, e.g. if you downloaded "Apoidea")
      dplyr::filter(family %in% c(Bee_Families, "", "NA") )
  } # END ALA data IF statement
  
  #### GBIF data ####
  if(grepl("GBIF_data", names(path_i)) == "TRUE"){
    # Read in each file and then merge together
    data_i <- readr::read_tsv(path_i, 
                              quote = "", col_names = TRUE,
                              col_types = readr::cols(.default = readr::col_character())) %>% 
      # Supress warnings from read_tsv
      suppressWarnings(., classes = "warning") %>% 
      # Include all columns from original template file
      dplyr::bind_rows(., data_template) %>%
      # Keep only the columns defined in ColTypeR
      dplyr::select(tidyselect::any_of(c(ColsToKeep))) 
      
    
  } # END GBIF data IF statement

  ####  iDigBio data   #### 
  if(grepl("iDigBio_data", names(path_i)) == "TRUE"){ # Start iDigBio IF statement
    # Import these data
    data_i <- readr::read_csv(path_i, col_names = TRUE,
                              # read in all columns as character for now
                              col_types = readr::cols(.default = readr::col_character()),
                              # Do not keep the some columns
                                col_select = !c("abcd:typifiedName",  "aec:associatedTaxa",
                                                "ala:photographer","ala:species","ala:subfamily",
                                                "ala:subspecies","ala:superfamily","chrono:ChronometricAge",
                                                "dc:language","dc:rights","dc:type",
                                                "dcterms:accessRights","dcterms:bibliographicCitation","dcterms:language",                    
                                                "dcterms:license","dcterms:modified","dcterms:references",                 
                                                "dcterms:rights","dcterms:rightsHolder","dcterms:source",                   
                                                "dcterms:type","obis:ExtendedMeasurementOrFact",
                                                "symbiota:recordEnteredBy","symbiota:recordID",
                                                "symbiota:verbatimScientificName",   
                                                "taxonRankID","zan:ChronometricDate")) %>%
      # Supress warnings from read_csv
      suppressWarnings(., classes = "warning")
    # Change column names to match other datasets
    # Remove dwc. (darwin core), idigbio. and gbif. prefixes from column names
    colnames(data_i) <- mgsub::mgsub(colnames(data_i), 
                                     c("dwc.", "idigbio.", "gbif.", "aec:"),
                                     c("", "","",""))
    # Filter the columns to only those that we want to select, based on the ColsToKeep vector 
    # that we created at the top of the R-script
    data_i <- data_i %>%  # The dataframe to match with
      # select columns that match the following string
      dplyr::select( dplyr::matches(      
        # Use the carrot "^" to signify start of string and dollar sign "$" to signify end of 
        # string. Effectivly, this will only return an exact match.
        paste("^",ColsToKeep,"$",sep="") ))  %>%
          # Remove day ranges that will stop occurrences from being read in.
      dplyr::mutate(day = stringr::str_replace(day, pattern = " -.*|/.*|-.*", replacement = "" ))
    # Define the column types to match our standard
    data_i <- data_i %>%
      # Select only Bee families (in case this is needed, e.g. if you downloaded "Apoidea")
      dplyr::filter(family %in% c(Bee_Families, "", "NA", NA) ) %>%
      # Keep only the columns defined in ColTypeR
      dplyr::select(tidyselect::any_of(c(ColsToKeep))) # %>%
      # Define the column types to match our standard
      # readr::type_convert(col_types = ColTypes) 
  } # End iDigBio IF statement
      
  
  ####  SCAN data #### 
  if(grepl("SCAN_data", names(path_i)) == "TRUE"){ # Start SCAN IF statement
    data_i <- readr::read_csv(path_i, col_names = TRUE,
                              # read in all columns as character for now
                              col_types = readr::cols(.default = readr::col_character())) %>% 
      # Supress warnings from read_csv
      suppressWarnings(., classes = "warning") %>% 
      # Filter the columns to only those that we want to select, based on the ColsToKeep vector 
      # that we created at the top of the R-script
      dplyr::rename("fieldNotes" = "occurrenceRemarks") %>%
      # select columns that match the following string
      dplyr::select( tidyselect::any_of(ColsToKeep)) %>%
      # Select only Bee families (in case )
      dplyr::filter(family %in% c(Bee_Families, "", "NA") ) %>%
    # Keep only the columns defined in ColTypeR
    dplyr::select(tidyselect::any_of(c(ColsToKeep))) # %>%
      # Define the column types to match our standard
        # readr::type_convert(col_types = ColTypes, 
        #                     guess_integer = TRUE) 
  }  # END SCAN IF statement
  return(data_i)
}

