# This function was written by James B Dorey on the 7th of September 2022.
  # It aims to match up database_id numbers applied between different runs by matching the
  # current and prior runs and a list of columns to find matches by. It finally matches the 
  # remaining occurrences by mergine by all columns.
  # For help, please contact jbdorey[at]me.com


#' Attempt to match database_ids from a prior run
#' 
#' This function attempts to match database_ids from a prior bdc or BeeBDC run in order to keep 
#' this column somewhat consistent between iterations. However, not all records contain sufficient
#' information for this to work flawlessly.
#'
#' @param currentData A data frame or tibble. The NEW occurrence records as input.
#' @param priorData A data frame or tibble. The PRIOR occurrence records as input.
#' @param matchBy A list of character vectors Should contain the columns to iteratively compare.
#' @param completeness_cols A character vector. The columns to check for completeness, arrange, 
#' and assign the relevant prior database_id.
#' @param excludeDataset A character vector. The dataSources that are to be excluded from data 
#' matching. These should be static dataSources from minor providers.
#'
#' @return The input data frame returned with an updated database_id column that shows the 
#' database_ids as in priorData where they could be matched. Additionally, a columnd called 
#' idContinuity is returned where TRUE indicates a match to a prior database_id and FALSE 
#' indicates that a new database_id was assigned.
#' 
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom stats complete.cases
#' @importFrom dplyr desc across
#'
#' @examples
#' # Get the example data
#' data("beesRaw", package = "BeeBDC")
#' # Which datasets are static and should be excluded from matching?
#' excludeDataset <- c("BMin", "BMont", "CAES", "EaCO", "Ecd", "EcoS",
#'                     "Gai", "KP", "EPEL", "USGS", "FSCA", "SMC", "Bal", "Lic", "Arm", "BBD", 
#'                     "MEPB")
#'   # Match the data to itself just as an example of running the code.
#' beesRaw_out <- idMatchR(
#'   currentData = beesRaw,
#'   priorData = beesRaw,
#'   # First matches will be given preference over later ones
#'   matchBy = dplyr::lst(c("gbifID"),
#'                         c("catalogNumber", "institutionCode", "dataSource"),
#'                         c("occurrenceID", "dataSource"),
#'                         c("recordId", "dataSource"),
#'                         c("id"),
#'                         c("catalogNumber", "institutionCode")),
#'   # You can exclude datasets from prior by matching their prefixs - before first underscore:
#'   excludeDataset = excludeDataset)

idMatchR <- function(
    currentData = NULL,
    priorData = NULL,
    matchBy = NULL,
    completeness_cols = NULL,
    excludeDataset = NULL){
  # locally bind variables to the function
  dataSource <- completeness <- database_id <- . <- currentConcat <- dataSourceShort <- 
    database_id_matched <- idContinuity <- databaseName <- database_id_current <-
    databaseNum <- missingNum <- database_id_new <- databaseNum_current <- NULL
  
  requireNamespace("dplyr")

    #### 0.0 Prep ####
    ##### 0.1 Errors ####
      ###### a. fatal ####
  if(is.null(currentData)){
    stop("Please proivde a 'currentData' input.")
  }
  if(is.null(priorData)){
    stop("Please proivde a 'priorData' input.")
  }
  if(is.null(matchBy)){
    stop("Please proivde a 'matchBy' input or inputs.")
  }
  
  ###### b. Warnings ####
  if(is.null(completeness_cols)){
    message(paste("Warning message: \n",
                  " - No completeness_cols provided. Using default of: ",
                  "c('decimalLatitude',  'decimalLongitude', 'scientificName', and 'eventDate')",
                  sep=""))
    completeness_cols = c("decimalLatitude",  "decimalLongitude",
                          "scientificName", "eventDate")
  }
  
    ##### 0.2 Rm datasets ####  
    # Change the dataSource for matching
  if(!is.null(excludeDataset)){
    priorData <- priorData %>%
        # Remove all text after the first "_"
      dplyr::mutate(dataSource = stringr::str_remove(dataSource, "_.*")) %>%
        # Remove the datasets
      dplyr::filter(!dataSource %in% excludeDataset)
  }

  
##### 0.3 Format data ####
  # First save a version of the currentData to keep and return
  returnData <- currentData
  # Save a count of priorData rows
  priorRowCount <- nrow(priorData)
  
    ###### a. dataSource ####
    # If the user is matching by DataSource, then simplify that column to only the over-arching 
    # source.
  if(any(stringr::str_detect(string = unlist(matchBy), pattern = "dataSource"))){
      # PRIOR dataset - Only run if Rm datasets isn't running already
    if(is.null(excludeDataset)){
    priorData <- priorData %>%
      # Remove all text after the first "_"
      dplyr::mutate(dataSource = stringr::str_remove(dataSource, "_.*"))
    }
      # CURRENT dataset
    currentData <- currentData %>%
      # Remove all text after the first "_"
      dplyr::mutate(dataSource = stringr::str_remove(dataSource, "_.*"))
  }
  
  ###### b. simplify by select ####
    # Only select the columns that are called by the function
  priorData <- priorData %>%
      # Keep only the columns called for and the database_id
    dplyr::select(tidyselect::all_of(c("database_id", unique(unlist(matchBy), completeness_cols))))
  currentData <- currentData %>%
    # Keep only the columns called for and the database_id
    dplyr::select(tidyselect::all_of(c("database_id", unique(unlist(matchBy), completeness_cols))))
  
  ##### 0.4 Completeness and arrange ####
  ###### a. completeness ####
  # Get the sum of the complete.cases of four important fields. Preference will be given to keeping 
  # the most-complete records
  writeLines(paste(
    " - Generating a basic completeness summary from the ", 
    paste(completeness_cols, collapse = ", "), " columns.","\n",
    "This summary is simply the sum of complete.cases in each column. It ranges from zero to the N",
    " of columns. This will be used to sort duplicate rows and select the most-complete rows.",
    sep = ""
  ))
  priorData <- priorData %>% 
    dplyr::rowwise() %>%
    # Create a new column called "completeness" where higher values are more-complete 
    dplyr::mutate(completeness = sum(complete.cases(completeness_cols))) %>%     
    dplyr::ungroup() %>%
      # Arrange so that the most-complete are on top. This might be overkill.
    dplyr::arrange( desc(completeness))
  gc()
  
  
    #### 1.0 loop ####
  
  writeLines(" - Starting core loop...")
    # Set up a loop dataframe to enter into
  loopDF <- dplyr::tibble()
  # Create a dataset to put unique values into
  for(i in 1:length(matchBy)){
    # Select the ith CustomComparisons to match with
    currentMatch <- matchBy[[i]]
    
    ##### 1.1 single input ####
    if(length(currentMatch) == 1){
      matched <- priorData %>%
        # Remove NA values and get distinct
        tidyr::drop_na(tidyselect::all_of(currentMatch)) %>%
        dplyr::distinct(dplyr::across(tidyselect::all_of(currentMatch)),
                        .keep_all = TRUE) %>%
        # Add a new column with these values concatenated,
        # dplyr::mutate(currentConcat = tidyselect::all_of(currentMatch))
        # JOIN datasets
        dplyr::left_join(., 
                         # FORMAT MATCH
                         currentData %>% dplyr::select(
                           tidyselect::all_of(c("database_id", currentMatch))) %>%
                           # Remove NA values and get distinct
                           tidyr::drop_na(tidyselect::all_of(currentMatch)) %>%
                           dplyr::distinct(dplyr::across(tidyselect::all_of(currentMatch)),
                                           .keep_all = TRUE),
                           by = currentMatch,
                         suffix = c("", "_current")) %>%
        # Extract only the matched ids
        dplyr::select(tidyselect::any_of(c("database_id", "database_id_current"))) %>%
        # Remove empty matches
        tidyr::drop_na()
      
      # User output
      writeLines(paste0(" - we matched ", 
                        format(nrow(matched), big.mark = ","),
                        " records using ", 
                        paste0(currentMatch, collapse = ", "), "."))
      
      # Merge with loopDF
      loopDF <- matched %>% 
        dplyr::bind_rows(loopDF)
    } # End single IF statement
    
    #### 1.2 multiple inputs ####
    if(length(currentMatch) > 1){
      matched <- priorData %>%
      # Remove NA values and get distinct
      tidyr::drop_na(tidyselect::all_of(currentMatch)) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(currentMatch)),
                      .keep_all = TRUE) %>%
      # Add a new column with these values concatenated,
      tidyr::unite(., tidyselect::all_of(currentMatch), col = currentConcat) %>% 
        # JOIN datasets
      dplyr::left_join(., 
                       # FORMAT MATCH
                       currentData %>% dplyr::select(
                         tidyselect::all_of(c("database_id", currentMatch))) %>%
                         # Remove NA values and get distinct
                         tidyr::drop_na(tidyselect::all_of(currentMatch)) %>%
                         dplyr::distinct(dplyr::across(tidyselect::all_of(currentMatch)), 
                                         .keep_all = TRUE) %>%
                         # Add a new column with these values concatenated,
                         tidyr::unite(., tidyselect::all_of(currentMatch), col = currentConcat),
                       by = "currentConcat",
                       suffix = c("", "_current")) %>%
        # Extract only the matched ids
      dplyr::select(tidyselect::any_of(c("database_id", "database_id_current"))) %>%
        # Remove empty matches
        tidyr::drop_na()
      
      # User output
      writeLines(paste0(" - we matched ", 
                        format(nrow(matched), big.mark = ","),
                        " records using ", paste0(currentMatch, collapse = ", "), "."))
      # Merge with loopDF
      loopDF <- matched %>% 
        dplyr::bind_rows(loopDF)
    } # End multiple IF statement

    #### 1.3 Rm matches ####
      # Remove the occurrences that were just matched before the next iteration
    priorData <- priorData %>%
      dplyr::filter(!database_id %in% loopDF$database_id)
    currentData <- currentData %>%
      dplyr::filter(!database_id %in% loopDF$database_id_current)
    
      # User output
    writeLines(paste0("This leaves ",
                      format(nrow(priorData), big.mark = ","),
                      " unmatched data in the priorData file"))
    }# END LOOP

    # Remove data no longer needed
  rm(priorData, currentData)
  
  
  #### 2.0 Data return ####
  writeLines(" - Combining ids and assigning new ones where needed...")
    # Add a column to that matched data:
      # idContinuity, that shows that these ids are continuous with prior versions
  loopDF <- loopDF %>%
    dplyr::mutate(idContinuity = TRUE)
  
    # Change the database_id column for return
  checkedData <- returnData %>%
    select(tidyselect::all_of(c("database_id", "dataSource"))) %>%
      # Filter to only the examined dataSources
    # Remove all text after the first "_"
    dplyr::mutate(dataSourceShort = stringr::str_remove(dataSource, "_.*")) %>%
      # Add the new database_id column, while removing the old one (database_id_current)
    dplyr::left_join(loopDF, by = c("database_id" = "database_id_current"),
                     suffix = c("", "_matched"), keep = FALSE) %>%
      # Remove existing [current] database_id columns
    dplyr::rename(database_id_current = database_id) %>%
      # Move this column to the start
    dplyr::relocate(database_id_matched) %>%
      # Rename the column
    dplyr::rename(database_id = database_id_matched) %>%
      # Highlight those records that might not be id-continuous
    dplyr::mutate(idContinuity = dplyr::if_else(is.na(idContinuity),
                                                FALSE, TRUE)) %>%
    # Add a new column with the database_id NAME
    dplyr::mutate(databaseName = stringr::str_extract(
      string = database_id, pattern = "[a-zA-Z_]+")) %>%
    # Fix those that failed to match
    dplyr::mutate(databaseName = dplyr::if_else(
      is.na(databaseName),
      stringr::str_extract(
        string = database_id_current, pattern = "[a-zA-Z_]+"),
      databaseName)) %>%
    # Add a new column with the database_id NUMBER
    dplyr::mutate(databaseNum = stringr::str_extract(
      string = database_id, "[0-9]+") %>% as.numeric(),
        # Get a column with the current numbers to start the MAX count from
      databaseNum_current = database_id_current %>% stringr::str_extract("[0-9]+") %>% 
        as.numeric()) %>%
    # Group by databaseName
    dplyr::group_by(databaseName) %>%
    # Sort
    dplyr::arrange(databaseNum, .by_group = TRUE)
  
    # Get new numbers for the new data
  newData <- checkedData  %>%
      # Apply by group
    dplyr::group_by(databaseName) %>%
      # Add a new column with the databaseNum numbers
    dplyr::mutate(missingNum = databaseNum) %>%
      # If the group is entirely unmatched, assign the first row in that group to equal 1
    dplyr::mutate(missingNum = dplyr::if_else(dplyr::row_number() == 1 & is.na(missingNum[[1]]),
                  1,
                  missingNum)) %>%
      # Fill down the missing numbers starting from 1+ the maximum within databaseName group.
    dplyr::mutate(missingNum = dplyr::if_else(is.na(missingNum), 
                                        (max(databaseNum_current, na.rm = TRUE)+
                                           dplyr::row_number()-sum(complete.cases(missingNum))  
                                         ),
                  missingNum)) %>%
      # Update the database_id column
    dplyr::mutate(database_id = stringr::str_c(databaseName, missingNum)) %>% 
      # Filter for only NA values on the databaseNum column
    dplyr::filter(is.na(databaseNum)) 
    
  
    # Now combine
  checkedData <- checkedData %>%
      # First, remove those newData from the checkedData
    dplyr::filter(!database_id_current %in% newData$database_id_current) %>%
      # now re-combine
    dplyr::bind_rows(newData) %>%
      # Remove groupings
    dplyr::ungroup() %>%
    dplyr::distinct(database_id, .keep_all = TRUE) %>%
    # Remove the excludeDataset
    dplyr::filter(!dataSourceShort %in% excludeDataset)

  # User output
  writeLines(paste0(" - We matched a total of ",
                    format(sum(complete.cases(checkedData$databaseNum)), big.mark = ","),
                    " database_id numbers. We then assigned new database_id numbers to ",
                    format(sum(complete.cases(checkedData$missingNum)), big.mark = ","),
                    " unmatched occurrences."
                    ))

  
    # Merge the new databse IDs with the returnData
  returnData <- returnData %>%
      # Join the checkedData dataset
    dplyr::left_join(checkedData %>% dplyr::select(
      tidyselect::any_of(c("database_id", "database_id_current", "idContinuity"))),
                     by = c("database_id" = "database_id_current"),
                     suffix = c("", "_new")) %>%
    # Update the database_id column to include the new database_ids, or the old ones where
      # new ones aren't available.
    dplyr::mutate(database_id = dplyr::if_else(is.na(database_id_new),
                                                # If from an excluded dataset, 
                                                # keep existing database_id
                                               database_id,
                                                # Otherwise Assign the newly matched id
                                               database_id_new)) %>%
    dplyr::select(!database_id_new)
  

    # Return the data
  return(returnData )
  
}# END function
