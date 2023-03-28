# This function was written by James Dorey to remove duplicates using between one and three methods
  # This was written between the 25th and 27th of May. For help, please contact James at
  # jbdorey@me.com



# SUPERSEDED. Previously run like:
      #     check_time <- jbd_occDeDuplicator(
      #       data = check_time,
      #       path = DataPath,
      #       # Years ABOVE this will not be used to sort dateIdentified
      #       maxYear = 2022,
      #       # Years BELOW this will not be used to sort dateIdentified
      #       minYear = 1700,
      #       # The columns to generate completeness info from (duplicate occurrences with more of these
      #       # columns full will be preferred)... I might need to think about this, but it'll be
      #       # harmless if left in anyway.
      #       completeness_cols = c("decimalLatitude",  "decimalLongitude",
      #                             "scientificName", "eventDate"),
      #       # options are "ID","collectionInfo", or "both"
      #       duplicatedBy = "both",
      #       # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo — co-examined
      #       # with the ID columns.
      #       idColumns = c("gbifID", "occurrenceID", "recordID","id"),
      #       # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
      #       collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
      #                          "recordedBy"),
      #       # The columns to combine, one-by-one with the collectionCols
      #       collectInfoColumns = c("recordNumber", "eventID", "catalogNumber", "otherCatalogNumbers",
      #                              "collectionID"),
      #       # If you want to de-deplicate on certain columns then select = TRUE. This will be processed LAST
      #       quickDeDuplicate = FALSE,
      #       # The columns to de-duplicate by. IF NULL, will use defaults:
      #       # "decimalLatitude",  "decimalLongitude", "scientificName", and "eventDate"
      #       # These defaults are extremely conservative. For the global bee dataset, you will end up with 
      #       # ~4.7 million unflagged occurrences.
      #       quickColumns = NULL
      #     )

# We will FLAG duplicates here. Duplicate removal will be first based on:
  # Matching gbifID, datasetID, occurrenceID, recordId, and id
# After these are are FLAGGED, we will FLAG duplicates based on:
  # (i) identical lat+long, (ii) identical scientificName, (iii) identical eventDate, 
  # (iv) recordNumber, (v) recordedBy, (vi) eventID, (vii) catalogNumber, 
  # (viii) otherCatalogNumbers, and (ix) collectionID while ignoring na values. For the latter 
  # six (iv <- ix), these will only be used when available.

jbd_occDeDuplicator <- function(
    data = NULL,
    maxYear = NULL,
    minYear = NULL,
      # The columns to generate completeness info from
    completeness_cols = NULL,
    duplicatedBy = NULL,
      # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
    collectionCols = NULL,
    chordDiagram = TRUE,
      # If you want to de-deplicate on certain columns then select = TRUE. This will be processed LAST
    quickDeDuplicate = NULL,
      # The columns to de-duplicate by.
    quickColumns = NULL
    ){
  startTime <- Sys.time()
  #### 0.0 Prep ####
    ##### 0.1 Errors ####
      ###### a. FATAL error ####
  if(is.null(data)){
    stop(" — Please provide an argument for data. I'm a program not a magician.")
  }
  
  ###### b. Warnings ####
  if(is.null(duplicatedBy)){
    message(paste("Warning message: \n",
                " — No duplicatedBy provided. Consider if you want to choose to find duplicates by (i)",
               " 'ID' columns only (for pre-cleaned data), by (ii) 'collectionInfo' columns only ",
               "(for cleaned data), or (ii) 'both'.\n",
               "NULL is acceptable, if quickDeDuplicate == TRUE",
               sep = ""))
  }
  if(is.null(maxYear)){
    message(paste("Warning message: \n",
                  " — No maxYear provided. Using default of: 2022.",
                  sep = ""))
    maxYear == 2022
  }
  if(is.null(minYear)){
    message(paste("Warning message: \n",
                  " — No minYear provided. Using default of: 1700",
                  sep = ""))
    minYear == 1700
  }
  if(is.null(completeness_cols)){
    message(paste("Warning message: \n",
                  " — No completeness_cols provided. Using default of: ",
                  "c('decimalLatitude',  'decimalLongitude', 'scientificName', and 'eventDate')",
                  sep=""))
    completeness_cols = c("decimalLatitude",  "decimalLongitude",
                          "scientificName", "eventDate")
  }
  

    ##### 0.2 Data prep #####
  # Get the sum of the complete.cases of four important fields. Preference will be given to keeping 
    # the most-complete records
  writeLines(paste(
    " — Generating a basic completeness summary from the ", 
    paste(completeness_cols, collapse = ", "), " columns.","\n",
    "This summary is simply the sum of complete.cases in each column. It ranges from zero to the N",
    " of columns. This will be used to sort duplicate rows and select the most-complete rows.",
    sep = ""
  ))
  data <- data %>% 
    dplyr::rowwise() %>%
      # Create a new column called "completeness" where higher values are more-complete 
    dplyr::mutate(completeness = sum(complete.cases(completeness_cols))) %>%     
    dplyr::ungroup()

  
    ##### 0.3 Set strings ####
  # select the columns to keep 
  cols2keep_1 <- unique(c("database_id",completeness_cols, 
                        "decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                        "gbifID", "datasetID", "occurrenceID", "recordId", "id",
                        "recordNumber", "recordedBy", "eventID", "catalogNumber", "otherCatalogNumbers", 
                        "collectionID",
                        "dateIdentified", "completeness","yearIdentified"))
  cols2keep_2 <- unique(c("database_id",completeness_cols, 
                          "decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                          "gbifID", "datasetID", "occurrenceID", "recordId", "id",
                          "recordNumber", "recordedBy", "eventID", "catalogNumber", "otherCatalogNumbers", 
                          "collectionID",
                          "dateIdentified", "completeness","yearIdentified"))
    # Strings of months to remove from ID columns
  monthStrings <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                    "jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "aug", "sep", "oct","nov", "dec",
                    "January", "February", "March", "April",
                    "May", "June","July","August",
                    "September","October","November","December",
                    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                    "JUL", "AUG", "SEP", "OCT","NOV", "DEC",
                    "JANUARY", "FEBRUARY", "MARCH", "APRIL",
                    "MAY", "JUNE","JULY","AUGUST",
                    "SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER",
                    "sept")
  duplicates_1 = NULL
  duplicates_2 = NULL
  duplicates_12 = NULL
    ##### 0.4 format dateIdentified ####
  # Format the dateIdentified column
  writeLines(paste(
    " — Formatting the dateIdentified column to date format...", 
    sep = ""
  ))
  data$dateIdentified <- lubridate::ymd_hms(data$dateIdentified,
                                                   truncated = 5, quiet = TRUE) %>%
    as.Date()
  
  # Add a column for yearIdentified
  data$yearIdentified <- lubridate::year(data$dateIdentified) 
  # Remove years out of the reasonable range defined by maxYear and minYear
  data$yearIdentified <- ifelse(data$yearIdentified > maxYear | 
                                  data$yearIdentified < minYear,
                                NA, data$yearIdentified)
  

  #### 1.0 ID duplicates ####
    # If user selects ID or both, run this...
  if(any(duplicatedBy %in% "ID" | duplicatedBy %in% "both")){
    ##### 1.1 ALL duplicated ####
    writeLines(paste(
      " — Identifying records that match for all columns (except for database_id)..."
    ))
    # First remove all rows that are entirely duplicated
  tempData <- data %>%
    dplyr::distinct(dplyr::across(!database_id), .keep_all = TRUE) %>%
    dplyr::select(database_id)

    # Start a list of duplicates to remove.
  identifiedDuplicates <- data %>%
    dplyr::filter(!database_id %in% tempData$database_id) %>%
    dplyr::select(database_id)
  # User output
  writeLines(paste(
    "We identified ", 
    format(nrow(data) - nrow(tempData), big.mark = ","), " completely duplicated records.",
    sep = ""))
  

    # Remove these duplicates from the data and turn into runningData
  runningData <- data %>%
      # Cut down the runningData to make it more-workable.
    dplyr::select( tidyselect::all_of(cols2keep_1)) %>%
    dplyr::filter(!database_id %in% identifiedDuplicates$database_id)
  
  ##### 1.2 id matches ####
    ###### a. gbifID #######
    # Keep a record of how many records you started with
  countStart21 <- nrow(runningData)
    # Get the ids for the de-duplicated records
  tempData <- runningData %>% 
      # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, gbifID) %>%
    # Select only cases with gbifID present.
    dplyr::filter(complete.cases(gbifID)) %>% 
      # Sort so that higher yearIdentified is given preference
    dplyr::arrange( desc(yearIdentified)) %>%
      # Sort so that higher completeness is given FIRST preference
    dplyr::arrange( desc(completeness)) %>%
      # Group by gbifID and then take the first occurrence record now that they have been sorted.
    dplyr::distinct(dplyr::across(gbifID), .keep_all = TRUE) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
    # Select non-examined cases to keep.
  temp_ids <- runningData %>% 
      # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, gbifID) %>%
      # Select only cases with gbifID present.
    dplyr::filter(!complete.cases(gbifID)) %>%
      # Return only these IDs.
    dplyr::select(database_id)
  
    # Filter the data
  runningData <- runningData %>%
    dplyr::filter(database_id %in% c(tempData$database_id, temp_ids$database_id))
  
  writeLines(paste(
    "We identified ", countStart21 - nrow(runningData), " duplicates using gbifID.",
  sep = ""))
  
          #    ###### ! b. datasetID ! ####
  # NO longer filtering by this.
        #    
          #    # Keep a record of how many records you started with
          #    countStart22 <- nrow(runningData)
          #      # Remove too-simple codes...
          #    runningData$datasetID <-  ifelse( stringr::str_count(runningData$datasetID, "[A-Za-z]") > 2 &
          #                      stringr::str_count(runningData$datasetID, "[0-9]") > 6 &
          #                       stringr::str_count(runningData$datasetID, "doi") == 0 &
          #                      stringr::str_count(runningData$datasetID, 
          #                                         paste(monthStrings, collapse = "|")) == 0,
          #                      
          #                     runningData$datasetID, NA)
          #    
          #    # Get the ids for the de-duplicated records
          #    tempData <- runningData %>% 
          #      # select a subset of columns to speed process and save space.
          #      dplyr::select(database_id, yearIdentified, completeness, datasetID) %>%
          #      # Select only cases with datasetID present.
          #      dplyr::filter(complete.cases(datasetID)) %>% 
          #      # Sort so that higher yearIdentified is given preference
          #      dplyr::arrange( desc(yearIdentified)) %>%
          #      # Sort so that higher completeness is given FIRST preference
          #      dplyr::arrange( desc(completeness)) %>%
          #      # Group by datasetID and then take the first occurrence record now that they have been sorted.
          #      dplyr::distinct(dplyr::across(datasetID), .keep_all = TRUE) %>%
          #      # Return only these IDs.
          #      dplyr::select(database_id)
          #    
          #    # Select non-examined cases to keep.
          #    temp_ids <- runningData %>% 
          #      # select a subset of columns to speed process and save space.
          #      dplyr::select(database_id, yearIdentified, completeness, datasetID) %>%
          #      # Select only cases with datasetID present.
          #      dplyr::filter(!complete.cases(datasetID)) %>%
          #      # Return only these IDs.
          #      dplyr::select(database_id)
          #    
          #    # Filter the data
          #    runningData <- runningData %>%
          #      dplyr::filter(database_id %in% c(tempData$database_id, temp_ids$database_id))
          #    
          #    writeLines(paste(
          #      "We identified ", format(countStart22 - nrow(runningData), big.mark = ","),
          #      " duplicates using datasetID.",
          #      sep = ""))
          #    
  
  ###### c. occurrenceID ####

  # Keep a record of how many records you started with
  countStart23 <- nrow(runningData)
  # Remove too-simple codes...
  runningData$occurrenceID <-  ifelse( stringr::str_count(runningData$occurrenceID, "[A-Za-z]") > 2 &
                                      stringr::str_count(runningData$occurrenceID, "[0-9]") > 6 &
                                      stringr::str_count(runningData$occurrenceID, "doi") == 0 &
                                      stringr::str_count(runningData$occurrenceID, 
                                                         paste(monthStrings, collapse = "|")) == 0,
                                    runningData$occurrenceID, NA)
  
  # Get the ids for the de-duplicated records
  tempData <- runningData %>% 
    # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, occurrenceID) %>%
    # Select only cases with occurrenceID present.
    dplyr::filter(complete.cases(occurrenceID)) %>% 
    # Sort so that higher yearIdentified is given preference
    dplyr::arrange( desc(yearIdentified)) %>%
    # Sort so that higher completeness is given FIRST preference
    dplyr::arrange( desc(completeness)) %>%
    # Group by occurrenceID and then take the first occurrence record now that they have been sorted.
    dplyr::distinct(dplyr::across(occurrenceID), .keep_all = TRUE) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
  # Select non-examined cases to keep.
  temp_ids <- runningData %>% 
    # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, occurrenceID) %>%
    # Select only cases with occurrenceID present.
    dplyr::filter(!complete.cases(occurrenceID)) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
  # Filter the data
  runningData <- runningData %>%
    dplyr::filter(database_id %in% c(tempData$database_id, temp_ids$database_id))
  
  writeLines(paste(
    "We identified ", format(countStart23 - nrow(runningData), big.mark = ","),
    " duplicates using occurrenceID.",
    sep = ""))
  
  
  ###### d. recordId ####

  # Keep a record of how many records you started with
  countStart24 <- nrow(runningData)
  # Remove too-simple codes...
  runningData$recordId <-  ifelse( stringr::str_count(runningData$recordId, "[A-Za-z]") > 2 &
                                         stringr::str_count(runningData$recordId, "[0-9]") > 6 &
                                         stringr::str_count(runningData$recordId, "doi") == 0 &
                                         stringr::str_count(runningData$recordId, 
                                                            paste(monthStrings, collapse = "|")) == 0,
                                       runningData$recordId, NA)
  
  # Get the ids for the de-duplicated records
  tempData <- runningData %>% 
    # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, recordId) %>%
    # Select only cases with recordId present.
    dplyr::filter(complete.cases(recordId)) %>% 
    # Sort so that higher yearIdentified is given preference
    dplyr::arrange( desc(yearIdentified)) %>%
    # Sort so that higher completeness is given FIRST preference
    dplyr::arrange( desc(completeness)) %>%
    # Group by recordId and then take the first occurrence record now that they have been sorted.
    dplyr::distinct(dplyr::across(recordId), .keep_all = TRUE) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
  # Select non-examined cases to keep.
  temp_ids <- runningData %>% 
    # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, recordId) %>%
    # Select only cases with recordId present.
    dplyr::filter(!complete.cases(recordId)) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
  # Filter the data
  runningData <- runningData %>%
    dplyr::filter(database_id %in% c(tempData$database_id, temp_ids$database_id))
  
  writeLines(paste(
    "We identified ", format(countStart24 - nrow(runningData), big.mark = ","),
    " duplicates using recordId.",
    sep = ""))
  
  
  
  ###### e. id ####

  # Keep a record of how many records you started with
  countStart25 <- nrow(runningData)
  # Remove too-simple codes...
  runningData$id <-  ifelse( stringr::str_count(runningData$id, "[A-Za-z]") > 2 &
                                     stringr::str_count(runningData$id, "[0-9]") > 6 &
                                     stringr::str_count(runningData$id, "doi") == 0 &
                                     stringr::str_count(runningData$id, 
                                                        paste(monthStrings, collapse = "|")) == 0,
                                   runningData$id, NA)
  
  # Get the ids for the de-duplicated records
  tempData <- runningData %>% 
    # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, id) %>%
    # Select only cases with id present.
    dplyr::filter(complete.cases(id)) %>% 
    # Sort so that higher yearIdentified is given preference
    dplyr::arrange( desc(yearIdentified)) %>%
    # Sort so that higher completeness is given FIRST preference
    dplyr::arrange( desc(completeness)) %>%
    # Group by id and then take the first occurrence record now that they have been sorted.
    dplyr::distinct(dplyr::across(id), .keep_all = TRUE) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
  # Select non-examined cases to keep.
  temp_ids <- runningData %>% 
    # select a subset of columns to speed process and save space.
    dplyr::select(database_id, yearIdentified, completeness, id) %>%
    # Select only cases with id present.
    dplyr::filter(!complete.cases(id)) %>%
    # Return only these IDs.
    dplyr::select(database_id)
  
  # Filter the data
  runningData <- runningData %>%
    dplyr::filter(database_id %in% c(tempData$database_id, temp_ids$database_id))
  
  writeLines(paste(
    "We identified ", format(countStart25 - nrow(runningData), big.mark = ","),
    " duplicates using id.",
    sep = ""))
  
  ##### 1.3 Merge ####
    # Create a new column in the data tibble which has FALSE for records that are duplicates.
  data_flagged <- data %>%
    dplyr::mutate(.duplicate = data$database_id %in% runningData$database_id)
  
    # User output of flagged records
  if(any(duplicatedBy %in% "ID")){
    writeLines(paste("\njbd_occDeDuplicator — 'ID' only:"))
    message(paste(format(sum(data_flagged$.duplicate == FALSE), big.mark = ",")))
    writeLines(paste("records were flagged.\nThe column, '.duplicate' was added to the database.\n"))
  }
  if(any(duplicatedBy %in% "both")){
    writeLines(paste("\njbd_occDeDuplicator — 'ID' component:"))
    message(paste(format(sum(data_flagged$.duplicate == FALSE), big.mark = ",")))
    writeLines(paste("records were flagged at this step",
                     ".\nThe column, '.duplicate' was added to the database.\n"))
  }
          
  writeLines(" — FINISHED 'ID'")
  
} # END 1.0 duplicatedBy
  
  #### 2.0 collectionInfo duplicates ####
  
  # After these are are FLAGGED, we will FLAG duplicates based on:
   # (i) identical lat+long, (ii) identical scientificName, (iii) identical eventDate, 
   # (iv) identical recordedBy, (v) recordNumber, (vi) eventID, (vii) catalogNumber, 
   # (viii) otherCatalogNumbers, and (ix) collectionID while ignoring na values. For the latter 
   # six (iv <- ix), these will only be used when available.
  # If user selects ID or both, run this...
  if(any(duplicatedBy %in% "collectionInfo" | duplicatedBy %in% "both")){
    
    # If BOTH were run, then return data_flagged as data
  if( any(duplicatedBy %in% "both")){
      # Convert the last sheet into the data object
    data <- data_flagged
    # First, remove any row that was already listed as a duplicate in 1.0 and make this runningData
    runningData <- data %>%
      dplyr::filter(!.duplicate == FALSE)
  } else{ # IF only collectionInfo was selected... prepare the datasets. 
    # If both were not run... fill an empty .duplicated column and define the data sheet as runningData
    data$.duplicate <- NA
    runningData <- data
  } # END duplicatedBy in "both"
    
    ##### 2.1 recordNumber ####
    if(is.null(collectionCols)){
      # Warning message
      message(paste("Warning message: \n",
                    " — No collectionCols provided. Using default of: ",
                    "c('decimalLatitude', 'decimalLongitude', 'scientificName', 'eventDate', and 'recordedBy')",
                    sep=""))
      collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                         "recordedBy")}
    
    writeLines(paste(
      " — Identifying duplicates via the recordNumber column AND the collectionCols..."
    ))
      # Sort and select only the cases where recordNumber is provided.
    tempData <- runningData %>%
      # Sort so that higher yearIdentified is given preference
      dplyr::arrange( desc(yearIdentified)) %>%
      # Sort so that higher completeness is given FIRST preference
      dplyr::arrange( desc(completeness)) %>%
        # ONLY examine complete recordNumber rows for this.
      dplyr::filter(complete.cases(recordNumber)) %>% 
      dplyr::select(
        database_id, tidyselect::all_of(collectionCols), recordNumber)
    
        # Find the distinct recordNumber rows and keep their database_id
      temp_ids <- tempData %>% 
        dplyr::distinct( dplyr::across(
          c(tidyselect::all_of(collectionCols), recordNumber)), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_recordNumber <- temp_ids
      duplicates_recordNumber <- tempData %>% 
        dplyr::filter(!database_id %in% deDuped_recordNumber$database_id) %>%
        dplyr::select(database_id)
    
      writeLines(paste(
        "From a total of ",
        format(nrow(tempData), big.mark = ","), 
        " occurrence records with a recordNumber present ",
        "we identified ", format(nrow(duplicates_recordNumber), big.mark = ","),
        " duplicates while also considering the ",
        paste(collectionCols[1:(length(collectionCols)-1)], collapse = ", "), ", and ",
              paste(collectionCols[(length(collectionCols))], sep = ""), " columns.",
        sep = ""))
        # REMOVE ALL records examined here
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% tempData$database_id)
      
      
      #### 2.2 eventID ####
      writeLines(paste(
        " — Identifying duplicates via the eventID column AND the collectionCols..."
      ))
      
      # Sort and select only the cases where eventID is provided.
      tempData <- runningData %>%
        # Sort so that higher yearIdentified is given preference
        dplyr::arrange( desc(yearIdentified)) %>%
        # Sort so that higher completeness is given FIRST preference
        dplyr::arrange( desc(completeness)) %>%
        # ONLY examine complete eventID rows for this.
        dplyr::filter(complete.cases(eventID)) %>% 
        dplyr::select(
          database_id, tidyselect::all_of(collectionCols), eventID)
      
      # Find the distinct eventID rows and keep their database_id
      temp_ids <- tempData %>% 
        dplyr::distinct( dplyr::across(
          c(paste(collectionCols, sep = ", "), eventID)), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_eventID <- temp_ids
      duplicates_eventID <- tempData %>% 
        dplyr::filter(!database_id %in% deDuped_eventID$database_id) %>%
        dplyr::select(database_id)
      
      writeLines(paste(
        "From a total of ",
        format(nrow(tempData), big.mark = ","), " occurrence records with a eventID present ",
        "we identified ", format(nrow(duplicates_eventID), big.mark = ","),
        " duplicates while also considering the ",
        paste(collectionCols[1:(length(collectionCols)-1)], collapse = ", "), ", and ",
        paste(collectionCols[(length(collectionCols))], sep = ""), " columns.",
        sep = ""))
      # REMOVE ALL records examined here
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% tempData$database_id)
  
    

    #### 2.3 catalogNumber ####
      writeLines(paste(
        " — Identifying duplicates via the catalogNumber column AND the collectionCols..."
      ))
      # Sort and select only the cases where catalogNumber is provided.
      tempData <- runningData %>%
        # Sort so that higher yearIdentified is given preference
        dplyr::arrange( desc(yearIdentified)) %>%
        # Sort so that higher completeness is given FIRST preference
        dplyr::arrange( desc(completeness)) %>%
        # ONLY examine complete catalogNumber rows for this.
        dplyr::filter(complete.cases(catalogNumber)) %>% 
        dplyr::select(
          database_id, tidyselect::all_of(collectionCols), catalogNumber)
      
      # Find the distinct catalogNumber rows and keep their database_id
      temp_ids <- tempData %>% 
        dplyr::distinct( dplyr::across(
          c(paste(collectionCols, sep = ", "), catalogNumber)), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_catalogNumber <- temp_ids
      duplicates_catalogNumber <- tempData %>% 
        dplyr::filter(!database_id %in% deDuped_catalogNumber$database_id) %>%
        dplyr::select(database_id)
      
      writeLines(paste(
        "From a total of ",
        format(nrow(tempData), big.mark = ","), " occurrence records with a catalogNumber present ",
        "we identified ", format(nrow(duplicates_catalogNumber), big.mark = ","),
        " duplicates while also considering the ",
        paste(collectionCols[1:(length(collectionCols)-1)], collapse = ", "), ", and ",
        paste(collectionCols[(length(collectionCols))], sep = ""), " columns.",
        sep = ""))
      # REMOVE ALL records examined here
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% tempData$database_id)
      
      
      #### 2.4 otherCatalogNumbers ####
      writeLines(paste(
        " — Identifying duplicates via the otherCatalogNumbers column AND the collectionCols..."
      ))
      # Sort and select only the cases where otherCatalogNumbers is provided.
      tempData <- runningData %>%
        # Sort so that higher yearIdentified is given preference
        dplyr::arrange( desc(yearIdentified)) %>%
        # Sort so that higher completeness is given FIRST preference
        dplyr::arrange( desc(completeness)) %>%
        # ONLY examine complete otherCatalogNumbers rows for this.
        dplyr::filter(complete.cases(otherCatalogNumbers)) %>% 
        dplyr::select(
          database_id, tidyselect::all_of(collectionCols), otherCatalogNumbers)
      
      # Find the distinct otherCatalogNumbers rows and keep their database_id
      temp_ids <- tempData %>% 
        dplyr::distinct( dplyr::across(
          c(paste(collectionCols, sep = ", "), otherCatalogNumbers)), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_otherCatalogNumbers <- temp_ids
      duplicates_otherCatalogNumbers <- tempData %>% 
        dplyr::filter(!database_id %in% deDuped_otherCatalogNumbers$database_id) %>%
        dplyr::select(database_id)
      
      writeLines(paste(
        "From a total of ",
        format(nrow(tempData), big.mark = ","), " occurrence records with a otherCatalogNumbers present ",
        "we identified ", format(nrow(duplicates_otherCatalogNumbers), big.mark = ","),
        " duplicates while also considering the ",
        "(i) decimalLatitude, (ii) decimalLongitude, (iii) identical scientificName, and ",
        "(iv) identical eventDate columns.",
        sep = ""))
      # REMOVE ALL records examined here
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% tempData$database_id)
      
      
      #### 2.5 collectionID ####
      writeLines(paste(
        " — Identifying duplicates via the collectionID column AND the collectionCols..."
      ))
      # Sort and select only the cases where collectionID is provided.
      tempData <- runningData %>%
        # Sort so that higher yearIdentified is given preference
        dplyr::arrange( desc(yearIdentified)) %>%
        # Sort so that higher completeness is given FIRST preference
        dplyr::arrange( desc(completeness)) %>%
        # ONLY examine complete collectionID rows for this.
        dplyr::filter(complete.cases(collectionID)) %>% 
        dplyr::select(
          database_id, tidyselect::all_of(collectionCols), collectionID)
      
      # Find the distinct collectionID rows and keep their database_id
      temp_ids <- tempData %>% 
        dplyr::distinct( dplyr::across(
          c(paste(collectionCols, sep = ", "), collectionID)), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_collectionID <- temp_ids
      duplicates_collectionID <- tempData %>% 
        dplyr::filter(!database_id %in% deDuped_collectionID$database_id) %>%
        dplyr::select(database_id)
      
      writeLines(paste(
        "From a total of ",
        format(nrow(tempData), big.mark = ","), " occurrence records with a collectionID present ",
        "we identified ", format(nrow(duplicates_collectionID), big.mark = ","),
        " duplicates while also considering the ",
        paste(collectionCols[1:(length(collectionCols)-1)], collapse = ", "), ", and ",
        paste(collectionCols[(length(collectionCols))], sep = ""), " columns.",
        sep = ""))
      # REMOVE ALL records examined here
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% tempData$database_id)
      
      # Combine all of the already-identified duplicates:
      duplicates_ALL <- dplyr::bind_rows(duplicates_recordNumber, duplicates_eventID, 
                                         duplicates_catalogNumber, duplicates_otherCatalogNumbers, 
                                         duplicates_collectionID)
      message(paste(
        "So far ", format(nrow(duplicates_ALL), big.mark = ","), 
        " duplicates have been identified in the first component of the 'collectionInfo' step alone...",
        sep = ""))
      
      
      #### 2.6 All above ####
      writeLines(paste(
        " — Now that those duplicates have been removed, we will identify the REMAINING non-duplicated",
        " rows using all of the id columns AND collectionCols. Hence, we may lose so columns that were",
        " otherwise kept...",
      sep = ""))
      # Now we will find duplicates across ALL of the above combined and so-far de-duplicated columns.
      runningData <- data %>%
          # REMOVE the already-identified duplicates
        dplyr::filter(!database_id %in% duplicates_ALL$database_id) %>%
          # Sort so that higher yearIdentified is given preference
        dplyr::arrange( desc(yearIdentified)) %>%
          # Sort so that higher completeness is given FIRST preference
        dplyr::arrange( desc(completeness)) %>%
          # Select the columns to keep in the analysis...
        dplyr::select( database_id, 
                       tidyselect::all_of(collectionCols), 
                       recordNumber, eventID, catalogNumber, otherCatalogNumbers, 
                       collectionID)
      
      # Find the distinct rows and keep their database_id
      temp_ids <- runningData %>% 
        dplyr::distinct( dplyr::across(
          c(tidyselect::all_of(collectionCols), 
            recordNumber, eventID, catalogNumber, otherCatalogNumbers, collectionID)), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_ALL <- temp_ids
      duplicates_ALL <- runningData %>% 
        dplyr::filter(!database_id %in% deDuped_ALL$database_id) %>%
        dplyr::select(database_id)
      
      writeLines(paste(
        "From a total of ",
        format(nrow(runningData), big.mark = ","), " occurrence records ",
        "we identified ", format(nrow(duplicates_ALL), big.mark = ","),
        " duplicates while also considering the ",
        paste(collectionCols[1:(length(collectionCols)-1)], collapse = ", "), ", and ",
        paste(collectionCols[(length(collectionCols))], sep = ""), " columns AND the ",
        "recordNumber, eventID, catalogNumber, otherCatalogNumbers, and collectionID", " columns.",
        sep = ""))
      
        # REMOVE the DUPLICATED records examined here
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% duplicates_ALL$database_id)
      
      
      #### 2.7 Update .duplicate ####
      # If only collectionInfo, ADD column, if not, then UPDATE column
      if(any(duplicatedBy %in% "collectionInfo")){
      data_flagged <- data %>%
        dplyr::mutate(.duplicate = data$database_id %in% runningData$database_id)
      }else{
          # Get the duplicates from 1.0...
        duplicates_1 <- data$.duplicate
          # Get the duplicates from 2.0...
        duplicates_2 <- data$database_id %in% runningData$database_id
          # Combine these lists so if either is FALSE (duplicated), this will be listed as such.
        duplicates_12 <- if_else(duplicates_1 == FALSE | duplicates_2 == FALSE,
                                 FALSE, TRUE) 
          # create new data_flagged
        data_flagged <- data
          # ADD the .duplicate column using duplicates_12
        data_flagged$.duplicate <- duplicates_12
      } # END any(duplicatedBy %in% "collectionInfo")
      
      # Record the number flagged here in case quickDeDuplicate is used
      Count_1_and_2 <- sum(data_flagged$.duplicate == FALSE)
      
      # User output of flagged records
      if(any(duplicatedBy %in% "collectionInfo")){
        writeLines(paste("\njbd_occDeDuplicator — 'collectionInfo' ONLY:"))
        message(paste(format(sum(data_flagged$.duplicate == FALSE), big.mark = ",")))
        writeLines(paste("records were flagged.\nThe column, '.duplicate' was added to the database.\n"))
      }
      if(any(duplicatedBy %in% "both")){
        writeLines(paste("\njbd_occDeDuplicator — 'both' steps combined:"))
        message(paste(format(sum(data_flagged$.duplicate == FALSE), big.mark = ","))) 
        writeLines(paste("records were flagged.\nThe column, '.duplicate' appended with this information.\n"))
      }
      
      writeLines(" — FINISHED 'collectionInfo'")

  } # END 2.0 duplicatedBy
  
  
    #### 3.0 quickDeDuplicate #####
  if(!is.null(quickDeDuplicate)){
  if(quickDeDuplicate == TRUE){
      ##### 3.1 Errors ####
    if(is.null(quickColumns)){
      # IF no quickColumns provided...
      message(paste("Warning message: \n",
        " — No quickColumns were provided. Using the default columns of ",
        "c('decimalLatitude',  'decimalLongitude', 'scientificName', and 'eventDate')",
        sep=""))
      quickColumns = c("decimalLatitude",  "decimalLongitude",
                            "scientificName", "eventDate")
    } # END is.null(quickColumns)
    writeLines(" — Starting the quickDeDuplicate function...")
    
      #### 3.2 Data input ####
    # If ONLY quickDeDuplicate is being run...
    if( is.null(duplicatedBy)){
      # Convert the data sheet into the data_flagged data object
      data_flagged <- data
      # Fill an empty .duplicated column and define the data sheet as runningData
      data$.duplicate <- NA
    } else{ # IF one of the duplicatedBy were run...
      # First, remove any row that was already listed as a duplicate in 1.0 and/or 2.0 and make this runningData
      runningData <- data_flagged %>%
        dplyr::filter(!.duplicate == FALSE)
    } # END is.null(duplicatedBy)
    
    
    #### 3.3 quickColumns ####
        # Sort and select data to de-duplicate...
      tempData <- runningData %>%
        # Sort so that higher yearIdentified is given preference
        dplyr::arrange( desc(yearIdentified)) %>%
        # Sort so that higher completeness is given FIRST preference
        dplyr::arrange( desc(completeness)) %>%
        # Select the minimum columns needed for the analysis.
        dplyr::select(
          database_id, tidyselect::all_of(quickColumns))
      
      # Find the distinct collectionID rows and keep their database_id
      temp_ids <- tempData %>% 
        dplyr::distinct( 
          dplyr::across(c(tidyselect::all_of(quickColumns))), 
          .keep_all = TRUE) %>%
        dplyr::select(database_id)
      
      # Save these ids for later examination and to be removed from the runningData for now.
      deDuped_ALL <- temp_ids
      duplicates_ALL <- tempData %>% 
        dplyr::filter(!database_id %in% deDuped_ALL$database_id) %>%
        dplyr::select(database_id)
      
      writeLines(paste(
        " — FINISHED 'quickDeDuplicate'.\n",
        "From a total of ",
        format(nrow(tempData), big.mark = ","), " occurrence records provided ",
        "we identified ", format(nrow(duplicates_ALL), big.mark = ","),
        " duplicates while also considering the ",
        paste(quickColumns[1:(length(quickColumns)-1)], collapse = ", "), ", and ",
        paste(quickColumns[(length(quickColumns))], sep = ""), " columns.",
        sep = ""))
        # Update runningData to remove the duplicates
      runningData <- runningData %>%
        dplyr::filter(!database_id %in% duplicates_ALL$database_id)
      
      #### 3.4 Update .duplicate ####
      # If Any other duplicatedBy was run...
      if(any(duplicatedBy %in% c("collectionInfo","both","ID") )){
          # Extract the prior duplicates.
        priorDuplicates <- data_flagged$.duplicate
          # Extract the quickDuplicates (duplicates == FALSE)
        quickDuplicates <- data_flagged$database_id %in% deDuped_ALL$database_id
        # Combine these lists so if either is FALSE (duplicated), this will be listed as such.
          # This is probably superfluous to quickDuplicates.
        allDuplicates <- dplyr::if_else(priorDuplicates == FALSE | quickDuplicates == FALSE,
                                 FALSE, TRUE) 
        # create new data_flagged
        data_flagged <- data
        # ADD the .duplicate column using duplicates_12
        data_flagged$.duplicate <- allDuplicates
      }else{
        data_flagged <- data %>%
          dplyr::mutate(.duplicate = data$database_id %in% deDuped_ALL$database_id)
      } # END any(duplicatedBy %in% c("collectionInfo","both","ID") )
      
      # User output of flagged records
  if(any(duplicatedBy %in% c("collectionInfo","both","ID") )){
    writeLines(paste("\njbd_occDeDuplicator — 'quickDeDuplicate' AND 'duplicatedBy':"))
    message(paste(format(sum(data_flagged$.duplicate == FALSE), big.mark = ",")))
    writeLines(paste(
      "records were flagged.\nThe column, '.duplicate' was appended with this information."))
    writeLines(paste(
      " — This was an additional ", 
      format( (sum(data_flagged$.duplicate == FALSE) - Count_1_and_2), big.mark = ","),
      " records flagged at this step.",
    sep = ""))
      }
  if(!any(duplicatedBy %in% c("collectionInfo","both","ID") )){
    writeLines(paste("\njbd_occDeDuplicator — 'quickDeDuplicate' ONLY:"))
    message(paste(format(sum(data_flagged$.duplicate == FALSE), big.mark = ","))) 
    writeLines(paste(
      "records were flagged.\nThe column, '.duplicate' was added to the database.\n"))
      }
      
      writeLines(" — FINISHED 'quickDeDuplicate'")
  }} # END quickDeDuplicate
  
  endTime <- Sys.time()
  message(paste(
    " — Completed in ", 
    round(difftime(endTime, startTime, units = "mins"), digits = 2 ),
    " minutes.",
  sep = ""))
  
  return(data_flagged)
} # END function
#### END #####

