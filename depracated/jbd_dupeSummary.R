# This function was written by James Dorey to remove duplicates using between one and two methods
  # This was written between the 11th of June 2022. For help, please contact James at
  # jbdorey@me.com

jbd_dupeSummary <- function(
    data = NULL,
    path = NULL,
    maxYear = lubridate::year(Sys.Date()),
    minYear = 1700,
    duplicatedBy = NULL,
    # The columns to generate completeness info from
    completeness_cols = NULL,
    idColumns = NULL,
    # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
    collectionCols = NULL,
      # The columns to combine, one-by-one with the collectionCols
    collectInfoColumns = NULL,
    CustomComparisonsRAW = NULL,
    # Custom comparisons — as a list of 
    CustomComparisons = NULL,
      # The order in which you want to KEEP duplicated based on data source
    sourceOrder = NULL,
      # To preference Paige's data over the RAW GBIF data, use the below
    PaigeSort = NULL,
    PaigeOrder = NULL,
      # Columns not to filter in .summary — default is below
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                        ".uncertaintyThreshold"),
    # Set the complexity threshold for id letter and number length
        # minimum number of characters when WITH the numberThreshold
    characterThreshold = 2,
        # minimum number of numbers when WITH the characterThreshold
    numberThreshold = 3,
        # Minimum number of numbers WITHOUT any characters
    numberOnlyThreshold = 5,
    catalogSwitch = TRUE
){
  # Load required packages
  require(dplyr)
  require(readr)
  require(stringr)
  require(lubridate)
  require(tibble)
  require(tidyselect)
  require(igraph)
    # Record start time
  startTime <- Sys.time()
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" — Please provide an argument for data. I'm a program not a magician.")
  }
  if(is.null(sourceOrder)){
    stop(paste("Warning message: \n",
               " — No sourceOrder provided. This must be provided as the name of each dataSource ",
               "before the first '_' in the desired order.",
               sep=""))
  }
  ###### b. Warnings ####
  if(is.null(maxYear)){
    message(paste("Warning message: \n",
                  " — No maxYear provided. Using default of this year: ",
                  lubridate::year(Sys.Date()),
                  sep = ""))
    maxYear == lubridate::year(Sys.Date())
  }
  if(is.null(minYear)){
    message(paste("Warning message: \n",
                  " — No minYear provided. Using default of: 1700",
                  sep = ""))
    minYear == 1700
  }
  if(is.null(duplicatedBy)){
    message(paste("Warning message: \n",
                  " — No duplicatedBy provided. Consider if you want to choose to find duplicates by (i)",
                  " 'ID' columns only (for pre-cleaned data), by (ii) 'collectionInfo' columns only ",
                  "(for cleaned data), or (ii) 'both'.\n",
                  "NULL is acceptable, if quickDeDuplicate == TRUE",
                  sep = ""))
  }
  if(is.null(idColumns) & stringr::str_detect(duplicatedBy, "ID|both")){
    message(paste("Warning message: \n",
                  " — No idColumns provided. Using default of: ",
                  "c('gbifID',  'occurrenceID', 'recordId', and 'id')",
                  sep=""))
    idColumns = c("gbifID", "occurrenceID", "recordId","id")
  }
  if(is.null(completeness_cols)){
    message(paste("Warning message: \n",
                  " — No completeness_cols provided. Using default of: ",
                  "c('decimalLatitude',  'decimalLongitude', 'scientificName', and 'eventDate')",
                  sep=""))
    completeness_cols = c("decimalLatitude",  "decimalLongitude",
                          "scientificName", "eventDate")
  }
  if(is.null(collectionCols)){
    message(paste("Warning message: \n",
                  " — No collectionCols provided. Using default of: ",
                  "c('decimalLatitude',  'decimalLongitude', 'scientificName', 'eventDate', and 'recordedBy')",
                  sep=""))
    collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                       "recordedBy")
  }
  if(is.null(collectInfoColumns)){
    message(paste("Warning message: \n",
                  " — No collectInfoColumns provided. Using default of: ",
                  "c('recordNumber',  'eventID', 'catalogNumber', 'otherCatalogNumbers', and 'collectionID')",
                  sep=""))
    collectInfoColumns = c("recordNumber", "eventID", "catalogNumber", "otherCatalogNumbers",
                           "collectionID")
  }
  

  ##### 0.2 Data prep #####
    ###### a. completeness ####
  # Get the sum of the complete.cases of four important fields. Preference will be given to keeping 
  # the most-complete records
  writeLines(paste(
    " — Generating a basic completeness summary from the ", 
    paste(completeness_cols, collapse = ", "), " columns.","\n",
    "This summary is simply the sum of complete.cases in each column. It ranges from zero to the N",
    " of columns. This will be used to sort duplicate rows and select the most-complete rows.",
    sep = ""
  ))
  
  
  # Update the .summary column, ignoring the dontFilterThese columns.
  writeLines(" — Updating the .summary column to sort by...")
  data <- summaryFun(
    data = data,
    # Don't filter these columns (or NULL)
    dontFilterThese = dontFilterThese,
    # Remove the filtering columns?
    removeFilterColumns = FALSE,
    # Filter to ONLY cleaned data?
    filterClean = FALSE)
  
  # Create the completeness column based on the completeness_cols
  Loop_data <- data %>%
  dplyr::mutate(completeness = data %>% 
                     dplyr::select(tidyselect::all_of(completeness_cols)) %>% 
                     apply(., MARGIN = 1, function(x) sum(complete.cases(x)))
  )
  
    ###### b. catalogSwitch ####
    # If the catalogNumber is empty, copy over the otherCatalogNumbers value and visa versa
  if(catalogSwitch == TRUE){
  Loop_data <- Loop_data %>%
    dplyr::mutate(
      otherCatalogNumbers = dplyr::if_else(is.na(otherCatalogNumbers),
                                           catalogNumber,
                                           otherCatalogNumbers),
      catalogNumber = dplyr::if_else(is.na(catalogNumber),
                                     otherCatalogNumbers,
                                     catalogNumber)
    )}
  
  ##### 0.3 format dateIdentified ####
    # Removed for now because dateIdentified is too poorly filled out.
  # Format the dateIdentified column
    #    writeLines(paste(
    #      " — Formatting the dateIdentified column to date format...", 
    #      sep = ""
    #    ))
    #    Loop_data$dateIdentified <- lubridate::ymd_hms(Loop_data$dateIdentified,
    #                                              truncated = 5, quiet = TRUE) %>%
    #      as.Date()
  
    #   # Add a column for yearIdentified
    #   Loop_data$yearIdentified <- lubridate::year(Loop_data$dateIdentified) 
    #   # Remove years out of the reasonable range defined by maxYear and minYear
    #   Loop_data$yearIdentified <- ifelse(Loop_data$yearIdentified > maxYear | 
    #                                        Loop_data$yearIdentified < minYear,
    #                                 NA, Loop_data$yearIdentified)
  # Add the dupColumn_s as NA for the first iteration
  Loop_data$dupColumn_s <- NA

    # Create a datset to put duplicates into
    runningDuplicates = tibble::tibble()

    
    
    #### 1.0 CUSTOM_RAW ####
    if(!is.null(CustomComparisonsRAW)){
      message(" — Working on CustomComparisonsRAW duplicates...")
      # Get complete cases of CustomComparisonsRAW from each dataset
      
      ##### 1.1 Loop ####
      # Create a dataset to put unique vaules into
      for(i in 1:length(CustomComparisonsRAW)){
        # Select the ith CustomComparisonsRAW to match with
        currentColumn <- CustomComparisonsRAW[[i]]
        
        ###### a. Identify duplicates ####
        # Do the duplicate matching
        dupSummary <- Loop_data %>%
          # Select the columns to keep
          dplyr::select(database_id, 
                        tidyselect::all_of(currentColumn),
                        dataSource, dupColumn_s, completeness,.summary) %>%
          # Drop any NA rows
          tidyr::drop_na( tidyselect::all_of(c(currentColumn))) %>%
          # Select the grouping (duplicate) columns
          dplyr::group_by( 
            dplyr::across(tidyselect::all_of(c(currentColumn)))) %>%
          # Select groups with more than one occurrence (duplicates)
          dplyr::filter(dplyr::n() > 1) %>%
          # Create a new column with the first occurrence in each group matching to the rest
          # Do the same for database_id
          dplyr::mutate(database_id_match = database_id[1], .after = database_id) %>%
          # Add the matching column as a column
          dplyr::mutate(
            dupColumn_s = stringr::str_c(
              dplyr::if_else(complete.cases( dupColumn_s),
                             stringr::str_c(dupColumn_s, 
                                            paste0(currentColumn, collapse = ", "), sep = " & "), 
                             paste0(currentColumn, collapse = ", "))))
        
        
        # Get numbers just for an output text
        duplicates2record <- dupSummary %>%
          dplyr::filter( dplyr::row_number() > 1) %>%
          nrow()
        keptCount = dupSummary %>%
          n_groups()
        
        ##### b. Running outputs ####
        # Bind the rows to a running file. Missing columns will be "NA"
        runningDuplicates = dplyr::bind_rows(runningDuplicates,
                                             dupSummary) %>% 
          distinct(database_id, database_id_match, .keep_all = TRUE)
        
        ##### c. User output ####
        message(paste0(
          "\nCompleted iteration ", i, " of ", length(CustomComparisonsRAW), ":"
        ))
        writeLines(
          paste0(" — Identified ", 
                 format(duplicates2record, big.mark = ","), 
                 " duplicate records and kept ",
                 format(keptCount, big.mark = ","),
                 " unique records using the column(s): \n",
                 paste(currentColumn, collapse = ", ")))
        # Remove this temporary dataset
        rm(dupSummary)
      } # END for 1.1 CUSTOM_RAW LOOP
    } # END for 1.0 CUSTOM_RAW

    #### 2.0 Remove simple codes ####
      ##### 2.1 Code removal ####
      # Remove the too-simple codes after make the RAW comparisons
    Loop_data <- Loop_data %>%
      dplyr::mutate(
        if("occurrenceID" %in% colnames(Loop_data)){
          occurrenceID = dplyr::if_else( stringr::str_count(occurrenceID, "[A-Za-z]") >= characterThreshold &
                                           stringr::str_count(occurrenceID, "[0-9]") >= numberThreshold |
                                           stringr::str_count(occurrenceID, "[0-9]") >= numberOnlyThreshold, 
                                         occurrenceID, NA_character_)},
        if("recordId" %in% colnames(Loop_data)){
          recordId = dplyr::if_else( stringr::str_count(recordId, "[A-Za-z]") >= characterThreshold &
                                       stringr::str_count(recordId, "[0-9]") >= numberThreshold |
                                       stringr::str_count(recordId, "[0-9]") >= numberOnlyThreshold, 
                                     recordId, NA_character_)},
        if("id" %in% colnames(Loop_data)){
          id = dplyr::if_else( stringr::str_count(id, "[A-Za-z]") >= characterThreshold &
                                 stringr::str_count(id, "[0-9]") >= numberThreshold |
                                 stringr::str_count(id, "[0-9]") >= numberOnlyThreshold, 
                               id, NA_character_)}, 
        if("catalogNumber" %in% colnames(Loop_data)){
          catalogNumber = dplyr::if_else( stringr::str_count(catalogNumber, "[A-Za-z]") >= characterThreshold &
                                            stringr::str_count(catalogNumber, "[0-9]") >= numberThreshold |
                                            stringr::str_count(catalogNumber, "[0-9]") >= numberOnlyThreshold, 
                                          catalogNumber, NA_character_)}, 
        if("otherCatalogNumbers" %in% colnames(Loop_data)){
          otherCatalogNumbers = dplyr::if_else( stringr::str_count(otherCatalogNumbers, "[A-Za-z]") >= characterThreshold &
                                                  stringr::str_count(otherCatalogNumbers, "[0-9]") >= numberThreshold |
                                                  stringr::str_count(otherCatalogNumbers, "[0-9]") >= numberOnlyThreshold, 
                                                otherCatalogNumbers, NA_character_)})
    ##### 2.2. catalogSwitch ####
    # If the catalogNumber is empty, copy over the otherCatalogNumbers value and visa versa
    if(catalogSwitch == TRUE){
      Loop_data <- Loop_data %>%
        dplyr::mutate(
          otherCatalogNumbers = dplyr::if_else(is.na(otherCatalogNumbers),
                                               catalogNumber,
                                               otherCatalogNumbers),
          catalogNumber = dplyr::if_else(is.na(catalogNumber),
                                         otherCatalogNumbers,
                                         catalogNumber)
        )}
    
    
    #### 3.0 CUSTOM ####
    if(!is.null(CustomComparisons)){
      message(" — Working on CustomComparisons duplicates...")
      # Get complete cases of CustomComparisons from each dataset
      
      ##### 3.1 Loop ####
      # Create a dataset to put unique vaules into
      for(i in 1:length(CustomComparisons)){
        # Select the ith CustomComparisons to match with
        currentColumn <- CustomComparisons[[i]]
        
        ###### a. Identify duplicates ####
        # Do the duplicate matching
        dupSummary <- Loop_data %>%
          # Select the columns to keep
          dplyr::select(database_id, 
                        tidyselect::all_of(currentColumn),
                        dataSource, dupColumn_s, completeness,.summary) %>%
          # Drop any NA rows
          tidyr::drop_na( tidyselect::all_of(c(currentColumn))) %>%
          # Select the grouping (duplicate) columns
          dplyr::group_by( 
            dplyr::across(tidyselect::all_of(c(currentColumn)))) %>%
          # Select groups with more than one occurrence (duplicates)
          dplyr::filter(dplyr::n() > 1) %>%
          # Create a new column with the first occurrence in each group matching to the rest
          # Do the same for database_id
          dplyr::mutate(database_id_match = database_id[1], .after = database_id) %>%
          # Add the matching column as a column
          dplyr::mutate(
            dupColumn_s = stringr::str_c(
              dplyr::if_else(complete.cases( dupColumn_s),
                             stringr::str_c(dupColumn_s, 
                                            paste0(currentColumn, collapse = ", "), sep = " & "), 
                             paste0(currentColumn, collapse = ", "))))
        
        
        # Get numbers just for an output text
        duplicates2record <- dupSummary %>%
          dplyr::filter( dplyr::row_number() > 1) %>%
          nrow()
        keptCount = dupSummary %>%
          n_groups()
        
        ##### b. Running outputs ####
        # Bind the rows to a running file. Missing columns will be "NA"
        runningDuplicates = dplyr::bind_rows(runningDuplicates,
                                             dupSummary) %>% 
          distinct(database_id, database_id_match, .keep_all = TRUE)
        
        ##### c. User output ####
        message(paste0(
          "\nCompleted iteration ", i, " of ", length(CustomComparisons), ":"
        ))
        writeLines(
          paste0(" — Identified ", 
                 format(duplicates2record, big.mark = ","), 
                 " duplicate records and kept ",
                 format(keptCount, big.mark = ","),
                 " unique records using the column(s): \n",
                 paste(currentColumn, collapse = ", ")))
        # Remove this temporary dataset
        rm(dupSummary)
      } # END for 3.1 CUSTOM LOOP
    } # END for 3.0 CUSTOM
    
    
  #### 4.0 ID ####
  if(duplicatedBy %in% c("ID","both")){
    message(" — Working on ID duplicates...")
    # Get complete cases of collectionInfo from each dataset
    
    ##### 4.1 Loop ####
    # Create a dataset to put unique values into
    for(i in 1:length(idColumns)){
      # Select the ith idColumns to match with
      currentColumn <- idColumns[i]
      
      ###### a. Identify duplicates ####
      # Do the duplicate matching
      dupSummary <- Loop_data %>%
        # Select the columns to keep
        dplyr::select(database_id, 
                      tidyselect::all_of(currentColumn),
                      dataSource, dupColumn_s, completeness,.summary) %>%
        # Drop any NA rows
        tidyr::drop_na(tidyselect::all_of(currentColumn)) %>%
        # Select the grouping (duplicate) columns
        dplyr::group_by( dplyr::across(dplyr::all_of(currentColumn))) %>%
        # Select groups with more than one occurrence (duplicates)
        dplyr::filter(dplyr::n() > 1) %>%
        # Create a new column with the first occurrence in each group matching to the rest to keep
        # Do the same for database_id
        dplyr::mutate(database_id_match = database_id[1], .after = database_id) %>%
        # Add the matching column as a column
        dplyr::mutate(
          dupColumn_s = stringr::str_c(
            dplyr::if_else(!is.na(dupColumn_s) ,
                           stringr::str_c(dupColumn_s, currentColumn, sep = " & "), 
                           currentColumn)))
      
      # Get numbers just for an output text
      duplicates2record <- dupSummary %>%
        dplyr::filter( dplyr::row_number() > 1) %>%
        nrow()
      keptCount = dupSummary %>%
        n_groups()
      
      
      ##### b. Running outputs ####
      # Bind the rows to a running file. Missing columns will be "NA"
      runningDuplicates = dplyr::bind_rows(runningDuplicates,
                                           dupSummary) %>% 
        distinct(database_id, database_id_match, .keep_all = TRUE)

      
      ##### c. User output ####
      message(paste0(
        "\nCompleted iteration ", i, " of ", length(idColumns), ":"
      ))
      writeLines(
        paste(" — Identified ", 
              format(duplicates2record, big.mark = ","), 
              " duplicate records and kept ",
              format(keptCount, big.mark = ","),
              " unique records using the column: \n",
              currentColumn), sep = "")
      # Remove this temporary dataset
      rm(dupSummary)
      
    } # END for LOOP
  } # END 4.0 ID
  
  
  
  #### 5.0 collectionInfo ####
  if(duplicatedBy %in% c("collectionInfo","both")){
    message(" — Working on collectionInfo duplicates...")
    # Get complete cases of collectionInfo from each dataset

          ##### 5.1 Loop ####
      # Create a dataset to put unique values into
    for(i in 1:length(collectInfoColumns)){
        # Select the ith collectInfoColumns to match with
      currentColumn <- collectInfoColumns[i]
      
      ###### a. Identify duplicates ####
        # Do the duplicate matching
    dupSummary <- Loop_data %>%
        # Select the columns to keep
      dplyr::select(database_id, 
                    tidyselect::all_of(collectionCols),
                    tidyselect::all_of(currentColumn),
                    dataSource, dupColumn_s, completeness, .summary) %>%
       # Drop any NA rows
      tidyr::drop_na( tidyselect::all_of(c(collectionCols, currentColumn))) %>%
        # Select the grouping (duplicate) columns
      dplyr::group_by( 
        dplyr::across(tidyselect::all_of(c(collectionCols, currentColumn)))) %>%
        # Select groups with more than one occurrence (duplicates)
      dplyr::filter(dplyr::n() > 1) %>%
        # Create a new column with the first occurrence in each group matching to the rest
        # Do the same for database_id
      dplyr::mutate(database_id_match = database_id[1], .after = database_id) %>%
      # Add the matching column as a column
      dplyr::mutate(
        dupColumn_s = stringr::str_c(
          dplyr::if_else(!is.na(dupColumn_s) ,
                         stringr::str_c(dupColumn_s, 
                                        paste0(currentColumn, collapse = ", "), sep = " & "), 
                         paste0(currentColumn, collapse = ", "))))

    
    # Get numbers just for an output text
    duplicates2record <- dupSummary %>%
      dplyr::filter( dplyr::row_number() > 1) %>%
      nrow()
    keptCount = dupSummary %>%
      n_groups()
    
    
    ##### b. Running outputs ####
    # Bind the rows to a running file. Missing columns will be "NA"
    runningDuplicates = dplyr::bind_rows(runningDuplicates,
                                         dupSummary)%>% 
      distinct(database_id, database_id_match, .keep_all = TRUE)


        ##### c. User output ####
    message(paste0(
      "\nCompleted iteration ", i, " of ", length(collectInfoColumns), ":"
    ))
    writeLines(
      paste0(" — Identified ", 
             format(duplicates2record, big.mark = ","), 
             " duplicate records and kept ",
             format(keptCount, big.mark = ","),
             " unique records using the columns: \n",
             paste(c(collectionCols), collapse = ", "), ", and ",
             currentColumn))
    # Remove this temporary dataset
    rm(dupSummary)
    
    } # END for 5.1 collectionInfo LOOP
    
  } # END for 5.0 collectionInfo
    

    #### 6.0 runningDuplicates File ####
    ##### 6.1 Clustering duplicates####
    writeLines(" — Clustering duplicate pairs...")
      # Cluster the id pairs into groups
    clusteredDuplicates <- runningDuplicates %>% 
      dplyr::select(database_id_match, database_id) %>% 
      igraph::graph.data.frame() %>%
      igraph::clusters()
      # Extract the id and the group only
    clusteredDuplicates <- clusteredDuplicates$membership %>% as.data.frame() %>%
      setNames("group") %>%
      dplyr::mutate(database_id = rownames(.)) %>% as_tibble()
      
      # Re-merge the relevant columns
    clusteredDuplicates <- clusteredDuplicates %>%
        # Re-merge the dupColumn_s column
      dplyr::left_join(runningDuplicates %>% 
                         dplyr::select(database_id, dupColumn_s) %>%
                         dplyr::distinct(database_id, .keep_all = TRUE),
                       by = "database_id") %>%
        # Re-merge the rest of the information
      dplyr::left_join(Loop_data %>%
                         dplyr::select( 
                           tidyselect::any_of(unique(c("database_id",
                                                       completeness_cols,
                                                       collectionCols,
                                                       collectInfoColumns,
                                                       lst(CustomComparisons) %>% 
                                                         unlist() %>% as.character(),
                                                       "dataSource", "completeness",
                                                       ".summary"),
                                                     fromLast = TRUE,
                                                     na.rm = TRUE))),
                       by = "database_id") %>%
        # Group by the clustered group number
      dplyr::group_by(group) 
    
    # User output
    writeLines(paste0(
      "Duplicate pairs clustered. There are ", 
      format(nrow(clusteredDuplicates) - clusteredDuplicates %>% n_groups(), 
             big.mark = ","), " duplicates across ", 
      format(clusteredDuplicates %>% n_groups(), big.mark = ","),
      " kept duplicates."))
    
    ##### 6.2 Arrange data ####
    # Prepare data order
    if(!is.null(PaigeSort)){
      writeLines(" — Ordering Paige's updates...")
      PaigeOrder = PaigeOrder
      clusteredDuplicates <- clusteredDuplicates %>%
        # Make a new column with the database_id SOURCE, not the full database_id with numbers
        dplyr::mutate(database_id_Main = stringr::str_replace(database_id,
                                                              pattern = "_.*",
                                                              replacement = "") %>%
                        factor(levels = PaigeOrder, ordered = TRUE) ) %>%
        # Sort so that certain datasets will be given preference over one another as user-defined.
        dplyr::arrange(database_id_Main) %>%
        # Remove this sorting column
        dplyr::select(!database_id_Main)
    }
    
    
    writeLines(" — Ordering data by 1. dataSource, 2. completeness",
               " and 3. .summary column...")
    clusteredDuplicates <- clusteredDuplicates %>%
      # Extract only the actual source, not the taxonomic level
      dplyr::mutate(dataSourceMain = stringr::str_replace(dataSource,
                                                          pattern = "_.*",
                                                          replacement = "") %>%
                      factor(levels = sourceOrder, ordered = TRUE) ) %>%
      # Sort so that certain datasets will be given preference over one another as user-defined.
      dplyr::arrange(dataSourceMain) %>%
      # Sort so that higher completeness is given FIRST preference
      dplyr::arrange( desc(completeness)) %>%
      # Sort by .summary so that TRUE is selected over FALSE
      dplyr::arrange( desc(.summary)) %>%
      # Remove these sorting columns
      dplyr::select(!c(dataSourceMain)) 
    
    ##### 6.3 Keep first #####
    writeLines(" — Find and FIRST duplicate to keep and assign other associated duplicates to that one (i.e., across multiple tests a 'kept duplicate', could otherwise be removed)...")
    # Find the first duplicate and assign the match to that one as the kept dupicate
    clusteredDuplicates <- clusteredDuplicates %>%
      dplyr::mutate(database_id_keep = database_id[1], .after = database_id) %>%
      dplyr::mutate(dataSource_keep = dataSource[1], .after = dataSource) %>%
      # Remove the row for the kept duplicate
      dplyr::filter(!database_id_keep == database_id)
    
       ##### 6.4 Save ####
      # Save the running 
    readr::write_csv(clusteredDuplicates, 
                     file = paste0(path, "/Output/Report/", 
                                  "duplicateRun_", paste(duplicatedBy, collapse = "_"),
                                  "_", Sys.Date(),
                                  ".csv"))
  writeLines(paste0(
    " — Duplicates have been saved in the file and location: ", 
    paste0(path, "/Output/Report/", 
           "duplicateRun_", paste(duplicatedBy, collapse = "_"),
           "_", Sys.Date(),
           ".csv")
  ))
    
    #### 7.0 Flag .duplicates ####  
      # Add a flag to any database_id that occurs in the clusteredDuplicates file. The rest will be 
        # TRUE (not duplicates)
  Loop_data <- data %>%
      # Add .duplicates flag column
    dplyr::mutate(.duplicates = !database_id %in% clusteredDuplicates$database_id) %>%
      # Add in a column to show the duplicate status of each occurrence
    dplyr::mutate(duplicateStatus = dplyr::if_else(
      database_id %in% clusteredDuplicates$database_id,
      "Duplicate", 
      dplyr::if_else(
        database_id %in% clusteredDuplicates$database_id_keep,
        "Kept duplicate", "Unique"))
      )
  
  
    #### Final output ####
 
    writeLines(paste0(
      " — Across the entire dataset, there are now ",
      format(sum(Loop_data$.duplicates == FALSE), big.mark = ","), " duplicates from a total of ",
      format(nrow(Loop_data), big.mark = ","), " occurrences."
    ))

  endTime <- Sys.time()
  message(paste(
    " — Completed in ", 
    round(difftime(endTime, startTime, units = "mins"), digits = 2 ),
    " minutes.",
    sep = ""))
    # Return data
  return(Loop_data)
} # END function



