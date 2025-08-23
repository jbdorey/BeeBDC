# This function was written by James Dorey to remove duplicates using between one and two methods
  # This was written between the 11th of June 2022. For help, please contact James at
  # jbdorey[at]me.com

#' Identifies duplicate occurrence records
#' 
#' This function uses user-specified inputs and columns to identify duplicate occurrence records. 
#' Duplicates are identified iteratively and will be tallied up, duplicate pairs clustered, and 
#' sorted at the end of the function.
#' The function is designed to work with Darwin Core data with a database_id column, 
#' but it is also modifiable to work with other columns.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param path A character path to the location where the duplicateRun_ file will be saved.
#' @param duplicatedBy A character vector. Options are c("ID", "collectionInfo", "both"). "ID" 
#' columns runs through a series of ID-only columns defined by idColumns. "collectionInfo" runs 
#' through a series of columns defined by collectInfoColumns, which are checked in combination 
#' with collectionCols. "both" runs both of the above.
#' @param idColumns A character vector. The columns to be checked individually for internal 
#' duplicates. Intended for use with ID columns only.
#' @param collectionCols A character vector. The columns to be checked in combination with each 
#' of the completeness_cols.
#' @param collectInfoColumns A character vector. The columns to be checked in combinatino with 
#' all of the collectionCols columns.
#' @param completeness_cols A character vector. A set of columns that are used to order and select 
#' duplicates by. For each occurrence, this function will calculate the sum of [complete.cases()].
#' Within duplicate clusters occurrences with a greater number of the completeness_cols filled 
#' in will be kept over those with fewer.
#' @param CustomComparisonsRAW A list of character vectors. Custom comparisons - as a list of 
#' columns to iteratively compare for duplicates. These differ from the CustomComparisons in 
#' that they ignore the minimum number and character thresholds for IDs.
#' @param CustomComparisons A list of character vectors. Custom comparisons - as a list of 
#' columns to iteratively compare for duplicates. These comparisons are made after character 
#' and number thresholds are accounted for in ID columns.
#' @param sourceOrder A character vector. The order in which you want to KEEP duplicated 
#' based on the dataSource column (i.e. what order to prioritize data sources). 
#' NOTE: These dataSources are simplified to the string prior 
#' to the first "_". Hence, "GBIF_Anthophyla" becomes "GBIF."
#' @param prefixOrder A character vector. Like sourceOrder, except based on the database_id prefix,
#' rather than the dataSource. Additionally, this is only examined if prefixOrder != NULL. 
#' Default = NULL.
#' @param dontFilterThese A character vector. This should contain the flag columns to be ignored 
#' in the creation or updating of the .summary column. Passed to  [BeeBDC::summaryFun()].
#' @param characterThreshold Numeric. The complexity threshold for ID letter length. This is the
#' minimum number of characters that need to be present in ADDITION TO the numberThreshold for an
#'  ID number to be tested for duplicates. Ignored by CustomComparisonsRAW. The columns that are 
#'  checked are occurrenceID, recordId, id, catalogNumber, and otherCatalogNumbers. Default = 2.
#' @param numberThreshold Numeric. The complexity threshold for ID number length. This is the
#' minimum number of numeric characters that need to be present in ADDITION TO the 
#' characterThreshold for an ID number to be tested for duplicates. Ignored by 
#' CustomComparisonsRAW. The columns that are checked are occurrenceID, recordId, id, 
#' catalogNumber, and otherCatalogNumbers. Default = 3.
#' @param numberOnlyThreshold Numeric. As numberThreshold except the characterThreshold is ignored.
#' Default = 5.
#' @param catalogSwitch Logical. If TRUE, and the catalogNumber is empty the function will copy over
#'  the otherCatalogNumbers into catalogNumber and visa versa. Hence, the function will attempt 
#'  to matchmore catalog numbers as both of these functions can be problematic. Default = TRUE.
#'
#' @return Returns data with an additional column called .duplicates where FALSE occurrences are 
#' duplicates and TRUE occurrences are either kept duplicates or unique. Also exports a .csv to 
#' the user-specified location with information about duplicate matching. This file is used by 
#' other functions including
#' [BeeBDC::manualOutlierFindeR()] and [BeeBDC::chordDiagramR()]
#'
#' @importFrom stats complete.cases setNames
#' @importFrom dplyr n_groups lst desc %>%
#' 
#' @seealso [BeeBDC::chordDiagramR()] for creating a chord diagram to visualise linkages between
#' dataSources and [BeeBDC::dupePlotR()] to visualise the numbers and proportions of duplicates in
#' each dataSource.
#' 
#' @export
#'
#' @examples
#' beesFlagged_out <- dupeSummary(
#' data = BeeBDC::beesFlagged,
#'   # Should start with paste0(DataPath, "/Output/Report/"), instead of tempdir():
#' path = paste0(tempdir(), "/"),
#' # options are "ID","collectionInfo", or "both"
#' duplicatedBy = "collectionInfo", # I'm only running ID for the first lot because we might 
#' # recover other info later
#' # The columns to generate completeness info from
#' completeness_cols = c("decimalLatitude",  "decimalLongitude",
#'                       "scientificName", "eventDate"),
#' # idColumns = c("gbifID", "occurrenceID", "recordId","id"),
#' # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
#' collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
#'                    "recordedBy"),
#' # The columns to combine, one-by-one with the collectionCols
#' collectInfoColumns = c("catalogNumber", "otherCatalogNumbers"),
#' # Custom comparisons - as a list of columns to compare
#' # RAW custom comparisons do not use the character and number thresholds
#' CustomComparisonsRAW = dplyr::lst(c("catalogNumber", "institutionCode", "scientificName")),
#' # Other custom comparisons use the character and number thresholds
#' CustomComparisons = dplyr::lst(c("gbifID", "scientificName"),
#'                                 c("occurrenceID", "scientificName"),
#'                                 c("recordId", "scientificName"),
#'                                 c("id", "scientificName")),
#' # The order in which you want to KEEP duplicated based on data source
#' # try unique(check_time$dataSource)
#' sourceOrder = c("CAES", "Gai", "Ecd","BMont", "BMin", "EPEL", "ASP", "KP", "EcoS", "EaCO",
#'                 "FSCA", "Bal", "SMC", "Lic", "Arm",
#'                 "USGS", "ALA", "GBIF","SCAN","iDigBio"),
#' # !!!!!! BELS > GeoLocate
#' # Set the complexity threshold for id letter and number length
#' # minimum number of characters when WITH the numberThreshold
#' characterThreshold = 2,
#' # minimum number of numbers when WITH the characterThreshold
#' numberThreshold = 3,
#' # Minimum number of numbers WITHOUT any characters
#' numberOnlyThreshold = 5)
#' 
#' 
dupeSummary <- function(
    data = NULL,
    path = NULL,
    duplicatedBy = NULL,
    # The columns to generate completeness info from
    completeness_cols = NULL,
    idColumns = NULL,
    # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
    collectionCols = NULL,
      # The columns to combine, one-by-one with the collectionCols
    collectInfoColumns = NULL,
    CustomComparisonsRAW = NULL,
    # Custom comparisons - as a list of 
    CustomComparisons = NULL,
      # The order in which you want to KEEP duplicated based on data source
    sourceOrder = NULL,
    prefixOrder = NULL,
      # Columns not to filter in .summary - default is below
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                        ".uncertaintyThreshold", ".unLicensed"),
    # Set the complexity threshold for id letter and number length
        # minimum number of characters when WITH the numberThreshold
    characterThreshold = 2,
        # minimum number of numbers when WITH the characterThreshold
    numberThreshold = 3,
        # Minimum number of numbers WITHOUT any characters
    numberOnlyThreshold = 5,
    catalogSwitch = TRUE
){
  # locally bind variables to the function
  database_id <- dataSource <- dupColumn_s <- completeness <- .summary <- database_id_match <-
    group <- database_id_Main <- dataSourceMain <- database_id_keep <- . <- NULL
  
  # Load required packages
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  requireNamespace("igraph")
    # Record start time
  startTime <- Sys.time()
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data. I'm a program not a magician.")
  }
  if(is.null(sourceOrder)){
    stop(paste("Warning message: \n",
               " - No sourceOrder provided. This must be provided as the name of each dataSource ",
               "before the first '_' in the desired order.",
               sep=""))
  }
  ###### b. Warnings ####
  if(is.null(duplicatedBy)){
    message(paste("Warning message: \n",
          " - No duplicatedBy provided. Consider if you want to choose to find duplicates by (i)",
          " 'ID' columns only (for pre-cleaned data), by (ii) 'collectionInfo' columns only ",
          "(for cleaned data), or (ii) 'both'.\n",
          "NULL is acceptable, if quickDeDuplicate == TRUE",
          sep = ""))
  }
  if(is.null(idColumns) & stringr::str_detect(duplicatedBy, "ID|both")){
    message(paste("Warning message: \n",
                  " - No idColumns provided. Using default of: ",
                  "c('gbifID',  'occurrenceID', 'recordId', and 'id')",
                  sep=""))
    idColumns = c("gbifID", "occurrenceID", "recordId","id")
  }
  if(is.null(completeness_cols)){
    message(paste("Warning message: \n",
                  " - No completeness_cols provided. Using default of: ",
                  "c('decimalLatitude',  'decimalLongitude', 'scientificName', and 'eventDate')",
                  sep=""))
    completeness_cols = c("decimalLatitude",  "decimalLongitude",
                          "scientificName", "eventDate")
  }
  if(is.null(collectionCols)){
    message(paste("Warning message: \n",
                  " - No collectionCols provided. Using default of: ",
      "c('decimalLatitude',  'decimalLongitude', 'scientificName', 'eventDate', and 'recordedBy')",
                  sep=""))
    collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                       "recordedBy")
  }
  if(is.null(collectInfoColumns)){
    message(paste("Warning message: \n",
                  " - No collectInfoColumns provided. Using default of: ",
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
    " - Generating a basic completeness summary from the ", 
    paste(completeness_cols, collapse = ", "), " columns.","\n",
    "This summary is simply the sum of complete.cases in each column. It ranges from zero to the N",
    " of columns. This will be used to sort duplicate rows and select the most-complete rows.",
    sep = ""
  ))
  
  
  # Update the .summary column, ignoring the dontFilterThese columns.
  writeLines(" - Updating the .summary column to sort by...")
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
    #      " - Formatting the dateIdentified column to date format...", 
    #      sep = ""
    #    ))
    #    Loop_data$dateIdentified <- lubridate::ymd_hms(Loop_data$dateIdentified,
    #                                              truncated = 5, quiet = TRUE) %>%
    #      as.Date()
  
  # Add the dupColumn_s as NA for the first iteration
  Loop_data$dupColumn_s <- NA

    # Create a datset to put duplicates into
    runningDuplicates = dplyr::tibble()

    
    
    #### 1.0 CUSTOM_RAW ####
    if(!is.null(CustomComparisonsRAW)){
      message(" - Working on CustomComparisonsRAW duplicates...")
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
          dplyr::select(tidyselect::all_of(c(currentColumn, "database_id",
                                             "dataSource", "dupColumn_s", "completeness",".summary"))
                        ) %>%
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
          paste0(" - Identified ", 
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
          occurrenceID = dplyr::if_else( 
            stringr::str_count(occurrenceID, "[A-Za-z]") >= characterThreshold &
              stringr::str_count(occurrenceID, "[0-9]") >= numberThreshold |
              stringr::str_count(occurrenceID, "[0-9]") >= numberOnlyThreshold, 
                                         occurrenceID, NA_character_)},
        if("recordId" %in% colnames(Loop_data)){
          recordId = dplyr::if_else( 
            stringr::str_count(recordId, "[A-Za-z]") >= characterThreshold &
              stringr::str_count(recordId, "[0-9]") >= numberThreshold |
              stringr::str_count(recordId, "[0-9]") >= numberOnlyThreshold, 
                                     recordId, NA_character_)},
        if("id" %in% colnames(Loop_data)){
          id = dplyr::if_else( stringr::str_count(id, "[A-Za-z]") >= characterThreshold &
                                 stringr::str_count(id, "[0-9]") >= numberThreshold |
                                 stringr::str_count(id, "[0-9]") >= numberOnlyThreshold, 
                               id, NA_character_)}, 
        if("catalogNumber" %in% colnames(Loop_data)){
          catalogNumber = dplyr::if_else( 
            stringr::str_count(catalogNumber, "[A-Za-z]") >= characterThreshold &
              stringr::str_count(catalogNumber, "[0-9]") >= numberThreshold |
              stringr::str_count(catalogNumber, "[0-9]") >= numberOnlyThreshold, 
                                          catalogNumber, NA_character_)}, 
        if("otherCatalogNumbers" %in% colnames(Loop_data)){
          otherCatalogNumbers = dplyr::if_else( 
            stringr::str_count(otherCatalogNumbers, "[A-Za-z]") >= characterThreshold &
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
      message(" - Working on CustomComparisons duplicates...")
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
          paste0(" - Identified ", 
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
    message(" - Working on ID duplicates...")
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
        paste(" - Identified ", 
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
    message(" - Working on collectionInfo duplicates...")
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
      paste0(" - Identified ", 
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
    writeLines(" - Clustering duplicate pairs...")
      # Cluster the id pairs into groups
    clusteredDuplicates <- runningDuplicates %>% 
      dplyr::select(database_id_match, database_id) %>% 
      igraph::graph_from_data_frame() %>%
      igraph::components()
      # Extract the id and the group only
    clusteredDuplicates <- clusteredDuplicates$membership %>% as.data.frame() %>%
      setNames("group") %>%
      dplyr::mutate(database_id = rownames(.)) %>% dplyr::as_tibble()
      
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
    if(!is.null(prefixOrder)){
      writeLines(" - Ordering prefixs...")
      prefixOrder = prefixOrder
      clusteredDuplicates <- clusteredDuplicates %>%
        # Make a new column with the database_id SOURCE, not the full database_id with numbers
        dplyr::mutate(database_id_Main = stringr::str_replace(database_id,
                                                              pattern = "_.*",
                                                              replacement = "") %>%
                        factor(levels = prefixOrder, ordered = TRUE) ) %>%
        # Sort so that certain datasets will be given preference over one another as user-defined.
        dplyr::arrange(database_id_Main) %>%
        # Remove this sorting column
        dplyr::select(!database_id_Main)
    }
    
    
    writeLines(paste0(" - Ordering data by 1. dataSource, 2. completeness",
               " and 3. .summary column..."))
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
    writeLines(paste0(" - Find and FIRST duplicate to keep and assign other associated", 
                      " duplicates to that one (i.e., across multiple tests a 'kept duplicate', ",
                      "could otherwise be removed)..."))
    # Find the first duplicate and assign the match to that one as the kept dupicate
    clusteredDuplicates <- clusteredDuplicates %>%
      dplyr::mutate(database_id_keep = database_id[1], .after = database_id) %>%
      dplyr::mutate(dataSource_keep = dataSource[1], .after = dataSource) %>%
      # Remove the row for the kept duplicate
      dplyr::filter(!database_id_keep == database_id)
    
       ##### 6.4 Save ####
      # Save the running 
    readr::write_excel_csv(clusteredDuplicates, 
                     file = paste0(path, 
                                  "/duplicateRun_", paste(duplicatedBy, collapse = "_"),
                                  "_", Sys.Date(),
                                  ".csv") %>% 
                       stringr::str_replace_all("//duplicateRun_", "/duplicateRun_"))
  writeLines(paste0(
    " - Duplicates have been saved in the file and location: ", 
    paste0(path, 
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
      " - Across the entire dataset, there are now ",
      format(sum(Loop_data$.duplicates == FALSE), big.mark = ","), " duplicates from a total of ",
      format(nrow(Loop_data), big.mark = ","), " occurrences."
    ))

  endTime <- Sys.time()
  message(paste(
    " - Completed in ", 
    round(difftime(endTime, startTime), digits = 2 ),
    " ",
    units(round(endTime - startTime, digits = 2)),
    sep = ""))
    # Return data
  return(Loop_data)
} # END function



