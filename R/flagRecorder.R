# This function was written by James Dorey to load and output updated summary information from flagging
  # This function was written from the 9th of June 2022. For questions, please email James
# at jbdorey@me.com


flagRecorder <- function(
    data = NULL,
    outpath = NULL,
    filename = NULL,
    findFilePath = NULL,
    idColumns = NULL,
    findFile = NULL,
    printSummary = FALSE){
    #### 0.0 Prep ####
      ##### 0.1 Packages ####
  require(tidyselect)
  require(dplyr)

      ##### 0.2 Warnings ####
  if(is.null(data)){
    warning(" — Please provide a dataset.")
  }
  if(is.null(outpath)){
    warning(" — Please provide an outpath to where the bdc folders are dataset.")
  }
  if(is.null(idColumns)){
    warning(paste(" — No ID columns were selected! We will keep the following id columns intact:\n",
                  "database_id, id, catalogNumber, and occurrenceID.",
            sep = ""))
    idColumns = c("database_id", "id", "catalogNumber", "occurrenceID")
  }
  if(is.null(findFile)){
    warning(" — Please provide a findFile argument — TRUE (find existing file) or FALSE (start from scratch).")
  }
  
  
    ##### 0.3 Data in ####
      ###### a. Occurrence dataset ####
  # Select the columns from the input occurrence dataset
  data <- data %>%
    dplyr::select( dplyr::all_of(idColumns), tidyselect::starts_with(".")) 
  
      ###### b. Existing data ####
  if(findFile == TRUE){
    # Find an existing file
  flagPath <- file_finder(path = outpath, fileName = "flagsRecorded_")
    # Read it in
  flagColumns <- readr::read_csv(flagPath)
    # Find the new columns that need adding in
  newColumns <- setdiff(colnames(data), colnames(flagColumns))
    # Merge the new columns to the data tibble
  data <- flagColumns %>%
    dplyr::left_join(
        # Select only the new columns to add, from the new tibble.
      dplyr::select(data, c(database_id, dplyr::all_of(newColumns))),
        # Merge by database_id
      by = "database_id", keep = FALSE)
    # Remove the spent dataframe
  rm(flagColumns)
  }
  
  
    #### 1.0 Yes .summary ####
  if(".summary" %in% colnames(data)){
    # Update .summary column
  summaryCol <- data  %>% 
    # Select all columns starting with "."
    dplyr::select(tidyselect::starts_with(".")) %>% 
    # Delete the summary column if it's there
    dplyr::select(!tidyselect::starts_with(".summary")) %>%
    # Make FALSE == 1 and TRUE == 0
    dplyr::mutate_if(is.logical, ~abs(as.numeric(.) - 1)) %>%
    # IF rowSum > 0 then there is at least one flag
    dplyr::mutate(rowSum = rowSums(., na.rm = TRUE)) %>%
    # Add the .summary column
    dplyr::mutate(.summary = dplyr::if_else(rowSum > 0,
                                            FALSE, TRUE)) %>%
    dplyr::select(.summary) 
    # Add this column in
  data <- data %>%
    dplyr::mutate(.summary = summaryCol$.summary)
  # User output
  message(" — .summary column detected. This will be over-written.")
  }
  
  #### 2.0 No .summary ####
  if(!".summary" %in% colnames(data)){
    # Update .summary column
    summaryCol <- data  %>% 
      # Select al lcolumns starting with "."
      dplyr::select(tidyselect::starts_with(".")) %>% 
      # Delete the summary column if it's there
      dplyr::select(!tidyselect::starts_with(".summary")) %>%
      # Make FALSE == 1 and TRUE == 0
      dplyr::mutate_if(is.logical, ~abs(as.numeric(.) - 1)) %>%
      # IF rowSum > 0 then there is at least one flag
      dplyr::mutate(rowSum = rowSums(., na.rm = TRUE)) %>%
      # Add the .summary column
      dplyr::mutate(.summary = dplyr::if_else(rowSum > 0,
                                              FALSE, TRUE)) %>%
      dplyr::select(.summary) 
    # Add this column in
    data <- data %>%
      dplyr::mutate(.summary = summaryCol$.summary)
      # User output
    message(" — NO .summary column detected. This will added to the data.")
  }
  
  #### 3.0 Save ####
    # Save this information as the csv flagsRecorded_DATE.csv
  readr::write_csv(data,
                   paste0(outpath, "/", filename, sep = ""))
    # User output
  message(paste(
    " — Data saved to ", paste0(outpath, "/", filename, sep = ""), 
    sep = ""))
  
    # User output
  writeLines(paste(
    " — Selected ", ncol(data), " columns. These include:\n",
    paste(colnames(data)[1:ncol(data)-1], collapse = ", "),
    ", and ", paste(colnames(data)[ncol(data)]),
    sep = ""
  ))
  
  # Print summary if requested
  if(printSummary == TRUE){
      summary(dplyr::select(data, tidyselect::starts_with(".")))
  }
  
  # Return this file
  return(data)
} # END flagRecorder