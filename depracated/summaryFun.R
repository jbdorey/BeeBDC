# This function was written by James B Dorey on 8th November 2022 to create a .summary column and
  # replace the bdc_summary_col function which fails with NA values

summaryFun <- function(
    data = NULL,
    dontFilterThese = NULL,
    removeFilterColumns = FALSE,
    filterClean = FALSE){
  
  #### 0.0 Prep ####
  if(is.null(data)){
    stop("You must provide a dataset in the 'data' argument.")
  }
  
  
  #### 1.0 Generate .summary column ####
    ##### 1.1 dontFilterThese present ####
    # User output
  if(!is.null(dontFilterThese)){
    writeLines(paste0(" — We will NOT flag the following columns. However, they will remain",
                      " in the data file.\n",
                      paste(dontFilterThese, collapse = ", ") ))
      # Run function
    dataOut <-
      data %>%
      # Which columns NOT to filter
      dplyr::select(!tidyselect::any_of(dontFilterThese)) %>%
      # Update .summary column
      # Select all columns starting with "."
      dplyr::select(tidyselect::starts_with(".")) %>% 
      # Delete the summary column if it's there
      dplyr::select(!tidyselect::any_of(".summary")) %>%
      # Make FALSE == 1 and TRUE == 0
      dplyr::mutate_if(is.logical, ~abs(as.numeric(.) - 1)) %>%
      # IF rowSum > 0 then there is at least one flag
      dplyr::mutate(rowSum = rowSums(., na.rm = TRUE)) %>%
      # Add the .summary column
      dplyr::mutate(.summaryNew = dplyr::if_else(rowSum > 0,
                                                 FALSE, TRUE)) %>%
      dplyr::select(.summaryNew) %>%
      dplyr::bind_cols(data, .) %>%
      dplyr::mutate(.summary = .summaryNew) %>% dplyr::select(!.summaryNew) 
  }
  
  ##### 1.2 dontFilterThese NULL ####
  if(is.null(dontFilterThese)){
    writeLines(paste0(" — We will flag all columns starting with '.'"))
      # Run function
    dataOut <-
      data %>%
      # Update .summary column
      # Select all columns starting with "."
      dplyr::select(tidyselect::starts_with(".")) %>% 
      # Delete the summary column if it's there
      dplyr::select(!tidyselect::any_of(".summary")) %>%
      # Make FALSE == 1 and TRUE == 0
      dplyr::mutate_if(is.logical, ~abs(as.numeric(.) - 1)) %>%
      # IF rowSum > 0 then there is at least one flag
      dplyr::mutate(rowSum = rowSums(., na.rm = TRUE)) %>%
      # Add the .summary column
      dplyr::mutate(.summaryNew = dplyr::if_else(rowSum > 0,
                                                 FALSE, TRUE)) %>%
      dplyr::select(.summaryNew) %>%
      dplyr::bind_cols(data, .) %>%
      dplyr::mutate(.summary = .summaryNew) %>% dplyr::select(!.summaryNew) 
  }
  
  ##### 1.3 User message ####
  message(paste(" — summaryColumnR:\nFlagged", 
                format(sum(dataOut$.summary == FALSE, na.rm = TRUE), big.mark = ","),
                "\n ",
                "The .summary column was added to the database.",
                sep = " "))
  

  #### 2.0 Optional extras ####
  ##### 2.1 Filter for clean ####
  # RFilter for only clean records here if user specifies
  if(filterClean == TRUE){
    dataOut <- dataOut   %>%
      # FILTER HERE
      dplyr::filter(.summary == TRUE) 
    message(paste(" — REMOVED all occurrences that were FALSE for the 'summary' column.")) 
  }
  
  ##### 2.2 Remove filtering columns ####
  # Remove filtering columns if user specifies
  if(removeFilterColumns == TRUE){
    dataOut <- dataOut %>%
      dplyr::select(!tidyselect::starts_with("."))
  }


#### 3.0 Output ####
return(dataOut)
} # End function

