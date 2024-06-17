# This function was written by James B Dorey on 8th November 2022 to create a .summary column and
  # replace the bdc_summary_col function which fails with NA values

#' Create or update the .summary flag column
#' 
#' Using all flag columns (column names starting with "."), this function either creates or updates
#' the .summary flag column which is FALSE when ANY of the flag columns are FALSE. Columns can be excluded
#'  and removed after creating the .summary column. Additionally, the occurrence dataset
#' can be filtered to only those where .summary = TRUE at the end of the function.
#'
#' @param data A data frame or tibble. Occurrence records to use as input.
#' @param dontFilterThese A character vector of flag columns to be ignored in the creation or updating
#'  of the .summary column. Cannot be specified with onlyFilterThese.
#' @param onlyFilterThese A character vector. The inverse of dontFilterThese, where columns identified
#' here will be filtered and no others. Cannot be specified with dontFilterThese.
#' @param removeFilterColumns Logical. If TRUE all columns starting with "." will be removed in the 
#' output data. This only makes sense to use when filterClean = TRUE. Default = FALSE.
#' @param filterClean Logical. If TRUE, the data will be filtered to only those occurrence where .summary
#' = TRUE (i.e., completely clean according to the used flag columns). Default = FALSE.
#'
#' @return Returns a data frame or tibble of the input data but modified based on the above parameters.
#' @export
#' 
#' @importFrom dplyr %>%
#' 
#'
#' @examples
#' # Read in example data
#' data(beesFlagged)
#' 
#' # To only update the .summary column
#' beesFlagged_out <- summaryFun(
#'     data = beesFlagged,
#'     dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms", ".unLicensed"),
#'     removeFilterColumns = FALSE,
#'     filterClean = FALSE)
#'   # View output
#' table(beesFlagged_out$.summary, useNA = "always")
#' 
#' # Now filter to only the clean data and remove the flag columns
#' beesFlagged_out <- summaryFun(
#'   data = BeeBDC::beesFlagged,
#'   dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms", ".unLicensed"),
#'   removeFilterColumns = TRUE,
#'   filterClean = TRUE)
#' # View output
#' table(beesFlagged_out$.summary, useNA = "always")
#' 
#' 
#' 

summaryFun <- function(
    data = NULL,
    dontFilterThese = NULL,
    onlyFilterThese = NULL,
    removeFilterColumns = FALSE,
    filterClean = FALSE){
  # locally bind variables to the function
  . <- rowSum <- .summaryNew <- .summary <- NULL
  
  #### 0.0 Prep ####
  if(is.null(data)){
    stop("You must provide a dataset in the 'data' argument.")
  }
  if(!is.null(dontFilterThese) & !is.null(onlyFilterThese)){
    stop("Please only choose dontFilterThese OR onlyFilterThese.")
  }
  
  ##### 0.1 onlyFilter to dontFilter ####
    # In order to use onlyFilterThese, simply transform it to dontFilterThese
  if(!is.null(onlyFilterThese)){
    dontFilterThese <- data %>%
      # Select all columns starting with "."
      dplyr::select(tidyselect::starts_with(".")) %>%
      colnames(.) %>%
      setdiff(., onlyFilterThese)
  }
  
  
  #### 1.0 Generate .summary column ####
    ##### 1.1 dontFilterThese present ####
    # User output
  if(!is.null(dontFilterThese)){
    writeLines(paste0(" - We will NOT flag the following columns. However, they will remain",
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
      dplyr::select(!tidyselect::starts_with(".summary")) %>%
      # Make FALSE == 1 and TRUE == 0
      dplyr::mutate_if(is.logical, ~abs(as.numeric(.) - 1)) %>%
      # IF rowSum > 0 then there is at least one flag
      dplyr::mutate(rowSum = base::rowSums(., na.rm = TRUE)) %>%
      # Add the .summary column
      dplyr::mutate(.summaryNew = dplyr::if_else(rowSum > 0,
                                                 FALSE, TRUE)) %>%
      dplyr::select(.summaryNew) %>%
      dplyr::bind_cols(data, .) %>%
      dplyr::mutate(.summary = .summaryNew) %>% dplyr::select(!.summaryNew) 
  }
  
  ##### 1.2 dontFilterThese NULL ####
  if(is.null(dontFilterThese)){
    writeLines(paste0(" - We will flag all columns starting with '.'"))
      # Run function
    dataOut <-
      data %>%
      # Update .summary column
      # Select all columns starting with "."
      dplyr::select(tidyselect::starts_with(".")) %>% 
      # Delete the summary column if it's there
      dplyr::select(!tidyselect::starts_with(".summary")) %>%
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
  message(paste(" - summaryFun:\nFlagged", 
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
    message(paste(" - REMOVED all occurrences that were FALSE for the 'summary' column.")) 
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

