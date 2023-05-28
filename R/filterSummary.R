
#' Build a per-species summary for each and all flags
#' 
#' Takes a flagged dataset and returns the total number of fails (FALSE) per flag (columns starting
#' with ".") and per species.
#' Users may define the column to take group the summary by. While it is intended to work with 
#' the scientificName column, users may select any grouping column (e.g., country).
#'
#' @param data A data frame or tibble. The flagged dataset.
#' @param column Character. The name of the column to group by and summarise the failed occurrences.
#' Default = "scientificName".
#' @param outpath A character path. Optional. The path and file name in which to save the table as 
#' a .csv. If no outpath is provided, the table will not be saved. Default = NULL.
#' 
#' @importFrom dplyr %>%
#'
#' @return A tibble with a column for each flag column (starting with ".") showing the number of 
#' failed (FALSE) occurrences per group. Also shows the (i) total number of records, (ii) total
#' number of failed records, and (iii) the percentage of failed records.
#' @export
#'
#' @examples
#' # Load the toy flagged bee data
#' data("beesFlagged")
#'
#' 
#' filterTibble <- filterSummary(data = beesFlagged,
#'                               column = "scientificName",
#'                               outpath = paste0(tempdir(), "/filterTable.csv"))
#'                               
#' 
filterSummary <- function(
    data = NULL,
    column = "scientificName",
    outpath = NULL){
  # locally bind variables to the function
  filterColumns <- dataFilters <- speciesColumn <- loopCol <- summaryColumn <- . <- NULL
  filterCol <- .summary <- totalFailed <- totalFailed <- total <-  NULL
  
  # Load required packages
  requireNamespace("dplyr")
  requireNamespace("tidyselect")
  requireNamespace("stats")
  requireNamespace("readr")
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data. I'm a program not a magician.")
  }
  
  #### 1.0 Data prep ####
  # Re-do the .summary column to be sure its up to date
data <- data %>%
  BeeDC::summaryFun()

  # Get a character vector of the filter columns 
filterColumns <- data %>% dplyr::select(tidyselect::starts_with(".")) %>% colnames()

  # Select only the species name and filter columns
dataFilters <- data %>%
  dplyr::select(tidyselect::all_of( c(column, filterColumns))) 

  # Create a column of only species names to add the summary of each column to below
speciesColumn <- dataFilters %>%
  dplyr::distinct(dplyr::across(column))

  #### 2.0 Filter column loop ####
  # Loop through each column to get species level counts, starting from column two so as not to 
    # count the column
for(i in 1:(ncol(dataFilters)-1) ){
    # For the ith column, get a COUNT of the FALSE (failed) occurrences per species (or column) 
  loopCol <- dataFilters %>%
      # Select the relevant columns and group by them
    dplyr::select(tidyselect::all_of( c(column, filterColumns[[i]]))) %>%
    dplyr::group_by(dplyr::across( c(column, filterColumns[[i]]))) %>%
      # Count the occurrences of both TRUE and FALSE
    dplyr::count(name = "n") %>%
      # Set the column names temporarily and then REMOVE the TRUE counts
    stats::setNames(paste0( c(column, "filterCol", "n" ))) %>%
    dplyr::filter(!filterCol == TRUE) %>%
     # ungroup
    dplyr::ungroup() %>%
      # select the relevant columns
    dplyr::select(tidyselect::all_of( c(column, "n"))) %>%
      # Rename the columns
    stats::setNames(paste0( c(column, filterColumns[[i]]) )) 
  
    # Add the count to the speciesColumn tibble
  speciesColumn <- speciesColumn %>% 
    dplyr::left_join(loopCol, by = column) %>%
    replace(is.na(.), 0)
}

#### 3.0 Summary columns ####
  # Add in the totals and percentage to the last columns
summaryColumn <- speciesColumn %>%
    # Add a count of total records
  dplyr::left_join(dataFilters %>%
                     dplyr::group_by(dplyr::across(column)) %>%
                     dplyr::count(name = "total"),
                   by = column) %>%
    # Change the .summary column to a total failed column
  dplyr::rename(totalFailed = .summary) %>%
  dplyr::relocate(totalFailed, .after = total) %>%
    # Add percentage failed
  dplyr::mutate(percentFailed = (totalFailed/total)*100)


  #### 4.0 Output ####
  # If user provided an outpath then save the file
if(!is.null(outpath)){
  summaryColumn %>%
    readr::write_csv(file = outpath)
}

  # return the table as a tibble
return(summaryColumn)

} # END function

