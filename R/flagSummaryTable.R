
#' Build a per-species summary for each and all flags
#' 
#' Takes a flagged dataset and returns the total number of fails (FALSE) per flag (columns starting
#' with ".") and per species. It will ignore the .scientificName_empty and .invalidName columns as 
#' species are not assigned.
#' Users may define the column to group the summary by. While it is intended to work with 
#' the scientificName column, users may select any grouping column (e.g., country).
#'
#' @param data A data frame or tibble. The flagged dataset.
#' @param column Character. The name of the column to group by and summarise the failed occurrences.
#' Default = "scientificName".
#' @param outPath A character path. The path to the directory in which the figure will be saved.
#' Default = OutPath_Report. If is NULL then no file will be saved to the disk.
#' @param fileName Character. The name of the file to be saved, ending in ".csv". 
#' Default = "flagTable.csv".
#' @param percentImpacted Logical. If TRUE (the default), the program will write the percentage of 
#' species impacted and over the percentThreshold for each flagging column.
#' @param percentThreshold Numeric. A number between 0 and 100 to indicate the percent of 
#' individuals (>; within each species) that is impacted by a flag, and to be included in the 
#' percentImpacted. Default = 0.
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
#'   # Run the function and build the flag table
#' flagTibble <- flagSummaryTable(data = beesFlagged,
#'                               column = "scientificName",
#'                               outPath = paste0(tempdir()),
#'                               fileName = "flagTable.csv")
#'                               
#' 
flagSummaryTable <- function(
    data = NULL,
    column = "scientificName",
    outPath = OutPath_Report,
    fileName = "flagTable.csv",
    percentImpacted = TRUE,
    percentThreshold = 0){
  # locally bind variables to the function
  flagColumns <- dataFlags <- speciesColumn <- loopCol <- summaryColumn <- . <- NULL
  flagCol <- .summary <- totalFailed <- totalFailed <- total <- OutPath_Report <- NULL
  .scientificName_empty <- .invalidName <- NULL
  
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
  if(percentThreshold > 100 | percentThreshold < 0){
    stop(" - percentThreshold must range from 0 to 100.")
  }
  
  
  #### 1.0 Data prep ####
    # Filter for bad names to not include in the table
  if(any(colnames(data) %in% ".scientificName_empty")){
    data <- data %>%
      dplyr::filter(!.scientificName_empty == FALSE)
  }
  if(any(colnames(data) %in% ".invalidName")){
    data <- data %>%
      dplyr::filter(!.invalidName == FALSE)
  }
  
  # Re-do the .summary column to be sure its up to date
data <- data %>%
      # Remove those without scientific name and that flag column
    dplyr::select(!tidyselect::any_of(".scientificName_empty")) %>%
      # Do the same for invalid names
    dplyr::select(!tidyselect::any_of(".invalidName")) %>%
      # Refresh the .summary column
    BeeBDC::summaryFun() 

  # Get a character vector of the flag columns 
  flagColumns <- data %>% dplyr::select(tidyselect::starts_with(".")) %>% colnames()

  # Select only the species name and flag columns
  dataFlags <- data %>%
  dplyr::select(tidyselect::all_of( c(column, flagColumns))) 

  # Create a column of only species names to add the summary of each column to below
speciesColumn <- dataFlags %>%
  dplyr::distinct(dplyr::across(tidyselect::all_of(column)))

  #### 2.0 Flag column loop ####
  # Loop through each column to get species level counts, starting from column two so as not to 
    # count the column
for(i in 1:(ncol(dataFlags)-1) ){
    # For the ith column, get a COUNT of the FALSE (failed) occurrences per species (or column) 
  loopCol <- dataFlags %>%
      # Select the relevant columns and group by them
    dplyr::select(tidyselect::all_of( c(column, flagColumns[[i]]))) %>%
    dplyr::group_by(dplyr::across( tidyselect::all_of(c(column, flagColumns[[i]])))) %>%
      # Count the occurrences of both TRUE and FALSE
    dplyr::count(name = "n") %>%
      # Set the column names temporarily and then REMOVE the TRUE counts
    stats::setNames(paste0( c(column, "flagCol", "n" ))) %>%
    dplyr::filter(!flagCol == TRUE) %>%
     # ungroup
    dplyr::ungroup() %>%
      # select the relevant columns
    dplyr::select(tidyselect::all_of( c(column, "n"))) %>%
      # Rename the columns
    stats::setNames(paste0( c(column, flagColumns[[i]]) )) 
  
    # Add the count to the speciesColumn tibble
  speciesColumn <- speciesColumn %>% 
    dplyr::left_join(loopCol, by = column) %>%
    replace(is.na(.), 0)
}

#### 3.0 Summary columns ####
  # Add in the totals and percentage to the last columns
summaryColumn <- speciesColumn %>%
    # Add a count of total records
  dplyr::left_join(dataFlags %>%
                     dplyr::group_by(dplyr::across(tidyselect::all_of(column))) %>%
                     dplyr::count(name = "total"),
                   by = column) %>%
    # Change the .summary column to a total failed column
  dplyr::rename(totalFailed = .summary) %>%
  dplyr::relocate(totalFailed, .after = total) %>%
    # Add percentage failed
  dplyr::mutate(percentFailed = (totalFailed/total)*100)

if(percentImpacted == TRUE){
  # Turn the percentThreshold into a proportion instead of a percentage
  percentThreshold <- percentThreshold/100
  # Get the percentages of species that are impacted by each flag
percentImpacted <- summaryColumn %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("."), 
                                 function(x){
                                   sum(x/total > percentThreshold)/length(x)*100
                                 })) %>%
    # Transpose the tibble
  tidyr::pivot_longer(cols = tidyselect::starts_with("."))
    # Provide use output
writeLines(paste0("The percentages of species impacted by each flag in your analysis are as follows: \n",
       paste0("  ", percentImpacted$name, " = ", round(percentImpacted$value, 2), "%", 
              collapse = "\n"))
       ) # END  writeLines
  } # END percentImpacted == TRUE

  #### 4.0 Output ####
  # If user provided an outPath then save the file
if(!is.null(outPath)){
  summaryColumn %>%
    readr::write_excel_csv(file = paste(outPath, fileName, sep = "/"))
}

  # return the table as a tibble
return(summaryColumn)

} # END function

