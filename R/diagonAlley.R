# This function was written by James B Dorey to identify occurrence records with potential fill-down
  # errors in the decimalLatitude and decimalLongitude columns. This function was written between
  # the 27th and 28th of May 2022. Please contact James at jbdorey[at]me.com with questions if needed.


#' Find fill-down errors
#' 
#' A simple function that looks for potential latitude and longitude fill-down errors by 
#' identifying identical coordinates in a user-defined number of consecutive records.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param minRepeats Numeric. The minimum number of lat or lon repeats needed to flag a record
#'
#' @return The function returns the input data with a new column, .sequential, where FALSE = 
#' records that have consecutive latitudes or longitudes greater than or equal to the user-defined 
#' threshold.
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom stats complete.cases
#'
#' @examples
#' # Read in the example data
#'   data(beesRaw)
#'  # Run the function
#'   beesRaw_out <- diagonAlley(
#'     data = beesRaw,
#'     # The minimum number of repeats needed to find a sequence in for flagging
#'     minRepeats = 4)
#'   
#' 
diagonAlley <- function(
  data = NULL,
  minRepeats = NULL
  ){
  # locally bind variables to the function
  eventDate<-recordedBy<-decimalLatitude<-decimalLongitude<-database_id<-.data<-leadingLat<-
    laggingLat<-diffLead_Lat<-diffLag_Lat<-diffLat<-leadingLong<-laggingLong<-diffLead_long<-
    diffLag_long<-diffLong <- NULL
  
  #### 0.0 Warnings ####
  if(is.null(data)){
    stop("\n - Please provide an argument for data. I'm a program, not a magician.")
  }
  if(is.null(minRepeats)){
    warning("\n - minRepeats not provided. Using default value of four")
    minRepeats = 4
  }

  #### 1.0 prepare data ####
  runningData <- data %>%
      # Remove incomplete values 
    dplyr::filter(complete.cases(eventDate, recordedBy)) %>%
    dplyr::filter(complete.cases(decimalLatitude, decimalLongitude)) %>%
      # Select fewer columns to make it easier on the old computer
    dplyr::select(database_id, eventDate, recordedBy, decimalLatitude, decimalLongitude) %>%
      # Group the data by eventDate and recordedBy
    dplyr::group_by( eventDate, recordedBy) %>%
      # Arrange from biggest to lowest decimalLatitude and then decimalLongitude by grouping
    dplyr::arrange(dplyr::desc(decimalLatitude), dplyr::desc(decimalLongitude), 
                   .by_group = TRUE) %>%
      # Remove duplicate lat and longs
    dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
      # Select those groups with four or more occurrences
    dplyr::filter(dplyr::n() >= minRepeats) 
  
    ##### 1.1 Lat sequences ####
  # Find the groups where ALL of the differences between values is the same (is.sequential)
    # Return their database_id
  runningData_Lat <- runningData %>% 
    # Sort 
    dplyr::arrange(dplyr::desc(.data$decimalLatitude), .by_group = TRUE) %>%
    # Add leading columns with the value of the next one
    dplyr::mutate(leadingLat = dplyr::lag(decimalLatitude)) %>%
    dplyr::mutate(laggingLat = dplyr::lead(decimalLatitude)) %>%
    # Add new new columns with the difference 
    dplyr::mutate(diffLead_Lat = (decimalLatitude - leadingLat))  %>%
    dplyr::mutate(diffLag_Lat = (decimalLatitude - laggingLat)) %>%
    # COMBINE these columns so that they are all complete from lead AND lag (no NAs)
    dplyr::mutate(diffLat = dplyr::if_else(is.na(diffLead_Lat),
                                            -diffLag_Lat,
                                            diffLead_Lat)) %>%
    # Remove extra columns
    dplyr::select(!c(leadingLat, laggingLat, diffLead_Lat, diffLag_Lat)) %>%
    # Filter for ONLY records where they are all different
    dplyr::filter(dplyr::n_distinct(decimalLatitude) == dplyr::n(), .preserve = TRUE) %>%
    dplyr::filter(dplyr::n_distinct(diffLat) == 1, .preserve = TRUE) %>%
    # Filter for ONLY records with an actual difference between Lats
    dplyr::filter(abs(diffLat) > 0, .preserve = TRUE) 
  
  ##### 1.1 Long sequences ####
  # Find the groups where ALL of the differences between values is the same (is.sequential)
    # Return their database_id
  runningData_long <- runningData %>% 
      # Sort 
    dplyr::arrange(dplyr::desc(.data$decimalLongitude), .by_group = TRUE) %>%
     # Add leading columns with the value of the next one
    dplyr::mutate(leadingLong = dplyr::lag(decimalLongitude)) %>%
    dplyr::mutate(laggingLong = dplyr::lead(decimalLongitude)) %>%
      # Add new new columns with the difference 
    dplyr::mutate(diffLead_long = (decimalLongitude - leadingLong))  %>%
    dplyr::mutate(diffLag_long = (decimalLongitude - laggingLong)) %>%
      # COMBINE these columns so that they are all complete from lead AND lag (no NAs)
    dplyr::mutate(diffLong = dplyr::if_else(is.na(diffLead_long),
                                            -diffLag_long,
                                            diffLead_long)) %>%
      # Remove extra columns
  dplyr::select(!c(leadingLong, laggingLong, diffLead_long, diffLag_long)) %>%
      # Filter for ONLY records where they are all different
    dplyr::filter(dplyr::n_distinct(decimalLongitude) == dplyr::n(), .preserve = TRUE) %>%
    dplyr::filter(dplyr::n_distinct(diffLong) == 1, .preserve = TRUE) %>%
      # Filter for ONLY records with an actual difference between longs
    dplyr::filter(abs(diffLong) > 0, .preserve = TRUE) 
  
  #### 2.0 Merge ####
    # Get the combined, but unique list of sequential lat/longs to flag
  seqOccs <- unique(c(runningData_Lat$database_id, runningData_long$database_id))
    # Add a new column called .sequential to flag sequential lats and longs as FALSE
  data <- data %>%
    dplyr::mutate(.sequential = !database_id %in% seqOccs)

    # Use output
  message("jbd_diagonAlley:\nFlagged ", 
          format(sum(data$.sequential == FALSE, na.rm = TRUE), big.mark = ","),
          " records\nThe .sequential column was added to the database.\n")
  
  return(data)
}# END function
