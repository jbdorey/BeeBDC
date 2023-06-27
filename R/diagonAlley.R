# This function was written by James B Dorey to identify occurrence records with potential fill-down
  # errors in the decimalLatitude and decimalLongitude columns. This function was written between
  # the 27th and 28th of May 2022. Please contact James at jbdorey[at]me.com with questions if needed.


#' Find fill-down errors
#' 
#' A simple function that looks for potential latitude and longitude fill-down errors by 
#' identifying consecutive occurrences with coordinates at regular intervals. This is accomplished
#' by using a sliding window with the length determined by [minRepeats]
#' 
#' The sliding window (and hence fill-down errors) will only be examined 
#' within the user-defined [groupingColumns]; if any of those 
#' columns are empty, that record will be excluded.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param minRepeats Numeric. The minimum number of lat or lon repeats needed to flag a record
#' @param groupingColumns Character. The column(s) to group the analysis by and search for fill-down
#' errors within. Default = c("eventDate", "recordedBy", "datasetName").
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
  minRepeats = NULL,
  groupingColumns = c("eventDate", "recordedBy", "datasetName")
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
  if(is.null(groupingColumns)){
    warning("\n - groupingColumns not provided. Using the default of, eventDate, recordedBy, ",
            "and datasetName.")
    groupingColumns <- c("eventDate", "recordedBy", "datasetName")
  }

  #### 1.0 prepare data ####
  runningData <- data %>%
      # Remove incomplete values 
    tidyr::drop_na( tidyselect::all_of(groupingColumns)) %>%
    filter(complete.cases(decimalLatitude, decimalLongitude)) %>%
      # Select fewer columns to make it easier on the old computer
    dplyr::select(database_id, decimalLatitude, decimalLongitude, 
                  tidyselect::all_of(groupingColumns)) %>%
      # Group the data by eventDate and recordedBy
    dplyr::group_by(  dplyr::across(tidyselect::all_of(groupingColumns))) %>%
      # Arrange from biggest to lowest decimalLatitude and then decimalLongitude by grouping
    dplyr::arrange(dplyr::desc(decimalLatitude), dplyr::desc(decimalLongitude), 
                   .by_group = TRUE) %>%
      # Remove duplicate lat and longs
    #dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)  %>%
      # Select those groups with four or more occurrences
    dplyr::filter(dplyr::n() >= minRepeats) 
  
    ##### 1.1 Lat sequences ####
  # Find the groups where ALL of the differences between values is the same (is.sequential)
    # Return their database_id
  runningData_Lat <- runningData %>% 
    # Sort 
    dplyr::arrange(dplyr::desc(.data$decimalLatitude), .by_group = TRUE) %>%
    dplyr::distinct(dplyr::across(c(decimalLongitude, decimalLatitude,
                                    tidyselect::all_of(groupingColumns))), .keep_all = TRUE) %>%
    # Add leading columns with the value of the next one
    dplyr::mutate(leadingLat = dplyr::lag(decimalLatitude)) %>%
    dplyr::mutate(laggingLat = dplyr::lead(decimalLatitude)) %>%
    # Add new new columns with the difference 
    dplyr::mutate(diffLead_Lat = (decimalLatitude - leadingLat)) %>%
    dplyr::mutate(diffLag_Lat = (decimalLatitude - laggingLat)) %>%
    # COMBINE these columns so that they are all complete from lead AND lag (no NAs)
    dplyr::mutate(diffLat = dplyr::if_else(is.na(diffLead_Lat),
                                           -diffLag_Lat,
                                           diffLead_Lat), 
                  diffLat = diffLat %>% round(digits = 9)) %>%
    # Remove extra columns
    dplyr::select(!c(leadingLat, laggingLat, diffLead_Lat, diffLag_Lat)) %>%
    # Remove groups below the threshold
    # Group by lat and lon and the groupingColumns
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupingColumns))) %>%
    dplyr::filter(!dplyr::n() < minRepeats)
  
  
  # Re-join with the runningData and match up duplicate lat/lon within groups and assign the same 
  # diffLat values
  runningData_Lat <- runningData_Lat %>% 
    dplyr::bind_rows(runningData %>% 
                       dplyr::filter(!database_id %in% runningData_Lat$database_id)) %>%
    # Group by lat and lon and the groupingColumns
    dplyr::group_by(decimalLatitude, decimalLongitude, 
                    dplyr::across(tidyselect::all_of(groupingColumns))) %>%
    dplyr::arrange(decimalLatitude) %>% 
    # Assign matching occurrences to the same diffLat number so that they will also be flagged
    dplyr::mutate(diffLat = diffLat[[1]]) %>%
    tidyr::drop_na(diffLat) %>%
    dplyr::filter(!diffLat == 0) %>%
    dplyr::ungroup()
  
    # Turn each of the groups into its own tibble within a list
  runningData_LatGrp <- runningData_Lat %>% 
      # Re-group by the groupingColumns and then filter to those that pass the minimum repeats
    dplyr::group_by( dplyr::across(tidyselect::all_of(groupingColumns)), .add = TRUE) %>%
    dplyr::filter(dplyr::n() >= minRepeats) %>%
      # Split groups into a list
    dplyr::group_split()
  
    # Run a loop where each list gets examined using a sliding window based on minRepeats
    # Set up a tibble for the flagged records
  flagRecords <- dplyr::tibble()
  for(i in 1:length(runningData_LatGrp)){
      # Select the ith list, get only the dataset and the miniumum columns required
    groupi <- runningData_LatGrp[i] %>%
      .[[1]] %>%
      dplyr::select(database_id, diffLat) %>%
      dplyr::mutate(diffLat = diffLat %>% as.character())
      # Run the sliding window
    for(j in 1:(nrow(groupi) - minRepeats+1)){
        # select an amount of rows based on minRepeats
      windowj <- groupi[j:(j+minRepeats-1),]
        # If all differences are equal, then add to a running list of database_ids
        if(all(windowj$diffLat == windowj$diffLat[1])){
          flagRecords <- flagRecords %>% 
            dplyr::bind_rows(windowj %>% 
                               dplyr::select(database_id) )
        }  # END if statement
    } # END J loop
    # Keep distinct flagRecords
    flagRecords <- flagRecords %>%
      dplyr::distinct()
  } # END I loop
  
  
  ##### 1.2 Lon sequences ####
  # Find the groups where ALL of the differences between values is the same (is.sequential)
    # Return their database_id
  runningData_Lon <- runningData %>% 
    # Sort 
    dplyr::arrange(dplyr::desc(.data$decimalLongitude), .by_group = TRUE) %>%
    dplyr::distinct(dplyr::across(c(decimalLongitude, decimalLatitude,
                                  tidyselect::all_of(groupingColumns))), .keep_all = TRUE) %>%
    # Add leading columns with the value of the next one
    dplyr::mutate(leadingLon = dplyr::lag(decimalLongitude)) %>%
    dplyr::mutate(laggingLon = dplyr::lead(decimalLongitude)) %>%
    # Add new new columns with the difference 
    dplyr::mutate(diffLead_Lon = (decimalLongitude - leadingLon)) %>%
    dplyr::mutate(diffLag_Lon = (decimalLongitude - laggingLon)) %>%
    # COMBINE these columns so that they are all complete from lead AND lag (no NAs)
    dplyr::mutate(diffLon = dplyr::if_else(is.na(diffLead_Lon),
                                           -diffLag_Lon,
                                           diffLead_Lon), 
                  diffLon = diffLon %>% round(digits = 9)) %>%
    # Remove extra columns
    dplyr::select(!c(leadingLon, laggingLon, diffLead_Lon, diffLag_Lon)) %>%
      # Remove groups below the threshold
    # Group by lat and lon and the groupingColumns
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupingColumns))) %>%
    dplyr::filter(!dplyr::n() < minRepeats)
  
  
    # Re-join with the runningData and match up duplicate lat/lon within groups and assign the same 
      # diffLon values
  runningData_Lon <- runningData_Lon %>% 
    dplyr::bind_rows(runningData %>% 
                       dplyr::filter(!database_id %in% runningData_Lon$database_id)) %>%
        # Group by lat and lon and the groupingColumns
    dplyr::group_by(decimalLatitude, decimalLongitude, 
                    dplyr::across(tidyselect::all_of(groupingColumns))) %>%
    dplyr::arrange(decimalLongitude) %>% 
    # Assign matching occurrences to the same diffLon number so that they will also be flagged
    dplyr::mutate(diffLon = diffLon[[1]]) %>%
    tidyr::drop_na(diffLon) %>%
    dplyr::filter(!diffLon == 0) %>%
    dplyr::ungroup()
  
  # Turn each of the groups into its own tibble within a list
  runningData_LonGrp <- runningData_Lon %>% 
    # Re-group by the groupingColumns and then filter to those that pass the minimum repeats
    dplyr::group_by( dplyr::across(tidyselect::all_of(groupingColumns)), .add = TRUE) %>%
    dplyr::filter(dplyr::n() >= minRepeats) %>%
    # Split groups into a list
    dplyr::group_split()
  
  # Run a loop where each list gets examined using a sliding window based on minRepeats
  for(i in 1:length(runningData_LonGrp)){
    # Select the ith list, get only the dataset and the miniumum columns required
    groupi <- runningData_LonGrp[i] %>%
      .[[1]] %>%
      dplyr::select(database_id, diffLon) %>%
      dplyr::mutate(diffLon = diffLon %>% as.character())
    # Run the sliding window
    for(j in 1:(nrow(groupi) - minRepeats+1)){
      # select an amount of rows based on minRepeats
      windowj <- groupi[j:(j+minRepeats-1),]
      # If all differences are equal, then add to a running list of database_ids
      if(all(windowj$diffLon == windowj$diffLon[1])){
        flagRecords <- flagRecords %>% 
          dplyr::bind_rows(windowj %>% 
                             dplyr::select(database_id) )
      }  # END if statement
    } # END J loop
    # Keep distinct flagRecords
    flagRecords <- flagRecords %>%
      dplyr::distinct()
  } # END I loop
  
  
  #### 2.0 Merge ####
    # Add a new column called .sequential to flag sequential lats and longs as FALSE
  data <- data %>%
    dplyr::mutate(.sequential = !database_id %in% flagRecords$database_id)

    # Use output
  message("jbd_diagonAlley:\nFlagged ", 
          format(sum(data$.sequential == FALSE, na.rm = TRUE), big.mark = ","),
          " records\nThe .sequential column was added to the database.\n")
  
  return(data)
}# END function
