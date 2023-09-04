# This function was written by James B Dorey to identify occurrence records with potential fill-down
  # errors in the decimalLatitude and decimalLongitude columns. This function was written between
  # the 27th and 28th of May 2022. Please contact James at jbdorey[at]me.com with questions if needed.


#' Find fill-down errors
#' 
#' A simple function that looks for potential latitude and longitude fill-down errors by 
#' identifying consecutive occurrences with coordinates at regular intervals. This is accomplished
#' by using a sliding window with the length determined by minRepeats.
#' 
#' The sliding window (and hence fill-down errors) will only be examined 
#' within the user-defined groupingColumns; if any of those 
#' columns are empty, that record will be excluded.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param minRepeats Numeric. The minimum number of lat or lon repeats needed to flag a record
#' @param groupingColumns Character. The column(s) to group the analysis by and search for fill-down
#' errors within. Default = c("eventDate", "recordedBy", "datasetName").
#' @param ndec Numeric. The number of decimal places below which records will not be considered
#' in the diagonAlley function. This is fed into [BeeBDC::jbd_coordinates_precision()]. Default = 3.
#' @param stepSize Numeric. The number of occurrences to process in each chunk. Default = 1000000.
#' @param mc.cores Numeric. If > 1, the function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory.
#'  Default = 1.
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
#'     minRepeats = 4,
#'     groupingColumns = c("eventDate", "recordedBy", "datasetName"),
#'     ndec = 3,
#'     stepSize = 1000000,
#'     mc.cores = 1)
#'   
#' 
diagonAlley <- function(
  data = NULL,
  minRepeats = NULL,
  groupingColumns = c("eventDate", "recordedBy", "datasetName"),
  ndec = 3,
  stepSize = 1000000,
  mc.cores = 1
  ){
  # locally bind variables to the function
  eventDate<-recordedBy<-decimalLatitude<-decimalLongitude<-database_id<-.data<-leadingLat<-
    laggingLat<-diffLead_Lat<-diffLag_Lat<-diffLat<- . <- NULL
  .rou <- leadingLon <- laggingLon <- diffLead_Lon <- diffLag_Lon <- diffLon <- NULL
  
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
  startTime <- Sys.time()
  
    ##### 1.1 ndec ####
    # If an ndec is provided, then filter to remove decimal places lower than ndec
  if(!is.null(ndec)){
    writeLines("Removing rounded coordinates with BeeBDC::jbd_coordinates_precision...")
    runningData <- data %>%
    BeeBDC::jbd_coordinates_precision(
      data = .,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      ndec = ndec,
      quieter = TRUE) %>%
      dplyr::filter(!.rou == FALSE) %>% 
      dplyr::select(!.rou)
  }else{
    runningData <- data
    }
  
    ##### 1.2 Initial filtering and prep ####
  runningData <- runningData %>%
    # Select fewer columns to make it easier on the old computer
    dplyr::select(database_id, decimalLatitude, decimalLongitude, 
                  tidyselect::all_of(groupingColumns)) %>%
      # Remove incomplete values 
    tidyr::drop_na( tidyselect::all_of(groupingColumns)) %>%
    tidyr::drop_na(decimalLatitude, decimalLongitude) %>%
      # Group the data by eventDate and recordedBy
    dplyr::group_by(  dplyr::across(tidyselect::all_of(groupingColumns))) %>%
      # Arrange from biggest to lowest decimalLatitude and then decimalLongitude by grouping
    dplyr::arrange(dplyr::desc(decimalLatitude), dplyr::desc(decimalLongitude), 
                   .by_group = TRUE) %>%
      # Remove duplicate lat and longs
    #dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)  %>%
      # Select those groups with four or more occurrences
    dplyr::filter(dplyr::n() >= minRepeats) 
  
  #### 2.0 Identify sequences ####
  if(nrow(runningData) > 0){

      ##### 2.1 Create function ####
    # Set up the loop function
    LatLonFun <- function(funData){
      for(i in 1:length(funData)){
        # Run the sliding window
        for(j in 1:(nrow(funData) - minRepeats+1)){
          # select an amount of rows based on minRepeats
          windowj <- funData[j:(j+minRepeats-1),]
          # If all differences are equal, then add to a running list of database_ids
          if(all(windowj$diff == windowj$diff[1])){
            flaggedRecords <- flaggedRecords %>% 
              dplyr::bind_rows(windowj %>%  
                                 dplyr::select(database_id) )}  # END if statement
          } # END J loop
        # Keep distinct flaggedRecords
        # Run distinct every 1000th iteration, or at the end
        if(i %in% seq(0, length(funData), 1000) | 
           i == length(funData)){
          flaggedRecords <- flaggedRecords %>%
            dplyr::distinct(.keep_all = TRUE)}
        } # END i loop
      return(flaggedRecords)
      }# End LatLonFun
    
    ##### 2.2 Lat sequences ####
    writeLines(" - Starting the latitude sequence...")
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
    dplyr::mutate(diff = dplyr::if_else(is.na(diffLead_Lat),
                                           -diffLag_Lat,
                                           diffLead_Lat), 
                  diff = diff %>% round(digits = 9)) %>%
    # Remove extra columns
    dplyr::select(!c(leadingLat, laggingLat, diffLead_Lat, diffLag_Lat)) %>%
    # Remove groups below the threshold
    # Group by lat and lon and the groupingColumns
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupingColumns))) %>%
    dplyr::filter(!dplyr::n() < minRepeats)
  
  
  # Re-join with the runningData and match up duplicate lat/lon within groups and assign the same 
  # diff values
  runningData_Lat <- runningData_Lat %>% 
    dplyr::bind_rows(runningData %>% 
                       dplyr::filter(!database_id %in% runningData_Lat$database_id)) %>%
    # Group by lat and lon and the groupingColumns
    dplyr::group_by(decimalLatitude, decimalLongitude, 
                    dplyr::across(tidyselect::all_of(groupingColumns))) %>%
    dplyr::arrange(decimalLatitude) %>% 
    # Assign matching occurrences to the same diff number so that they will also be flagged
    dplyr::mutate(diff = diff[[1]]) %>%
    tidyr::drop_na(diff) %>%
    dplyr::filter(!diff == 0) %>%
    dplyr::ungroup()
  
    # Turn each of the groups into its own tibble within a list
  runningData_LatGrp <- runningData_Lat %>% 
      # Re-group by the groupingColumns and then filter to those that pass the minimum repeats
    dplyr::group_by( dplyr::across(tidyselect::all_of(groupingColumns)), .add = TRUE) %>%
    dplyr::filter(dplyr::n() >= minRepeats) %>%
    dplyr::mutate(diff = diff %>% as.character()) %>%
      # Split groups into a list
    dplyr::group_split()
  
    # Remove the spent dataset
  rm(runningData_Lat)
  # Remove excess columns from list
  runningData_LatGrp <- lapply(runningData_LatGrp, function(x) x[(names(x) %in% c("database_id", "diff"))])
  
  # Set up a tibble for the flagged records
  flaggedRecords <- dplyr::tibble()
    # Run the loop function in parallel
  flagRecords_Lat <- runningData_LatGrp %>% 
    parallel::mclapply(LatLonFun, mc.cores = mc.cores) %>%
      # Re-bind the list elements 
    dplyr::bind_rows() 
  
    # Remove the spent dataset
  rm(runningData_LatGrp)
  
  
  ##### 2.3 Lon sequences ####
  writeLines(" - Starting the longitude sequence...")
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
    dplyr::mutate(diff = dplyr::if_else(is.na(diffLead_Lon),
                                           -diffLag_Lon,
                                           diffLead_Lon), 
                  diff = diff %>% round(digits = 9)) %>%
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
    dplyr::mutate(diff = diff[[1]]) %>%
    tidyr::drop_na(diff) %>%
    dplyr::filter(!diff == 0) %>%
    dplyr::ungroup()
  
  # Turn each of the groups into its own tibble within a list
  runningData_LonGrp <- runningData_Lon %>% 
    # Re-group by the groupingColumns and then filter to those that pass the minimum repeats
    dplyr::group_by( dplyr::across(tidyselect::all_of(groupingColumns)), .add = TRUE) %>%
    dplyr::filter(dplyr::n() >= minRepeats) %>%
    dplyr::mutate(diff = diff %>% as.character()) %>%
    # Split groups into a list
    dplyr::group_split()
  
    # Remove the spent dataset
  rm(runningData_Lon)
  # Remove excess columns from list
  runningData_LonGrp <- lapply(runningData_LonGrp, function(x) x[(names(x) %in% c("database_id", "diff"))])

  # Run the loop function in parallel
  flagRecords_Lon <- runningData_LonGrp %>% 
    parallel::mclapply(LatLonFun, mc.cores = mc.cores) %>%
    # Re-bind the list elements 
    dplyr::bind_rows() 
  
    ##### 2.4 Merge lat lon ####
    # Merge the minor runs
  flagRecords <- dplyr::bind_rows(flagRecords_Lat, flagRecords_Lon) %>% 
    dplyr::distinct()
  
  }else{
    flagRecords = dplyr::tibble(database_id = NA_character_)
  } # END nrow(runningData) > 0
    # Remove the spent dataset
  rm(runningData_LonGrp)

  
  #### 3.0 Merge ####
  writeLines(" - Merging results and adding the .sequential column...")
    # Add a new column called .sequential to flag sequential lats and longs as FALSE
  data <- data %>%
    dplyr::mutate(.sequential = !database_id %in% flagRecords$database_id)

    # Use output
  message("\ndiagonAlley:\nFlagged ", 
          format(sum(data$.sequential == FALSE, na.rm = TRUE), big.mark = ","),
          " records\nThe .sequential column was added to the database.\n")
  
    # Return runtime
  endTime <- Sys.time()
  
  message(paste(
    " - Completed in ", 
    round(difftime(endTime, startTime), digits = 2 )," ",
    units(round(endTime - startTime, digits = 2)),
    sep = ""))
  return(data)
}# END function

