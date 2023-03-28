# This function was written by James Dorey to chunk the bdc_coordinates_transposed function
# to allow bigger datasets to be analysed without consuming too much RAM.
# This function was written on the 31st of May 2022. For questions, please email jbdorey@me.com

jbd_Ctrans_chunker <- function(
    data = NULL,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    id = "databse_id",
    country = "country_suggested",
    countryCode = "countryCode",
    sci_names = NULL,
    border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
    save_outputs = TRUE,
    
     # How many rows to process at a time
    stepSize = 1000000,
     # Start row
    chunkStart = 1,
     # If FALSE it may overwrite existing dataset
    append = TRUE){
  
  require(bdc)
  require(dplyr)
  require(magrittr)
  require(readr)
  require(tibble)
  
  #### 0.0 Prep ####
  startTime <- Sys.time()
    # Select the output file name to be saved as you run
  fileName <- paste("01_coordinates_transposed_", Sys.Date(), ".csv", sep = "")
    #### 0.1 nChuncks ####
   # Find the number of chunks needed to complete the run
  nChunks = ceiling(nrow(data)/stepSize)
   # IF a run failed you can start again from the same spot using append = TRUE
  #### 0.2 Append ####
  if(append == TRUE){
      # Read in the Tranps_tibble csv
    Tranps_tibble = readr::read_csv("Tranps_tibble")
     # set the chunkStart to the number of rows completed plus one
    chunkStart = nrow(Tranps_tibble) + 1
    nChunks = ceiling((nrow(data)-chunkStart)/stepSize)
  } # END append IF statement
  # The chunkEnd is the same as the stepSize initially, but the chunkEnd will change with each iteration
  # It will also differ if append == true based on where the run is at.
  chunkEnd = (chunkStart + stepSize) - 1
  
  #### 0.3 User text ####
  # Write user output
  writeLines(paste(" — Running chunker with:", "\n",
                   "stepSize = ", 
                   format(stepSize, big.mark=",",scientific=FALSE), "\n",
                   "chunkStart = ", 
                   format(chunkStart, big.mark=",",scientific=FALSE), "\n",
                   "chunkEnd = ", 
                   format(chunkEnd, big.mark=",",scientific=FALSE), "\n",
                   "append = ", append, 
                   sep = ""))
  #### 1.0 Function Loop ####
  # Loop — from chunkStart to the end, process rows in batches of chunkEnd
  for(i in 1:nChunks){
    # Select rows from chunkStart to chunkEnd
    loop_check_pf = data[chunkStart:chunkEnd,] %>%
        # Drop unused factors
      base::droplevels()

    # User output
    writeLines(paste(" — Starting chunk ", i, "...", "\n",
                     "From ",  
                     format(chunkStart, big.mark=",",scientific=FALSE), " to ", 
                     format(chunkEnd, big.mark=",",scientific=FALSE),
                     sep = ""))
      ##### 1.1 Function ####
    # Run the bdc_country_from_coordinates function from the bdc package
    loop_check_pf <- jbd_coordinates_transposed(
      data = loop_check_pf,
      lat = lat,
      lon = lon,
      sci_names = sci_names,
      country = country,
      countryCode = countryCode,
      id = id,
      border_buffer = border_buffer,
      save_outputs = save_outputs,
      fileName = fileName)
    
    #### 1.2 Save + bind file ####
    # Save a smaller csv file with the database_id and country name to be matched later
    # For the first instance in the loop...
    if(i == 1 && append == FALSE){
      Tranps_tibble = tibble::tibble(loop_check_pf)
    }else{
      Tranps_tibble = dplyr::bind_rows(Tranps_tibble, 
                                       loop_check_pf)
    }
    # Set chunkStart to be chunkEnd +1 for the next row
    chunkStart = chunkStart + stepSize
    chunkEnd = chunkEnd + stepSize
    # Make room on the RAM by cleaning up the garbage
    # user output
      #### 1.3 Clear RAM ####
    writeLines(paste(" — Cleaning RAM.", sep = ""))
    gc()
    
      #### 1.4 User text ####
    # Print use output
    writeLines(paste(" — Finished chunk ", i, " with ", nChunks,
                     " remaining",
                     ". ","Records examined: ", 
                     format(nrow(Tranps_tibble), big.mark=",",scientific=FALSE),
                     sep = "") )
    # Save as a csv after each iteration
    readr::write_csv(Tranps_tibble, file = "Tranps_tibble")
  } # END loop
  # Remove NA values
  Tranps_tibble <- Tranps_tibble %>%
    dplyr::filter(!is.na(database_id)) %>%
    # Remove any duplicates that have been introduced
    dplyr::distinct()
  endTime <- Sys.time()
  
  message(paste(
    " — Completed in ", 
    round(difftime(endTime, startTime, units = "mins"), digits = 2 ),
    " minutes.",
    sep = ""))
  return(Tranps_tibble)
} # END function
