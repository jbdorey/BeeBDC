# This function was written by James Dorey to chunk the bdc_coordinates_country_inconsistent function
# to allow bigger datasets to be analysed without consuming too much RAM.
# This function was written on the 3rdst of June 2022. For questions, please email jbdorey@me.com

jbd_CCInconsistent_chunker <- function(
    data = NULL,
    country_name = NULL,
    country = "country_suggested",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1, # in decimal degrees (~11 km at the equator)
    
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
  fileName <- paste("01_coordinates_transposed_", Sys.time(), ".csv", sep = "")
  #### 0.1 nChuncks ####
  # Find the number of chunks needed to complete the run
  nChunks = ceiling(nrow(data)/stepSize)
  # IF a run failed you can start again from the same spot using append = TRUE
  #### 0.2 Append ####
  if(append == TRUE){
    # Read in the CCInconsistent_tibble csv
    CCInconsistent_tibble = readr::read_csv("CCInconsistent_tibble")
    # set the chunkStart to the number of rows completed plus one
    chunkStart = nrow(CCInconsistent_tibble) + 1
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
  
  ##### simplify data ####
  data <- data %>%
    dplyr::select(database_id, country_suggested, decimalLongitude, decimalLatitude)
  
  ##### START ProgBar ####
  # Initializes the progress bar
  pb1 <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                        max = nChunks, # Maximum value of the progress bar
                        style = 3,    # Progress bar style (also available style = 1 and style = 2)
                        width = NA,   # Progress bar width. Defaults to getOption("width")
                        char = "=")   # Character used to create the bar
  
  
  #### 1.0 Function Loop ####
  # Loop — from chunkStart to the end, process rows in batches of chunkEnd
  for(i in 1:nChunks){
    # Sets the progress bar to the current state
    setTxtProgressBar(pb1, i)
    # Select rows from chunkStart to chunkEnd
    loop_check_pf = data[chunkStart:chunkEnd,]
    
    # User output
    writeLines(paste(" — Starting chunk ", i, "...", "\n",
                     "From ",  
                     format(chunkStart, big.mark=",",scientific=FALSE), " to ", 
                     format(chunkEnd, big.mark=",",scientific=FALSE),
                     sep = ""))
    ##### 1.1 Function ####
    # Run the bdc_coordinates_country_inconsistent function from the bdc package
    check_pf <-
      bdc::bdc_coordinates_country_inconsistent(
        data = data,
        country_name = country_name,
        country = country,
        lon = lon,
        lat = lat,
        dist = dist # in decimal degrees (~11 km at the equator)
      )
    
    
    #### 1.2 Save + bind file ####
    # Save a smaller csv file with the database_id and country name to be matched later
    # For the first instance in the loop...
    if(i == 1 && append == FALSE){
      CCInconsistent_tibble = tibble::tibble(loop_check_pf)
    }else{
      CCInconsistent_tibble = dplyr::bind_rows(CCInconsistent_tibble, 
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
                     format(nrow(CCInconsistent_tibble), big.mark=",",scientific=FALSE),
                     sep = "") )
    # Save as a csv after each iteration
    readr::write_csv(CCInconsistent_tibble, file = "CCInconsistent_tibble")
    # Sets the progress bar to the current state
    setTxtProgressBar(pb1, i)
  } # END loop COLUMNS
  #### END progBar ####
  close(pb1) # Close the connection
  
  endTime <- Sys.time()
  
  message(paste(
    " — Completed in ", 
    round(difftime(endTime, startTime, units = "mins"), digits = 2 ),
    " minutes.",
    sep = ""))
  
  return(CCInconsistent_tibble)
} # END function
