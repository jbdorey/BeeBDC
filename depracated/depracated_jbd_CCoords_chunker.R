# This function was written by James Dorey to chunk the CoordinateCleaner::clean_coordinates function
# to allow bigger datasets to be analysed.
# This function was written on the 31st of May 2022. For questions, please email jbdorey@me.com

jbd_CCoords_chunker <- function(
    data =  check_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = NULL, # Tests if coords are from x country. This is not needed.
    tests = c(
      "capitals",     # records within 1 km around country and province centroids
      "centroids",    # records within 0.5 km of capitals centroids
      # "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 100m of zoo and herbaria
      # "outliers",     # outliers
      "zeros",         # records with coordinates 0,0
      "seas"
    ),
    capitals_rad = 1000,
    centroids_rad = 500,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 100, # remove zoo and herbaria within 100m
    # outliers_method = "quantile",
    # outliers_mtp = 5,
    # outliers_td = 1000,
    # outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    seas_scale = 50,
    # seas_ref = continent_border,
    # seas_scale = 110,
    value = "spatialvalid", # result of tests are appended in separate columns
    
    # How many rows to process at a time
    stepSize = 1000000,
    # Start row
    chunkStart = 1,
    # If FALSE it may overwrite existing dataset
    append = FALSE,
    # Select the output file name to be saved as you run
    fileName = paste("clean_coordinates", Sys.time(), ".csv", sep = "")
    ){
  
  require(bdc)
  require(dplyr)
  require(magrittr)
  require(readr)
  require(tibble)
  require(CoordinateCleaner)
  
  #### 0.0 Prep ####
  startTime <- Sys.time()

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
    loop_check_pf = data[chunkStart:chunkEnd,]
    
    # User output
    writeLines(paste(" — Starting chunk ", i, "...", "\n",
                     "From ",  
                     format(chunkStart, big.mark=",",scientific=FALSE), " to ", 
                     format(chunkEnd, big.mark=",",scientific=FALSE),
                     sep = ""))
    ##### 1.1 Function ####
    # Run the bdc_country_from_coordinates function from the bdc package
    loop_check_pf <- CoordinateCleaner::clean_coordinates(
      x =  data,
      lon = lon,
      lat = lat,
      species = species,
      countries = countries, # Tests if coords are from x country. This is not needed.
      tests = tests,
      capitals_rad = capitals_rad,
      centroids_rad = centroids_rad,
      centroids_detail = centroids_detail, # test both country and province centroids
      inst_rad = inst_rad, # remove zoo and herbaria within 100m
      range_rad = range_rad,
      zeros_rad = zeros_rad,
      capitals_ref = capitals_ref,
      centroids_ref = centroids_ref,
      country_ref = country_ref,
      country_refcol = country_refcol,
      inst_ref = inst_ref,
      range_ref = range_ref,
      seas_scale = seas_scale,
      value = value)
    
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
    readr::write_csv(Tranps_tibble, file = fileName)
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
