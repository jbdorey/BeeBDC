# This function was written by James Dorey to chunk the HarmonizeR function
# to allow bigger datasets to be analysed without consuming too much RAM.
# This function was written on the 7th August 2022. For questions, please email jbdorey@me.com

jbd_HarmonizeR_chunker <- function(
    path = NULL, #The path to a folder that the output can be saved
    SynList = NULL, # The formatted taxonomy file
    occurrences = NULL,
    # How many rows to process at a time
    stepSize = 1000000,
    # Start row
    chunkStart = 1){
  #### 0.0 Prep ####
  ##### 0.1 nChunks ####
  # Find the number of chunks needed to complete the run
  nChunks = ceiling(nrow(occurrences)/stepSize)
  # The chunkEnd is the same as the stepSize initially, but the chunkEnd will change with each iteration
  ##### 0.3 chunkEnd ####
  chunkEnd = (chunkStart + stepSize) - 1
  
  ##### 0.4 Text out ####
  # Write user output
  message(paste(" — Running chunker with:", "\n",
                "stepSize = ", 
                format(stepSize, big.mark=",",scientific=FALSE), "\n",
                "chunkStart = ", 
                format(chunkStart, big.mark=",",scientific=FALSE), "\n",
                "chunkEnd = ", 
                format(chunkEnd, big.mark=",",scientific=FALSE), "\n",
                "No append option yet provided", 
                sep = ""))
  #### 1.0 Function Loop ####
  # Loop — from chunkStart to the end, process rows in batches of chunkEnd
  for(i in 1:nChunks){
    # Select rows from chunkStart to chunkEnd
    loop_harmon = occurrences[chunkStart:chunkEnd,]
    
    # User output
    writeLines(paste(" — Starting chunk ", i, "...", "\n",
                     "From ",  
                     format(chunkStart, big.mark=",",scientific=FALSE), " to ", 
                     format(chunkEnd, big.mark=",",scientific=FALSE),
                     sep = ""))
    ##### 1.1 Function ####
    # Run the bdc_country_from_coordinates function from the bdc package
    loop_harmon <- HarmonizeR(path = path, #The path to a folder that the output can be saved
                                SynList = SynList, # The formatted taxonomy file
                                occurrences = occurrences)
    #### 1.2 Save file ####
    # Save a smaller csv file with the database_id and country name to be matched later
    # For the first instance in the loop...
    if(i == 1){
      outHarmon = loop_harmon
    }else{
      outHarmon = dplyr::bind_rows(outHarmon, 
                                     tibble::tibble(loop_harmon))
    }# END ele
    
    #### 1.3 New chunks ####
    # Set chunkStart to be chunkEnd +1 for the next row
    chunkStart = chunkStart + stepSize
    chunkEnd = chunkEnd + stepSize
    ###### 1.4 Text and gc ####
    # Make room on the RAM by cleaning up the garbage
    # user output
    writeLines(paste(" — Cleaning RAM.", sep = ""))
    gc()
    
    # Print use output
    writeLines(paste(" — Finished chunk ", i, " of ", nChunks,
                     " chunks",
                     ". ","Records examined: ", 
                     format(nrow(outHarmon), big.mark=",",scientific=FALSE),
                     sep = "") )
    ##### 1.5 Save ####
    # Save as a csv after each iteration
    readr::write_csv(outHarmon, file = "outHarmon.csv")
  } # END loop
  colnames(outHarmon) <- c("database_id", "country")
  
  #### 2.0 Return ####
  return(outHarmon)
} # END function
