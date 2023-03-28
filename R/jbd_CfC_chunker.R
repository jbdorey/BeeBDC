# This function was written by James Dorey to chunk the bdc_country_from_coordinates function
  # to allow bigger datasets to be analysed without consuming too much RAM.
# This function was written on the 12th of May 2022. For questions, please email jbdorey@me.com

#' A wrapper around [BeeDC::jbd_country_from_coordinates()] to chunk analyses
#' 
#' Because the [BeeDC::jbd_country_from_coordinates()] function is very RAM-intensive, this wrapper 
#' allows a user to specify chunk-sizes and only analyse a small portion of the occurrence data at a 
#' time. The prefix jbd_ is used to highlight the difference between that function and the related
#' [bdc::bdc_country_from_coordinates()].
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param lat Character. The name of the column to use as latitude. Default = "decimalLatitude".
#' @param lon Character. The name of the column to use as longitude. Default = "decimalLongitude".
#' @param country Character. The name of the column containing country names. Default = "country.
#' @param stepSize Numeric. The number of occurrences to process in each chunk. Default = 1000000.
#' @param chunkStart Numeric. The chunk number to start from. This can be > 1 when you need to restart 
#' the function from a certain chunk; for example if R failed unexpectedly. 
#' @param append Logical. If TRUE, the function will look to append an existing file.
#'
#' @return A data frame containing database_ids and a country column 
#' that needs to be re-merged with the data input.
#' @export
#'
#' @examples
#' # Because this function iteratively adds to a save file in the background, for simplicity's sake
#' and for the sake of transparency the output needs to be added to the data input
#' outside of the main function.
#' # Tibble of common issues in country names and their replacements
#' commonProblems <- tibble::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES','United States','U.S.A','MX','CA','Bras.','Braz.','Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
#'                                  fix = c('United States of America','United States of America','United States of America','United States of America','United States of America','United States of America','United States of America','Mexico','Canada','Brazil','Brazil','Brazil','Northern Mariana Islands','PUERTO.RICO'))
#' 
#' beesFlagged_out <- jbd_countryNameCleanR(
#'   data = BeeDC::beesFlagged,
#'   commonProblems = commonProblems)
#' 
#' suppressWarnings(
#'   countryOutput <- jbd_CfC_chunker(data = beesFlagged_out,
#'                                    lat = "decimalLatitude",
#'                                    lon = "decimalLongitude",
#'                                    country = "country",
#'                                    # How many rows to process at a time
#'                                    stepSize = 1000000,
#'                                    # Start row
#'                                    chunkStart = 1,
#'                                    append = FALSE),
#'   classes = "warning")
#' 
#' 
#' # Left join these datasets
#' beesFlagged_out <- left_join(beesFlagged_out, countryOutput, by = "database_id")  %>% 
#'   # merge the two country name columns into the "country" column
#'   dplyr::mutate(country = dplyr::coalesce(country.x, country.y)) %>%
#'   # remove the now redundant country columns 
#'   dplyr::select(!c(country.x, country.y)) %>%
#'   # put the column back 
#'   dplyr::relocate(country) %>% 
#'   # Remove duplicates if they arose!
#'   dplyr::distinct()
#' 
#' # Remove illegal characters
#' beesFlagged_out$country <- beesFlagged_out$country %>%
#'   stringr::str_replace(., pattern = paste("\\[", "\\]", "\\?",
#'                                           sep=  "|"), replacement = "")
#' # Replace the problems as they occur
#' beesFlagged_out <- beesFlagged_out %>%
#'   dplyr::left_join(commonProblems, by = c("country" = "problem")) %>%
#'   dplyr::mutate(country = 
#'                   dplyr::if_else(country %in% commonProblems$problem,
#'                                  fix, country)) %>%
#'   dplyr::select(!fix)


jbd_CfC_chunker <- function(data = NULL,
                            lat = "decimalLatitude",
                            lon = "decimalLongitude",
                            country = "country",
                            # How many rows to process at a time
                            stepSize = 1000000,
                            # Start row
                            chunkStart = 1,
                            # If FALSE it may overwrite existing dataset
                            append = FALSE){
  #### 0.0 Prep ####
    ##### 0.1 nChunks ####
  # Find the number of chunks needed to complete the run
  nChunks = ceiling(nrow(data)/stepSize)
  # IF a run failed you can start again from the same spot using append = TRUE
    ##### 0.2 append ####
  if(append == TRUE){
    # Read in the CountryList csv
    CountryList = readr::read_csv("CountryList.csv")
    # set the chunkStart to the number of rows completed plus one
    chunkStart = nrow(CountryList) + 1
    nChunks = ceiling((nrow(data)-chunkStart)/stepSize)
  } # END append IF statement
  # The chunkEnd is the same as the stepSize initially, but the chunkEnd will change with each iteration
  # It will also differ if append == true based on where the run is at.
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
    loop_check_pf <- jbd_country_from_coordinates(
      data = loop_check_pf,
      lat = lat,
      lon = lon,
      country = country)
    #### 1.2 Save file ####
    # Save a smaller csv file with the database_id and country name to be matched later
    # For the first instance in the loop...
    if(i == 1 && append == FALSE){
      CountryList = tibble::tibble(loop_check_pf$database_id, loop_check_pf$country)
    }else{
      CountryList = dplyr::bind_rows(CountryList, 
                                     tibble::tibble(loop_check_pf$database_id,
                                                    loop_check_pf$country))
    }# END else
    
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
                     format(nrow(CountryList), big.mark=",",scientific=FALSE),
                     sep = "") )
      ##### 1.5 Save ####
    # Save as a csv after each iteration
    readr::write_csv(CountryList, file = "CountryList.csv")
  } # END loop
  colnames(CountryList) <- c("database_id", "country")
  
    #### 2.0 Return ####
  # Clean a little
  return(CountryList %>%
            # Drop na rows
           tidyr::drop_na(country))
} # END function
