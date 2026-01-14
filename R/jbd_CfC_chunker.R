# This function was written by James Dorey to chunk the bdc_country_from_coordinates function
  # to allow bigger datasets to be analysed without consuming too much RAM.
# This function was written on the 12th of May 2022. For questions, please email jbdorey[at]me.com
#' Get country names from coordinates
#' 
#' Because the [bdc::bdc_country_from_coordinates()] function is very RAM-intensive, this wrapper 
#' allows a user to specify chunk-sizes and only analyse a small portion of the occurrence data at a 
#' time. The prefix jbd_ is used to highlight the difference between this function and the original
#' [bdc::bdc_country_from_coordinates()].
#'
#' @param data A data frame or tibble. Occurrence records to use as input.
#' @param lat Character. The name of the column to use as latitude. Default = "decimalLatitude".
#' @param lon Character. The name of the column to use as longitude. Default = "decimalLongitude".
#' @param country Character. The name of the column containing country names. Default = "country.
#' @param stepSize Numeric. The number of occurrences to process in each chunk. Default = 1000000.
#' @param chunkStart Numeric. The chunk number to start from. This can be > 1 when you need to 
#' restart the function from a certain chunk. For example, can be used if R failed unexpectedly.
#' @param path Character. The directory path to a folder in which to save the running countrylist
#' csv file.
#' @param scale Passed to rnaturalearth's ne_countries().
#' Scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large'. Default = "large".
#' @param mc.cores Numeric. If > 1, the function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory.
#'  Default = 1.
#'
#' @return A data frame containing database_ids and a country column 
#' that needs to be re-merged with the data input.
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' if(requireNamespace("rnaturalearthdata")){
#' library("dplyr")
#' data(beesFlagged)
#' HomePath = tempdir()
#' # Tibble of common issues in country names and their replacements
#' commonProblems <- dplyr::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
#' 'United States','U.S.A','MX','CA','Bras.','Braz.','Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
#'                                  fix = c('United States of America','United States of America',
#'                                  'United States of America','United States of America',
#'                                  'United States of America','United States of America',
#'                                  'United States of America','Mexico','Canada','Brazil','Brazil',
#'                                  'Brazil','Northern Mariana Islands','Puerto Rico'))
#'                                  
#' beesFlagged <- beesFlagged %>%
#'       # Replace a name to test
#'    dplyr::mutate(country = stringr::str_replace_all(country, "Brazil", "Brasil"))
#' 
#' beesFlagged_out <- countryNameCleanR(
#'   data = beesFlagged,
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
#'                                    path = HomePath,
#'                                    scale = "medium"),
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
#' } # END if require


jbd_CfC_chunker <- function(data = NULL,
                            lat = "decimalLatitude",
                            lon = "decimalLongitude",
                            country = "country",
                            # How many rows to process at a time
                            stepSize = 1000000,
                            # Start row
                            chunkStart = 1,
                            scale = "medium",
                            path = tempdir(),
                            mc.cores = 1){
  BeeBDC_order <- . <- .data <- id_temp <- name_long <- geometry <- inData <- country_OG <- NULL
  #### 0.0 Prep ####
  startTime <- Sys.time()
    ##### 0.1 nChunks ####
  # Find the number of chunks needed to complete the run
  nChunks = ceiling(nrow(data)/stepSize)
    # Find the max nrow
  nrowMax <- nrow(data)
  # The chunkEnd is the same as the stepSize initially, but the chunkEnd will change with each 
    # iteration
    ##### 0.3 chunkEnd ####
  chunkEnd = (chunkStart + stepSize) - 1

  
  ##### 0.4 Text out ####
  # Write user output
  message(paste(" - Running chunker with:", "\n",
                   "stepSize = ", 
                   format(stepSize, big.mark=",",scientific=FALSE), "\n",
                   "chunkStart = ", 
                   format(chunkStart, big.mark=",",scientific=FALSE), "\n",
                   "chunkEnd = ", 
                   format(chunkEnd, big.mark=",",scientific=FALSE), 
                   sep = ""))
 
  
  #### 1.0 Parallel ####
      ##### 1.1 Input function for parallel ####
    funCoordCountry <-
      function(inData) {
        suppressWarnings({
          check_require_cran("rnaturalearth")
          # check_require_github("ropensci/rnaturalearthdata")
        })

        # create an id_temp
        inData$id_temp <- 1:nrow(inData)
        minimum_colnames <- c(lat, lon)
        if(!all(minimum_colnames %in% colnames(inData))) {
          stop(
            "These columns names were not found in your database: ",
            paste(minimum_colnames[!minimum_colnames %in% colnames(inData)],
                  collapse = ", "),
            call. = FALSE
          )}
        # check if inData has a country column
        has_country <- any(colnames(inData) == country)
        
        if(!has_country) {
          inData$country <- NA}
        
        # converts coordinates columns to numeric
        inData <- inData %>%
          dplyr::mutate(decimalLatitude = as.numeric(.data[[lat]]),
                        decimalLongitude = as.numeric(.data[[lon]]))
        
        worldmap <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf") %>%
          sf::st_make_valid()
        
        data_no_country <- inData %>%
          dplyr::filter(is.na(country) | country == "")
        
        if(nrow(data_no_country) == 0) {
          inData <- inData %>% dplyr::select(-id_temp)
        }else{
          # converts coordinates columns to spatial points
          suppressWarnings({
            data_no_country <-
              CoordinateCleaner::cc_val(
                x = data_no_country,
                lon = lon,
                lat = lat,
                verbose = FALSE
              ) %>%
              sf::st_as_sf(
                .,
                coords = c("decimalLongitude", "decimalLatitude"),
                remove = FALSE
              ) %>%
              sf::st_set_crs(., sf::st_crs(worldmap))
          })
          
          worldmap <-
            sf::st_as_sf(worldmap) %>% dplyr::select(name_long)
          
          # Extract country names from coordinates
          suppressWarnings({
            suppressMessages({
              ext_country <-
                data_no_country %>%
                dplyr::select(id_temp, geometry) %>%
                sf::st_intersection(., worldmap)
            })
          })
          
          ext_country$geometry <- NULL
          
          res <- dplyr::left_join(data_no_country, ext_country, by = "id_temp") %>%
            dplyr::distinct(id_temp, .keep_all = TRUE)
          
          id_replace <- res$id_temp 
          inData[id_replace, "country"] <- res$name_long
          inData <- inData %>% dplyr::select(-id_temp)
        }
        return(dplyr::as_tibble(inData))
      }
    
    ##### 1.2 Run mclapply ####
    # User output
    writeLines(paste(" - Starting parallel operation. Unlike the serial operation (mc.cores = 1)",
                     ", a parallel operation will not provide running feedback. Please be patient",
                     " as this function may take some time to complete. Each chunk will be run on",
                     " a seperate thread so also be aware of RAM usage."))
    loop_check_pf = data %>%
          # Make a new column with the ordering of rows
      dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
          # Group by the row number and step size
      dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
          # Split the dataset up into a list by group
      dplyr::group_split(.keep = TRUE) %>%
    # Run the actual function
      parallel::mclapply(., funCoordCountry,
                         mc.cores = mc.cores
    ) %>%
      # Combine the lists of tibbles
      dplyr::bind_rows()
    CountryList = dplyr::tibble(database_id = loop_check_pf$database_id, 
                                country = loop_check_pf$country, 
                                BeeBDC_order = loop_check_pf$BeeBDC_order) %>%
      # Arrange these
      dplyr::arrange(BeeBDC_order) %>%
        # Remove extra columns
      dplyr::select(!tidyselect::any_of("BeeBDC_order"))
  
  
    #### 2.0 Return ####
  colnames(CountryList) <- c("database_id", "country")
  
  endTime <- Sys.time()
  message(paste(
    " - Completed in ", 
    round(difftime(endTime, startTime), digits = 2 ),
    " ",
    units(round(endTime - startTime, digits = 2)),
    sep = ""))
  
    # Get a subset of the input data
  data <- data %>% 
    dplyr::select(tidyselect::any_of(c("database_id", "country")))
  
  # Clean a little
  CountryList <-  CountryList %>%
    # Drop na rows
    tidyr::drop_na(country)
  
    # Get a summary of the output
  summaryTable <- CountryList %>%
    dplyr::left_join(data, by = "database_id",
                     suffix = c("", "_OG")) %>%
      # Assign changed == 1 if the country name has changed from the original
    dplyr::mutate(changed = dplyr::if_else(is.na(country_OG), 1, 0))
  
  writeLines(paste0(" - We have updated the country names of ", 
                    format(sum(summaryTable$changed), big.mark = ","), 
                    " occurrences that previously had no country name assigned."))
  
  return(CountryList)
} # END function

