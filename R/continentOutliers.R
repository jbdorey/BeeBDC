# This Function is designed to check continent-level outliers using the Discover Life checklist.
# It was written by James B Dorey from the 8th of November 2022.

#' Flag continent-level outliers with a provided checklist.
#'
#'This function flags continent-level outliers using the checklist provided with this package. 
#'For additional context and column names, see [BeeBDC::beesChecklist()].
#'
#' @param checklist A data frame or tibble. The formatted checklist which was built based on the Discover Life website.
#' @param data A data frame or tibble. The a Darwin Core occurrence dataset.
#' @param keepAdjacentContinent Logical. If TRUE, occurrences in continents that are adjacent to checklist continents will be 
#' kept. If FALSE, they will be flagged. Defualt = FALSE.
#' @param pointBuffer Numeric. A buffer around points to help them align with a continent or coastline.
#' This provides a good way to retain points that occur right along the coast or borders of the 
#' maps in rnaturalearth
#' @param scale Numeric. The value fed into the map scale parameter for
#'  [rnaturalearth::ne_countries()]'s scale parameter:
#' 	Scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large', where smaller numbers 
#' 	are higher resolution. WARNING: This function is tested on 110 and 50.
#' @param stepSize Numeric. The number of occurrences to process in each chunk. Default = 1000000.
#' @param mc.cores Numeric. If > 1, the function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory. If the cores throw issues, consider
#' setting mc.cores to 1.
#'  Default = 1.
#'
#' @return The input data with two new columns, .continentOutlier or .sea. There are three possible 
#' values for 
#' the new column: TRUE == passed, FALSE == failed (not in continent or in the ocean),
#'  NA == did not overlap with rnaturalearth map.
#' 
#' @seealso [BeeBDC::countryOutlieRs()] for implementation at the country level. Country-level 
#' implementation will be more data-hungry and, where data do not yet exist, difficult to implement.
#' Additionally, see [BeeBDC::beesChecklist()] for input data. Note, not all columns are 
#' necessary if you are building your own dataset. At a minimum you will need *validName* and
#' *continent*.
#' 
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' if(requireNamespace("rnaturalearthdata")){
#' library(magrittr)
#'   # Load in the test dataset
#' beesRaw <- BeeBDC::beesRaw
#'   # For the sake of this example, use the testChecklist
#' system.file("extdata", "testChecklist.rda", package="BeeBDC") |> load()
#'   # For real examples, you might download the beesChecklist from FigShare using 
#'   #  [BeeBDC::beesChecklist()]
#' 
#' beesRaw_out <- continentOutlieRs(checklist = testChecklist,
#'                                data = beesRaw %>%
#'                                dplyr::filter(dplyr::row_number() %in% 1:50),
#'                                keepAdjacentContinent = FALSE,
#'                                pointBuffer = 1,
#'                                scale = 50,
#'                                stepSize = 1000000,
#'                                mc.cores = 1)
#' table(beesRaw_out$.continentOutlier, useNA = "always")
#' } # END if require

continentOutlieRs <- function(
    checklist = NULL,
    data = NULL,
    keepAdjacentContinent = FALSE,
    pointBuffer = NULL,
    scale = 50,
    stepSize = 1000000,
    mc.cores = 1
){
  # locally bind variables to the function
  name<-name_long<-continent<-geometry<-continentOutlieRs<-decimalLongitude<-
    decimalLatitude<-database_id<-countryOutlieRs<-scientificName<-species<-
    family<-subfamily<-genus<-specificEpithet<-
    scientificNameAuthorship<-country<-stateProvince<-eventDate<-
    institutionCode<-recordNumber<-catalogNumber<-dataSource<-countryOutlieRs<-
    verbatim_scientificName<-.<-neighbours<-rowNum<-neighboursText<-
    SciContinent<-validName<-SciContinent_noYear<-
    neighbourMatch_noYear<-exactMatch_noYear<-matchType<-continentMatch<-.continentOutlier <- 
    BeeBDC_order <- BeeBDC_group <- points <- 
    inData <- indexMatch <- NULL
  
  # REMOVE - TEST thinning
  #  data <- data %>%
  #    filter(row_number() %% 100 == 1)
  startTime <- Sys.time()
  #### 0.0 Warnings ####
  if(is.null(checklist)){
    stop("You must provide a checklist of countries")
  }
  if(is.null(data)){
    stop("You must provide occurrence data (data). Honestly, what do you think I was gonna do without that?")
  }
  
  #### 1.0 Data prep ####
  ##### 1.1 data ####
  # Drop .continentOutlier if its already present
  data <- data %>%
    dplyr::select(!tidyselect::any_of(".continentOutlier")) %>%
    # Remove other columns made by this function
    dplyr::select(!tidyselect::starts_with(c( "continentMatch", "continent")))
  
  ##### 1.2 rNaturalEarth ####
  # Download world map using rnaturalearth packages
  suppressWarnings({
    countryMap <- rnaturalearth::ne_countries(returnclass = "sf", country = NULL,
                                              type = "countries", scale = scale)  %>%
      # buffer by zero and make geometry valid to avoid potential issues with polygon intersection
      sf::st_make_valid() %>%
      # Select only a subset of the naturalearthdata columns to extract
      dplyr::select(name, name_long, continent, geometry) 
  })
  
  # Simplify the world map ONCE to be used later
  simplePoly <- countryMap %>% sf::st_drop_geometry() %>%
    dplyr::mutate(indexMatch = dplyr::row_number())
  
    # Make a simplified continent map
  suppressWarnings({
    suppressMessages(
  continentMap <- countryMap %>% 
    dplyr::group_by(continent) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::ungroup()
    )
  })
  # Simplify the continent map ONCE to be used later
  simpleConty <- continentMap %>% sf::st_drop_geometry() %>%
    dplyr::mutate(indexMatch = dplyr::row_number())
  
  # Dont's use spherical geometry
  sf::sf_use_s2(FALSE)
  
  
  #### 2.0 Use occ. data ####
  ##### 2.1 Create functions ####
  ###### a. simple intersect ####
  # Hijack st_intersection to allow it to be run in parallel
  jbd_intersection <- function(inData){
    inData <- inData %>% tidyr::drop_na(decimalLongitude, decimalLatitude)
    suppressWarnings({ suppressMessages({
      # Turn inData into a simple point feature
      points <- sf::st_as_sf(inData,
                             coords = c("decimalLongitude", "decimalLatitude"),
                             na.fail = TRUE,
                             # Assign the CRS from the rnaturalearth map to the point inData
                             crs = sf::st_crs(continentMap)) %>%
        # Use a subset of columns
        dplyr::select(database_id, scientificName, species, family, subfamily, genus, specificEpithet, 
                      scientificNameAuthorship,
                      stateProvince, eventDate, institutionCode, recordNumber, catalogNumber,
                      dataSource, verbatim_scientificName, geometry)
      #Extract polygon information to points
      points_extract <- sf::st_intersects(points, continentMap) %>%
        # return a tibble with the index of each match or NA where there was no match
        dplyr::tibble(indexMatch = .) %>%
        # Convert to numeric
        dplyr::mutate(indexMatch = indexMatch %>% as.character() %>%
                        # deal with problems - Take the first number where two are provided
                        stringr::str_extract("[0-9]+") %>% 
                        # Remove zero to NA
                        stringr::str_replace("^[0]$", NA_character_) %>% as.numeric()) %>%
        dplyr::left_join(simpleConty,
                         by = "indexMatch") %>%
        # Add in the database_id
        dplyr::bind_cols(inData %>% sf::st_drop_geometry())
    }) })
    # Return the points
    return(points_extract)
  } # END jbd_intersection
  
  ###### b. buffered intersect ####
  # Hijack st_intersection to allow it to be run in parallel
  jbd_bufferedIntersection <- function(inData){
    suppressWarnings({ suppressMessages({
      inData <- inData %>% 
        # Use only complete lat and lon data
        tidyr::drop_na(decimalLongitude, decimalLatitude) %>%
        # Remove the previous column names from jbd_intersection
        dplyr::select(!tidyselect::any_of(c("name","name_long","continent",
                                            "indexMatch")))
      # Turn inData into a simple point feature
      points <- sf::st_as_sf(inData,
                             coords = c("decimalLongitude", "decimalLatitude"),
                             na.fail = TRUE,
                             # Assign the CRS from the rnaturalearth map to the point inData
                             crs = sf::st_crs(continentMap)) %>%
        # Use a subset of columns
        dplyr::select(database_id, scientificName, species, family, subfamily, genus, specificEpithet, 
                      scientificNameAuthorship,
                      stateProvince, eventDate, institutionCode, recordNumber, catalogNumber,
                      dataSource, verbatim_scientificName, geometry) %>%
        # Buffer the points by the pointBuffer
        sf::st_buffer(dist = pointBuffer)
      
      #Extract polygon information to points
      points_extract <- sf::st_intersects(points, continentMap) %>%
        # return a tibble with the index of each match or NA where there was no match
        dplyr::tibble(indexMatch = . ) %>%
        # Convert to numeric
        dplyr::mutate(indexMatch = indexMatch %>% as.character() %>%
                        # deal with problems - Take the first number where two are provided
                        stringr::str_extract("[0-9]+") %>% 
                        # Remove zero to NA
                        stringr::str_replace("^[0]$", NA_character_) %>% as.numeric()) %>%
        dplyr::left_join(simpleConty,
                         by = "indexMatch") %>%
        # Add in the database_id
        dplyr::bind_cols(inData %>% sf::st_drop_geometry() %>% 
                           dplyr::select(!tidyselect::any_of("country")))
    })  })
    # Return the points
    return(points_extract)
  } # END jbd_intersection
  
  
  
  ##### 2.2 Extraction ####
  ###### a. exactContinent ####
  writeLines(" - Extracting continent data from points...")
  points_extract <- data %>%
    # Make a new column with the ordering of rows
    dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
    # Group by the row number and step size
    dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
    # Split the dataset up into a list by group
    dplyr::group_split(.keep = TRUE) %>%
    # Run the actual function
    parallel::mclapply(., jbd_intersection,
                       mc.cores = mc.cores
    ) %>%
    # Combine the lists of tibbles
    dplyr::bind_rows() %>%
    # Drop those occurrences that did not intersect with a continent
    tidyr::drop_na(continent)
  
  
  if(!is.null(pointBuffer)){
    # Failed extractions
    points_failed <- data %>%
      dplyr::filter(!database_id %in% points_extract$database_id)
    
    writeLines(" - Buffering failed points by pointBuffer...")
    
    points_failed <- points_failed %>%
      # Make a new column with the ordering of rows
      dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
      # Group by the row number and step size
      dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
      # Split the dataset up into a list by group
      dplyr::group_split(.keep = TRUE) %>%
      # Run the actual function
      parallel::mclapply(., jbd_bufferedIntersection,
                         mc.cores = mc.cores
      ) %>%
      # Combine the lists of tibbles
      dplyr::bind_rows() %>%
      # Drop those occurrences that did not intersect with a continent
      tidyr::drop_na(continent)
    
    if(nrow(points_failed) > 0){
      # Re-merge good with failed
      points_extract <- points_extract %>%
        sf::st_drop_geometry() %>%
        # remove buffer-matched occurrences
        dplyr::filter(!database_id %in% points_failed$database_id) %>%
        # replace these, but now matched
        dplyr::bind_rows(points_failed %>% sf::st_drop_geometry())
    }
  } # End if pointBuffer
  
  writeLines(" - Prepare the neighbouring continent dataset...")
  ###### b. neighbouringContinents ####
  # Get a list of continents that share borders
  continentsBordering <- sf::st_intersects(continentMap, continentMap) %>%
    paste(., sep = ";")
  # Make a new tibble with these information
  neighbouringContinents <- dplyr::tibble(
    rowNum = 1:nrow(continentMap),
    continent = continentMap$continent,
    neighbours = continentsBordering,
    # Modify the text in column
    neighboursText = mgsub::mgsub(string = neighbours,
                                  pattern = rowNum,
                                  replacement = continent) %>%
      stringr::str_replace(string = .,
                           pattern = "c\\(", replacement = "") %>%
      stringr::str_replace(string = .,
                           pattern = "\\)", replacement = "") %>%
      stringr::str_replace(string = .,
                           pattern = ":", replacement = ", ")) 
  
  # Make a long-format tibble with neighbouring countries
  neighbouringContinents <- neighbouringContinents %>%
    tidyr::separate_rows(data = ., neighboursText,
                         sep = ",")  %>%
    # Remove continent matching themselves
    dplyr::filter(!(continent == neighboursText))
  # Remove extra spaces
  neighbouringContinents <- neighbouringContinents %>%
    dplyr::mutate(neighboursText = stringr::str_squish(neighboursText),
                  neighboursText = stringr::str_remove_all(neighboursText, " \\(open ocean"))

  # Join the datasets together so that we can make a list of adjacent countries to match also
  neighbouringContinents <- checklist %>%
    dplyr::left_join(neighbouringContinents %>% 
                       dplyr::select(tidyselect::any_of(c("continent", "neighboursText"))),
                     by = c("continent" = "continent"),
                     multiple = "all", relationship = "many-to-many")
  
  ###### c. Sea points ####
  # Find the points that did not overlap with countries but that had coordinates
  seaPoints <- sf::st_as_sf(data %>% tidyr::drop_na(decimalLongitude, decimalLatitude),
                            coords = c("decimalLongitude", "decimalLatitude"),
                            na.fail = TRUE,
                            # Assign the CRS from the rnaturalearth map to the point data
                            crs = sf::st_crs(countryMap)) %>%
    # Use a subset of columns
    dplyr::select(database_id, scientificName, species, family, subfamily, genus, specificEpithet, 
                  scientificNameAuthorship,
                  stateProvince, eventDate, institutionCode, recordNumber, catalogNumber,
                  dataSource, verbatim_scientificName, geometry) %>% 
    sf::st_drop_geometry() %>%
    dplyr::filter(!database_id %in% points_extract$database_id) %>%
    dplyr::select(database_id)
  
  ##### 2.3 Compare ####
  writeLines(" - Compare points with the checklist...")
  # Get a smaller subset of the columns AND make a new columns with scientific name and country
  points_simple <- points_extract %>% 
    dplyr::select(database_id, continent, scientificName) %>%
    dplyr::mutate(SciContinent = stringr::str_c(scientificName, 
                                              continent, sep = "_")) %>% 
    # Remove grammar and caps from SciContinent
    dplyr::mutate(SciContinent = tolower(SciContinent) %>%
                    # Replace punctuation
                    stringr::str_replace_all("[\\(\\)\\,\\.\\-]", "") %>%
                    # Replace white spaces with underscores
                    stringr::str_replace_all(" ", "_")) %>%
    # Make the new column to match with full species name, (NO AUTHORHSIP YEAR), and country
    # Remove grammar and caps from SciContinent
    dplyr::mutate(SciContinent_noYear = tolower(SciContinent) %>%
                    # Replace numbers
                    stringr::str_replace_all("[0-9]", "")%>%
                    # replace double __
                    stringr::str_replace_all("__", "_"))
  
  
  ###### a. exactContinent ####
  # Do the same for the ascher checklist
  checklist_simple <- checklist %>%
    # Select subset
    dplyr::select(validName,continent) %>%
    # Make the new column to match with full species name, authorship, and continent
    dplyr::mutate(SciContinent = stringr::str_c(validName, continent, sep = "_")) %>% 
    # Remove grammar and caps from SciContinent
    dplyr::mutate(SciContinent = tolower(SciContinent) %>%
                    # Replace punctuation
                    stringr::str_replace_all("[\\(\\)\\,\\.\\-]", "") %>%
                    # Replace white spaces with underscores
                    stringr::str_replace_all(" ", "_")) %>%
    # Make the new column to match with full species name, (NO AUTHORHSIP YEAR), and continent
    # Remove grammar and caps from SciContinent
    dplyr::mutate(SciContinent_noYear = tolower(SciContinent) %>%
                    # Replace numbers
                    stringr::str_replace_all("[0-9]", "") %>%
                    # replace double __
                    stringr::str_replace_all("__", "_"))
  
  
  
  # Make a new columns showing if that species is expected in that continent
  points_match <- points_simple %>%
    #dplyr::filter(continent == "United states") %>%
    #dplyr::mutate(exactMatch = dplyr::if_else(SciContinent %in% checklist_simple$SciContinent,
    #                                    TRUE, FALSE)) %>%
    dplyr::mutate(exactMatch_noYear = dplyr::if_else(SciContinent_noYear %in% checklist_simple$SciContinent_noYear,
                                                     TRUE, FALSE)) %>%
    dplyr::left_join(dplyr::select(checklist_simple, SciContinent_noYear), 
                     by = "SciContinent_noYear",
                     multiple = "all", relationship = "many-to-many")
  
  
  ###### b. neighbouringContinents ####
  # Get a smaller subset of the data AND make a new columns with scientific name and continent
  nchecklist_simple <- neighbouringContinents %>%
    # Select subset
    dplyr::select(validName, neighboursText) %>%
    # Make the new column to match with
    dplyr::mutate(SciContinent = stringr::str_c(validName, neighboursText, sep = "_")) %>%
    # Remove grammar and caps from SciContinent
    dplyr::mutate(SciContinent = tolower(SciContinent) %>%
                    # Replace punctuation
                    stringr::str_replace_all("[\\(\\)\\,\\.\\-]", "") %>%
                    # Replace white spaces with underscores
                    stringr::str_replace_all(" ", "_")) %>%
    # Make the new column to match with full species name, (NO AUTHORHSIP YEAR), and continent
    # Remove grammar and caps from SciContinent
    dplyr::mutate(SciContinent_noYear = tolower(SciContinent) %>%
                    # Replace numbers
                    stringr::str_replace_all("[0-9]", "") %>%
                    # replace double __
                    stringr::str_replace_all("__", "_")) %>%
    # Get a unique set
    dplyr::distinct(SciContinent, .keep_all = TRUE)
  # Make a new columns showing if that species is expected in that continent
  npoints_match <- points_simple %>%
    dplyr::mutate(neighbourMatch_noYear = dplyr::if_else(SciContinent_noYear %in% nchecklist_simple$SciContinent_noYear,
                                                         TRUE, FALSE)) 
  
  
  #### 3.0 Merge ####
  writeLines(" - Combining data...")
  # Merge both points_match datasets
  bpoints_match <- dplyr::tibble(points_match) %>%
    # Join the two datasets togehter keeping only neighbourMatch and assignmentCertainty from the 
    # neighbour-joined dataset
    dplyr::left_join(dplyr::select(npoints_match, c(database_id, neighbourMatch_noYear)),
                     by = "database_id",
                     multiple = "all", relationship = "many-to-many") %>%
    # Remove geometry column
    dplyr::select(!tidyselect::starts_with("geometry")) %>%
    # Combine exactMatch_noYear and neighbourMatch_noYear
    dplyr::mutate(matchType = dplyr::if_else(exactMatch_noYear == TRUE,
                                             "exact", dplyr::if_else(neighbourMatch_noYear == TRUE,
                                                                     "neighbour", 
                                                                     "noMatch"))) 
  
  #### 4.0 Output ####
  ##### 4.1 User output ####
  bpoints_match <- bpoints_match %>%
    # Select the columns to keep
    dplyr::select(database_id, continent, matchType) %>%
    dplyr::rename(
      continentMatch = matchType) 
  # Set flag for those that don't pass continentMatch
  
  ###### a. keepAC == TRUE ####
  if(keepAdjacentContinent == TRUE){
    bpoints_match <- bpoints_match %>%
      dplyr::mutate(
        .continentOutlier = dplyr::if_else(continentMatch != "exact" & continentMatch != "neighbour",
                                         FALSE, TRUE)
      ) # END mutate
  } # END TRUE
  
  ###### b. keepAC == FALSE ####
  if(keepAdjacentContinent == FALSE){
    bpoints_match <- bpoints_match %>%
      dplyr::mutate(
        .continentOutlier = dplyr::if_else(continentMatch != "exact",
                                         FALSE, TRUE)
      ) # END mutate
  }# END FALSE
  
  # Keep only entirely unique records
  bpoints_match <- bpoints_match %>%
    dplyr::distinct() 
  
  ###### c. distinct buffer ####
  # For those buffered records that might have overlapped with >1 continent, select the unfiltered one, if it exists.
  if(!is.null(pointBuffer)){
    writeLines(" - Sorting and removing potentially duplicated buffered points...")
    bpoints_match <- bpoints_match %>%
      dplyr::group_by(database_id) %>%
      dplyr::arrange(desc(.continentOutlier), .by_group = TRUE) %>%
      dplyr::distinct(database_id, .keep_all = TRUE)
  }
  
  
  # Merge with original dataset
  output <- data %>%
    dplyr::left_join(bpoints_match, by = "database_id",
                     multiple = "all", relationship = "many-to-many") %>%
    # Add in .sea usign the seaPoints
    dplyr::mutate(.sea = dplyr::if_else(database_id %in% seaPoints$database_id,
                                        FALSE, TRUE))
  
  
  writeLines(paste0(
    " - Finished. \n",
    "We have matched ", 
    format(sum(bpoints_match$continentMatch == "exact", na.rm = TRUE), big.mark = ","),
    " records to their exact continent and ", 
    format(sum(bpoints_match$continentMatch == "neighbour", na.rm = TRUE), big.mark = ","), 
    " to an adjacent continent\n", 
    "We failed to match ",
    format(sum(bpoints_match$continentMatch == "noMatch", na.rm = TRUE), big.mark = ","), 
    " occurrences to any 'exact' or 'neighbouring' continent\n",
    "There are ",
    format(sum(is.na(output$.continentOutlier)), big.mark = ","), 
    " 'NA' occurrences for the .continentOutlier column.\n"
  ))
  
  
  
  # return message
  message(paste0("continentOutlieRs:\nFlagged ", 
                format(sum(output$.continentOutlier == FALSE, na.rm = TRUE), big.mark = ","), 
                " for continent outlier and flagged ",
                format(sum(output$.sea == FALSE, na.rm = TRUE), big.mark = ","), 
                " for in the .sea ",
                "records.\nThree columns were added to the database:\n1. ",
                "The '.continentOutlier' column was added which is a filtering column. \n2. ",
                "The 'continentMatch' columns indicates exact, neighbour, or noMatch. \n",
                "3. The '.sea' column was added as a filtering column for points in the ocean.",
                " The '.sea' column includes the user input buffer in its calculation."),
          sep = "")
  
  # rm(countryMap, points, points_extract, continentsBordering, neighbouringContinents, points_simple,
  #    checklist_simple, points_match, nchecklist_simple, npoints_match, bpoints_match)
  # Return file
  endTime <- Sys.time()
  # Time output
  message(paste(
    " - Completed in ", 
    round(difftime(endTime, startTime), digits = 2 )," ",
    units(round(endTime - startTime, digits = 2)),
    sep = ""))
  return(output)
  
} # END continentOutlieRs
