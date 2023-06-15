# This Function is designed to check country-level outliers using the Discover Life checklist.
# It was written by James B Dorey from the 8th of November 2022.

#' Flag country-level outliers with a provided checklist.
#'
#'This function flags country-level outliers using the checklist provided with this package. 
#'For additional context and column names, see [BeeDC::beesChecklist()].
#'
#' @param checklist A data frame or tibble. The formatted checklist which was built based on the Discover Life website.
#' @param occData A data frame or tibble. The a Darwin Core occurrence dataset.
#' @param keepAdjacentCountry Logical. If TRUE, occurrences in countries that are adjacent to checklist countries will be 
#' kept. If FALSE, they will be flagged.
#' @param pointBuffer Numeric. A buffer around points to help them align with a country or coastline.
#' This provides a good way to retain points that occur right along the coast or borders of the 
#' maps in rnaturalearth
#' @param rnearthScale Numeric. The value fed into the map scale parameter for
#'  [rnaturalearth::ne_countries()]'s scale parameter:
#' 	Scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large', where smaller numbers 
#' 	are higher resolution. WARNING: This function is tested on 110 and 50.
#'
#' @return The input data with a new column, .countryOutlier. There are three possible values for 
#' the new column: TRUE == passed, FALSE == failed, NA == did not overlap with rnaturalearth map.
#' 
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' 
#' beesRaw_out <- countryOutlieRs(checklist = BeeDC::beesChecklist,
#'                                occData = BeeDC::beesRaw,
#'                                keepAdjacentCountry = TRUE,
#'                                pointBuffer = 0.05,
#'                                rnearthScale = 50)
#' table(beesRaw_out$.countryOutlier, useNA = "always")

countryOutlieRs <- function(
    checklist = NULL,
    occData = NULL,
    keepAdjacentCountry = TRUE,
    pointBuffer = NULL,
    rnearthScale = 50
    ){
  # locally bind variables to the function
  iso_a2<-iso_a3<-name<-name_long<-continent<-geometry<-countryOutlieRs<-decimalLongitude<-
    countryOutlieRs<-decimalLatitude<-database_id<-countryOutlieRs<-scientificName<-species<-
    family<-subfamily<-genus<-countryOutlieRs<-specificEpithet<-countryOutlieRs<-
    scientificNameAuthorship<-country<-stateProvince<-eventDate<-countryOutlieRs<-
    institutionCode<-recordNumber<-catalogNumber<-dataSource<-countryOutlieRs<-
    verbatim_scientificName<-.<-neighbours<-rowNum<-countryOutlieRs<-neighboursText<-
    SciCountry<-validName<-countryOutlieRs<-SciCountry_noYear<-countryOutlieRs<-
    neighbourMatch_noYear<-countryOutlieRs<-exactMatch_noYear<-matchType<-countryMatch<-
    countryOutlieRs<-countryOutlieRs<-.countryOutlier <- NULL
  
  # REMOVE - TEST thinning
   #  occData <- occData %>%
   #    filter(row_number() %% 100 == 1)
  startTime <- Sys.time()
#### 0.0 Warnings ####
  if(is.null(checklist)){
    stop("You must provide a checklist of countries")
  }
  if(is.null(occData)){
    stop("You must provide occurrence data (occData). Honestly, what do you think I was gonna do without that?")
  }

#### 1.0 Data prep ####
  ##### 1.1 occData ####
    # Drop .countryOutlier if its already present
  occData <- occData %>%
    dplyr::select(!tidyselect::any_of(".countryOutlier"))

  ##### 1.2 rNaturalEarth ####
# Download world map using rnaturalearth packages
countryMap <- rnaturalearth::ne_countries(returnclass = "sf", country = NULL,
                                        type = "countries", scale = rnearthScale)  %>%
      # buffer by zero and make geometry valid to avoid potential issues with polygon intersection
    sf::st_make_valid() %>%
   # sf::st_buffer(., dist = 0) %>%
  # Select only a subset of the naturalearthdata columns to extract
  dplyr::select(iso_a2, iso_a3, name, name_long, continent, geometry) %>%
  # Replace some country codes to match the checklist
  dplyr::mutate(iso_a3 = iso_a3 %>% stringr::str_replace_all(
    c("ALA" = "FIN", # Aland Islands == Finland
    "ASM" = "WSM", # American Samoa == Samoa
    "HKG" = "CHN", # Hong Kong == China
    "MAF" = "SXM"))) # Saint-Martin == Sint Maarten

  # Dont's use spherical geometry
sf::sf_use_s2(FALSE)


  #### 2.0 Use occ. data ####
  ##### 2.1 Occ data ####
  # Turn occData into a simple point feature
  points <- sf::st_as_sf(occData %>% tidyr::drop_na(decimalLongitude, decimalLatitude),
                         coords = c("decimalLongitude", "decimalLatitude"),
                         na.fail = TRUE,
                            # Assign the CRS from the rnaturalearth map to the point data
                            crs = sf::st_crs(countryMap)) %>%
      # Use a subset of columns
    dplyr::select(database_id, scientificName, species, family, subfamily, genus, specificEpithet, 
                  scientificNameAuthorship,
                  country, stateProvince, eventDate, institutionCode, recordNumber, catalogNumber,
                  dataSource, verbatim_scientificName, geometry)
  
    ##### 2.2 Extraction ####
    ###### a. exactCountry ####
  writeLines(" - Extracting country data from points...")
    #Extract polygon information to points
  points_extract <- sf::st_intersection(countryMap,
                              points)
  
  if(!is.null(pointBuffer)){
  # Failed extractions
  points_failed <- points %>%
    dplyr::filter(!database_id %in% points_extract$database_id)
  
  writeLines(" - Buffering failed points by pointBuffer...")
  # Buffer the natural earth map
  suppressWarnings({
      # Buffer the failed points 
    points_failed <- points_failed %>%
      sf::st_buffer(dist = pointBuffer)
      # attempt to overlay these points
    points_failed <- sf::st_intersection(countryMap,
                                         points_failed) 
  })
  # Re-merge good with failed
  points_extract <- points_extract %>%
    sf::st_drop_geometry() %>%
      # remove buffer-matched occurrences
    dplyr::filter(!database_id %in% points_failed$database_id) %>%
      # replace these, but now matched
    dplyr::bind_rows(points_failed %>% sf::st_drop_geometry())
  } # End if pointBuffer
  
  writeLines(" - Prepare the neighbouring country dataset...")
  ###### b. neighbouringCountries ####
    # Get a list of countries that share borders
  countriesBordering <- sf::st_intersects(countryMap, countryMap) %>%
    paste(., sep = ";")
    # Make a new tibble with these information
  neighbouringCountries <- tibble::tibble(
    rowNum = 1:nrow(countryMap),
    country = countryMap$iso_a3,
    neighbours = countriesBordering,
      # Modify the text in column
    neighboursText = mgsub::mgsub(string = neighbours,
                                  pattern = rowNum,
                                  replacement = country) %>%
      stringr::str_replace(string = .,
                           pattern = "c\\(", replacement = "") %>%
      stringr::str_replace(string = .,
                           pattern = "\\)", replacement = "") %>%
      stringr::str_replace(string = .,
                           pattern = ":", replacement = ", ")) 
  
    # Make a long-format tibble with neighbouring countries
  neighbouringCountries <- neighbouringCountries %>%
    tidyr::separate_rows(data = ., neighboursText,
                              sep = ",")  %>%
      # Remove country matching themselves
    dplyr::filter(!(country == neighboursText))
    # Remove extra spaces
  neighbouringCountries$neighboursText <- stringr::str_squish(neighbouringCountries$neighboursText)
    # Join the datasets together so that we can make a list of adjacent countries to match also
  neighbouringCountries <- checklist %>%
    dplyr::left_join(neighbouringCountries %>% 
                       dplyr::select(tidyselect::any_of(c("country", "neighboursText"))),
                     by = c("Alpha-3" = "country"),
                     multiple = "all", relationship = "many-to-many")
    
###### c. Sea points ####
    # Find the points that did not overlap with contries but that had coordinates
  seaPoints <- points %>% 
    sf::st_drop_geometry() %>%
    dplyr::filter(!database_id %in% points_extract$database_id) %>%
    dplyr::select(database_id)
  
    ##### 2.3 Compare ####
  writeLines(" - Compare points with the checklist...")
    # Get a smaller subset of the columns AND make a new columns with scientific name and country
  points_simple <- points_extract %>% 
    dplyr::select(database_id, iso_a3, scientificName, country) %>%
    dplyr::mutate(SciCountry = stringr::str_c(scientificName, 
                                              iso_a3, sep = "_")) %>% 
    # Remove grammar and caps from SciCountry
        dplyr::mutate(SciCountry = tolower(SciCountry) %>%
                          # Replace punctuation
                        stringr::str_replace_all("[\\(\\)\\,\\.\\-]", "") %>%
                        # Replace white spaces with underscores
                        stringr::str_replace_all(" ", "_")) %>%
    # Make the new column to match with full species name, (NO AUTHORHSIP YEAR), and country
    # Remove grammar and caps from SciCountry
    dplyr::mutate(SciCountry_noYear = tolower(SciCountry) %>%
                    # Replace numbers
                    stringr::str_replace_all("[0-9]", "")%>%
                    # replace double __
                    stringr::str_replace_all("__", "_"))
    
  
    ###### a. exactCountry ####
    # Do the same for the ascher checklist
  checklist_simple <- checklist %>%
      # Select subset
    dplyr::select(validName, 'Alpha-3') %>%
      # Harmonise column names
         dplyr::rename(iso_a3 = 'Alpha-3') %>%
      # Make the new column to match with full species name, authorship, and country
    dplyr::mutate(SciCountry = stringr::str_c(validName, iso_a3, sep = "_"))%>% 
    # Remove grammar and caps from SciCountry
    dplyr::mutate(SciCountry = tolower(SciCountry) %>%
                    # Replace punctuation
                    stringr::str_replace_all("[\\(\\)\\,\\.\\-]", "") %>%
                    # Replace white spaces with underscores
                    stringr::str_replace_all(" ", "_")) %>%
  # Make the new column to match with full species name, (NO AUTHORHSIP YEAR), and country
    # Remove grammar and caps from SciCountry
    dplyr::mutate(SciCountry_noYear = tolower(SciCountry) %>%
                    # Replace numbers
                    stringr::str_replace_all("[0-9]", "") %>%
                    # replace double __
                    stringr::str_replace_all("__", "_"))
  
  
  
    # Make a new columns showing if that species is expected in that country
  points_match <- points_simple %>%
    #dplyr::filter(country == "United states") %>%
    #dplyr::mutate(exactMatch = dplyr::if_else(SciCountry %in% checklist_simple$SciCountry,
    #                                    TRUE, FALSE)) %>%
    dplyr::mutate(exactMatch_noYear = dplyr::if_else(SciCountry_noYear %in% checklist_simple$SciCountry_noYear,
                                              TRUE, FALSE)) %>%
    dplyr::left_join(dplyr::select(checklist_simple, SciCountry_noYear), 
                     by = "SciCountry_noYear",
                     multiple = "all")

  
    ###### b. neighbouringCountries ####
  # Get a smaller subset of the data AND make a new columns with scientific name and country
  nchecklist_simple <- neighbouringCountries %>%
    # Select subset
    dplyr::select(validName, neighboursText) %>%
    # Make the new column to match with
    dplyr::mutate(SciCountry = stringr::str_c(validName, neighboursText, sep = "_")) %>%
    # Remove grammar and caps from SciCountry
    dplyr::mutate(SciCountry = tolower(SciCountry) %>%
    # Replace punctuation
    stringr::str_replace_all("[\\(\\)\\,\\.\\-]", "") %>%
    # Replace white spaces with underscores
    stringr::str_replace_all(" ", "_")) %>%
  # Make the new column to match with full species name, (NO AUTHORHSIP YEAR), and country
  # Remove grammar and caps from SciCountry
  dplyr::mutate(SciCountry_noYear = tolower(SciCountry) %>%
                  # Replace numbers
                  stringr::str_replace_all("[0-9]", "") %>%
                  # replace double __
                  stringr::str_replace_all("__", "_")) %>%
      # Get a unique set
    dplyr::distinct(SciCountry, .keep_all = TRUE)
  # Make a new columns showing if that species is expected in that country
  npoints_match <- points_simple %>%
    # dplyr::filter(country == "United states") %>%
    #dplyr::mutate(neighbourMatch = dplyr::if_else(SciCountry %in% nchecklist_simple$SciCountry,
    #                                          TRUE, FALSE)) %>%
    dplyr::mutate(neighbourMatch_noYear = dplyr::if_else(SciCountry_noYear %in% nchecklist_simple$SciCountry_noYear,
                                                     TRUE, FALSE)) 
  
  
  #### 3.0 Merge ####
  writeLines(" - Combining data...")
    # Merge both points_match datasets
  bpoints_match <- tibble::tibble(points_match) %>%
      # Join the two datasets togehter keeping only neighbourMatch and assignmentCertainty from the 
      # neighbour-joined dataset
    dplyr::left_join(dplyr::select(npoints_match, c(database_id, neighbourMatch_noYear)),
                     by = "database_id",
                     multiple = "all") %>%
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
    dplyr::select(database_id, iso_a3, matchType) %>%
    dplyr::rename(
      countryMatch = matchType) 
      # Set flag for those that don't pass countryMatch
  
    ###### a. keepAC == TRUE ####
  if(keepAdjacentCountry == TRUE){
    bpoints_match <- bpoints_match %>%
      dplyr::mutate(
        .countryOutlier = dplyr::if_else(countryMatch != "exact" & countryMatch != "neighbour",
                         FALSE, TRUE)
      ) # END mutate
  } # END TRUE
  
    ###### b. keepAC == FALSE ####
  if(keepAdjacentCountry == FALSE){
    bpoints_match <- bpoints_match %>%
      dplyr::mutate(
        .countryOutlier = dplyr::if_else(countryMatch != "exact",
                                         FALSE, TRUE)
      ) # END mutate
  }# END FALSE
  
    # Keep only entirely unique records
  bpoints_match <- bpoints_match %>%
    dplyr::distinct() 
  
  ###### c. distinct buffer ####
    # For those buffered records that might have overlapped with >1 country, select the unfiltered one, if it exists.
  if(!is.null(pointBuffer)){
    writeLines(" - Sorting and removing potentially duplicated buffered points...")
    bpoints_match <- bpoints_match %>%
      dplyr::group_by(database_id) %>%
      dplyr::arrange(desc(.countryOutlier), .by_group = TRUE) %>%
      dplyr::distinct(database_id, .keep_all = TRUE)
    }
  
  writeLines(paste0(
    " - Finished. \n",
    "We have matched ", 
    format(sum(bpoints_match$countryMatch == "exact", na.rm = TRUE), big.mark = ","),
    " records to their exact country and ", 
    format(sum(bpoints_match$countryMatch == "neighbour", na.rm = TRUE), big.mark = ","), 
    " to an adjacent country\n", 
    "We failed to match ",
    format(sum(bpoints_match$countryMatch == "noMatch", na.rm = TRUE), big.mark = ","), 
    " occurrences to any 'exact' or 'neighbouring' country.\n",
    "There are ",
    format(sum(is.na(bpoints_match$countryMatch)), big.mark = ","), 
    " 'NA' occurrences for this column.\n"
  ))
  
      # Merge with original dataset
    output <- occData %>%
      dplyr::left_join(bpoints_match, by = "database_id",
                       multiple = "all") %>%
        # Add in .sea usign the seaPoints
      dplyr::mutate(.sea = dplyr::if_else(database_id %in% seaPoints$database_id,
                                          FALSE, TRUE))

    
    # return message
    message(paste("countryOutlieRs:\nFlagged", 
                  format(sum(output$.countryOutlier == FALSE, na.rm = TRUE), big.mark = ","), 
                  " for country outlier and flagged ",
                  format(sum(output$.sea == FALSE, na.rm = TRUE), big.mark = ","), 
                  " for in the .sea",
                  "records.\nThree columns were added to the database:\n 1. ",
                  "The '.countryOutlier' column was added which is a filtering column. \n 2. ",
                  "The 'countryMatch' columns indicates exact, neighbour, or noMatch. \n",
                  "3. The '.sea' column was added as a filtering column for points in the ocean.",
                  " The '.sea' column includes the user input buffer in its calculation."),
            sep = "")
    
    # rm(countryMap, points, points_extract, countriesBordering, neighbouringCountries, points_simple,
    #    checklist_simple, points_match, nchecklist_simple, npoints_match, bpoints_match)
  # Return file
    endTime <- Sys.time()
    # Time output
    message(paste(
      " - Completed in ", 
      round(difftime(endTime, startTime, units = "mins"), digits = 2 ),
      " minutes.",
      sep = ""))
return(output)

} # END checklistOutlieR