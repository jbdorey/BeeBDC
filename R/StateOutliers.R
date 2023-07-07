# This Function is designed to check state-level outliers of the t2t project.
# It was written by James B Dorey from the 1st of August 2022.
#' @importFrom dplyr %>%

StateOutlieRs <- function(
    checklist = NULL,
    checklistColumns = NULL,
    countryList = NULL,
    occData = NULL,
    findHawaii = FALSE
    ){
    # locally bind variabls to the function
  . <- SciPost <- neighbourMatch <- nAssignmentCertainty <- exactMatch <- matchType <- 
    matchCertainty <- stateMatch <- jurisdiction_value<-stateValue<-adm1_code<-iso_3166_2<-name<-
    region<-postal<- geometry<-longitude<-latitude<-database_id<-scientificName<-species<-family<-
    subfamily<-genus<-specificEpithet<-country<-stateProvince<-eventDate<-institutionCode<-
    recordNumber<-catalogNumber<-dataSource<-verbatim_scientificName<-neighbours<-rowNum<-state<-
    neighboursText<-Genus<-Species<-assignmentCertainty <- NULL
  
#### 0.0 Warnings ####
  if(is.null(checklist)){
    stop("You must provide a checklist of states.")
  }
  if(is.null(checklistColumns)){
    stop("You must provide vector of checklistColumns.")
  }
  if(is.null(countryList)){
    stop("You must provide vector of countryList for rnaturalearth to download states from.")
  }
  if(is.null(occData)){
    stop("You must provide occurrence data (occData). Honestly, what do you think I was gonna do without that?")
  }

#### 1.0 Data prep ####
  ##### 1.1 Ascher ####

  # Find Hawaii if true
  if(findHawaii == TRUE){
    checklist <- checklist %>%
      dplyr::mutate(HI = 
                      # IF Hawaii is mentioned in the jurisdiction_value column, then the species is known from there (introduced or not)
                      dplyr::if_else(stringr::str_detect(
                        string = jurisdiction_value,
                        pattern = "Hawaii"),
                        "HI", ""))
    # Add Hawaii to checklistColumns
    checklistColumns <- c(checklistColumns, "HI")
  }
  # For the above columns, turn into long-format spreadhseet. Each species in EACH state will have a 
  # row of data. 
CL <- checklist %>%
  tidyr::pivot_longer(data = .,
                      cols = tidyselect::all_of(checklistColumns),
                      names_to = "state", values_to = "stateValue") %>%
    # remove empty cells for the values
  tidyr::drop_na(stateValue) %>%
    # add a certainty level == "Poor" for states with a "?"
  dplyr::mutate(assignmentCertainty = 
                  dplyr::if_else(stringr::str_detect(string = stateValue,
                                                     pattern = "\\?"),
                                 "Poor", "Good"))

  ##### 1.2 rNaturalEarth ####
# Download world map using rnaturalearth packages
stateMap <- rnaturalearth::ne_states(returnclass = "sf", country = countryList)  %>%
  # Select only a subset of the naturalearthdata columns to extract
  dplyr::select(adm1_code, iso_3166_2, name, region, postal, geometry)
  # Dont's use spherical geometry
sf::sf_use_s2(FALSE)

  # Can examine missing states between the two
if(length(setdiff(sort(unique(stateMap$postal)), sort(unique(CL$state))) > 0)){
  message(paste0("In the Ascher list, you are mising the following state(s) that occur in the rnaturalearth package:\n",
                 stringr::str_c(setdiff(sort(unique(stateMap$postal)), sort(unique(CL$state))), 
                                collapse = ", ")
                 ))}
if(length(setdiff( sort(unique(CL$state)), sort(unique(stateMap$postal))) > 0)){
  message(paste0("In the rnaturalearth list, you are mising the following state(s) that occur in the ascher list:\n",
                 stringr::str_c(setdiff( sort(unique(CL$state)), sort(unique(stateMap$postal))), 
                                collapse = ", ")
                 ))}


  #### 2.0 Use occ. data ####
  ##### 2.1 Angela data ####
  # Turn occData into a simple point feature
  points <- sf::st_as_sf(occData %>%
                           tidyr::drop_na(longitude, latitude),
                         coords = c("longitude", "latitude"),
                            # Assign the CRS from the rnaturalearth map to the point data
                            crs = sf::st_crs(stateMap)) %>%
      # Use a subset of columns
    dplyr::select(database_id, scientificName, species, family, subfamily, genus, specificEpithet, 
                  country, stateProvince, eventDate, institutionCode, recordNumber, catalogNumber,
                  dataSource, verbatim_scientificName, geometry)
  
    ##### 2.2 Extraction ####
    ###### a. exactState ####
  writeLines(" - Extracting state data from points...")
    #Extract polygon information to points
  points_extract <- sf::st_intersection(stateMap,
                              points)
  ###### a. neighbouringStates ####
    # Get a list of states that share borders
  statesBordering <- sf::st_intersects(stateMap, stateMap) %>%
    paste(., sep = ";")
    # Make a new tibble with these information
  neighbouringStates <- dplyr::tibble(
    rowNum = 1:nrow(stateMap),
    state = stateMap$postal,
    neighbours = statesBordering,
      # Modify the text in column
    neighboursText = mgsub::mgsub(string = neighbours,
                                  pattern = rowNum,
                                  replacement = state) %>%
      stringr::str_replace(string = .,
                           pattern = "c\\(", replacement = "") %>%
      stringr::str_replace(string = .,
                           pattern = "\\)", replacement = "") %>%
      stringr::str_replace(string = .,
                           pattern = ":", replacement = ", ")) 
  
    # Make a long-format tibble with neighbouring states
  neighbouringStates <- neighbouringStates %>%
    tidyr::separate_rows(data = ., neighboursText,
                              sep = ",")  %>%
      # Remove states matching themselves
    dplyr::filter(!(state == neighboursText))
    # Remove extra spaces
  neighbouringStates$neighboursText <- stringr::str_squish(neighbouringStates$neighboursText)
    # Join the datasets togehter so that we can make a list of adjacent states to match also
  neighbouringStates <- CL %>%
    dplyr::left_join(dplyr::select(neighbouringStates, c(state, neighboursText)),
                     by = "state", multiple = "all", relationship = "many-to-many")
    

  
    ##### 2.3 Compare ####
    # Get a smaller subset of the data AND make a new columns with scientific name and state
  points_simple <- points_extract %>% 
    dplyr::select(database_id, postal, genus, specificEpithet, country) %>%
    dplyr::mutate(SciPost = stringr::str_c(genus, specificEpithet, postal, sep = "_"))
  
    ###### a. exactState ####
    # Do the same for the ascher checklist
  CL_simple <- CL %>%
      # Select subset
    dplyr::select(Genus, Species, state, assignmentCertainty) %>%
      # Harmonise column names
    dplyr::rename(genus = Genus,
                  species = Species,
                  postal = state) %>%
      # Make the new column to match with
    dplyr::mutate(SciPost = stringr::str_c(genus, species, postal, sep = "_"))
    # Make a new columns showing if that species is expected in that state.
  points_match <- points_simple %>%
    dplyr::filter(stringr::str_detect(tolower(country), "united states")) %>%
    dplyr::mutate(exactMatch = dplyr::if_else(SciPost %in% CL_simple$SciPost,
                                        TRUE, FALSE)) %>%
      # join the assignmentCertainty column if there's a match
    dplyr::left_join(dplyr::select(CL_simple, SciPost, assignmentCertainty), 
                     by = "SciPost", multiple = "all", relationship = "many-to-many" )
    # Show a quick summary
    # table(points_match$exactMatch, useNA = "always")
    # table(points_match$assignmentCertainty, useNA = "always")
  
  
    ###### b. neighbouringStates ####
  # Get a smaller subset of the data AND make a new columns with scientific name and state
  nCL_simple <- neighbouringStates %>%
    # Select subset
    dplyr::select(Genus, Species, neighboursText, assignmentCertainty) %>%
    # Harmonise column names
    dplyr::rename(genus = Genus,
                  species = Species,
                  postal = neighboursText) %>%
    # Make the new column to match with
    dplyr::mutate(SciPost = stringr::str_c(genus, species, postal, sep = "_")) %>%
      # Get a unique set
    dplyr::distinct(SciPost, .keep_all = TRUE)
  # Make a new columns showing if that species is expected in that state.
  npoints_match <- points_simple %>%
    dplyr::filter(stringr::str_detect(tolower(country), "united states")) %>%
    dplyr::mutate(neighbourMatch = dplyr::if_else(SciPost %in% nCL_simple$SciPost,
                                              TRUE, FALSE)) %>%
      # Assign neighbourMatch for assignmentCertainty where occurrence was neighbour matched.
    dplyr::mutate(nAssignmentCertainty = dplyr::if_else(neighbourMatch == TRUE,
                                                       "neighbourMatch",""))
  
  # Show a quick summary
    # table(npoints_match$neighbourMatch, useNA = "always")
    # table(npoints_match$nAssignmentCertainty, useNA = "always")

  
  #### 3.0 Merge ####
  writeLines(" - Combining data...")
    # Merge both points_match datasets
  bpoints_match <- dplyr::tibble(points_match) %>%
      # Join the two datasets togehter keeping only neighbourMatch and assignmentCertainty from the 
      # neighbour-joined dataset
    dplyr::left_join(dplyr::select(npoints_match, c(database_id, neighbourMatch, 
                                                       nAssignmentCertainty)),
                     by = "database_id", multiple = "all", relationship = "many-to-many") %>%
      # Remove geometry column
    dplyr::select(!tidyselect::starts_with("geometry")) %>%
      # Combine exactMatch and neighbourMatch
    dplyr::mutate(matchType = dplyr::if_else(exactMatch == TRUE,
                                             "exact", dplyr::if_else(neighbourMatch == TRUE,
                                                                     "neighbour", 
                                                                     "noMatch"))) %>%
    # combine assignmentCertainty and nAssignmentCertainty
    dplyr::mutate(matchCertainty = dplyr::if_else(matchType == "exact",
                                                  assignmentCertainty, 
                                                  dplyr::if_else(matchType == "neighbour",
                                                                 nAssignmentCertainty, "NA")))
  
  #### 4.0 Output ####
    ##### 4.1 User output ####
  writeLines(paste0(
    " - Finished. \n",
    "We have matched ", 
    format(sum(bpoints_match$matchType == "exact", na.rm = TRUE), big.mark = ","),
    " records to their exact state and ", 
    format(sum(bpoints_match$matchType == "neighbour", na.rm = TRUE), big.mark = ","), 
    " to an adjacent state.\n", 
    "We failed to match ",
    format(sum(bpoints_match$matchType == "noMatch", na.rm = TRUE), big.mark = ","), 
    " occurrences to any 'exact' or 'neighbouring' state.\n",
    "Of the 'exact' matches, ",
    format(sum(bpoints_match$matchCertainty == "Good", na.rm = TRUE), big.mark = ","), 
    " are 'good matches' according to John Ascher and ",
    format(sum(bpoints_match$matchCertainty == "Poor", na.rm = TRUE), big.mark = ","), 
    " are 'poor matches' might benefit from checking"
  ))
  
  bpoints_match <- bpoints_match %>%
    # Select the columns to keep
    dplyr::select(database_id, postal, matchType, matchCertainty) %>%
    dplyr::rename(
      stateMatch = matchType,
      stateMatch_certainty = matchCertainty) %>%
      # Set flag for those that don't pass statematch
    dplyr::mutate(
      .stateOutlier = dplyr::if_else(stateMatch != "exact" & stateMatch != "neighbour",
                                     FALSE, TRUE))
      # Merge with orignal dataset
    output <- occData %>%
      dplyr::left_join(bpoints_match, by = "database_id", multiple = "all", relationship = "many-to-many") %>%
      dplyr::distinct(database_id, .keep_all = TRUE)

    # return message
    message(paste("\nchecklistOutlieR:\nFlagged", 
                  format(sum(output$.stateOutlier == FALSE, na.rm = TRUE), big.mark = ","), 
                  "records.\nThe column, '.stateOutlier',",
                  "was added to the database.\n"), sep = "")
  # Return file
return(output)

} # END checklistOutlieR