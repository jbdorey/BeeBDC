  # This function was written by James Dorey on the 16th of June 2022 to join
    # Paige's cleaned dataset as best as possible.

#' Integrate manually-cleaned data from Paige Chesshire
#' 
#' Replaces publicly available data with data that has been manually cleaned and error-corrected for use in
#' the paper Chesshire, P. R., Fischer, E. E., Dowdy, N. J., Griswold, T., Hughes, A. C., Orr, M. J., . . . McCabe, L. M. (In Press). Completeness analysis for over 3000 United States bee species identifies persistent data gaps. Ecography. 
#'
#' @param db_standardized A data frame or tibble. Occurrence records as input.
#' @param PaigeNAm A data frame or tibble. The Paige Chesshire dataset.
#' @param columnStrings A list of character vectors. Each vector is a set of columns that will be 
#' used to iteratively match the public dataset against the Paige dataset.
#'
#' @importFrom dplyr %>%
#' @importFrom stats complete.cases
#'
#' @return Returns db_standardized (input occurrence records) with the Paige Chesshire data integrated.
#' @export
#'
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # set the DataPath to tempdir for this example
#' DataPath <- tempdir()
#' # Integrate Paige Chesshire's cleaned dataset.
# Import Paige's cleaned N. American data
# IF you haven't figured out by now, dont worry about the column name warning - not all columns occur here.
#'PaigeNAm <- readr::read_csv(paste(DataPath, "Paige_data", "NorAmer_highQual_only_ALLfamilies.csv",
#'                                  sep = "/"), col_types = ColTypeR()) %>%
#'  # Change the column name from Source to dataSource to match the rest of the data.
#'  dplyr::rename(dataSource = Source) %>%
#'  # add a NEW database_id column
#'  dplyr::mutate(
#'    database_id = paste0("Paige_data_", 1:nrow(.)),
#'    .before = scientificName)
#'
#'  # Set up the list of character vectors to iteratively check for matches with public data.
#'columnList <- list(
#'  c("decimalLatitude", "decimalLongitude", 
#'    "recordNumber", "recordedBy", "individualCount", "samplingProtocol",
#'    "associatedTaxa", "sex", "catalogNumber", "institutionCode", "otherCatalogNumbers",
#'    "recordId", "occurrenceID", "collectionID"), # Iteration 1
#'  c("catalogNumber", "institutionCode", "otherCatalogNumbers",
#'    "recordId", "occurrenceID", "collectionID"), # Iteration 2
#'  c("decimalLatitude", "decimalLongitude", 
#'    "recordedBy", "genus", "specificEpithet"), # Iteration 3
#'  c("id", "decimalLatitude", "decimalLongitude"), # Iteration 4
#'  c("recordedBy", "genus", "specificEpithet", "locality"), # Iteration 5
#'  c("recordedBy", "institutionCode", "genus", 
#'    "specificEpithet","locality"),# Iteration 6
#'  c("occurrenceID","decimalLatitude", "decimalLongitude"), # Iteration 7
#'  c("catalogNumber","decimalLatitude", "decimalLongitude"), # Iteration 8
#'  c("catalogNumber", "locality") # Iteration 9
#') 
#'
#'# Merge Paige's data with downloaded data
#'db_standardized <- BeeBDC::PaigeIntegrater(
#'  db_standardized = db_standardized,
#'  PaigeNAm = PaigeNAm,
#'  columnStrings = columnList)
#' }
#' 
#' 
PaigeIntegrater <- function(
    db_standardized = NULL,
    PaigeNAm = NULL,
    columnStrings = NULL){
  
  # locally bind variables to the function
  occurrenceID<-database_id<-database_id_p<-database_id_d<-finalLatitude<-finalLongitude<-
    Dorey_match<-decimalLatitude<-decimalLongitude<-scientificName<-genus<-specificEpithet<-
    infraspecificEpithet<-country<-coordinateUncertaintyInMeters<-decimalLatitude_m<-
    database_id_m<-decimalLongitude_m<-scientificName_m<-genus_m<-specificEpithet_m<-
    infraspecificEpithet_m<-country_m<-coordinateUncertaintyInMeters_m <- . <- NULL
  
  requireNamespace("dplyr")
  

#### 1.0 occurrenceID ####
# Make a temporary dataset
tempData <- db_standardized %>%
  dplyr::filter(stats::complete.cases(occurrenceID))
# Find the matches for occurrenceID
occMatched <- dplyr::tibble(
  Dorey_match = tempData$database_id[cbind(
    match(PaigeNAm$occurrenceID, tempData$occurrenceID )
  )], # Match by occurrenceID
  Paige_match = PaigeNAm$database_id)
# User output
writeLines(paste0(
  " - INITIAL match with occurrenceID only ", 
  format(sum(stats::complete.cases(occMatched$Dorey_match)), big.mark = ","), " of ",
  format(nrow(occMatched), big.mark = ","), " Paige occurrences.\n",
  "There are ", 
  format(nrow(occMatched) - sum(stats::complete.cases(occMatched$Dorey_match)), big.mark = ","),
  " occurrences remaining to match."))
# Save the number remaining
numMatched <- (nrow(occMatched) - sum(stats::complete.cases(occMatched$Dorey_match)))
  # Set matchedPaige to feed into the loop
matchedPaige <- occMatched

#### 2.0 Loop ####
  # loop through the number of column strings
for(i in 1:length(columnStrings)){
  message(paste0(" - Starting iteration ", i))
  # Get the Paige occurrence records that are not matched above
  matchedPaige <- matchedPaige %>%
    dplyr::filter(stats::complete.cases(matchedPaige$Dorey_match))
  unMatchedPaige <- PaigeNAm %>%
    # Remove the already-matched records
    dplyr::filter(!database_id %in% matchedPaige$Paige_match)
  # Select the columns to match by
  colOverlap <- unlist(columnStrings[i])
  # Get a subset of the db_standardized to feed in below
  temp_db <- db_standardized %>%
    # Get distinct data for theabove columns
    dplyr::distinct(dplyr::across(tidyselect::all_of(colOverlap)), 
                    .keep_all = TRUE) %>%
    dplyr::select(c(database_id, tidyselect::all_of(colOverlap))) %>%
      # Remove already-matched occurrences
    dplyr::filter(!database_id %in% matchedPaige$Dorey_match)
  
  # GET THE MATCHED occurrences
  matchedPaige <- unMatchedPaige %>%
    # Merge datasets
    dplyr::left_join(temp_db, by = c(colOverlap),
                     suffix = c("_p", "_d")) %>%
    # Select the id columns
    dplyr::select(database_id_p, database_id_d) %>%
    # Keep ONLY the matched columns
    dplyr::filter(stats::complete.cases(database_id_p)) %>%
    # Rename those columns
    dplyr::rename(Dorey_match = database_id_d, Paige_match = database_id_p) %>%
    # bind with the last lot of matched names
    dplyr::bind_rows(matchedPaige)
  
  # User output
  writeLines(paste0(
    "Matched ",
    format(sum(stats::complete.cases(matchedPaige$Dorey_match)), big.mark = ","), " of ",
    format(nrow(matchedPaige), big.mark = ","), " Paige occurrences.\n",
    "There are ", 
    format(nrow(matchedPaige) - sum(stats::complete.cases(matchedPaige$Dorey_match)), big.mark = ","),
    " occurrences remaining to match.\n",
    "This step has found ", 
    format(
      numMatched - (nrow(matchedPaige) - sum(stats::complete.cases(matchedPaige$Dorey_match))),
    big.mark = ","),
    " extra occurrences from the last iteration."
    ))
      # Update numMatched for next iteration
  numMatched = (nrow(matchedPaige) - sum(stats::complete.cases(matchedPaige$Dorey_match)))
} # END loop

#### 3.0 Append #### 
  # Update the data from Paige
  writeLines(" - Updating Paige datasheet to merge...")
matchedPaige <- PaigeNAm %>%
    # Select the matched records.
  dplyr::filter(database_id %in% matchedPaige$Paige_match) %>%
    # Replace the lat/lon columns
  dplyr::mutate(
    decimalLatitude = finalLatitude,
    decimalLongitude = finalLongitude) %>%
  dplyr::select(!c(finalLatitude, finalLongitude)) %>%
    # Add on the associated Dorey database_id
  dplyr::left_join(matchedPaige, by = c("database_id" = "Paige_match") ) %>%
    # Make sure that all Dorey_match's are unique
  dplyr::distinct(Dorey_match, .keep_all = TRUE)

writeLines(" - Updating the final datasheet with new information from Paige...")
  # Merge the new information
db_standardized <- db_standardized %>%
        # Join select fields of the Paige data
      dplyr::left_join(matchedPaige %>%
                         dplyr::select(., c(Dorey_match, decimalLatitude, decimalLongitude,
                                            scientificName, genus, specificEpithet,
                                            infraspecificEpithet, database_id, country,
                                            coordinateUncertaintyInMeters)),
      by = c("database_id" = "Dorey_match"), suffix = c("", "_m")) %>%
        # Rename those fields to replace existing fields 
  dplyr::mutate(
    decimalLatitude = dplyr::if_else(stats::complete.cases(decimalLatitude_m), decimalLatitude_m,
                                     decimalLatitude),
    database_id = dplyr::if_else(stats::complete.cases(database_id_m), database_id_m,
                                 database_id),
    decimalLongitude = dplyr::if_else(stats::complete.cases(decimalLongitude_m), decimalLongitude_m,
                                      decimalLongitude),
    scientificName = dplyr::if_else(stats::complete.cases(scientificName_m), scientificName_m,
                                    scientificName),
    genus = dplyr::if_else(stats::complete.cases(genus_m), genus_m,
                           genus),
    specificEpithet = dplyr::if_else(stats::complete.cases(specificEpithet_m), specificEpithet_m,
                                     specificEpithet),
    infraspecificEpithet = dplyr::if_else(stats::complete.cases(infraspecificEpithet_m), 
                                          infraspecificEpithet_m,
                                          infraspecificEpithet),
    country = dplyr::if_else(stats::complete.cases(country_m), country_m,
                             country),
    coordinateUncertaintyInMeters = dplyr::if_else(stats::complete.cases(coordinateUncertaintyInMeters_m),
                                                   coordinateUncertaintyInMeters_m,
                                                   coordinateUncertaintyInMeters)) %>%
    # Remove the additional columns
  dplyr::select(!c(decimalLatitude_m, decimalLongitude_m,
                   scientificName_m, genus_m, specificEpithet_m,
                   infraspecificEpithet_m, database_id_m, country_m,
                   coordinateUncertaintyInMeters_m))
    
  # Return the object
return(db_standardized)
} # END function



