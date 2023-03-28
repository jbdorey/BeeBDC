# This function was written by James Dorey to harmonise the names of bees using the Ascher-Orr-Chesshire
  # bee taxonomies.
# The function first merges names based on scientificName, then merging the bdc cleaned_name and
  # scientificNameAuthorship and matching those, followed by matching to canoncial with flags, then canonical. 
  # In all of these cases, names that are ambiguous at that level are removed so that only confident
  # matches are maintaned. 
# This function was written between the 18th and 20th of May 2022. For questions, please email James
  # at jbdorey@me.com

      
HarmoniseR <- function(
  path = NULL, #The path to a folder that the output can be saved
  SynList = NULL, # The formatted taxonomy file
  occurrences = NULL
  ) {  # the formatted occurrence data
    # Load required packages 
  require(praise)
  require(rlang)
  require(dplyr)
  require(stringr)
  require(readr)
  require(tidyselect)

  # Make a synonym index list
  writeLines(paste(" — Formatting SynList for matching..."))
    # save the original column names
  OG_colnames <- colnames(occurrences)
    # Save the original number of rows
  OG_rowNum <- nrow(occurrences)
  #### 1.0  _match columns ####
    # Add a new column which has the canonical names matched to the synonyms
  SynMatched <- SynList %>%
    dplyr::left_join(x = ., 
                      # left join ONLY the validName, canonical, and canonical_withFlags
                     y = dplyr::select(SynList, c(id, validName, canonical, canonical_withFlags,
                                                  family, subfamily,
                                                  genus, subgenus, species, infraspecies, authorship)), 
                     by = c("accid" = "id"), suffix = c("", "_valid"))
    # Now, also duplicate the accepted names into the ._matched columns  
  AccMatched <- SynMatched %>% 
      # select only the ACCEPTED NAMES
    dplyr::filter(taxonomic_status == "accepted") %>%
      # duplicate the valid columns into the matched column locations
    dplyr::mutate(validName_valid = validName,
                  canonical_valid = canonical,
                  canonical_withFlags_valid = canonical_withFlags,
                  family_valid = family,
                  subfamily_valid = subfamily,
                  genus_valid = genus, 
                  subgenus_valid = subgenus, 
                  species_valid = species, 
                  infraspecies_valid = infraspecies, 
                  authorship_valid = authorship)
  
    # Merge these datasets
  allMatched <- SynMatched %>%
      # First filter for the reverse of above — SYNONYM NAMES
    dplyr::filter(taxonomic_status == "synonym") %>%
      # combine
    dplyr::bind_rows(AccMatched)
  
  rm(SynMatched, AccMatched)
  

  #### 2.0 Harmonise data ####
  writeLines(paste("\n",
                   " — Harmonise the occurrence data...", sep = ""))
  
    ##### 2.1 Valid Name ####
      ###### a. prep synonyms ####
  # Filter out the AMBIGUOUS validNames prior to matching
  unAm_ValName_Matched <- allMatched %>%
        # Remove amgibuous validNames
    dplyr::filter(!flags %in% c("ambiguous validName"))
  
      ###### b. assign names ####
  # Clean up some illegal characters
  occurrences$scientificName <- occurrences$scientificName %>%
    stringr::str_replace(pattern = "^\"", replacement = "") %>%
    stringr::str_replace(pattern = "\"$", replacement = "")
  
  # Match names first with the validName column
  validName_matchedOccs <- occurrences %>%
    dplyr::left_join( dplyr::select(unAm_ValName_Matched,
                                    id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                    family_valid, subfamily_valid,
                                    canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                    species_valid, infraspecies_valid, authorship_valid),
                        # Match scientific name with the valid synonym name
                      by = c("scientificName" = "validName"),
                      suffix = c("", "_harmon")) 
   #   # Add a column to express the name-match quality — "high" IF there is a match at this point
   # dplyr::mutate(nameQuality = dplyr::if_else(complete.cases(validName_valid),
   #   "high", "NA")) 3,703
  
  ###### c. return Occs ####
  # Return the matched occurrences
  validName_matchedOccs <- validName_matchedOccs %>%
    dplyr::filter(complete.cases(validName_valid)) # 1,927
  

  ##### 2.2 validName_comb ####
    # Now we will try and match the valid name by combining the names_clean and scientificNameAuthorship columns
  ###### a. prep synonyms ####
  # For those that did not match, attempt to match them with the Canonical with flags column...
  # Filter out the AMBIGUOUS validNames prior to matching
    ## SAME as 2.1 ##
  occ_vN_match <- allMatched %>%
    # Remove amgibuous validNames
    dplyr::filter(!flags %in% c("ambiguous validName"))
  
  ###### c. assign names ####
  # Match names first with the validName column
  newCol_vN2 <- occurrences %>%
    # remove already-matched names
    dplyr::filter(!database_id %in% validName_matchedOccs$database_id) %>%
      # Make a new column by combining names_clean and scientificNameAuthorship
    tidyr::unite(col = "united_SciName", names_clean, scientificNameAuthorship, sep = " ",
                 na.rm = TRUE) 
  
  # Match names first with the validName column
  occ_vN_match <- newCol_vN2 %>%
    dplyr::left_join( dplyr::select(occ_vN_match,
                                    id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                    family_valid, subfamily_valid,
                                    canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                    species_valid, infraspecies_valid, authorship_valid),
                      # Match scientific name with the valid synonym name
                      by = c("united_SciName" = "validName"),
                      suffix = c("", "_harmon")) 

  ###### d. return Occs ####
  # Return the matched occurrences
  validName22_matchedOccs <- occ_vN_match %>%
    dplyr::filter(complete.cases(validName_valid)) %>%
      # Bind the previous rows
    dplyr::bind_rows(validName_matchedOccs) # 2,678
    # Remove this spent files
  rm(validName_matchedOccs, occ_vN_match, newCol_vN2)

  
  ##### 2.3 canonical_wFlags ####
      ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
  # Filter out the AMBIGUOUS validNames prior to matching
  occ_cWf_match <- allMatched %>%
    # Remove amgibuous validNames and can_wFlags
    dplyr::filter(!flags %in% c("ambiguous validName","ambiguous can_wFlags")) %>%
      # remove the rows where the canonical and canonical_withFlags match
    dplyr::filter(!canonical == canonical_withFlags)
  
    
    ###### b. assign names ####
  # Match names first with the validName column
  occ_cWf_match <- occurrences %>%
      # remove already-matched names
    dplyr::filter(!database_id %in% validName22_matchedOccs$database_id) %>%
    dplyr::left_join( dplyr::select(occ_cWf_match,
                                    id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                    family_valid, subfamily_valid,
                                    canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                    species_valid, infraspecies_valid, authorship_valid),
                      # Match scientific name with the valid synonym name
                      by = c("species" = "canonical_withFlags"),
                      suffix = c("", "_harmon")) 
  
  ###### c. return Occs ####
  # Return the matched occurrences
  canonwFlags_matchedOccs <- occ_cWf_match %>%
    dplyr::filter(complete.cases(validName_valid)) %>%
      # Bind the previous rows
    dplyr::bind_rows(validName22_matchedOccs)
  # Remove this spent file 
  rm(validName22_matchedOccs, occ_cWf_match)
  

  ##### 2.4 canonical ####
      ###### a. prep synonyms ####
  # For those that did not match, attempt to match them with the Canonical with flags column...
  # Filter out the AMBIGUOUS validNames prior to matching
  occ_can_match <- allMatched %>%
    # Remove amgibuous validNames and can_wFlags
    dplyr::filter(!flags %in% c("ambiguous validName","ambiguous can_wFlags", "ambiguous canonical"))


      ###### b. assign names ####
  # Match names first with the validName column
  occ_can_match <- occurrences %>%
    # Keep the unmatched names
    dplyr::filter(!database_id %in% canonwFlags_matchedOccs$database_id) %>%
    dplyr::left_join( dplyr::select(occ_can_match,
                                    id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                    family_valid, subfamily_valid,
                                    canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                    species_valid, infraspecies_valid, authorship_valid),
                      # Match scientific name with the valid synonym name
                      by = c("names_clean" = "canonical"),
                      suffix = c("", "_harmon")) 
  
    ###### c. return Occs ####
  # Return the matched occurrences
  canon_matchedOccs <- occ_can_match %>%
    dplyr::filter(complete.cases(validName_valid)) %>%
      # Bind the previous rows
    dplyr::bind_rows(canonwFlags_matchedOccs)
  # Remove spent file
  rm(canonwFlags_matchedOccs, occ_can_match, allMatched)
  
  gc()
  
    #### 3.0 Merge ####
  writeLines(" — Formatting merged datasets...")
    # merge datasets
  complete_matched <- canon_matchedOccs %>%
      # Put the scientific name into a new column called verbatim_scientificName
    dplyr::mutate(verbatim_scientificName = scientificName) %>%
      # select the columns we want to keep
    dplyr::select( c(tidyselect::all_of(OG_colnames), validName_valid, verbatim_scientificName,
                     family_valid, subfamily_valid,
                     canonical_withFlags_valid, genus_valid, subgenus_valid, 
                     species_valid, infraspecies_valid, authorship_valid)) %>%
      # REMOVE this column
    dplyr::select(!scientificName) %>%
      # rename validName_valid to scientificName and place it where it used to sit.
    dplyr::mutate(scientificName = validName_valid, .after = database_id) %>%
      # Add in the other taxonomic data
    dplyr::mutate(species = canonical_withFlags_valid,
                  family = family_valid, 
                  subfamily = subfamily_valid,
                  genus = genus_valid,
                  subgenus = subgenus_valid,
                  specificEpithet = species_valid,
                  infraspecificEpithet = infraspecies_valid,
                  scientificNameAuthorship = authorship_valid,
                  .after = scientificName) %>%
    # Remove extra columns
    dplyr::select(!c(canonical_withFlags_valid, family_valid, subfamily_valid, genus_valid,
                     subgenus_valid, species_valid, infraspecies_valid, authorship_valid,
                     validName_valid)) %>%
      # Add the .invalidName columns as TRUE (not flagged)
    dplyr::mutate(.invalidName = TRUE)
  # Remove spent files
  rm(canon_matchedOccs)
  
    ##### 3.1 User output ####
  nMatchedRows <- nrow(complete_matched)
  nUnmatchedRows <- nrow(occurrences) -  nrow(complete_matched)
  
      ###### a. genus-level ####
  # Find the occurences that did not match
  failedMatches <- occurrences %>%
    # Remove the matched names from the OG dataset
    dplyr::filter(!database_id %in% complete_matched$database_id) %>%
    # Add the .invalidName columns as TRUE (not flagged)
    dplyr::mutate(.invalidName = FALSE) 
    # Remove this spent file
  rm(occurrences)
  

    ###### b. Add column ####
  complete_matched <- complete_matched %>%
      # Bind the failed matches
    dplyr::bind_rows(failedMatches) %>%
      # Make sure no duplicates have snuck in
    dplyr::distinct(database_id, .keep_all = TRUE)

  # Cut down the failed list...
  failedMatches <- failedMatches %>%
    dplyr::select(scientificName)


  # Get these strings ready to count
      # failedMatches <-  stringr::str_remove(string = failedMatches$scientificName, 
      #                                      # Clean up a few known endings that might mislead the rough count
      #                                      pattern = paste("sp", "sp.", "spp", "\\([A-Z][a-z]+\\, [0-9]+\\)","[0-9]", 
      #                                                  " [A-Z][a-z]+\\,", "nov.", "nov","\\(Life\\)",
      #                                                  " [A-Z][a-z]+", sep = "|")) %>%
      #   # Clean up wight spaces and return to tibble
      #   stringr::str_trim(side = "both") %>% tibble::tibble()
      # # Count the number of words in each string...
      # failedCounts <- sapply(strsplit(failedMatches$., " "), length)
      # failedCount1 <- length(failedCounts[failedCounts == 1])
  
    ###### c. output ####
  writeLines(paste(
    " — We matched valid names to ", format(nMatchedRows, big.mark = ","), " of ",
    format(OG_rowNum, big.mark = ","), " occurrence records. This leaves a total of ",
    format(nUnmatchedRows, big.mark = ","), " unmatched occurrence records.",
    # " Of the unmatched records, approximately ", format(failedCount1, big.mark = ","), 
    # " are only identified to genus.",
  sep = ""))
  
  writeLines(paste("\nHarmoniseR:"))
  message(paste(format(sum(complete_matched$.invalidName == FALSE), big.mark = ","))) 
  writeLines(paste(
    "records were flagged.\nThe column, '.invalidName' was added to the database.\n"))
  
  message(paste0(
    " — We updated the following columns: scientificName, species, family, subfamily, genus, subgenus, ",
    "specificEpithet, infraspecificEpithet, and scientificNameAuthorship. ",
    "The previous scientificName column was converted to verbatimScientificName"
  ))

    # Return this file
  return(complete_matched)
}
