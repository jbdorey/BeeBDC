# This function, written by James Dorey merges the Ascher and other datasets with the columns:
  # "Original" and "Correct"
# For queries, please contact James Dorey at jbdorey[at]me.com
# This function was started on 13th May 2022 and last updated 17th May 2022
#' @importFrom dplyr %>%
#' @importFrom stats complete.cases 
#' @importFrom dplyr row_number
#' 
taxoMergeR <- function(currentNames = NULL,
                             newNames = NULL,
                             HigherNameList = NULL,
                       inKingdom = "Animalia",
                       inPhylum = "Arthropoda",
                       inClass = "Insecta",
                       inOrder = "Hymenoptera",
                       outPath = getwd(),
                       fileName = "taxonomy_taxoMergeR",
                       simpleNames = NULL,
                       problemStrings = NULL
                       ){
  # locally bind variables to the function
  Original <- Correct <- accid <- id <- validName <- canonical <- canonical_withFlags <- NULL
  genus <- species <- . <- tempIndex <- authorship <- taxonomic_status <- 
    authorship_nameSplit <- NULL
  
  requireNamespace("dplyr")

  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(simpleNames)){
    stop(" - Please provide an argument for simpleNames. This should be TRUE if using ",
         "'Genus species'",
         " format and FALSE if a more complex format like 'Genus (Subgenus) species Authority'")
  }
  
    #### 1.0 Data prep ####
    ##### 1.1 redundant names ####
  Original_newNames_Count <- nrow(newNames)
  # Remove redundant rows in NEW data (where Original == Correct or != unique())
  newNames <- newNames %>%
    # remove duplicate rows
    dplyr::distinct() %>%
    # Remove rows where Original == Correct
    dplyr::filter(Original != Correct) %>%
    # Remove empty spaces
    dplyr::mutate(Original = Original %>% stringr::str_squish(),
                  Correct = Correct %>% stringr::str_squish())
    # Simply count how many rows there are to output later
  Original_unNew_Count <- nrow(newNames)



  # Make a temporary index
  newNames$tempIndex <- 1:nrow(newNames)

  ##### 1.2 Unique synonyms [new] ####
    # In the newNames file, ONLY keep those synonyms that DO NOT occur in the list already
    ###### a. current names #####
    # Get all of the current names
  nameList <- currentNames %>%
    dplyr::select(accid, id, validName, canonical, canonical_withFlags, genus, species) %>%
    dplyr::mutate(Genus_species = stringr::str_c(dplyr::if_else(!is.na(genus), genus, ""),
                                                 dplyr::if_else(!is.na(species), species, ""),
                                                 sep = " "))
  
  ###### b. filter #### 
    # Filter out names that are already an Ascher synonym or accepted name
    # IF simple
  if(simpleNames == TRUE){
    newNames <- newNames %>%
    dplyr::filter(!Original %in% nameList$Genus_species)
  }
  
    # IF complex
  if(simpleNames == FALSE){
    newNames <- newNames %>%
    dplyr::filter(!Original %in% nameList$validName) %>%
    dplyr::filter(!Original %in% nameList$canonical) %>%
    dplyr::filter(!Original %in% nameList$canonical_withFlags) %>%
    dplyr::filter(!Original %in% nameList$Genus_species) 
  }
  
  ##### 1.3 Name splitting ####
  # Split the Original column into genus and species
  ###### a. simpleNames ####
  # Match simple Genus species format names
  # Do this in-line
    # if(simpleNames == TRUE){
    #   nameSplit <- newNames %>% 
    #     tidyr::separate(Original, into = c("genusNew", "speciesNew"), sep = " ",
    #                     remove = FALSE)}
  ###### b. complexNames ####
  # Remove problemStrings if they exist
  if(is.null(problemStrings) == FALSE){
    # Make a new loop string of names
    loopNames <- newNames$Original
    for(i in 1:length(problemStrings)){
      # Select the ith problemString
      str2remove = problemStrings[i]
      loopNames <- loopNames %>%
        stringr::str_replace_all(string = .,
                                 pattern = str2remove,
                                 replacement = "")
    }# END for problemStrings
    # Squish out extra white spaces
    loopNames <- loopNames %>% stringr::str_squish()
    # Add this as a new column
    newNames$Original_cleaned <- loopNames
  }else{# END if problemStrings
    newNames$Original_cleaned <- newNames$Original
  }
  
  # Match complex names
  if(simpleNames == FALSE){
    # create an empty tibble to populate
    loopTibble <- dplyr::tibble()
    # loop through each name to extract the relevant information
    for(i in 1:nrow(newNames)){
      # Extract the ith original name
      loopName <- newNames$Original_cleaned[i]
      loopSource <- newNames$Source[i]
      # Use nameSplitR to extract the information within
      loopData <- nameSplitR(NameInput = loopName, #NameInput
                             Authority_patterns = NULL) %>% 
        as.data.frame(stringsAsFactors = FALSE, row.names = FALSE) %>% dplyr::tibble() %>%        
        dplyr::mutate(Source = loopSource)
      # Add these data to loopTibble
      loopTibble <- loopTibble %>%
        dplyr::bind_rows(loopData)
    }
    # Sometimes a flag might be called due to a lack of author year. Make sure this does not happen.
    loopTibble$flags <- dplyr::if_else(stringr::str_detect(string = loopTibble$validName,
                                                           pattern = "[0-9]"),
                                       loopTibble$flags,
                                       "")
    # create a temporary tibble to populate
    matchTibble <- dplyr::tibble(
      tempIndex = newNames$tempIndex) %>%
      dplyr::bind_cols(loopTibble)
    ### END loop section
    # Add this information to newNames
    nameSplit <- newNames %>%
      # Remove the columns to be replace
      dplyr::select(!tidyselect::any_of(colnames(loopTibble))) %>%
      # add new info using the tempIndex column
      dplyr::left_join(matchTibble, by = "tempIndex")
  }# END simpleNames == FALSE
  
  
  #### 2.0 Simple Names ####
  if(simpleNames == TRUE){
  ##### 2.1 Acc. match ####
  # find all of the matching columns between New and Ascher using the final and flagged name columns
  Correct_matched <- newNames %>%
    dplyr::inner_join(currentNames, by = c("Correct" = "canonical_withFlags"), keep = TRUE,
                      relationship = "many-to-many")
  ##### 2.2 single match ####
  # subset those names that matched a single Ascher name
  SingleMatch <- Correct_matched %>%
    dplyr::group_by(tempIndex) %>%
    dplyr::filter(dplyr::n() == 1)
  # Split the Original column into genus and species
  suppressWarnings(
    SingleMatch_split <- SingleMatch %>% 
      tidyr::separate(Original, into = c("genusNew", "speciesNew"), sep = " ",
                      remove = FALSE),
    classes = "warning")
  
  
  # Find out if the Original New name (synonym) has a match and authority
  Syn_matched <- newNames %>%
    dplyr::inner_join(currentNames, by = c("Original" = "canonical_withFlags"), keep = TRUE,
                      relationship = "many-to-many") %>%
    # Keep only the current tempIndeces
    dplyr::filter(tempIndex %in% SingleMatch_split$tempIndex)%>%
    # Keep those with a single match
    dplyr::group_by(tempIndex) %>%
    dplyr::filter(dplyr::n() == 1)
  # Attach the authority to the SingleMatch_split tibble
  Single_newMatched <- SingleMatch_split %>%
    dplyr::left_join( dplyr::select(Syn_matched, c(tempIndex, authorship)), by = "tempIndex")
  
  ###### a. accepted ####
  # For those that match accepted names, use the id as the accid
  SOM_acc <- Single_newMatched %>%
    dplyr::filter(taxonomic_status == "accepted")
  
  # Merge these into a single tibble with the correct data 
  SOM_acc_final <- dplyr::bind_cols(
    flags = SOM_acc$flags,
    taxonomic_status = "synonym",
    source = SOM_acc$Source,
    accid = SOM_acc$id,  # Get the ACCEPTED name's id for accid
    id = NA,     # Enter later
    kingdom = inKingdom,
    phylum = inPhylum,
    class = inClass,
    order = inOrder,
    family = SOM_acc$family,
    subfamily = SOM_acc$subfamily,
    tribe = SOM_acc$tribe,
    subtribe = SOM_acc$subtribe,
    validName = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_acc$genusNew) & SOM_acc$genusNew != "NA",
                     SOM_acc$genusNew, ""),
      dplyr::if_else(complete.cases(SOM_acc$authorship.y) & SOM_acc$authorship.y != "NA",
                     SOM_acc$authorship.y, ""),
      sep = " "),
    canonical_withFlags = "NA",
    canonical = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_acc$genusNew) & SOM_acc$genusNew != "NA",
                     SOM_acc$genusNew, ""),
      dplyr::if_else(complete.cases(SOM_acc$genusNew) & SOM_acc$genusNew != "NA",
                     SOM_acc$genusNew, ""),
      dplyr::if_else(complete.cases(SOM_acc$speciesNew) & SOM_acc$speciesNew != "NA",
                     SOM_acc$speciesNew, ""),
      sep = " "),
    genus = SOM_acc$genusNew,
    subgenus = "NA",
    species = SOM_acc$speciesNew,
    infraspecies = "NA",
    authorship = SOM_acc$authorship.y,
    taxon_rank = "species",
    valid = FALSE,
    tempIndex = SOM_acc$tempIndex,
    notes = SOM_acc$notes
  )
  
  ###### b. synonyms ####
  # For those that match SYNONYM names, use the accid as the accid - MATCH the actual accepted name.
  SOM_syn <- Single_newMatched %>% 
    dplyr::filter(taxonomic_status == "synonym")
  # Merge these into a single tibble with the correct data 
  SOM_syn_final <- dplyr::bind_cols(
    flags = SOM_syn$flags,
    taxonomic_status = "synonym",
    source = SOM_syn$Source,
    # Get the accepted id for accid. Hence, find the accepted name from the synonym this matched to
    accid = SOM_syn$accid,  
    id = NA,     # Assign later
    kingdom = inKingdom,
    phylum = inPhylum,
    class = inClass,
    order = inOrder,
    family = SOM_syn$family,
    subfamily = SOM_syn$subfamily,
    tribe = SOM_syn$tribe,
    subtribe = SOM_syn$subtribe,
    validName = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_syn$genusNew) & SOM_syn$genusNew != "NA",
                     SOM_syn$genusNew, ""),
      dplyr::if_else(complete.cases(SOM_syn$speciesNew) & SOM_syn$speciesNew != "NA",
                     SOM_syn$speciesNew, ""),
      dplyr::if_else(complete.cases(SOM_syn$authorship.y) & SOM_syn$authorship.y != "NA",
                     SOM_syn$authorship.y, ""),
      sep = " "),
    canonical_withFlags = "NA",
    canonical = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_syn$genusNew) & SOM_syn$genusNew != "NA",
                     SOM_syn$genusNew, ""),
      dplyr::if_else(complete.cases(SOM_syn$speciesNew) & SOM_syn$speciesNew != "NA",
                     SOM_syn$speciesNew, ""),
      sep = " "),
    genus = SOM_syn$genusNew,
    subgenus = "NA",
    species = SOM_syn$speciesNew,
    infraspecies = "NA",
    authorship = SOM_syn$authorship.y,
    taxon_rank = "species",
    valid = FALSE,
    tempIndex = SOM_syn$tempIndex,
    notes = SOM_syn$notes
  )
  
  
  ##### 2.3 multiple matches ####
  ###### a. take accepted ####
  # subset those names that matched multiple Ascher names
  MultiMatch <- Correct_matched %>%
    # group by the temporary index number
    dplyr::group_by(tempIndex) %>%
    # Find all with more than one
    dplyr::filter(dplyr::n() > 1) %>% 
    # Sort in order of waht you want to keep
    dplyr::arrange(accid,
                   .by_group = TRUE) %>%
    # Filter for the first row only - this will direct to the accepted name hopefully
    dplyr::filter(dplyr::row_number() == 1)
  
  ###### a. find accepted (if needed) ####
  # If any rows do not == 0 (accepted names), then find those names.
  synMatched <- MultiMatch %>% 
    # Get those without an accepted match
    dplyr::filter(accid != 0) 
  # Remove the ascher columns
  synMatched_reduced <- synMatched %>%
    dplyr::select(!colnames(currentNames)) 
  # Re-add the accid
  synMatched_reduced$accid <- synMatched$accid
  synMatched_reduced$authorship_nameSplit <- synMatched$authorship
  # re-combine with the new accepted name's data
  synMatched <- synMatched_reduced %>% 
    dplyr::left_join(., currentNames, by = c("accid" = "id"))
  # These will need to have their accid become id - and changed back in a later step
  synMatched <- synMatched %>%
    dplyr::mutate(id = accid)
  
  ###### c. recombine ####
  MultiMatch <- MultiMatch %>%
    # Remove the old synonym records if they exist
    dplyr::filter(!tempIndex %in% synMatched$tempIndex) %>%
    dplyr::bind_rows(synMatched)
  
  ###### d. authorship ####
  # Give preference to the occurrence's authorship and then combine
  MultiMatch$authorship <- dplyr::if_else(MultiMatch$authorship_nameSplit == "" |
                                            is.na(MultiMatch$authorship_nameSplit),
                                          MultiMatch$authorship,
                                          MultiMatch$authorship_nameSplit )
  
  # Get the accepted names
  Mult_newAcc <- MultiMatch %>%
    dplyr::filter(taxonomic_status == "accepted")
  # Get the synonym names
  Mult_newSyn <- MultiMatch %>%
    dplyr::filter(taxonomic_status == "synonym")
  
  # IF there are synonyms that dont occur in the accepted names, STOP. Because this isn't a 
  # problem for me.
  MULTSynTest <- Mult_newSyn %>%
    dplyr::filter(!Original %in% Mult_newAcc$Original)
  if(nrow(MULTSynTest) > 0){
    stop(paste0("There are multiple-match synonyms that aren't in the mult.accepted list. ",
                "This is new. Look for 'MULTSynTest'"))}
  
  # Merge these into a single tibble with the correct data 
  MO_FINAL <- dplyr::bind_cols(
    flags = Mult_newAcc$flags,
    taxonomic_status = "synonym",
    source = Mult_newAcc$Source,
    accid = Mult_newAcc$id,  # Get the accepted id for accid
    id = NA,     # Get the new id from the number of rows plus i
    kingdom = inKingdom,
    phylum = inPhylum,
    class = inClass,
    order = inOrder,
    family = Mult_newAcc$family,
    subfamily = Mult_newAcc$subfamily,
    tribe = Mult_newAcc$tribe,
    subtribe = Mult_newAcc$subtribe,
    validName = stringr::str_c(
      dplyr::if_else(complete.cases(Mult_newAcc$genus) & Mult_newAcc$genus != "NA",
                     Mult_newAcc$genus, ""),
      dplyr::if_else(complete.cases(Mult_newAcc$species) & Mult_newAcc$species != "NA",
                     Mult_newAcc$species, ""),
      dplyr::if_else(complete.cases(Mult_newAcc$authorship) & Mult_newAcc$authorship != "NA",
                     Mult_newAcc$authorship, ""),
      sep = " "),
    canonical_withFlags = "NA",
    canonical = stringr::str_c(
      dplyr::if_else(complete.cases(Mult_newAcc$genus) & Mult_newAcc$genus != "NA",
                     Mult_newAcc$genus, ""),
      dplyr::if_else(complete.cases(Mult_newAcc$species) & Mult_newAcc$species != "NA",
                     Mult_newAcc$species, ""),
      sep = " "),
    genus = Mult_newAcc$genus,
    subgenus = "NA",
    species = Mult_newAcc$species,
    infraspecies = "NA",
    authorship = Mult_newAcc$authorship,
    taxon_rank = "species",
    valid = FALSE,
    tempIndex = Mult_newAcc$tempIndex
    #notes = SM_GenSp$notes
  )
  } # END simpleNames == TRUE
  #
  
  
  
  #### 3.0 Complex names ####
  if(simpleNames == FALSE){
  ##### 3.1 Acc. match ####
    # find all of the matching columns between New and Ascher using the final and flagged 
      # name columns
  Correct_matched <- nameSplit %>%
      # Match with canonical_withFlags
    dplyr::inner_join(currentNames, by = c("Correct" = "canonical_withFlags"), keep = TRUE,
                      suffix = c("_nameSplit", ""), relationship = "many-to-many")
      # Match with validName for those that FAILED with canonical_withFlags
  Correct_matched2 <- nameSplit %>%
      # remove already matched
    dplyr::filter(!tempIndex %in% Correct_matched$tempIndex) %>%
    dplyr::inner_join(currentNames %>%
                          # Makes ure to sort by acceptednames first
                        dplyr::arrange(accid),
                        , by = c("Correct" = "validName"), keep = TRUE,
                      suffix = c("_nameSplit", ""), 
                        # Only keep the first match (will be the lowest accid)
                      multiple ="first")
  #merge
  Correct_matched <- Correct_matched %>%
    dplyr::bind_rows(Correct_matched2)
    rm(Correct_matched2)
    
    #### 3.2 single match ####
    # subset those names that matched a single Ascher name
  Single_newMatched <- Correct_matched %>%
    dplyr::group_by(tempIndex) %>%
    dplyr::filter(dplyr::n() == 1)
  
        # Give preference to the occurrence's authorship and then combine
  Single_newMatched$authorship <- dplyr::if_else(Single_newMatched$authorship_nameSplit == "" |
                                is.na(Single_newMatched$authorship_nameSplit),
                                Single_newMatched$authorship,
                                Single_newMatched$authorship_nameSplit ) 
       
        ###### a. accepted ####
       # For those that match accepted names, use the id as the accid
       SOM_acc <- Single_newMatched %>%
         dplyr::filter(taxonomic_status == "accepted")
       
    # Merge these into a single tibble with the correct data 
  SOM_acc_final <- dplyr::tibble(
    flags = SOM_acc$flags,
    taxonomic_status = "synonym",
    source = SOM_acc$Source,
    accid = SOM_acc$id,  # Get the ACCEPTED name's id for accid
    id = NA,     # Enter later
    kingdom = inKingdom,
    phylum = inPhylum,
    class = inClass,
    order = inOrder,
    family = SOM_acc$family,
    subfamily = SOM_acc$subfamily,
    tribe = SOM_acc$tribe,
    subtribe = SOM_acc$subtribe,
    validName = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_acc$genus_nameSplit) & SOM_acc$genus_nameSplit != "NA",
                     SOM_acc$genus_nameSplit, ""),
        # subgenus
      dplyr::if_else(complete.cases(SOM_acc$subgenus_nameSplit) & 
                       SOM_acc$subgenus_nameSplit != "NA",
                     paste0("(",SOM_acc$subgenus_nameSplit ,")"), ""),
      dplyr::if_else(complete.cases(SOM_acc$species_nameSplit) & 
                       SOM_acc$species_nameSplit != "NA",
                     SOM_acc$species_nameSplit, ""),
      dplyr::if_else(complete.cases(SOM_acc$infraspecies_nameSplit) & 
                       SOM_acc$infraspecies_nameSplit != "NA",
                     SOM_acc$infraspecies_nameSplit, ""),
      dplyr::if_else(complete.cases(SOM_acc$authorship) & SOM_acc$authorship != "NA",
                     SOM_acc$authorship, ""),
      sep = " "),
    canonical_withFlags = "NA",
    canonical = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_acc$genus_nameSplit) & SOM_acc$genus_nameSplit != "NA",
                     SOM_acc$genus_nameSplit, ""),
      # subgenus
      dplyr::if_else(complete.cases(SOM_acc$subgenus_nameSplit) & 
                       SOM_acc$subgenus_nameSplit != "NA",
                     paste0("(",SOM_acc$subgenus_nameSplit ,")"), ""),
      dplyr::if_else(complete.cases(SOM_acc$species_nameSplit) & SOM_acc$species_nameSplit != "NA",
                     SOM_acc$species_nameSplit, ""),
      dplyr::if_else(complete.cases(SOM_acc$infraspecies_nameSplit) & 
                       SOM_acc$infraspecies_nameSplit != "NA",
                     SOM_acc$infraspecies_nameSplit, ""),
      sep = " "),
    genus = SOM_acc$genus_nameSplit,
    subgenus = dplyr::if_else(complete.cases(SOM_acc$subgenus_nameSplit) & 
                                SOM_acc$subgenus_nameSplit != "NA",
                              SOM_acc$subgenus_nameSplit, ""),
    species = SOM_acc$species_nameSplit,
      # EDIT:
    infraspecies = SOM_acc$infraspecies_nameSplit,
    authorship = SOM_acc$authorship,
      # EDIT:
    taxon_rank = dplyr::if_else(complete.cases(SOM_acc$infraspecies_nameSplit) & 
                                  SOM_acc$infraspecies_nameSplit != "NA",
                                "infraspecies", 
                                dplyr::if_else(complete.cases(SOM_acc$species) & 
                                                 SOM_acc$species != "NA", 
                                               "species",
                                               dplyr::if_else(complete.cases(SOM_acc$genus), 
                                                              "genus", "higher"))),
    valid = FALSE,
    tempIndex = SOM_acc$tempIndex,
    notes = SOM_acc$notes
    )

    ###### b. synonyms ####
  # For those that match SYNONYM names, use the accid as the accid - MATCH the actual accepted name.
  SOM_syn <- Single_newMatched %>% 
    dplyr::filter(taxonomic_status == "synonym")
  # Merge these into a single tibble with the correct data 
  SOM_syn_final <- dplyr::bind_cols(
    flags = SOM_syn$flags,
    taxonomic_status = "synonym",
    source = SOM_syn$Source,
    accid = SOM_syn$accid,  # Get the accepted id for accid. Hence, find the accepted name from 
    # the synonym this matched to
    id = NA,     # Assign later
    kingdom = inKingdom,
    phylum = inPhylum,
    class = inClass,
    order = inOrder,
    family = SOM_syn$family,
    subfamily = SOM_syn$subfamily,
    tribe = SOM_syn$tribe,
    subtribe = SOM_syn$subtribe,
    validName = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_syn$genus_nameSplit) & SOM_syn$genus_nameSplit != "NA",
                     SOM_syn$genus_nameSplit, ""),
      # subgenus
      dplyr::if_else(complete.cases(SOM_syn$subgenus_nameSplit) & 
                       SOM_syn$subgenus_nameSplit != "NA",
                     paste0("(",SOM_syn$subgenus_nameSplit ,")"), ""),
      dplyr::if_else(complete.cases(SOM_syn$species_nameSplit) & SOM_syn$species_nameSplit != "NA",
                     SOM_syn$species_nameSplit, ""),
      dplyr::if_else(complete.cases(SOM_syn$infraspecies_nameSplit) & 
                       SOM_syn$infraspecies_nameSplit != "NA",
                     SOM_syn$infraspecies_nameSplit, ""),
      dplyr::if_else(complete.cases(SOM_syn$authorship) & SOM_syn$authorship != "NA",
                     SOM_syn$authorship, ""),
      sep = " "),    
    canonical_withFlags = "NA",
      # EDIT:
    canonical = stringr::str_c(
      dplyr::if_else(complete.cases(SOM_syn$genus_nameSplit) & SOM_syn$genus_nameSplit != "NA",
                     SOM_syn$genus_nameSplit, ""),
      # subgenus
      dplyr::if_else(complete.cases(SOM_syn$subgenus_nameSplit) & 
                       SOM_syn$subgenus_nameSplit != "NA",
                     paste0("(",SOM_syn$subgenus_nameSplit ,")"), ""),
      dplyr::if_else(complete.cases(SOM_syn$species_nameSplit) & SOM_syn$species_nameSplit != "NA",
                     SOM_syn$species_nameSplit, ""),
      dplyr::if_else(complete.cases(SOM_syn$infraspecies_nameSplit) & 
                       SOM_syn$infraspecies_nameSplit != "NA",
                     SOM_syn$infraspecies_nameSplit, ""),
      sep = " "),
    genus = SOM_syn$genus_nameSplit,
    subgenus = dplyr::if_else(complete.cases(SOM_syn$subgenus_nameSplit) & 
                                SOM_syn$subgenus_nameSplit != "NA",
                              paste0(SOM_syn$subgenus_nameSplit), ""),
    species = SOM_syn$species_nameSplit,
    # EDIT:
    infraspecies = SOM_syn$infraspecies_nameSplit,
    authorship = SOM_syn$authorship,
    # EDIT:
    taxon_rank = dplyr::if_else(complete.cases(SOM_syn$infraspecies_nameSplit) & 
                                  SOM_syn$infraspecies_nameSplit != "NA",
                                "infraspecies", 
                                dplyr::if_else(complete.cases(SOM_syn$species) & 
                                                 SOM_syn$species != "NA",
                                               "species",
                                               dplyr::if_else(complete.cases(SOM_syn$genus) & 
                                                                SOM_syn$genus != "NA",
                                                              "genus", "higher"))),
    valid = FALSE,
    tempIndex = SOM_syn$tempIndex,
    notes = SOM_syn$notes
  )
  
  
    ##### 3.3 multiple matches ####
      ###### a. take accepted ####
    # subset those names that matched multiple Ascher names
  MultiMatch <- Correct_matched %>%
      # group by the temporary index number
    dplyr::group_by(tempIndex) %>%
      # Find all with more than one
    dplyr::filter(dplyr::n() > 1) %>% 
      # Sort in order of waht you want to keep
    dplyr::arrange(accid,
                   .by_group = TRUE) %>%
      # Filter for the first row only - this will direct to the accepted name hopefully
    dplyr::filter(dplyr::row_number() == 1)
  
  ###### a. find accepted (if needed) ####
    # If any rows do not == 0 (accepted names), then find those names.
  synMatched <- MultiMatch %>% 
      # Get those without an accepted match
    dplyr::filter(accid != 0) 
  # Remove the ascher columns
  synMatched_reduced <- synMatched %>%
    dplyr::select(!colnames(currentNames)) 
    # Re-add the accid
  synMatched_reduced$accid <- synMatched$accid
  # re-combine with the new accepted name's data
  synMatched <- synMatched_reduced %>% 
    dplyr::left_join(., currentNames, by = c("accid" = "id"))
    # These will need to have their accid become id - and changed back in a later step
  synMatched <- synMatched %>%
    dplyr::mutate(id = accid)
  
  ###### c. recombine ####
  MultiMatch <- MultiMatch %>%
      # Remove the old synonym records if they exist
    dplyr::filter(!tempIndex %in% synMatched$tempIndex) %>%
    dplyr::bind_rows(synMatched)
  
    ###### d. authorship ####
  # Give preference to the occurrence's authorship and then combine
  MultiMatch$authorship <- dplyr::if_else(MultiMatch$authorship_nameSplit == "" |
                                                   is.na(MultiMatch$authorship_nameSplit),
                                          MultiMatch$authorship,
                                          MultiMatch$authorship_nameSplit )
  # Remove redundant column
  MultiMatch <- MultiMatch %>% 
    dplyr::select(!c(authorship_nameSplit))
  
       
        # Get the accepted names
       Mult_newAcc <- MultiMatch %>%
         dplyr::filter(taxonomic_status == "accepted")
       # Get the synonym names
       Mult_newSyn <- MultiMatch %>%
         dplyr::filter(taxonomic_status == "synonym")
       
        # IF there are synonyms that dont occur in the accepted names, STOP. Because this isn't a 
        # problem for me.
       MULTSynTest <- Mult_newSyn %>%
         dplyr::filter(!Original %in% Mult_newAcc$Original)
       if(nrow(MULTSynTest) > 0){
         stop(paste0("There are multiple-match synonyms that aren't in the mult.accepted list.",
                     " This is new. Look for 'MULTSynTest'"))}
       
  # Merge these into a single tibble with the correct data 
  MO_FINAL <- dplyr::bind_cols(
    flags = Mult_newAcc$flags,
    taxonomic_status = "synonym",
    source = Mult_newAcc$Source,
    accid = Mult_newAcc$id,  # Get the accepted id for accid
    id = NA,     # Get the new id from the number of rows plus i
    kingdom = inKingdom,
    phylum = inPhylum,
    class = inClass,
    order = inOrder,
    family = Mult_newAcc$family,
    subfamily = Mult_newAcc$subfamily,
    tribe = Mult_newAcc$tribe,
    subtribe = Mult_newAcc$subtribe,
    validName = stringr::str_c(
      dplyr::if_else(complete.cases(Mult_newAcc$genus_nameSplit) & 
                       Mult_newAcc$genus_nameSplit != "NA",
                     Mult_newAcc$genus_nameSplit, ""),
        #subgenus
      dplyr::if_else(complete.cases(Mult_newAcc$subgenus_nameSplit) & 
                       Mult_newAcc$subgenus_nameSplit != "NA",
                     paste0("(",Mult_newAcc$subgenus_nameSplit ,")"), ""),
      dplyr::if_else(complete.cases(Mult_newAcc$species_nameSplit) & 
                       Mult_newAcc$species_nameSplit != "NA",
                     Mult_newAcc$species_nameSplit, ""),
      dplyr::if_else(complete.cases(Mult_newAcc$infraspecies_nameSplit) & 
                       Mult_newAcc$infraspecies_nameSplit != "NA",
                     Mult_newAcc$infraspecies_nameSplit, ""),
      dplyr::if_else(complete.cases(Mult_newAcc$authorship) & Mult_newAcc$authorship != "NA",
                     Mult_newAcc$authorship, ""),
      sep = " "),
    canonical_withFlags = "NA",
    canonical = stringr::str_c(
      dplyr::if_else(complete.cases(Mult_newAcc$genus_nameSplit) & 
                       Mult_newAcc$genus_nameSplit != "NA",
                     Mult_newAcc$genus_nameSplit, ""),
      #subgenus
      dplyr::if_else(complete.cases(Mult_newAcc$subgenus_nameSplit) & 
                       Mult_newAcc$subgenus_nameSplit != "NA",
                     paste0("(",Mult_newAcc$subgenus_nameSplit ,")"), ""),
      dplyr::if_else(complete.cases(Mult_newAcc$species_nameSplit) & 
                       Mult_newAcc$species_nameSplit != "NA",
                     Mult_newAcc$species_nameSplit, ""),
      dplyr::if_else(complete.cases(Mult_newAcc$infraspecies_nameSplit) & 
                       Mult_newAcc$infraspecies_nameSplit != "NA",
                     Mult_newAcc$infraspecies_nameSplit, ""),
      sep = " "),
    genus = Mult_newAcc$genus_nameSplit,
    subgenus = dplyr::if_else(complete.cases(Mult_newAcc$subgenus_nameSplit) & 
                                Mult_newAcc$subgenus_nameSplit != "NA",
                              paste0(Mult_newAcc$subgenus_nameSplit), ""),
    species = Mult_newAcc$species_nameSplit,
    # EDIT:
    infraspecies = Mult_newAcc$infraspecies_nameSplit,
    authorship = Mult_newAcc$authorship,
    # EDIT:
    taxon_rank = dplyr::if_else(
      complete.cases(Mult_newAcc$infraspecies_nameSplit) & Mult_newAcc$infraspecies_nameSplit != "NA",
                                "infraspecies", 
                                dplyr::if_else(complete.cases(Mult_newAcc$species) & 
                                                 Mult_newAcc$species != "NA", 
                                               "species",
                                               dplyr::if_else(complete.cases(Mult_newAcc$genus) & 
                                                                Mult_newAcc$genus != "NA", 
                                                              "genus", "higher"))),
    valid = FALSE,
    tempIndex = Mult_newAcc$tempIndex
    #notes = SM_GenSp$notes
  )
  } # END simpleNames == FALSE
  
  #
  
  #### 4.0 Merge ####
    # Merge the output rows
  merged_names <- dplyr::bind_rows(SOM_acc_final, SOM_syn_final, MO_FINAL) 
  
  # Remove " NA" from end of validNames where there was no authority
  merged_names <- merged_names %>%
    dplyr::mutate(validName = stringr::str_replace(validName, 
                                                         pattern = " NA$", 
                                                         replacement = "") %>% 
                          stringr::str_squish())
    
    # Find those rows that did not match
  failed_names <- newNames %>%
    dplyr::filter(!tempIndex %in% merged_names$tempIndex)   
  
  # Select the desired columns
  merged_names <-  merged_names %>%
    dplyr::select(colnames(currentNames))
  # Add id numbers
  idMax <- max(currentNames$id) + 1
  idEnd <- idMax + nrow(merged_names) -1
  merged_names$id <- seq(from = idMax, to = idEnd, by = 1)
  
  # Remove silly names... for some reason I cant '!' inverse select these...
    # Not sure why this isn't filtering in the negative...
 #   toRemove <- newNames %>%
 #     dplyr::filter(!Correct %in% 
 #                     c("extinct", "genus", "Genus", "GENUS", "NIDB", "not_a_species",
 #                       "not a bee", "NOT A BEE", "not a species", "Not_a_bee", "NOT_A_BEE",
 #                       "untraced", "x", "Check"))
 #   
  
    # Write user output
  writeLines(paste(
   " - Names merged. ","\n",
   "We removed ", format(Original_newNames_Count - Original_unNew_Count, big.mark = ","), 
   " duplicate new synonyms ",
   "\n ", "We successfuly matched: ", "\n ",
   format(nrow(SOM_acc_final), big.mark = ","), " new names to the current accepted names;", "\n ",
   format(nrow(SOM_syn_final), big.mark = ","), 
   " new names to the current synonyms, and then their accepted name;",
   "\n ",format(nrow(MO_FINAL), big.mark = ","), 
   " new names that matched the current accepted and synonym names.",
   " These are matched to their accepted names;", "\n   ",
   "!! There were a total of ", format(nrow(failed_names), big.mark = ","), 
   " new names that failed",
   " to find a match in the the current list !!", "\n ",
   "We kept a total of ", format(nrow(merged_names), big.mark = ","), " new synonyms.",
  sep = ""))

  #### 3.0 Clean data ####  
  writeLines(paste(
    " - Cleaning new data...", sep = ""
  ))
  ## manage new data ##
  writeLines(paste(
    " - Adding higher names with the HigherNamer function...", sep = ""
  ))
    # Add higher order names
  merged_names_cl <- HigherNamer(HigherNameList = HigherNameList,
                      InSynList = merged_names)
    # manage flags 
  writeLines(paste(
    " - Managing flags with the FlagManager function...", sep = ""
  ))
  merged_names_cl <- FlagManager(InSynList = merged_names_cl,
                           flagCol = "notes")

  #### 4.0 Save ####
    writeLines(paste(
    " - Saving the matched new component of the data to the file ",
    outPath, "/", fileName, "_", Sys.Date(), ".csv",  " seperately...", sep = ""
  ))
  # Save the  current-matched new dataset
  readr::write_excel_csv(merged_names_cl, file = paste(outPath,"/",
    fileName, "_", Sys.Date(), ".csv", sep = ""))
  writeLines(paste(" - ", nrow(failed_names), 
    " names from the new list did not have an accepted or synonym match to the current list. They ",
    "will be removed. ", "\n",
    "Saving these no-match names to ",
    outPath, "/", fileName, "_", Sys.Date(), ".csv",  " seperately...", sep = ""
  ))
  # Save noMatch_df
  readr::write_excel_csv(failed_names, 
                   file = paste(outPath, "/",
                                fileName, "_failed_", Sys.Date(), ".csv",  sep = ""))

    # Add this dataset to the Ascher dataset
  merged_names_cl <- dplyr::tibble(merged_names_cl)
      # Convert some column types to merge with Ascher data
    merged_names_cl$id <- as.numeric(merged_names_cl$id)
    merged_names_cl$accid <- as.numeric(merged_names_cl$accid)
    merged_names_cl$valid <- as.logical(merged_names_cl$valid)
    
  namesFinal <- currentNames %>%
    dplyr::bind_rows(merged_names_cl)
  # Return this object
  return(namesFinal)
} # END Function
