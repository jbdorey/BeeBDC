# This script was written by James Dorey to use Paige Chesshire's taxonomy updates to update the 
  # Orr and Ascher combined taxonomy
# For queries, please contact James Dorey at jbdorey@me.com

# The function to integrate Paige's changes
PaigesAcceptance <- function(AccChanges = AcceptedNameChanges,
                             SynFile = SynL_AO){
  
  # Set up an empty dataframe for new rows to go into
  NewRow_df <- tibble::tibble()
  OldRow_df <- tibble::tibble()
  
  #### 1.0 Accepted name changes ####
  # i. make the new name the accepted name AND take the existing accepted name's id
  for(i in 1:nrow(AcceptedNameChanges)){
    # Find the ith name to change
    loopAccName <- AccChanges$DLifeName_s2[i]
    # Find that name in the original list
    SLAO_row <- SynFile %>% 
      dplyr::filter(canonical == loopAccName) %>%
      dplyr::filter(taxonomic_status == "accepted")
      # If there are multiple accepted names, check with the canonical_withFlags column and the
        # Final_name column...
    if(nrow(SLAO_row) > 1){
      SLAO_row <- SynFile %>% 
        dplyr::filter(canonical_withFlags == AccChanges$Final_Name[i])
    } # END multiple SLAO_row s
    # There should be only one row... to make sure of this...
    if(nrow(SLAO_row) == 1){ # Edit the row.
        # Extract the final name to replace the current accepted name
      name2keep <- AccChanges$Final_Name[i]
        # Get genus an species names
      genusIn <- strsplit(name2keep, split = " ")[[1]][1] # Get first word only
      speciesIn <- strsplit(name2keep, split = " ")[[1]][2] # Get LAST word only
      # Build a new row to replace in the dataset
      NewRow <- tibble::tibble(
        flags = SLAO_row$flags,
        taxonomic_status = SLAO_row$taxonomic_status,
        source = SLAO_row$source,
        accid = SLAO_row$accid,  # Get the accepted id for accid
        id = SLAO_row$id,     # Get the new id from the number of rows plus i
        kingdom = SLAO_row$kingdom,
        phylum = SLAO_row$phylum,
        class = SLAO_row$class,
        order = SLAO_row$order,
        family = SLAO_row$family,
        subfamily = SLAO_row$subfamily,
        tribe = SLAO_row$tribe,
        subtribe = SLAO_row$subtribe,
        validName = paste(genusIn, speciesIn, SLAO_row$authorship, sep = " "),
        canonical_withFlags = paste(genusIn, speciesIn, sep = " "),
        canonical = paste(genusIn, speciesIn, sep = " "),
        genus = genusIn,
        subgenus = NA,
        species = speciesIn,
        infraspecies = NA,
        authorship = SLAO_row$authorship,
        taxon_rank = SLAO_row$taxon_rank,
        valid = SLAO_row$valid,
        notes = paste(SLAO_row$notes,"Paige's_accepted_name", sep = ", ") %>%
          gsub(pattern = "NA, ", replacement = "", x = .)
      ) # END NewRow
      # Add NewRow to dataframe
      NewRow_df <- dplyr::bind_rows(NewRow_df, NewRow)
      # Build a new row to replace in the dataset
      OldRow <- tibble::tibble(
        flags = SLAO_row$flags,
        taxonomic_status = "synonym",
        source = "Paige_Chesshire",
        accid = SLAO_row$id,  # Get the accepted id for accid
        id = (max(SynFile$id)+i),     # Get the new id from the number of rows plus i
        kingdom = SLAO_row$kingdom,
        phylum = SLAO_row$phylum,
        class = SLAO_row$class,
        order = SLAO_row$order,
        family = SLAO_row$family,
        subfamily = SLAO_row$subfamily,
        tribe = SLAO_row$tribe,
        subtribe = SLAO_row$subtribe,
        validName = SLAO_row$validName,
        canonical_withFlags = SLAO_row$canonical_withFlags,
        canonical = SLAO_row$canonical,
        genus = SLAO_row$genus,
        subgenus = SLAO_row$subgenus,
        species = SLAO_row$species,
        infraspecies = SLAO_row$infraspecies,
        authorship = SLAO_row$authorship,
        taxon_rank = SLAO_row$taxon_rank,
        valid = SLAO_row$valid,
        notes = paste(SLAO_row$notes,"Paige's_synonym", sep = ", ") %>%
          gsub(pattern = "NA, ", replacement = "", x = .)
      ) # END OldRow
        # Add existing row to the OldRow_df
      OldRow_df <- dplyr::bind_rows(OldRow_df, OldRow)
    }else{
      stop(writeLines(paste("Oh no! There are ", nrow(SLAO_row),
                            " SLAO_row. There should only be one!",
                            " Please check the species: ", loopAccName,
                            )))
    } # END ERROR if no match
  } # END 1.0 loop
  
  # Now, remove the accepted names from the original dataset
  SynL_AO <- SynL_AO %>%
    # remove rows
    dplyr::filter(!id %in% NewRow_df$id) %>%
    # ADD in new rows
    dplyr::bind_rows(NewRow_df, OldRow_df) %>%
    # ARRANGE by id number
    dplyr::arrange(id)
  # ii. assign a new id to the *new* synonym and assign accid to new accepted name change details
  # to synonym details


  
} # END function

