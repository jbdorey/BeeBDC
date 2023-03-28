  # This function was written by James Dorey to remove duplicates from the combined Orr and Ascher
    # synonym lists. For questions, please email jbdorey@me.com
# This function was started on 17th May 2022 and last updated 17th May 2022


taxoDuplicator <- function(
    SynList = NULL){
  
    # Load required packages
  require(dplyr)
  require(tibble)
  
  #### 0.0 Prep ####
  # Look for duplicated names in the DiscoverLife subset of data
  duplicates <- SynList %>% 
    #dplyr::filter(source == "DiscoverLife") %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(n() > 1)
  # User output
  writeLines(paste(" — ", format(nrow(duplicates), big.mark = ","),
                                 " duplicates found in the data.", sep = ""))
  
    # Build subsetted datasets to examine 
  DLaccepted <- duplicates %>% dplyr::filter(accid == 0 & source == "DiscoverLife")
  OrrAccepted <- duplicates %>% dplyr::filter(accid == 0 & source == "Orr_et_al_2021_CurrBiol")
  DLsynonyms <- duplicates %>% dplyr::filter(accid != 0 & source == "DiscoverLife")
  OrrSyns <- duplicates %>% dplyr::filter(accid != 0 & source == "Orr_et_al_2021_CurrBiol")
  
  #### 1.0 DL_Orr #### 
    ##### 1.1 acc. names ####
    # Find all duplicated valid names that occur in the Orr list and the Ascher list. This looks like 
      # All of them! These will later be REMOVED.
  OrrAcc2remove <- OrrAccepted %>%
    dplyr::filter(validName %in% DLaccepted$validName)
  # Do any of the accids in the full list match these names' ids?
  DLIDmatch <- OrrAcc2remove %>%
    dplyr::filter(id %in% SynList$accid)
  # Stop here becuase I have no matches, but this might be important down the track if someone finds them!
  if(nrow(DLIDmatch) > 0 ){
    return(DLIDmatch)
    stop(paste(" — That's odd! There is an Orr accepted name that is referred to by another name.",
               "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
               "I have returned the list of offending names."))
  }
  # For now, because these are all duplicates, I will not return these data.
  
    ##### 1.2 synonyms ####
    # Find all duplicated SYNONYMS names that occur in the Orr list and the Ascher list. 
  OrrDupeSyns <- OrrSyns %>%
    dplyr::filter(validName %in% DLsynonyms$validName)
    # Do any of the accids in the full list match these names' ids?
  OrrIDmatch <- OrrDupeSyns %>%
    dplyr::filter(id %in% SynList$accid)
    # Stop here becuase I have no matches, but this might be important down the track if someone finds them!
  if(nrow(OrrIDmatch) > 0 ){
    return(OrrIDmatch)
    stop(paste(" — That's odd! There is an Orr synonym that is referred to as an accepted name.",
               "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
               "I have returned the list of offending names."))
  } 
    # Which names in the Orr list can we keep as unique synonyms?
  OrrUnique <- OrrSyns %>%
    dplyr::filter(!validName %in% DLsynonyms$validName)
      # Pass these names onto 3.0

  
  #### 2.0 DL duplicates ####
    ##### 2.1 acc. names ####
    # Look for internal DiscoverLife duplicated ACCEPTED names
  DLduplicates <- DLaccepted %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(n() > 1)
  # Stop here becuase I have no matches, but this might be important down the track if someone finds them!
  if(nrow(DLduplicates) > 0 ){
    return(DLduplicates)
    stop(paste(" — That's odd! There is an internal DiscoverLife synonym.",
               "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
               "I have returned the list of offending names."))
  }
  # Because none of these are duplicates, I will KEEP the original dataset DLaccepted.
  
    ##### 2.2 valName synonyms ####
  # Look for internal DiscoverLife duplicated SYNONYMS
  DLduplicateSyns <- DLsynonyms %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(n() > 1)
  
 DLdupes_nest <- DLduplicateSyns %>%
    # ungroup but nest the data by valid name instead
   dplyr::ungroup() %>%
   dplyr::nest_by(validName) 
 
 ###### a. DL loop ####
 # Set up empty dataframes for loop
 ambiSyns <- tibble::tibble()
 nonAmbiSyns <- tibble::tibble()
    # Run a loop to examine each duplicate pair in the list
 for(i in 1:nrow(DLdupes_nest)){
   # Get the first tibble
   LoopTibble <- DLdupes_nest$data[[i]] %>% 
     # add the validName column back in to each row
   tibble::add_column(validName = DLdupes_nest$validName[[i]], .after = "subtribe")
   
       # FOR n == 2
       if(nrow(LoopTibble) == 2){
        # LOGICAL both duplicates match to the same accid
         logiTest <- all(duplicated(LoopTibble$accid) | duplicated(LoopTibble$accid, fromLast = TRUE))
          # IF the duplicates match the same accid (accepted name) — NON-ambiguous
         if(logiTest == TRUE){
           nonAmbiSyns <- nonAmbiSyns %>% dplyr::bind_rows(LoopTibble)
         } # END TRUE
           # IF the duplicates match different accid (accepted name) — AMBIGUOUS
         if(logiTest == FALSE){
           ambiSyns <- ambiSyns %>% dplyr::bind_rows(LoopTibble)
         } # END FALSE
       }# END n == 2
   
        # FOR n > 2
      if(nrow(LoopTibble) > 2){
          # Find non-ambiguous duplicates
        nrow_nonAmbi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(n() > 1) %>%
          nrow()
          # Find ambiguous duplicates
        nrow_Ambi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(n() == 1) %>%
          nrow()
          # IF ALL of these rows have the same accid, then they are just regular synonym duplicates
        if(nrow_nonAmbi == nrow(LoopTibble)){
          # Add the lowest id number to the nonAmbiSyns tibble
          LoopTibble <- LoopTibble %>% dplyr::arrange(id)
          nonAmbiSyns <- nonAmbiSyns %>% 
            dplyr::bind_rows(dplyr::filter(LoopTibble, row_number() == 1))
        }else{ # ALL of the others have been ambiguous so far
            # Add these data to the ambiSyns dataframe
          ambiSyns <- ambiSyns %>% 
            dplyr::bind_rows(LoopTibble)
        } # END else
      } # END n > 2
 } # END Ambiguous loop
    ###### b. loop_clean ####
    # Take only one of each non-ambiguous synonyms
 nonAmbiSyns_deDuped <- nonAmbiSyns %>%
   dplyr::group_by(validName) %>%
   dplyr::filter(row_number() == 1)
    # For ambiguous accids, add this to the flags
 ambiSyns$flags <- "ambiguous validName"
    ###### c. merge ####
  # Merge this back to the DLsynonyms data. This will have duplicates removed and 
    # internally-ambiguous names flagged.
 DLsynonyms <- DLsynonyms %>%
    # REMOVE the duplicated valid names
   dplyr::filter(!validName %in% DLduplicateSyns$validName) %>%
    # ADD the cleaned rows back into the dataset
   dplyr::bind_rows(nonAmbiSyns_deDuped, ambiSyns) 
  # KEEP DLsynonyms
 
 
  #### 3.0 Orr duplicates ####
    # Look for internal DiscoverLife duplicates
  OrrDuplicates <- OrrUnique %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(n() > 1)
    # Yep, there are Orr synonym duplicates to deal with!
  # Do any of the accids in the full list match these names' ids?
  OrrIDmatches <- OrrDuplicates %>%
    dplyr::filter(id %in% SynList$accid)
       # Stop here becuase I have no matches, but this might be important down the track if someone finds them!
       if(nrow(OrrIDmatches) > 0 ){
         return(OrrIDmatches)
         stop(paste(" — That's odd! There are accids matching to Orr synonym IDs.",
                    "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
                    "I have returned the list of offending names. You're welcome."))
       }
      # Take only the lowest id number match
  OrrOriginals <- OrrDuplicates %>% 
      # Sort by id number
    dplyr::arrange(id) %>%
      # Filter out ANY duplicated rows for validName
    dplyr::group_by(validName) %>%
      # take the first row
    dplyr::filter(row_number() == 1)
    # KEEP OrrOriginals
  
  
  #### 4.0 Merge ####
  dupeMerge <- dplyr::bind_rows(DLaccepted, OrrOriginals, DLsynonyms) %>%
      # sort again by id
    dplyr::arrange(id)
  
    # Check to make sure that all ids are unique
  UniqueIDcheck <- dupeMerge %>%
    dplyr::arrange(id) %>%
    dplyr::filter(!duplicated(id))
  # FIRST, for now remove ambiguous names
  ambi_VNcheck <- UniqueIDcheck %>% dplyr::filter(flags %in% "ambiguous validName")
  NonAmbi_VNcheck <- UniqueIDcheck %>% dplyr::filter(!flags %in% "ambiguous validName")
    # Check to make sure that all validNames are unique
  UniqueVNcheck <- NonAmbi_VNcheck %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(n() > 1)
  
     ##### 4.1 ValSyn_clean ####
      # look for matches between DiscoverLife accepted names and Orr synonyms
      # At present, all of these represent an accepted Ascher name with a contradictory Orr name.
        # Keep the Ascher name but warn the user if this changes...
     dupes2remove_UnVNcheck <- UniqueVNcheck %>%
      dplyr::group_by(validName) %>%
      dplyr::filter(accid != 0)
      # Stop if this is not half of the original (not all correspond to an Orr Syn)
    if(nrow(dupes2remove_UnVNcheck) != (nrow(UniqueVNcheck)/2)){
      stop(paste(" — This is new! There is a problem at 4.1 ValSyn_clean. Please go and have a look.",
                 "\n", "Good luck, LOL."))
    }
      # Remove dupes2remove_UnVNcheck (duplicates) from the list to return, and then add the new names.
  dupes2keep <- dupeMerge %>%
      # Remove duplicates...
    dplyr::filter(!id %in% dupes2remove_UnVNcheck$id) 
  
  # Merge these with the original dataset
  deDuplicated <- SynList %>% 
    # FIRST, remove all of the original duplicate rows
    dplyr::filter(!validName %in% duplicates$validName) %>% 
    # Add in the duplicates we want to keep
    dplyr::bind_rows(dupes2keep)

    ##### 4.2 Duplicate ids ####
    # There might be some rows with duplicate ids. These are now unique validNames. Assign these all NEW ids
  dupID <- deDuplicated %>%
    dplyr::filter(duplicated(id) | duplicated(id, fromLast = TRUE))
    # IF so, remove them
  if(nrow(dupID) > 0){
    # Remove these from the original dataset
  deDuplicated <- deDuplicated %>%
    dplyr::filter(!id %in% unique(dupID$id))
  # Replace the ids with new ones starting from +1 the max id number already existing
    # find the largest id...
      SeqStart <- max(deDuplicated$id)+1
      SeqEnd <- as.numeric(SeqStart+nrow(dupID))-1
    dupID$id <- seq(from = SeqStart, to = SeqEnd, by = 1)
  # re-merge
    deDuplicated <- deDuplicated %>%
      dplyr::bind_rows(dupID)
  } # END dupID
  
  ##### 4.3 Ambi accepted ####
    # Some ambiguous names are accepted names. Therefore, I will remove the associated ambiguous synonyms
    ambiAcc <- deDuplicated %>%
        # Find the duplicate names
      dplyr::filter(duplicated(validName)|duplicated(validName, fromLast = TRUE)) %>% 
        # Find the accepted names
      dplyr::filter(taxonomic_status == "accepted")
      # Get the number of accepted-assocaited ambiguous names that were removed.
    ambiAccCount <- nrow(dplyr::filter(deDuplicated, validName %in% ambiAcc$validName)) - nrow(ambiAcc)
        # REMOVE those names from the whole dataset
    deDuplicated <- deDuplicated %>% 
        # remove
      dplyr::filter(!validName %in% ambiAcc$validName) %>%
        # rejoin those accepted name rows
      dplyr::bind_rows(ambiAcc)
    
    #### 5.0 Final Ambi ####
      ##### 5.1 can_wFl synonyms ####
    # Look for internal DiscoverLife duplicated SYNONYMS
    DLduplicateSyns_51 <- deDuplicated %>%
      dplyr::group_by(canonical_withFlags) %>%
      dplyr::filter(n() > 1) 

    DLdupes_nest <- DLduplicateSyns_51 %>%
      # ungroup but nest the data by valid name instead
      dplyr::ungroup() %>%
      dplyr::nest_by(canonical_withFlags) 
    
    ###### a. DL loop ####
    # Set up empty dataframes for loop
    ambiSyns_51 <- tibble::tibble()
    nonAmbiSyns_51 <- tibble::tibble()
    # IF DLduplicateSyns_51 is EMPTy, do not run.
    if(nrow(DLduplicateSyns_51) > 0){
      # Run a loop to examine each duplicate pair in the list
      for(i in 1:nrow(DLdupes_nest)){
        # Get the first tibble
        LoopTibble <- DLdupes_nest$data[[i]] %>% 
          # add the canonical_withFlags column back in to each row
          tibble::add_column(canonical_withFlags = DLdupes_nest$canonical_withFlags[[i]], .after = "canonical")
        
        # FOR n == 2
        if(nrow(LoopTibble) == 2){
          # LOGICAL both duplicates match to the same accid
          logiTest <- all(duplicated(LoopTibble$accid) | duplicated(LoopTibble$accid, fromLast = TRUE))
          # IF the duplicates match the same accid (accepted name) — NON-ambiguous
          if(logiTest == TRUE){
            nonAmbiSyns_51 <- nonAmbiSyns_51 %>% dplyr::bind_rows(LoopTibble)
          } # END TRUE
          # IF the duplicates match different accid (accepted name) — AMBIGUOUS
          if(logiTest == FALSE){
              # If one of these matches the other, they are NOT ambiguous.
            accTEST <- any(LoopTibble$id %in% LoopTibble$accid)
                 if(accTEST == FALSE){
               ambiSyns_51 <- ambiSyns_51 %>% dplyr::bind_rows(LoopTibble)
                 }
          } # END FALSE
        }# END n == 2
        
        # FOR n > 2
        if(nrow(LoopTibble) > 2){
          # Find non-ambiguous duplicates
          nrow_nonAmbi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(n() > 1) %>%
            nrow()
          # Find ambiguous duplicates
          nrow_Ambi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(n() == 1) %>%
            nrow()
          # IF ALL of these rows have the same accid, then they are just regular synonym duplicates
          if(nrow_nonAmbi == nrow(LoopTibble)){
            #  # Add the lowest id number to the nonAmbiSyns_51 tibble
            ambiSyns_51 <- ambiSyns_51 %>% 
              dplyr::bind_rows(LoopTibble)
          }else{ # ALL of the others have been ambiguous so far
            # Logical — if ALL but one accid matches an id, take the to mean they are all pointing at
            # the same record. None shold match for now.
            accTest <- sum(LoopTibble$id %in% LoopTibble$accid) == nrow(LoopTibble)-1
               # Add these data to the ambiSyns_51 dataframe
               if(accTest == FALSE){ # Ad all as synonyms
               ambiSyns_51 <- ambiSyns_51 %>% 
                 dplyr::bind_rows(LoopTibble)
               }else(
                 stop(" — unique problem at 5.1. :(")
               )
          } # END else
        } # END n > 2
      } # END Ambiguous loop
    }else{
      ambiSyns_51 = tibble::tibble()
      nonAmbiSyns_51 = tibble::tibble()
    } # END big IF
    ###### b. loop_clean ####
      # NON-AMBIGUOUS
    if(nrow(nonAmbiSyns_51) > 0){
      nonAmbiSyns_51_nAmb <- nonAmbiSyns_51  %>%
        # Filter for ONLY the names that AREN'T already flagged as ambiguous
        dplyr::filter(!flags %in% c("ambiguous validName")) 
      # For ambiguous accids, add this to the flags
      nonAmbiSyns_51_nAmb$flags <- paste(nonAmbiSyns_51_nAmb$flags, "non-ambiguous can_wFlags", sep = ", ") %>%
        # REMOVE EMPTYS
        stringr::str_replace(pattern = "NA, ", "")
      
      # internally-ambiguous names flagged.
      deDuplicated_51 <- deDuplicated %>%
          # REMOVE the matching ids
        dplyr::filter(!id %in% nonAmbiSyns_51_nAmb$id) %>%
         # ADD the new rows
        dplyr::bind_rows(nonAmbiSyns_51_nAmb) 
    } else{
        # If not, pass this new name onto the next section
      deDuplicated_51 <- deDuplicated
    }
    
      # AMBIGUOUS
    if(nrow(ambiSyns_51) > 0){
      ambiSyns_51_NavN <- ambiSyns_51 %>%
        # Filter for ONLY the names that AREN'T already flagged as ambiguous
        dplyr::filter(!flags %in% c("ambiguous validName")) 
      # For ambiguous accids, add this to the flags
      ambiSyns_51_NavN$flags <- paste(ambiSyns_51_NavN$flags, "ambiguous can_wFlags", sep = ", ") %>%
          # REMOVE EMPTYS
        stringr::str_replace(pattern = "NA, ", "")
        # Filter the VALID ambiguities and ADD to the wflags
      ambiSyns_51_all <- ambiSyns_51 %>% 
          # filter
        dplyr::filter(!id %in% ambiSyns_51_NavN$id) %>%
          # add
        dplyr::bind_rows(ambiSyns_51_NavN)
      # Merge this back to the deDuplicated data. This will have duplicates removed and 
      # internally-ambiguous names flagged.
      deDuplicated_51 <- deDuplicated_51 %>%
        # REMOVE the duplicated valid names
        dplyr::filter(!canonical_withFlags %in% ambiSyns_51_all$canonical_withFlags) %>%
        # ADD the cleaned rows back into the dataset
        dplyr::bind_rows(ambiSyns_51_all) 
    }else{
      ambiSyns_51 = tibble::tibble()
      # If not, pass this new name onto the next section
      deDuplicated_51 <- deDuplicated
    } # END ambiSyns_51 IF
    


    ##### 5.2 canon synonyms ####
    # Look for internal DiscoverLife duplicated SYNONYMS
    DLduplicateSyns_52 <- deDuplicated_51 %>%
      dplyr::group_by(canonical) %>%
      dplyr::filter(n() > 1) 
    
    DLdupes_nest <- DLduplicateSyns_52 %>%
      # ungroup but nest the data by valid name instead
      dplyr::ungroup() %>%
      dplyr::nest_by(canonical) 
    
    ###### a. DL loop ####
    # Set up empty dataframes for loop
    ambiSyns_52 <- tibble::tibble()
    nonAmbiSyns_52 <- tibble::tibble()
    # IF DLduplicateSyns_52 is EMPTy, do not run.
    if(nrow(DLduplicateSyns_52) > 0){
      # Run a loop to examine each duplicate pair in the list
      for(i in 1:nrow(DLdupes_nest)){
        # Get the first tibble
        LoopTibble <- DLdupes_nest$data[[i]] %>% 
          # add the canonical column back in to each row
          tibble::add_column(canonical = DLdupes_nest$canonical[[i]], .after = "validName")
        
        # FOR n == 2
        if(nrow(LoopTibble) == 2){
          # LOGICAL both duplicates match to the same accid
          logiTest <- all(duplicated(LoopTibble$accid) | duplicated(LoopTibble$accid, fromLast = TRUE))
          # IF the duplicates match the same accid (accepted name) — NON-ambiguous
          if(logiTest == TRUE){
            nonAmbiSyns_52 <- nonAmbiSyns_52 %>% dplyr::bind_rows(LoopTibble)
          } # END TRUE
          # IF the duplicates match different accid (accepted name) — AMBIGUOUS
          if(logiTest == FALSE){
            # If one of these matches the other, they are NOT ambiguous.
            accTEST <- any(LoopTibble$id %in% LoopTibble$accid)
            if(accTEST == FALSE){
              ambiSyns_52 <- ambiSyns_52 %>% dplyr::bind_rows(LoopTibble)
            }
          } # END FALSE
        }# END n == 2
        
        # FOR n > 2
        if(nrow(LoopTibble) > 2){
          # Find non-ambiguous duplicates
          nrow_nonAmbi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(n() > 1) %>%
            nrow()
          # Find ambiguous duplicates
          nrow_Ambi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(n() == 1) %>%
            nrow()
          # IF ALL of these rows have the same accid, then they are just regular synonym duplicates
          if(nrow_nonAmbi == nrow(LoopTibble)){
            #  # Add the lowest id number to the nonAmbiSyns_52 tibble
            ambiSyns_52 <- ambiSyns_52 %>% 
              dplyr::bind_rows(LoopTibble)
          }else{ # ALL of the others have been ambiguous so far
            # Logical — if ALL but one accid matches an id, take the to mean they are all pointing at
            # the same record. None shold match for now.
            accTest <- sum(LoopTibble$id %in% LoopTibble$accid) == nrow(LoopTibble)-1
            # Add these data to the ambiSyns_52 dataframe
            if(accTest == FALSE){ # Ad all as synonyms
              ambiSyns_52 <- ambiSyns_52 %>% 
                dplyr::bind_rows(LoopTibble)
            }else(
              stop(" — unique problem at 5.2! :(")
            )
          } # END else
        } # END n > 2
      } # END Ambiguous loop
    }else{
      ambiSyns_52 = tibble::tibble()
      nonAmbiSyns_52 = tibble::tibble()
    } # END big IF
    ###### b. loop_clean ####
    # NON-AMBIGUOUS
    if(nrow(nonAmbiSyns_52) > 0){
      # Take only one of each non-ambiguous synonyms
      nonAmbiSyns_deDuped_52 <- nonAmbiSyns_52 %>%
        dplyr::group_by(validName) %>%
        dplyr::filter(row_number() == 1)
    }
    
    # NON-AMBIGUOUS
    if(nrow(nonAmbiSyns_52) > 0){
      nonAmbiSyns_52_nAmb <- nonAmbiSyns_52  %>%
        # Filter for ONLY the names that AREN'T already flagged as ambiguous
        dplyr::filter(!flags %in% c("ambiguous validName", "ambiguous can_wFlags", 
                                    "non-ambiguous can_wFlags")) 
      # For ambiguous accids, add this to the flags
      nonAmbiSyns_52_nAmb$flags <- paste(nonAmbiSyns_52_nAmb$flags, "non-ambiguous canonical", sep = ", ") %>%
        # REMOVE EMPTYS
        stringr::str_replace(pattern = "NA, ", "")
      
      # internally-ambiguous names flagged.
      deDuplicated_52 <- deDuplicated_51 %>%
        # REMOVE the matching ids
        dplyr::filter(!id %in% nonAmbiSyns_52_nAmb$id) %>%
        # ADD the new rows
        dplyr::bind_rows(nonAmbiSyns_52_nAmb) 
    } else{
      # If not, pass this new name onto the next section
      deDuplicated_52 <- deDuplicated_51
    }
    
    # AMBIGUOUS
    if(nrow(ambiSyns_52) > 0){
      ambiSyns_52_NavN <- ambiSyns_52 %>%
        # Filter for ONLY the names that AREN'T already flagged as ambiguous
        dplyr::filter(!flags %in% c("ambiguous validName", "ambiguous can_wFlags")) 
      # For ambiguous accids, add this to the flags
      ambiSyns_52_NavN$flags <- paste(ambiSyns_52_NavN$flags, "ambiguous canonical", sep = ", ") %>%
        # REMOVE EMPTYS
        stringr::str_replace(pattern = "NA, ", "")
      # Filter the VALID ambiguities and ADD to the wflags
      ambiSyns_52_all <- ambiSyns_52 %>% 
        # filter
        dplyr::filter(!id %in% ambiSyns_52_NavN$id) %>%
        # add
        dplyr::bind_rows(ambiSyns_52_NavN)
      # Merge this back to the deDuplicated data. This will have duplicates removed and 
      # internally-ambiguous names flagged.
      deDuplicated_52 <- deDuplicated_52 %>%
        # REMOVE the duplicated valid names
        dplyr::filter(!canonical %in% ambiSyns_52_all$canonical) %>%
        # ADD the cleaned rows back into the dataset
        dplyr::bind_rows(ambiSyns_52_all) 
    }else{
      ambiSyns_52 = tibble::tibble()
    } # END ambiSyns_52 IF
    # KEEP deDuplicated_52
  

    # What an adventure that was!
    # Now, lets try and return some user information 
writeLines(paste(    " — Cleaning complete! From an initial dataset of ", 
                 format(nrow(SynList), big.mark = ","), " names, there ",
                 "remain ", format(nrow(deDuplicated_52), big.mark = ",")," names.",  "\n",
                     " — We removed:", "\n"   ,
                 nrow(DLduplicates), " Ascher accepted names,", "\n"   ,
                 nrow(OrrAcc2remove), " Orr 'accepted' names,", "\n",
                  # 2.2 — synonyms removed
                 format(nrow(nonAmbiSyns)-nrow(nonAmbiSyns_deDuped), big.mark = ","),
                        " Ascher synonyms,", "\n"   ,
                 format(nrow(OrrSyns) - nrow(OrrUnique), big.mark = ",")
                 , " Orr synonyms internally duplicated,", "\n"   ,
                 nrow(OrrDuplicates)-nrow(OrrOriginals), " Orr synonyms duplicated with the Ascher list,", "\n"   ,
                 nrow(dupes2remove_UnVNcheck), " subsequent duplicates after merging,", "\n",
                  # AMBIGUOUS flagged
                    " — We flagged:", "\n"   ,
                 sum(deDuplicated_52$flags %in% "ambiguous validName"), 
                  " ambiguous validName, ", "\n"   ,
                 sum(deDuplicated_52$flags %in% "ambiguous can_wFlags"), 
                  " ambiguous canonical_withFlags names, ", "\n"   ,
                 sum(deDuplicated_52$flags %in% "ambiguous canonical"), 
                 " ambiguous canonical names, ", "\n",
                 sum(deDuplicated_52$flags %in% "non-ambiguous can_wFlags"), 
                 " NON-ambiguous, but duplicated, canonical_withFlags names, ", "\n",
                 sum(deDuplicated_52$flags %in% "non-ambiguous canonical"), 
                 " NON-ambiguous, but duplicated, canonical names, ", "\n",
                     " — We removed: ", "\n",
                 ambiAccCount, " ambiguous synonyms associated with accepted names.", "\n",
                    " — We re-assigned:", "\n"   ,
                 nrow(dupID), " duplicated [non-duplicate] ids",
sep = "")) 
  # Return the cleaned dataset
return(deDuplicated_52)
}
  #### END ####