  # This function was written by James Dorey to remove duplicates from the combined 
  # source2 and source1 — user input, but originally Orr 2021 and Discover Life
    # synonym lists. For questions, please email jbdorey[at]me.com
# This function was started on 17th May 2022 and last updated 17th May 2022
#' @importFrom dplyr %>%
#' @importFrom dplyr row_number

taxoDuplicator <- function(
    SynList = NULL,
    source1 = "DiscoverLife",
    source2 = "Orr_et_al_2021_CurrBiol"){
  # locally bind variables to the function
  validName <- accid <- id <- flags <- taxonomic_status <- canonical_withFlags <- canonical <- NULL
  
    # Load required packages
  requireNamespace("dplyr")
  
  #### 0.0 Prep ####
  ##### 0.1 Remove existing flags ####
  writeLines("Removing previous flags generated with this function")
    # Remove the xisitng flags generated from this function
  SynList <- SynList %>%
    dplyr::mutate(flags = stringr::str_remove_all(flags, "non-ambiguous can_wFlags") %>%
                    stringr::str_remove_all("non-ambiguous canonical") %>%
                    stringr::str_remove_all("ambiguous canonical") %>%
                    stringr::str_remove_all("ambiguous validName") %>%
                    stringr::str_remove_all("ambiguous can_wFlags") %>%
                    stringr::str_remove_all("ambiguous can_wFlags") %>%
                    stringr::str_remove_all("^, $") %>%
                    stringr::str_replace(", , ", ", ") )

  ##### 0.2 Find duplciates ####
  # Look for duplicated names in the DiscoverLife subset of data
  duplicates <- SynList %>% 
    #dplyr::filter(source == "DiscoverLife") %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(dplyr::n() > 1)
  # User output
  writeLines(paste(" - ", format(nrow(duplicates), big.mark = ","),
                                 " duplicates found in the data.", sep = ""))
  
    # Build subsetted datasets to examine 
  S1accepted <- duplicates %>% dplyr::filter(accid == 0 & source %in% source1)
  S2Accepted <- duplicates %>% dplyr::filter(accid == 0 & source %in% source2)
  S1synonyms <- duplicates %>% dplyr::filter(accid != 0 & source %in% source1)
  S2synonyms <- duplicates %>% dplyr::filter(accid != 0 & source %in% source2)
  
  #### 1.0 S1_S2 #### 
    ##### 1.1 acc. names ####
    # Find all duplicated valid names that occur in the source2 list and the source1 list. 
    # This looks  
      # like All of them! These will later be REMOVED.
  S2Acc2remove <- S2Accepted %>%
    dplyr::filter(validName %in% S1accepted$validName)
  # Do any of the accids in the full list match these names' ids?
  S1IDmatch <- S2Acc2remove %>%
    dplyr::filter(id %in% SynList$accid)
  # Stop here becuase I have no matches, but this might be important down the track if someone 
    # finds them!
  if(nrow(S1IDmatch) > 0 ){
    return(S1IDmatch)
    stop(paste(" - That's odd! There is an S2 accepted name that is referred to by another name.",
               "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
               "I have returned the list of offending names."))
  }
  # For now, because these are all duplicates, I will not return these data.
  
    ##### 1.2 synonyms ####
    # Find all duplicated SYNONYMS names that occur in the source2 list and the source1 list. 
  S2DupeSyns <- S2synonyms %>%
    dplyr::filter(validName %in% S1synonyms$validName)
    # Do any of the accids in the full list match these names' ids?
  S2IDmatch <- S2DupeSyns %>%
    dplyr::filter(id %in% SynList$accid)
    # Stop here becuase I have no matches, but this might be important down the track if someone 
    # finds them!
  if(nrow(S2IDmatch) > 0 ){
    return(S2IDmatch)
    stop(paste(" - That's odd! There is an S2 synonym that is referred to as an accepted name.",
               "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
               "I have returned the list of offending names."))
  } 
    # Which names in the source2 list can we keep as unique synonyms?
  S2Unique <- S2synonyms %>%
    dplyr::filter(!validName %in% S1synonyms$validName)
      # Pass these names onto 3.0

  
  #### 2.0 S1 duplicates ####
    ##### 2.1 acc. names ####
    # Look for internal source1 duplicated ACCEPTED names
  S1duplicates <- S1accepted %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(dplyr::n() > 1)
  # Stop here becuase I have no matches, but this might be important down the track if someone 
    # finds them!
  if(nrow(S1duplicates) > 0 ){
    return(S1duplicates)
    stop(paste(" - That's odd! There is an internal S1 synonym.",
               "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
               "I have returned the list of offending names."))
  }
  # Because none of these are duplicates, I will KEEP the original dataset S1accepted.
  
    ##### 2.2 valName synonyms ####
  # Look for internal source1 duplicated SYNONYMS
  S1duplicatesyns <- S1synonyms %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(dplyr::n() > 1)
  
 S1dupes_nest <- S1duplicatesyns %>%
    # ungroup but nest the data by valid name instead
   dplyr::ungroup() %>%
   dplyr::nest_by(validName) 
 
 ###### a. source1 loop ####
 # Set up empty dataframes for loop
 ambiSyns <- dplyr::tibble()
 nonAmbiSyns <- dplyr::tibble()
    # Run a loop to examine each duplicate pair in the list
 if(nrow(S1dupes_nest) > 0){
 for(i in 1:nrow(S1dupes_nest)){
   # Get the first tibble
   LoopTibble <- S1dupes_nest$data[[i]] %>% 
     # add the validName column back in to each row
   dplyr::mutate(validName = S1dupes_nest$validName[[i]], .after = "subtribe")
   
       # FOR n == 2
       if(nrow(LoopTibble) == 2){
        # LOGICAL both duplicates match to the same accid
         logiTest <- all(duplicated(LoopTibble$accid) | duplicated(LoopTibble$accid, fromLast = TRUE))
          # IF the duplicates match the same accid (accepted name) - NON-ambiguous
         if(logiTest == TRUE){
           nonAmbiSyns <- nonAmbiSyns %>% dplyr::bind_rows(LoopTibble)
         } # END TRUE
           # IF the duplicates match different accid (accepted name) - AMBIGUOUS
         if(logiTest == FALSE){
           ambiSyns <- ambiSyns %>% dplyr::bind_rows(LoopTibble)
         } # END FALSE
       }# END n == 2
   
        # FOR n > 2
      if(nrow(LoopTibble) > 2){
          # Find non-ambiguous duplicates
        nrow_nonAmbi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(dplyr::n() > 1) %>%
          nrow()
          # Find ambiguous duplicates
        nrow_Ambi <- LoopTibble %>% dplyr::group_by(accid) %>% dplyr::filter(dplyr::n() == 1) %>%
          nrow()
          # IF ALL of these rows have the same accid, then they are just regular synonym duplicates
        if(nrow_nonAmbi == nrow(LoopTibble)){
          # Add the lowest id number to the nonAmbiSyns tibble
          LoopTibble <- LoopTibble %>% dplyr::arrange(id)
          nonAmbiSyns <- nonAmbiSyns %>% 
            dplyr::bind_rows(dplyr::filter(LoopTibble, dplyr::row_number() == 1))
        }else{ # ALL of the others have been ambiguous so far
            # Add these data to the ambiSyns dataframe
          ambiSyns <- ambiSyns %>% 
            dplyr::bind_rows(LoopTibble)
        } # END else
      } # END n > 2
 } # END Ambiguous loop
} # END length(S1dupes_nest) > 0
    ###### b. loop_clean ####
 if(nrow(nonAmbiSyns) > 0){
    # Take only one of each non-ambiguous synonyms
 nonAmbiSyns_deDuped <- nonAmbiSyns %>%
   dplyr::group_by(validName) %>%
   dplyr::filter(dplyr::row_number() == 1)
    # For ambiguous accids, add this to the flags
 ambiSyns$flags <- "ambiguous validName"
    ###### c. merge ####
  # Merge this back to the S1synonyms data. This will have duplicates removed and 
    # internally-ambiguous names flagged.
 S1synonyms <- S1synonyms %>%
    # REMOVE the duplicated valid names
   dplyr::filter(!validName %in% S1duplicatesyns$validName) %>%
    # ADD the cleaned rows back into the dataset
   dplyr::bind_rows(nonAmbiSyns_deDuped, ambiSyns) }else{
     
      # IF there are no non-ambiguous names then...
     # For ambiguous accids, add this to the flags
     ambiSyns$flags <- "ambiguous validName"
     
     # Merge this back to the S1synonyms data. This will have duplicates removed and 
     # internally-ambiguous names flagged.
     S1synonyms <- S1synonyms %>%
       # REMOVE the duplicated valid names
       dplyr::filter(!validName %in% S1duplicatesyns$validName) %>%
       # ADD the cleaned rows back into the dataset
       dplyr::bind_rows(ambiSyns)
   }
  # KEEP S1synonyms
 
 
  #### 3.0 source2 duplicates ####
    # Look for internal source1 duplicates
  S2Duplicates <- S2Unique %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(dplyr::n() > 1)
    # Yep, there are source2 synonym duplicates to deal with!
  # Do any of the accids in the full list match these names' ids?
  S2IDmatches <- S2Duplicates %>%
    dplyr::filter(id %in% SynList$accid)
       # Stop here because I have no matches, but this might be important down the track if 
    # someone finds them!
       if(nrow(S2IDmatches) > 0 ){
         return(S2IDmatches)
         stop(paste(" - That's odd! There are accids matching to source2 synonym IDs.",
                    "This hasn't happened before, but you'll need to sort it out, chump!", "\n",
                    "I have returned the list of offending names. You're welcome."))
       }
      # Take only the lowest id number match
  S2Originals <- S2Duplicates %>% 
      # Sort by id number
    dplyr::arrange(id) %>%
      # Filter out ANY duplicated rows for validName
    dplyr::group_by(validName) %>%
      # take the first row
    dplyr::filter(dplyr::row_number() == 1)
    # KEEP S2Originals
  
  
  #### 4.0 Merge ####
  dupeMerge <- dplyr::bind_rows(S1accepted, S2Originals, S1synonyms) %>%
      # sort again by id
    dplyr::arrange(id)
  
    # Check to make sure that all ids are unique
  UniqueIDcheck <- dupeMerge %>%
    dplyr::arrange(id) %>%
    dplyr::filter(!duplicated(id))
  # FIRST, for now remove ambiguous names
  ambi_VNcheck <- UniqueIDcheck %>% dplyr::filter(flags %in% "ambiguous validName")
  NonAmbi_VNcheck <- UniqueIDcheck %>% dplyr::filter(!flags %in% "ambiguous validName") %>%
    dplyr::ungroup()
    # Check to make sure that all validNames are unique
  UniqueVNcheck <- NonAmbi_VNcheck %>%
    dplyr::group_by(validName) %>%
    dplyr::filter(dplyr::n() > 1)
  
     ##### 4.1 ValSyn_clean ####
      # look for matches between source1 accepted names and source2 synonyms
      # At present, all of these represent an accepted source1 name with a contradictory source2
    # name.
        # Keep the source1 name but warn the user if this changes...
     dupes2remove_UnVNcheck <- UniqueVNcheck %>%
      dplyr::group_by(validName) %>%
      dplyr::filter(accid != 0)
      # Stop if this is not half of the original (not all correspond to an source2 Syn)
    if(nrow(dupes2remove_UnVNcheck) != (nrow(UniqueVNcheck)/2)){
      stop(paste(" - This is new! There is a problem at 4.1 ValSyn_clean. Please go and have a look.",
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
    # Look for internal source1 duplicated SYNONYMS
    S1duplicatesyns_51 <- deDuplicated %>%
      dplyr::group_by(canonical_withFlags) %>%
      dplyr::filter(canonical_withFlags %>% stringr::str_detect(
        "_"
      )) %>%
      dplyr::filter(dplyr::n() > 1) 

    S1dupes_nest <- S1duplicatesyns_51 %>%
      # ungroup but nest the data by valid name instead
      dplyr::ungroup() %>%
      dplyr::nest_by(canonical_withFlags) 
    
    ###### a. source1 loop ####
    # Set up empty dataframes for loop
    ambiSyns_51 <- dplyr::tibble()
    nonAmbiSyns_51 <- dplyr::tibble()
    # IF S1duplicatesyns_51 is EMPTy, do not run.
    if(nrow(S1duplicatesyns_51) > 0){
      # Run a loop to examine each duplicate pair in the list
      for(i in 1:nrow(S1dupes_nest)){
        # Get the first tibble
        LoopTibble <- S1dupes_nest$data[[i]] %>% 
          # add the canonical_withFlags column back in to each row
          dplyr::mutate(canonical_withFlags = S1dupes_nest$canonical_withFlags[[i]], .after = "canonical")
        
        # FOR n == 2
        if(nrow(LoopTibble) == 2){
          # LOGICAL both duplicates match to the same accid
          logiTest <- all(duplicated(LoopTibble$accid) | duplicated(LoopTibble$accid, fromLast = TRUE))
          # IF the duplicates match the same accid (accepted name) - NON-ambiguous
          if(logiTest == TRUE){
            nonAmbiSyns_51 <- nonAmbiSyns_51 %>% dplyr::bind_rows(LoopTibble)
          } # END TRUE
          # IF the duplicates match different accid (accepted name) - AMBIGUOUS
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
          nrow_nonAmbi <- LoopTibble %>% dplyr::group_by(accid) %>% 
            dplyr::filter(dplyr::n() > 1) %>%
            nrow()
          # Find ambiguous duplicates
          nrow_Ambi <- LoopTibble %>% dplyr::group_by(accid) %>% 
            dplyr::filter(dplyr::n() == 1) %>%
            nrow()
          # IF ALL of these rows have the same accid, then they are just regular synonym duplicates
          if(nrow_nonAmbi == nrow(LoopTibble)){
            #  # Add the lowest id number to the nonAmbiSyns_51 tibble
            ambiSyns_51 <- ambiSyns_51 %>% 
              dplyr::bind_rows(LoopTibble)
          }else{ # ALL of the others have been ambiguous so far
            # Logical - if ALL but one accid matches an id, take the to mean they are all pointing at
            # the same record. None shold match for now.
            accTest <- sum(LoopTibble$id %in% LoopTibble$accid) == nrow(LoopTibble)-1
               # Add these data to the ambiSyns_51 dataframe
               if(accTest == FALSE){ # Ad all as synonyms
               ambiSyns_51 <- ambiSyns_51 %>% 
                 dplyr::bind_rows(LoopTibble)
               }else(
                 stop(" - unique problem at 5.1. :(")
               )
          } # END else
        } # END n > 2
      } # END Ambiguous loop
    }else{
      ambiSyns_51 = dplyr::tibble()
      nonAmbiSyns_51 = dplyr::tibble()
    } # END big IF
    
    ###### b. loop_clean ####
      # NON-AMBIGUOUS — because accid matches
    if(nrow(nonAmbiSyns_51) > 0){
      nonAmbiSyns_51_nAmb <- nonAmbiSyns_51  %>%
        # Filter for ONLY the names that AREN'T already flagged as ambiguous
        dplyr::filter(!flags %in% c("ambiguous validName")) %>%
        dplyr::filter(canonical_withFlags %>% stringr::str_detect("_homonym"))
      # For ambiguous accids, add this to the flags
      nonAmbiSyns_51_nAmb$flags <- paste(nonAmbiSyns_51_nAmb$flags, "non-ambiguous can_wFlags", 
                                         sep = ", ") %>%
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
    
      # AMBIGUOUS 2
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
      ambiSyns_51 = dplyr::tibble()
      # If not, pass this new name onto the next section
      deDuplicated_51 <- deDuplicated
    } # END ambiSyns_51 IF
    


    ##### 5.2 canon synonyms ####
    # Look for internal source1 duplicated SYNONYMS
    S1duplicatesyns_52 <- deDuplicated_51 %>%
      dplyr::group_by(canonical) %>%
      dplyr::filter(dplyr::n() > 1) 
    
    S1dupes_nest <- S1duplicatesyns_52 %>%
      # ungroup but nest the data by valid name instead
      dplyr::ungroup() %>%
      dplyr::nest_by(canonical) 
    
    ###### a. source1 loop ####
    # Set up empty dataframes for loop
    ambiSyns_52 <- dplyr::tibble()
    nonAmbiSyns_52 <- dplyr::tibble()
    # IF S1duplicatesyns_52 is EMPTy, do not run.
    if(nrow(S1duplicatesyns_52) > 0){
      # Run a loop to examine each duplicate pair in the list
      for(i in 1:nrow(S1dupes_nest)){
        # Get the first tibble
        LoopTibble <- S1dupes_nest$data[[i]] %>% 
          # add the canonical column back in to each row
          dplyr::mutate(canonical = S1dupes_nest$canonical[[i]], .after = "validName")
        
        # FOR n == 2
        if(nrow(LoopTibble) == 2){
          # LOGICAL both duplicates match to the same accid
          logiTest <- all(duplicated(LoopTibble$accid) | duplicated(LoopTibble$accid, fromLast = TRUE))
          # IF the duplicates match the same accid (accepted name) - NON-ambiguous
          if(logiTest == TRUE){
            nonAmbiSyns_52 <- nonAmbiSyns_52 %>% dplyr::bind_rows(LoopTibble)
          } # END TRUE
          # IF the duplicates match different accid (accepted name) - AMBIGUOUS
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
          nrow_nonAmbi <- LoopTibble %>% dplyr::group_by(accid) %>% 
            dplyr::filter(dplyr::n() > 1) %>%
            nrow()
          # Find ambiguous duplicates
          nrow_Ambi <- LoopTibble %>% dplyr::group_by(accid) %>% 
            dplyr::filter(dplyr::n() == 1) %>%
            nrow()
          # IF ALL of these rows have the same accid, then they are just regular synonym duplicates
          if(nrow_nonAmbi == nrow(LoopTibble)){
            #  # Add the lowest id number to the nonAmbiSyns_52 tibble
            ambiSyns_52 <- ambiSyns_52 %>% 
              dplyr::bind_rows(LoopTibble)
          }else{ # ALL of the others have been ambiguous so far
            # Logical - if ALL but one accid matches an id, take the to mean they are all pointing at
            # the same record. None shold match for now.
            accTest <- sum(LoopTibble$id %in% LoopTibble$accid) == nrow(LoopTibble)-1
            # Add these data to the ambiSyns_52 dataframe
            if(accTest == FALSE){ # Ad all as synonyms
              ambiSyns_52 <- ambiSyns_52 %>% 
                dplyr::bind_rows(LoopTibble)
            }else(
              stop(" - unique problem at 5.2! :(")
            )
          } # END else
        } # END n > 2
      } # END Ambiguous loop
    }else{
      ambiSyns_52 = dplyr::tibble()
      nonAmbiSyns_52 = dplyr::tibble()
    } # END big IF
    ###### b. loop_clean ####
    # NON-AMBIGUOUS because accids match
    if(nrow(nonAmbiSyns_52) > 0){
      # Take only one of each non-ambiguous synonyms
      nonAmbiSyns_deDuped_52 <- nonAmbiSyns_52 %>%
        dplyr::group_by(validName) %>%
        dplyr::filter(dplyr::row_number() == 1)
    }
    
    # NON-AMBIGUOUS
    if(nrow(nonAmbiSyns_52) > 0){
      nonAmbiSyns_52_nAmb <- nonAmbiSyns_52  %>%
        # Filter for ONLY the names that AREN'T already flagged as ambiguous
        dplyr::filter(!flags %in% c("ambiguous validName", "ambiguous can_wFlags", 
                                    "ambiguous can_wFlags")) 
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
      ambiSyns_52 = dplyr::tibble()
    } # END ambiSyns_52 IF
    # KEEP deDuplicated_52
    
    
    

    # What an adventure that was!
    # Now, lets try and return some user information 
writeLines(paste(    " - Cleaning complete! From an initial dataset of ", 
                 format(nrow(SynList), big.mark = ","), " names, there ",
                 "remain ", format(nrow(deDuplicated_52), big.mark = ",")," names.",  "\n",
                     " - We removed:", "\n"   ,
                 nrow(S1duplicates), " source1 accepted names,", "\n"   ,
                 nrow(S2Acc2remove), " source2 'accepted' names,", "\n"))
                  # 2.2 - synonyms removed
if(exists("nonAmbiSyns_deDuped")){
                   writeLines(paste(
                 format(nrow(nonAmbiSyns)-nrow(nonAmbiSyns_deDuped), big.mark = ","),
                        " source1 synonyms,", "\n"   ))}
writeLines(paste(
                 format(nrow(S2synonyms) - nrow(S2Unique), big.mark = ",")
                 , " source2 synonyms internally duplicated,", "\n"   ,
                 nrow(S2Duplicates)-nrow(S2Originals), " source2 synonyms duplicated with the source1 list,", "\n"   ,
                 nrow(dupes2remove_UnVNcheck), " subsequent duplicates after merging,", "\n",
                  # AMBIGUOUS flagged
                    " - We flagged:", "\n"   ,
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
                     " - We removed: ", "\n",
                 ambiAccCount, " ambiguous synonyms associated with accepted names.", "\n",
                    " - We re-assigned:", "\n"   ,
                 nrow(dupID), " duplicated [non-duplicate] ids",
sep = "")) 

  #### 6.0 Clean flags ####
deDuplicated_52 <- deDuplicated_52 %>%
  dplyr::mutate(flags = 
                    # Fix non-ambiguous canonical repeat
                  dplyr::if_else(flags %>% stringr::str_count("non-ambiguous canonical") > 1,
                                 stringr::str_remove_all(flags, "non-ambiguous canonical") %>%
                                   stringr::str_c("non-ambiguous canonical"), flags),
                    # Fix contradictory non- and is-
                flags = dplyr::if_else(stringr::str_detect(
                  flags, "ambiguous canonical, non-ambiguous canonical") |
                    stringr::str_detect(flags, "non-ambiguous canonical, ambiguous canonical"),
                                   stringr::str_remove_all(flags, "ambiguous canonical") %>% 
                                     stringr::str_remove_all("non-ambiguous canonical") %>% 
                                     stringr::str_remove("^, |, $") %>%
                                     stringr::str_replace(", , ", ", ") %>%
                                     stringr::str_c(" ambiguous canonical "), flags),
                flags = dplyr::if_else(stringr::str_detect(
flags, 
"non-ambiguous can_wFlags, ambiguous canonical, non-ambiguous can_wFlags, ambiguous canonical"),
stringr::str_c(flags, " non-ambiguous can_wFlags, ambiguous canonical "), flags
                ),
### 3
                flags = dplyr::if_else(stringr::str_detect(flags,
"non-ambiguous can_wFlags, non-ambiguous can_wFlagsnon-ambiguous canonical"),
"non-ambiguous can_wFlags, non-ambiguous canonical", flags),
### 4
                flags = dplyr::if_else(stringr::str_detect(flags,
"non-ambiguous can_wFlags, ambiguous canonical, non-ambiguous can_wFlags, ambiguous canonical non-ambiguous can_wFlags, ambiguous canonical"),
"non-ambiguous can_wFlags, ambiguous canonical", flags),
### 5
                flags = dplyr::if_else(stringr::str_detect(flags,
"ambiguous can_wFlags, ambiguous can_wFlags, ambiguous canonical"),
"ambiguous can_wFlags, ambiguous canonical", flags),
### 6
flags = dplyr::if_else(stringr::str_detect(flags,
                                           "non- ambiguous canonical"),
                       "non-ambiguous canonical", flags),
### 7
flags = dplyr::if_else(stringr::str_detect(flags,
                                           "ambiguous canonical, ambiguous canonical"),
                       "ambiguous canonical", flags),
### cleanup
                flags = flags %>% stringr::str_squish() %>%
  stringr::str_remove("^, |, $") %>%
  stringr::str_replace(", , ", ", ") 
  )

  # Return the cleaned dataset
return(deDuplicated_52)
}
  #### END ####