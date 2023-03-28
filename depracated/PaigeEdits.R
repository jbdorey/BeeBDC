# This script was written by James Dorey to use Paige Chesshire's taxonomy updates to update the 
    # Orr and Ascher combined taxonomy
    # This function adds rows to the AO taxonomy or modifies rows
# For queries, please contact James Dorey at jbdorey@me.com
# This function was started on 15th May 2022 and last updated 17th May 2022


PaigeEdits <- function(PaigeSheet = Paige_changes_loc,
                       SynFile = SynL_AO,
                       PaigeAcc = Paige_acc_loc,
                          # Removes the regional synonyms.
                       ignoreRegional = TRUE){
  
  # Read in the relevant sheets from the excel file
  acceptedNames <- readr::read_csv(PaigeAcc)
  rows2add <- readr::read_csv(PaigeSheet)

  # Remove regional rows if necessary. i.e., any row that has a value (i.e. is regional will be removed)
  if(ignoreRegional == TRUE){
    rows2add <- rows2add %>% 
      dplyr::filter(is.na(Change_if_in))
    acceptedNames <- acceptedNames %>% 
      dplyr::filter(is.na(Change_if_in))
  } # END ignoreRegional
   # Remove extra columns that wont match with AO taxonomy. Only keep those present in AO taxonomy.
      # rows2add <- rows2add %>% 
      #   dplyr::select( colnames(SynFile))
  
  #### 1.0 Add accepted names ####
    ##### 1.1 valid acc.+ ####
  # For the rows which simply need to be ADDED
    # Get those rows without an invalid name to match to
  validAcc <- acceptedNames %>%
    dplyr::filter(is.na(INvalidMatch))
  if(nrow(validAcc) > 0){
    # Add id numbers
  validAcc$id <- seq(from = max(SynFile$id)+1, to = max(SynFile$id)+nrow(validAcc))
  
  # append the notes column
  validAcc$notes <- paste(append(validAcc$notes, validAcc$ChangeNotes), collapse = ", ") %>%
    mgsub::mgsub(pattern = c("^, ", ", $", "NA, "),
                 replacement = c("","",""), .)
  # Remove extra columns 
  validAcc <- validAcc %>% dplyr::select(colnames(SynFile))
    # Add these rows to the complete dataset
  SynFile <- SynFile %>%
    dplyr::bind_rows(validAcc)}
  
  
    ##### 1.2 INvalid acc.+ ####
      ###### a. fix accepted rows ####
  # These are now fixed in DiscoverLife
             # For the accepted rows which need to be modified and matched back to another name
               # Get those rows WITH an invalid name to match to
  if(nrow(acceptedNames) > 0 ){
             INvalidAcc <- acceptedNames %>%
               dplyr::filter(complete.cases(INvalidMatch))
               # Find the matching valid names in the OG dataset
             INvalidAcc_OG <- SynFile %>%
               dplyr::filter(tolower(validName) %in% tolower(INvalidAcc$INvalidMatch))
               # Replace the ids of the new rows (INvalidAcc)
             INvalidAcc$id <- INvalidAcc_OG$id
               # Remove the offending rows in the OG dataset AND then add the new rows to that dataset
             SynFile <- SynFile %>%
               dplyr::filter(!validName %in% INvalidAcc$INvalidMatch) %>%
                 # Also, select just original intended columns
               dplyr::bind_rows( dplyr::select(INvalidAcc, colnames(SynFile)) )
             
                 ###### b. add synonyms ####
              # The once-accepted rows should now be added as synonyms
               # Assign new id numbers
               # Add id numbers
             INvalidAcc_OG$id <- seq(from = max(SynFile$id)+1, to = max(SynFile$id)+nrow(INvalidAcc_OG))
               # Make certain BOTH datasets are in the SAME order
             INvalidAcc_OG <- INvalidAcc_OG %>% arrange(validName)
             INvalidAcc <- INvalidAcc %>% arrange(INvalidMatch)
               # assign the accids
             INvalidAcc_OG$accid <- INvalidAcc$id
  }
    
  #### 2.0 Add rows ####
  # user output
  writeLines(paste(
    " — Adding or modifying ", nrow(rows2add), " names...",
    sep = ""
  ))
  
    ##### 2.1 loop ####
  # Set up empty tibbles...
    addRows_df <- tibble::tibble()
    ids2remove <- tibble::tibble()
  # Find the max id number in the current taxonomy  
  idMax <- max(SynFile$id)
    # This loop will prepare all new rows to be added AND it will highlight the existing rows to be replaced
  for(i in 1:nrow(rows2add)){
      # Get the ith row
    NewRow <- rows2add[i,]
      # And assign a new id number to it based on idMax plus ith iteration
    NewRow$id <- idMax + i
      # name to find
    accName <- NewRow$validMatch
      # Find the name to match to from the ORIGINAL dataset
    accRow <- SynFile %>%
      dplyr::filter(validName == accName) %>%
        # Make sure this is the accepted name
      dplyr::filter(taxonomic_status == "accepted")
    # If this fails to match, try looking for the accepted name using accName as a synonym
    if(nrow(accRow) == 0){
      accRow <- SynFile %>%
        dplyr::filter(validName == accName)
      accRow = SynFile %>%
        dplyr::filter(id == accRow$accid)
    }
    
      # append the notes column
    NewRow$notes <- paste(append(NewRow$notes, NewRow$ChangeNotes), collapse = ", ") %>%
      mgsub::mgsub(pattern = c("^, ", ", $"),
                   replacement = c("",""), .)
      # IF there 
    # Assign the new accid
    NewRow$accid <- accRow$id
    
      # Remove extra columns
    NewRow <- NewRow %>%
        dplyr::select( colnames(SynFile))
      # Add this new row to the empty dataframe
    addRows_df <- addRows_df %>%
      dplyr::bind_rows(NewRow) 
    
    ## Check ##
    # EXISTING NAME CHECK
      # Now, if this row exists in the OG dataset, MARK it for removal
    existingRow <- SynFile %>%
      dplyr::filter(validName == NewRow$validName)
    if(nrow(existingRow) > 0){
        # IF the name exists in the original dataset, add it to the list of ids to remove
    ids2remove <- ids2remove %>%
      dplyr::bind_rows( tibble::tibble(id = existingRow$id, 
                                       validName = existingRow$validName))
    }
  } # END 2.0 loop
    
  
    # Incorporate changes
  SynFile_add <- SynFile %>% 
      # FIRST, remove the rows that are to be replaced
    dplyr::filter(!id %in% ids2remove$id) %>%
      # SECOND, add in the columns to be added
    dplyr::bind_rows(addRows_df)

  # Return the full list
  return(SynFile_add)
  
}
  