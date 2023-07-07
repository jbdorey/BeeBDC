# This script was written by James Dorey to use Paige Chesshire's taxonomy updates to update the 
  # Orr and Ascher combined taxonomy
  # This function checks if there is a match in the current taxonomy or not
# For queries, please contact James Dorey at jbdorey[at]me.com
# This function was started on 15th May 2022 and last updated 17th May 2022
#' @importFrom dplyr %>%

PaigesSyns <- function(PaigeSheet = Paige_sheet_loc,
                       SynFile = SynL_AO){
  # locally bind variables to the function
  . <- Paige_sheet_loc <- SynL_AO <- canonical <- id <- NULL
  
  # Read in the relevant sheets from the excel file
  SynChanges <- openxlsx::read.xlsx(PaigeSheet, sheet = "Synonym_changes")
  
  
#### 2.0 Synonyms ####
# Set up an empty tibble...
SynAnnotate_df <- dplyr::tibble()
# Run a loop
for(i in 1:nrow(SynChanges)){
  # Find the ith name to change
  loopName_ori <- SynChanges$OriginalScientificName_DL_s1[i]
  loopName_DL <- SynChanges$DLifeName_s2[i]
  loopName_Final <- SynChanges$Final_Name[i]
  ##### 2.1 One name ####
  # If there is only one name...
  if(loopName_ori == loopName_DL || loopName_DL %in% c("Remove","remove (illogical)")){
    # Find that name in the original list
    SLAO_row <- SynFile %>% 
      dplyr::filter(canonical == loopName_ori)
    # Now find the accepted row based on the accid
    acc_SLAO_row <- SynFile %>% 
      dplyr::filter(id %in% SLAO_row$accid)
    # Check to see if any of the DiscoverLife accepted names match Paige's name
    taxaCurrentTest <- any(acc_SLAO_row$canonical %in% loopName_Final)
    # . note this gives a TRUE or FALSE to see if the names match the current taxonomy.
    if(taxaCurrentTest == TRUE){
      SynAnnotate_df <- dplyr::tibble(
        FinalName <- loopName_Final, originalName = loopName_ori, DisLifeName = loopName_DL,
        PotentialMatches = nrow(SLAO_row), iteration = i,
        Correct = TRUE, note = "One name provided, VALID") %>%
        dplyr::bind_rows(SynAnnotate_df, .)
    }else{
      SynAnnotate_df <- dplyr::tibble(
        FinalName <- loopName_Final, originalName = loopName_ori, DisLifeName = loopName_DL,
        PotentialMatches = nrow(SLAO_row), iteration = i,
        Correct = FALSE, note = "One name provided, INVALID")%>%
        dplyr::bind_rows(SynAnnotate_df, .)
    }
  }# End 1. valid name
  ##### 2.2 Two names ####
  # If there is only one name...
  if(loopName_ori != loopName_DL && !loopName_DL %in% c("Remove","remove (illogical)")){
    # Find that name in the original list
    SLAO_row <- SynFile %>% 
      dplyr::filter(canonical %in% c(loopName_ori, loopName_DL))
    # Now find the accepted row based on the accid
    acc_SLAO_row <- SynFile %>% 
      dplyr::filter(id %in% SLAO_row$accid)
    # Check to see if any of the DiscoverLife accepted names match Paige's name
    taxaCurrentTest <- any(acc_SLAO_row$canonical %in% loopName_Final)
    # . note this gives a TRUE or FALSE to see if the names match the current taxonomy.
    if(taxaCurrentTest == TRUE){
      SynAnnotate_df <- dplyr::tibble(
        FinalName <- loopName_Final, originalName = loopName_ori, DisLifeName = loopName_DL,
        PotentialMatches = nrow(SLAO_row), iteration = i,
        Correct = TRUE, note = "Two names provided, VALID") %>%
        dplyr::bind_rows(SynAnnotate_df, .)
    }else{
      SynAnnotate_df <- dplyr::tibble(
        FinalName <- loopName_Final, originalName = loopName_ori, DisLifeName = loopName_DL,
        PotentialMatches = nrow(SLAO_row), iteration = i,
        Correct = FALSE, note = "Two names provided, INVALID")%>%
        dplyr::bind_rows(SynAnnotate_df, .)
    } # END else
  } # END 2.2 Two names
}# End 2.0 Synonym loop

  # Use output
writeLines( paste("The output can be used to manually check names. ", 
                  "There are ", sum(SynAnnotate_df$Correct == FALSE), " names that require checking",
                  " and ", sum(SynAnnotate_df$Correct == TRUE), " that appear fine.",
                  sep = ""))

return(SynAnnotate_df)
} # END function



