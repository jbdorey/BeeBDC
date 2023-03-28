# This function was made by James Dorey to remove duplicated synonyms and flag ambiguous synonyms

ambiguousFinder <- function(SynFile = DLdf){
  require(tibble)
  require(tidyr)
  require(dplyr)
  require(stringr)
#### 1.0 Ambiguous name ####
# filter out accepted names for synonym flagging
# Make a temporary index
SynFile$tempIndex <- 1:nrow(SynFile)
# Find duplicated names
duplicated_SynFile <- SynFile %>%
  dplyr::filter( duplicated(validName))
# Get the inverse of this list
invDuplicated_SynFile <- SynFile %>%
  dplyr::filter( !duplicated(SynFile$validName))
# Set up an empty tibble for the tempIndeces to remove
indeces2remove <- tibble::tibble()

writeLines(" — finding ambiguous names...")
##### START ProgBar 1 ####
# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(duplicated_SynFile), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = NA,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar


# For the duplicates that have the same accid, keep the DL version
for(i in 1:nrow(duplicated_SynFile)){
  #  choose the ith row and its duplicated name
  dupRowi <- duplicated_SynFile[i,]
  dupNamei <- dupRowi$validName
  #  Find the INVERSE row — the row that matched
  invDupRowi <- invDuplicated_SynFile %>%
    dplyr::filter(validName == dupNamei)
  # IF ONE of these is the accepted name... test 
  AccNamePresent <- (invDupRowi$taxonomic_status == "accepted" || dupRowi$taxonomic_status == "accepted")

  # IF accepted name is present
  ###### 1. Synonyms duplicating accepted names ####
  if(AccNamePresent == TRUE){
    # bind these rows
    CombinedRows <- dplyr::bind_rows(dupRowi, invDupRowi)
    # IF synonym's accid == the accepted id, mark the synonym for deletion
    AcceptedRow <- CombinedRows %>% dplyr::filter(taxonomic_status == "accepted")
    SynRow <- CombinedRows %>% dplyr::filter(taxonomic_status != "accepted")
    if(SynRow$accid == AcceptedRow$id){ # TRUE add index number for removal
      indeces2remove <- dplyr::bind_rows(indeces2remove, 
                                         tibble::tibble(species = SynRow$validName,
                                                        RMtempIndex = SynRow$tempIndex, 
                                                        reason = "Matched an accepted name"))
    } # END TRUE add index number for removal
  } # END 1.
  
  ##### 2. AMBIGUOUS synonyms ####
  if(AccNamePresent == FALSE && dupRowi$accid != invDupRowi$accid){
    indeces2remove <- dplyr::bind_rows(indeces2remove, 
                                       tibble::tibble(species = dupRowi$validName,
                                                      RMtempIndex = dupRowi$tempIndex, 
                                                      reason = "ambiguous synonym"))
    indeces2remove <- dplyr::bind_rows(indeces2remove, 
                                       tibble::tibble(species = invDupRowi$validName,
                                                      RMtempIndex = invDupRowi$tempIndex, 
                                                      reason = "ambiguous synonym"))
  }# END 2.
  
  ##### 3. Duplicated synonyms ####
  if(AccNamePresent == FALSE && dupRowi$accid == invDupRowi$accid){
    # If they are both from DiscoverLife, remove the duplicate
    if(dupRowi$source == "DiscoverLife" && invDupRowi$source == "DiscoverLife"){
      indeces2remove <- dplyr::bind_rows(indeces2remove, 
                                         tibble::tibble(species = dupRowi$validName,
                                                        RMtempIndex = dupRowi$tempIndex, 
                                                        reason = "Two Ascher synonyms"))
    } # END both DiscoverLife
    # If one is from DiscoverLife, keep that name
    if(dupRowi$source != "DiscoverLife" && invDupRowi$source == "DiscoverLife"){
      indeces2remove <- dplyr::bind_rows(indeces2remove, 
                                         tibble::tibble(species = dupRowi$validName,
                                                        RMtempIndex = dupRowi$tempIndex, 
                                                        reason = "One Ascher synonyms"))
    } # END one DiscoverLife
    # If neither is from DiscoverLife, remove the duplicate
    if(dupRowi$source != "DiscoverLife" && invDupRowi$source != "DiscoverLife"){
      indeces2remove <- dplyr::bind_rows(indeces2remove, 
                                         tibble::tibble(species = dupRowi$validName,
                                                        RMtempIndex = dupRowi$tempIndex, 
                                                        reason = "No Ascher synonyms"))
    } # END neither DiscoverLife
  }# End 3.
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
} # END duplicated loop
##### END progBar 2 ####
close(pb) # Close the connection

# Save the duplicate output — indeces2remove
writeLines(
  paste(" — Saving the ambiguous names and notes as: ",
        getwd(), "/indeces2remove_ambiguousNamesPlus.csv",
        sep = "")
)
readr::write_csv(indeces2remove, file = "indeces2remove_ambiguousNamesPlus.csv")

#### 2.0 remove duplicates ####
# Get the duplicated names to remove
dupNames <- indeces2remove %>%
  dplyr::filter(reason != "ambiguous synonym")
# User output
writeLines(paste(" — Removing ", format(nrow(dupNames), big.mark = ","), " duplicated names.",
                 sep = ""))
# Remove the duplicates
SynFile <- SynFile %>% 
  dplyr::filter(!tempIndex %in% dupNames$RMtempIndex) # 13,685 duplicates
# result: 56,089 names. Perfect.

#### 3.0 Flag ambiguous names ####  
# Get the ambiguous names to flag
ambiNames <- indeces2remove %>%
  dplyr::filter(reason == "ambiguous synonym")
  # Because there can be MULTIPLE ambiguous matches, one index might have been matched and hence occur
    # multiple times. Remove those duplicates.
ambiNames <- ambiNames %>%
  filter(!duplicated(RMtempIndex))
  # user output
writeLines(paste(" — There are ", nrow(ambiNames), " ambiguous names to flag.",
                 sep = ""))
# Merge the datasets.
ambiguousName <- ambiNames %>% 
  # rejoin this column to the OG dataframe
  dplyr::full_join(SynFile, by = c("RMtempIndex" = "tempIndex"),
                   keep = FALSE) 
# Combine the flag columns...
newFlagsCol <- ambiguousName %>%
  dplyr::select(flags, reason) %>%
    # Replace "" with NA
  dplyr::na_if("") %>%
  # Concatenate the columns into one
  tidyr::unite(., col = Combined, 
               na.rm = TRUE, sep = ", ")
# Replace the flags column with this new column
ambiguousName$flags <- newFlagsCol$Combined

# Filter out extra columns
ambiguousName <- ambiguousName %>%
    # remove excess columns
  dplyr::select( !c(species.x, RMtempIndex, reason)) %>%
    # rename species column after full_join
  dplyr::rename(species = species.y) %>%
    # re-order columns by id
  dplyr::arrange(id)

# Return the object
return(ambiguousName)
} # FIN