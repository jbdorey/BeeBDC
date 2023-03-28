# This function, written by James Dorey merges the Ascher and Orr datasets
# For queries, please contact James Dorey at jbdorey@me.com
# This function was started on 13th May 2022 and last updated 17th May 2022

OrrAscher_merger <- function(AscherData = DLdf,
                             OrrData = OrrTable,
                             HigherNameList = HigherOrders){
  require(dplyr)
  require(tibble)
  require(readr)
  # Remove redundant rows in Orr data (where Original == Correct_Final or != unique())
  Unique_Orr <- OrrData %>%
    # remove duplicate rows
    dplyr::distinct() %>%
    # Remove rows where Original == Correct_Final
    dplyr::filter(Original != Correct_Final)
  
  # Set up an empty data frame for the new rows
  NewRow_df <- tibble::tibble()
  NoMatch_df <- tibble::tibble()
  
  writeLines(" — Merging datasets...")
  #### START ProgBar ####
  # Initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = nrow(Unique_Orr), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = NA,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  stringr::str_which(Unique_Orr$Correct_Final, "Bombus pyrenaeus_homonym")
  
  #### loopStart ####
  # loop to add Orr variants into the taxonomy
  LoopOutput <- for(i in 1:nrow(Unique_Orr)){ 
    # choose the ith row's correct species name
    CorrectName <- Unique_Orr$Correct_Final[i]
      # Find the ACCEPTED name in the Ascher dataset that's associate with the Orr et al corrected name
    AccRow <- AscherData %>%
      dplyr::filter(canonical == CorrectName | canonical_withFlags == CorrectName)
    
    #  choose the ith row's correct species name
    SynName <- Unique_Orr$Original[i]
    #  Find the SYNONYM name in the Ascher dataset that's associate with the Orr et al corrected name
    SynRow <- AscherData %>%
      dplyr::filter(canonical == SynName | canonical_withFlags == SynName)
    
    ##### AcceptedMatch ####
    # IF a matching accepted name was found...
    if(nrow(AccRow) == 1){
        # Get genus an species names
      genusIn <- strsplit(as.character(Unique_Orr[i,1]), split = " ")[[1]][1] # Get first word only
      speciesIn <- strsplit(as.character(Unique_Orr[i,1]), split = " ")[[1]][2] # Get LAST word only
        # IF there is a matching synonym, get the authorship from that
      if(nrow(SynRow > 0)){
        authorIn <- SynRow$authorship
        authorName <- authorIn
        authFlag <- NA
      } else{ 
        authorIn <- "Unknown"
        authorName <- ""
        authFlag <- "author not provided"
      }# END SynRow authorIn 
      suppressMessages(
      flagsIn <- dplyr::bind_cols("orthographical variant", authFlag) %>%
        # Concatenate the columns into one
        tidyr::unite(., col = Combined, 
                     na.rm = TRUE, sep = ", ")
      )
        
      # Build a new row to add to the dataset
      NewRow <- tibble::tibble(
        flags = flagsIn[[1]],
        taxonomic_status = "synonym",
        source = "Orr_et_al_2021_CurrBiol",
        accid = AccRow$id,  # Get the accepted id for accid
        id = (max(AscherData$id) + i),     # Get the new id from the number of rows plus i
        kingdom = "Animalia",
        phylum = "Arthropoda",
        class = "Insecta",
        order = "Hymenoptera",
        family = SynRow$family[1],
        subfamily = SynRow$subfamily[1],
        tribe = SynRow$tribe[1],
        subtribe = SynRow$subtribe[1],
        validName = paste(genusIn, speciesIn, authorName, sep = " "),
        canonical_withFlags = "NA",
        canonical = as.character(Unique_Orr[i,1]),
        genus = genusIn,
        subgenus = "NA",
        species = speciesIn,
        infraspecies = "NA",
        authorship = authorIn,
        taxon_rank = "species",
        valid = FALSE,
        notes = SynRow$notes[1]
      ) # END NewRow
      # Add NewRow to dataframe
      NewRow_df <- dplyr::bind_rows(NewRow_df, NewRow)
    }# END accepted name found IF statement
    
    #### SynonymMatch ####
      # IF a matching accepted name was NOT found...
    if(nrow(SynRow) == 1 & nrow(AccRow) == 0){
      # Get genus an species names
      genusIn <- strsplit(as.character(Unique_Orr[i,1]), split = " ")[[1]][1] # Get first word only
      speciesIn <- strsplit(as.character(Unique_Orr[i,1]), split = " ")[[1]][2] # Get LAST word only
      # IF there is a matching synonym, get the authorship from that
      if(nrow(SynRow > 0)){
        authorIn <- SynRow$authorship
        authorName <- authorIn
        authFlag <- NA
      } else{ 
        authorIn <- "Unknown"
        authorName <- ""
        authFlag <- "author not provided"
      }# END SynRow authorIn 
      suppressMessages(
        flagsIn <- dplyr::bind_cols("orthographical variant", authFlag) %>%
          # Concatenate the columns into one
          tidyr::unite(., col = Combined, 
                       na.rm = TRUE, sep = ", ")
      )
      
      # Build a new row to add to the dataset
      NewRow <- tibble::tibble(
        flags = flagsIn[[1]],
        taxonomic_status = "synonym",
        source = "Orr_et_al_2021_CurrBiol",
        accid = SynRow$accid,  # Get the accepted id for accid
        id = (max(AscherData$id) + i),     # Get the new id from the number of rows plus i
        kingdom = "Animalia",
        phylum = "Arthropoda",
        class = "Insecta",
        order = "Hymenoptera",
        family = SynRow$family[1],
        subfamily = SynRow$subfamily[1],
        tribe = SynRow$tribe[1],
        subtribe = SynRow$subtribe[1],
        validName = paste(genusIn, speciesIn, authorName, sep = " "),
        canonical_withFlags = "NA",
        canonical = as.character(OrrData[i,1]),
        genus = genusIn, # Get first word only
        subgenus = "NA",
        species = speciesIn, # Get LAST word only
        infraspecies = "NA",
        authorship = authorIn,
        taxon_rank = "species",
        valid = FALSE,
        notes = SynRow$notes[1]
      ) # END NewRow
      # Add NewRow to dataframe
      NewRow_df <- dplyr::bind_rows(NewRow_df, NewRow)
    } # END accepted name NOT found
    
    #### noMatch ####
    # IF a matching accepted name was NOT found...
    if(nrow(AccRow) == 0 & nrow(SynRow) == 0){
      # Build a new row to add to the dataset
      NoMatch <- tibble::tibble(
        CorrectName, SynName
      ) # END NewRow
      # Add NewRow to dataframe
      NoMatch_df <- dplyr::bind_rows(NoMatch_df, NoMatch)
    } # END Else
    
    # Sets the progress bar to the current state
    setTxtProgressBar(pb, i)
  } # END loop
  #### END progBar ####
  close(pb) # Close the connection
  
  writeLines(paste(
    " — Cleaning new data...", sep = ""
  ))
  #### manage new data ####
    # Add higher order names
  NewRow_df <- HigherNamer(HigherNameList = HigherNameList,
                      InSynList = NewRow_df)
    # manage flags 
  NewRow_df <- FlagManager(InSynList = NewRow_df,
                           flagCol = "notes")
    # Remove duplicated notes
  NewRow_df$notes <- NewRow_df$notes %>%
    stringr::str_split( pattern = "\\|", n = 3,
                       simplify = FALSE) %>%
    lapply(unique)

    writeLines(paste(
    " — Saving the Ascher-matched Orr component of the data to the file ",
    getwd(), "/Ascher-matched_Orr_taxonomy.csv",  " seperately...", sep = ""
  ))
  # Save the Ascher-matched Orr dataset
  readr::write_csv(NewRow_df, file = "Ascher-matched_Orr_taxonomy.csv")
  writeLines(paste(" — ", nrow(NoMatch_df), 
    " names from the Orr list did not have an accepted or synonym match to the Ascher list. They ",
    "will be removed. ", "\n",
    "Saving these no-match names to ",
    getwd(), "/Ascher-Orr_NoMatch_taxonomy.csv",  " seperately...", sep = ""
  ))
  # Save noMatch_df
  readr::write_csv(NoMatch_df, file = "Ascher-Orr_NoMatch_taxonomy.csv")
  # Set correct column types
  NewRow_df$accid <- as.numeric(NewRow_df$accid)
  NewRow_df$id <- as.numeric(NewRow_df$id)
  NewRow_df$valid <- as.logical(NewRow_df$valid)
  NewRow_df$notes <- as.character(NewRow_df$notes)
  
  # Bind the NewRow_df and the AscherData
  AscherData <- dplyr::bind_rows(AscherData, NewRow_df)
  # Return this object
  return(AscherData)
} # END Function
