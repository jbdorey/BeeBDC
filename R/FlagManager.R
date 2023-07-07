# This function was made by James Dorey to manage flags in the synonym lists
# For queries, please contact James Dorey at jbdorey[at]me.com
# This function was started on 15th May 2022 and last updated 17th May 2022
#' @importFrom dplyr %>%


FlagManager <- function(InSynList = DLdf,
                        flagCol = flags){
  # locally bind variables to the function
  . <- DLdf <- flags <- genus <- validName <- canonical <- NULL
  
  #### Flags option ####
  if(flagCol == "flags"){
    base::writeLines(" - Using the flags column.")
  # If there is no canonical_withFlags, make one with the canonical column, assuming it has flags
  suppressWarnings( CWFtest <- InSynList$canonical_withFlags,
                    classes = "warning")
  if(is.null(CWFtest) == TRUE){
    canonical_withFlags <- InSynList$canonical
    InSynList <- dplyr::mutate(InSynList, canonical_withFlags, .after = "canonical")
  }
    # IF all of the canonical_withFlags column is empty, replace it with the canonical valuse
  if(all(CWFtest == "NA") == TRUE){
    InSynList$canonical_withFlags <- InSynList$canonical
  }
  
  base::writeLines(" - 1. Remove flag from validName column...")
  ##### START ProgBar 1 ####
  # Initializes the progress bar
  pb1 <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = nrow(InSynList), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = NA,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  # Remove flag from validName
  for(i in 2:nrow(InSynList)){ 
    if(is.na(InSynList$flags[i]) == FALSE){ # Enter statement only IF there is a flag present
      InSynList$validName[i] <- sub(InSynList$flags[i], "", InSynList$validName[i] , fixed = TRUE) %>%
        trimws( which = "right" , whitespace = "[\\, ]")  # Trim some special characters from the END of the string
    } # END IF statement
    
    # Sets the progress bar to the current state
    utils::setTxtProgressBar(pb1, i)
  } # END loop COLUMNS
  #### END progBar 1 ####
  close(pb1) # Close the connection
  
  # Clean up double spaces in data frame
  InSynList <- as.data.frame(apply(InSynList, 2, FUN = stringr::str_squish ), stringsAsFactors = FALSE)
  # Remove empty rows that contain the following strings
  InSynList <- subset(InSynList, genus!="NA" & genus!="F" & genus!="V" & genus!="Discover" & 
                        genus!="Updated:" & genus!="Other" & validName!="Kinds of" & 
                        validName!="Scientific name" & canonical != "Kim, NA")
  
  # Find annotations and add the instead to the flags column
  # Which columns to search through
  FlagAnnot_cols <- c("genus",	"subgenus",	"species",	"infraspecies", "authorship")
  # Which flags to find
  FlagsAnnot <- c(
    # Name flags
    "_homonym[1-4]?","_homonytm","_homony","_sic","_sensu_lato",
    "_unavailable","_invalid","_nomen_nudum","_emend",
    " var "," subvar "," forma "," form "," race "," auct ","^var ","^subvar ","^forma ",
    "^form ","^race ",
    # unsure flags
    " f ","^f "," m ","^m "," r ","^r "," ab ","^ab ",
    # Specific cases
    "_Urban_not_Pasteels","_Friese_not_Megerle","_Friese_not_H\\u00fcbner","_Friese_not_Stimpson",
    # Author flags
    "Auctorum, sensu","sensu auct not","Auct non","Auct, not","auct, not","_auct not_",
    "_auct","^auct ") %>% 
    paste(collapse="|")
  
  
  base::writeLines(" - 2. Find and add flags to the 'flags' column...")
  #### START ProgBar 2 ####
  # Initializes the progress bar
  pb2 <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = length(FlagAnnot_cols), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = NA,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  # Find and add flags to flag column
  for(j in 1:length(FlagAnnot_cols)){ # COLUMNS
    for(i in 1:nrow(InSynList)){ # ROWS
      if(grepl(FlagsAnnot,  InSynList[i,FlagAnnot_cols[j]], fixed = FALSE) == TRUE){
        # Extract the flag from the string and add the column name
        ExtractedFlag <- stringr::str_extract_all(InSynList[i,FlagAnnot_cols[j]], 
                                                  FlagsAnnot, simplify = TRUE) %>%
          paste(FlagAnnot_cols[j],.,sep=" ", collapse = "|") %>%
          gsub("  ", " ",. ) # Remove double spaces
        if( is.na(InSynList[i,"flags"]) == TRUE){ # If there is NO flag, insert this flag as is
          InSynList[i,"flags"] <- ExtractedFlag
        }else{ # If there IS a flag, ADD this to the existing flag
          InSynList[i,"flags"] <- paste(InSynList[i,"flags"], ExtractedFlag, sep =  " | ")
        } # END if else statement
      } # END find flag IF statement
    } # END for loop of flag annotations - ROWS
    
    # Sets the progress bar to the current state
    utils::setTxtProgressBar(pb2, j)
  } # END loop COLUMNS
  #### END progBar 2 ####
  close(pb2) # Close the connection
  
  # Trim internal white spaces in flag column
  InSynList$flags <- gsub(" \\| | \\||\\| ", "|", InSynList$flags)  
  # Remove these strings from the relevant columns now that they are saved as flags
  TempCols <- as.data.frame(apply(InSynList[c("canonical",FlagAnnot_cols) ], 2,
                                  function(y) gsub(FlagsAnnot, " ", y))) 
  # Replace existing columns with these new, trimmed, columns
  InSynList[c(FlagAnnot_cols)] <- TempCols[, FlagAnnot_cols]
  # Add the canonical column after the canonical column with flags - later convert to 
    # canonical and canonical_withFlags
  CanTest <- InSynList$canonical
    # IF there IS already a canonical column repalce that column with the new one...
  if(is.null(CanTest) == FALSE){
    InSynList$canonical <- TempCols$canonical
  }else{
    InSynList <- dplyr::mutate(InSynList, TempCols$canonical, .after = "canonical")
  }
  #  Clean up double spaces in data frame
  InSynList <- as.data.frame(apply(InSynList, 2, FUN = stringr::str_squish ), stringsAsFactors = FALSE)
  } # END IF flags col
  
#### Notes option ####
  if(flagCol == "notes"){
    base::writeLines(" - Using the notes column.")
    # If there is no canonical_withFlags, make one with the canonical column, assuming it has flags
    suppressWarnings( CWFtest <- InSynList$canonical_withFlags,
                      classes = "warning")
    if(is.null(CWFtest) == TRUE){
      canonical_withFlags <- InSynList$canonical
      InSynList <- dplyr::mutate(InSynList, canonical_withFlags, .after = "canonical")
    }
    # IF all of the canonical_withFlags column is empty, replace it with the canonical valuse
    if(all(CWFtest == "NA") == TRUE){
      InSynList$canonical_withFlags <- InSynList$canonical
    }
    
    base::writeLines(" - 1. Remove flag from validName column...")
    ##### START ProgBar 1 ####
    # Initializes the progress bar
    pb1 <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                          max = nrow(InSynList), # Maximum value of the progress bar
                          style = 3,    # Progress bar style (also available style = 1 and style = 2)
                          width = NA,   # Progress bar width. Defaults to getOption("width")
                          char = "=")   # Character used to create the bar
    
    # Remove flag from validName
    for(i in 2:nrow(InSynList)){ 
      if(is.na(InSynList$notes[i]) == FALSE){ # Enter statement only IF there is a flag present
        InSynList$validName[i] <- sub(InSynList$notes[i], "", InSynList$validName[i] , fixed = TRUE) %>%
          trimws( which = "right" , whitespace = "[\\, ]")  # Trim some special characters from the END of the string
      } # END IF statement
      
      # Sets the progress bar to the current state
      utils::setTxtProgressBar(pb1, i)
    } # END loop COLUMNS
    #### END progBar 1 ####
    close(pb1) # Close the connection
    
    # Clean up double spaces in data frame
    InSynList <- as.data.frame(apply(InSynList, 2, FUN = stringr::str_squish ), stringsAsFactors = FALSE)
    # Remove empty rows that contain the following strings
    InSynList <- subset(InSynList, genus!="NA" & genus!="F" & genus!="V" & genus!="Discover" & 
                          genus!="Updated:" & genus!="Other" )
    
    # Find annotations and add the instead to the flags column
    # Which columns to search through
    FlagAnnot_cols <- c("genus",	"subgenus",	"species",	"infraspecies", "authorship")
    # Which flags to find
    FlagsAnnot <- c(
      # Name flags
      "_homonym[1-4]?","_homonytm","_homony","_sic","_sensu_lato",
      "_unavailable","_invalid","_nomen_nudum","_emend",
      " var "," subvar "," forma "," form "," race "," auct ","^var ","^subvar ","^forma ",
      "^form ","^race ",
      # unsure flags
      " f ","^f "," m ","^m "," r ","^r "," ab ","^ab ",
      # Specific cases
      "_Urban_not_Pasteels","_Friese_not_Megerle","_Friese_not_H\\u00fcbner","_Friese_not_Stimpson",
      # Author flags
      "Auctorum, sensu","sensu auct not","Auct non","Auct, not","auct, not","_auct not_",
      "_auct","^auct ") %>%
      paste(collapse="|")
    
    
    base::writeLines(" - 2. Find and add flags to the 'flags' column...")
    #### START ProgBar 2 ####
    # Initializes the progress bar
    pb2 <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                          max = length(FlagAnnot_cols), # Maximum value of the progress bar
                          style = 3,    # Progress bar style (also available style = 1 and style = 2)
                          width = NA,   # Progress bar width. Defaults to getOption("width")
                          char = "=")   # Character used to create the bar
    
    # Find and add flags to notes column
    for(j in 1:length(FlagAnnot_cols)){ # COLUMNS
      for(i in 1:nrow(InSynList)){ # ROWS
        if(grepl(FlagsAnnot,  InSynList[i,FlagAnnot_cols[j]], fixed = FALSE) == TRUE){
          # Extract the flag from the string and add the column name
          ExtractedFlag <- stringr::str_extract_all(InSynList[i,FlagAnnot_cols[j]], 
                                                    FlagsAnnot, simplify = TRUE) %>%
            paste(FlagAnnot_cols[j],.,sep=" ", collapse = "|") %>%
            gsub("  ", " ",. ) # Remove double spaces
          if( is.na(InSynList[i,"notes"]) == TRUE){ # If there is NO flag, insert this flag as is
            InSynList[i,"notes"] <- ExtractedFlag
          }else{ # If there IS a flag, ADD this to the existing flag
            InSynList[i,"notes"] <- paste(InSynList[i,"notes"], ExtractedFlag, sep =  " | ")
          } # END if else statement
        } # END find flag IF statement
      } # END for loop of flag annotations - ROWS
      
      # Sets the progress bar to the current state
      utils::setTxtProgressBar(pb2, j)
    } # END loop COLUMNS
    #### END progBar 2 ####
    close(pb2) # Close the connection
    
    # Trim internal white spaces in flag column
    InSynList$notes <- gsub(" \\| | \\||\\| ", "|", InSynList$notes)  
    # Remove these strings from the relevant columns now that they are saved as flags
    TempCols <- as.data.frame(apply(InSynList[c("canonical",FlagAnnot_cols) ], 2,
                                    function(y) gsub(FlagsAnnot, " ", y))) 
    # Replace existing columns with these new, trimmed, columns
    InSynList[c(FlagAnnot_cols)] <- TempCols[, FlagAnnot_cols]
    # Add the canonical column after the canonical column with flags - later convert to 
    # canonical and canonical_withFlags
    CanTest <- InSynList$canonical
    # IF there IS already a canonical column repalce that column with the new one...
    if(is.null(CanTest) == FALSE){
      InSynList$canonical <- TempCols$canonical
    }else{
      InSynList <- dplyr::mutate(InSynList, TempCols$canonical, .after = "canonical")
    }
    #  Clean up double spaces in data frame
    InSynList <- as.data.frame(apply(InSynList, 2, FUN = stringr::str_squish ), stringsAsFactors = FALSE)
  } # END IF notes col
  
  #### neither ####
  if(flagCol != "flags" & flagCol != "notes"){
    base::writeLines("!! The flagCol option must equal 'flags' or 'notes' !!")
  }
  
  #backupdf <- InSynList
  #InSynList <- backupdf
  
  # change column names
  #colnames(InSynList) <- c("flags","taxonomic_status","source","accid","id","kingdom","phylum","class","order","family","subfamily","tribe","subtribe","validName","canonical_withFlags","canonical","genus","subgenus","species","infraspecies","authorship")
  return(InSynList)
} # END FlagManager