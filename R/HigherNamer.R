# This function was written by James Dorey to match higher order names to species.genus names
# For queries, please contact James Dorey at jbdorey@me.com
# This function was started on 15th May 2022 and last updated 17th May 2022


HigherNamer <- function(HigherNameList = HigherOrders,
                        InSynList = DLdf){
  # Match and copy the Higher Order names across
  InSynList$family  <- HigherNameList$family[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  InSynList$subfamily  <- HigherNameList$subfamily[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  InSynList$tribe  <- HigherNameList$tribe[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  InSynList$subtribe  <- HigherNameList$subtribe[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  
  #### START ProgBar ####
  # Initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = nrow(InSynList), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = NA,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  
  # Make sure these are nurmeric columns...
  InSynList$accid <- as.numeric(InSynList$accid)
  InSynList$id <- as.numeric(InSynList$id)
  
  # This loop will go through each row and if a Higher Order name is missing and there's a synonym/accepted name above, it will add those to that synonym
  # Starts at three because the first row is empty and the second has an empty above it
  for(i in 3:nrow(InSynList)){
    if( is.na(InSynList$family[i]) == TRUE & InSynList$accid[i] == InSynList$id[i-1]|
        InSynList$accid[i-1] & InSynList$accid[i] != "0"){ # IF the family is missing AND the accid matches the previous id OR the previous accid, replace the family name AND is not an accepted name
      InSynList$family[i] <- InSynList$family[i-1]
    }
    if( is.na(InSynList$subfamily[i]) == TRUE & InSynList$accid[i] == InSynList$id[i-1]|
        InSynList$accid[i-1] & InSynList$accid[i] != "0"){ # IF the subfamily is missing AND the accid matches the previous id OR the previous accid, replace the subfamily name AND is not an accepted name
      InSynList$subfamily[i] <- InSynList$subfamily[i-1]
    }
    if( is.na(InSynList$tribe[i]) == TRUE & InSynList$accid[i] == InSynList$id[i-1]|
        InSynList$accid[i-1] & InSynList$accid[i] != "0"){ # IF the tribe is missing AND the accid matches the previous id OR the previous accid, replace the tribe name AND is not an accepted name
      InSynList$tribe[i] <- InSynList$tribe[i-1]
    }
    if( is.na(InSynList$subtribe[i]) == TRUE & InSynList$accid[i] == InSynList$id[i-1]|
        InSynList$accid[i-1] & InSynList$accid[i] != "0"){ # IF the subtribe is missing AND the accid matches the previous id OR the previous accid, replace the subtribe name AND is not an accepted name
      InSynList$subtribe[i] <- InSynList$subtribe[i-1]
    }
    # Sets the progress bar to the current state
    setTxtProgressBar(pb, i)
  } # END loop
  #### END progBar ####
  close(pb) # Close the connection
  
  return(InSynList)
} # END function
