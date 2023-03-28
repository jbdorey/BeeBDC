# For queries, please contact James Dorey at jbdorey@me.com
# This function was started on 10th May 2022 and last updated 17th May 2022


flagBasisOfRecord <- function(occurDF,
           flag = "UNCERTAIN_BASISOFRECORD",
           description = "Record not based on physical, contemporary record.",
           flagCol = "poorBasisRecord"){
    
    require(dplyr)
    
    cols <- attr(occurDF, 'keyCols')
    
    params = "basisOfRecord == 'human_observation|observation'"
    
    flagged <- occurDF %>%
      dplyr::select(all_of(c(cols, "basisOfRecord"))) %>%
      filter(basisOfRecord == "HUMAN_OBSERVATION" |
               basisOfRecord == "OBSERVATION" |
               basisOfRecord == "humanobservation"  |
               basisOfRecord == "HumanObservation"  |
               basisOfRecord == "observation") %>%
      pull(id)
    
    #add new flag column to dataset
    occurDF <- addFlagCol(occurDF,
                          flagged,
                          flagCol,
                          cols)
    
    #add new flagCol to flagCols attribute
    occurDF <- updateFlagCols(occurDF, flagCol)
    
    #get summary of cleaning steps
    outStats <- statsOut(occurDF)
    
    #update 'cleaning_steps' attribute
    occurDF <- updateFlagInfo(occurDF,
                              flag = flag,
                              flagCol = flagCol,
                              description = description,
                              params = params,
                              flagStats = outStats)
    
    #print summary of cleaning step
    print(clean_summary(outStats))
    
    return(occurDF)
}


#### ——————— ####


flagFossils <- function(occurDF,
                              flag = "RECORDED_FOSSIL",
                              description = "Record on based on a fossil — not a contemporary specimen.",
                              flagCol = "fossilRecord"){
  
  require(dplyr)
  
  cols <- attr(occurDF, 'keyCols')
  
  params = "basisOfRecord == 'fossil_specimen'"
  
  flagged <- occurDF %>%
    dplyr::select(all_of(c(cols, "basisOfRecord"))) %>%
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             basisOfRecord == "fossilspecimen" |
             basisOfRecord == "FossilSpecimen") %>%
    pull(id)
  
  
  #add new flag column to dataset
  occurDF <- addFlagCol(occurDF,
                        flagged,
                        flagCol,
                        cols)
  
  #add new flagCol to flagCols attribute
  occurDF <- updateFlagCols(occurDF, flagCol)
  
  #get summary of cleaning steps
  outStats <- statsOut(occurDF)
  
  #update 'cleaning_steps' attribute
  occurDF <- updateFlagInfo(occurDF,
                            flag = flag,
                            flagCol = flagCol,
                            description = description,
                            params = params,
                            flagStats = outStats)
  
  #print summary of cleaning step
  print(clean_summary(outStats))
  
  return(occurDF)
}


#### ——————— ####


flagLicense <- function(occurDF){
  require(dplyr, rlang, dplyr, tibble)
  #### Load strings ####
  # unrestricted_strings 
  unrestricted_strings <- c("1.0","zero","CC0","CC0_1_0",
                            "Free usage", "Open Access","Público","No Restrictions",
                            "www.natagora.be",
                            "www.natuurpunt.be",
                            "publicdomain/zero/1.0")
  # Restricted strings
  restricted_strings <- c("All Rights Reserved", "All rights reserved.",
                          "ND", # noDerivatives to be distributed
                          "Not for public") # I fear this has been uploaded in error?
  # attribution_strings 
  attribution_strings <- c("2.5","4.0","3.0","4_0","3_0","2_5",
                           "Attribution","NC","BY","SA","CC-BY",
                           "biodiversity.ku.edu", "Creative Commons - Share Alike", 
                           "License Elements","ShareAlike")
  # noncommercial_strings
  noncommercial_strings <- c("NonCommercial","NC","-nc","no comerciales","3.0","not-for-profit",
                             "no comercial","Sólo para uso académico","Non-Commercial","3_0",
                             "Not for Profit","www.fieldmuseum.org","www.vertnet.org",
                             "Not for commercial use")
  # unknown_strings
  unknown_strings <- c("Unknown", "Creative Commons - license at record level", "UNSPECIFIED",
                       "Custom", "NA", "")
  
  # Extract the key columns from the data
  cols <- attr(occurDF, 'keyCols')
  
  flagDF <- occurDF %>%
    dplyr::select(tidyselect::all_of(c(cols, "rights","license","accessRights")))
  
    # Find and flag records where any of these columns match the criteria strings.
      #### Unrestricted ####
  unrestrictedFlags <- if_else(grepl(paste(unrestricted_strings, collapse = "|"), flagDF$rights) |
                                 grepl(paste(unrestricted_strings, collapse = "|"), flagDF$license) |
                                 grepl(paste(unrestricted_strings, collapse = "|"), flagDF$accessRights) == TRUE,
                                  TRUE, FALSE)
      # Bind the unrestricted flags to the flag dataframe, while naming that column
  occurDF <- dplyr::bind_cols(occurDF, rlang::set_names(tibble::tibble(unrestrictedFlags), "unrestrictedFlags")) 
  
  ##### — strings ####
    # Set the flag name 
  flag = "CC_unrestrictedUse"
    # Set flagcol name
  flagCol = "unrestrictedFlags"
    # Set the description
  description = "These records may be used with no restrictions."
  
  #add new flagCol to flagCols attribute
  occurDF <- updateFlagCols(occurDF, flagCol)
  #get summary of cleaning steps
  outStats <- statsOut(occurDF)
  # Set parameters
  params = paste("'rights''|'license'|'accessRights' == ", 
                 paste(unrestricted_strings, collapse = "|"))
  #update 'cleaning_steps' attribute
  occurDF <- updateFlagInfo(occurDF,
                            flag = flag,
                            flagCol = flagCol,
                            description = description,
                            params = params,
                            flagStats = outStats)
  
  #print summary of cleaning step
  writeLines(paste("For ", flagCol,", ", clean_summary(outStats), sep = ""))
  
  
    #### Restricted #### 
  restrictedFlags <- dplyr::if_else(grepl(paste(restricted_strings, collapse = "|"), flagDF$rights) |
                                 grepl(paste(restricted_strings, collapse = "|"), flagDF$license) |
                                 grepl(paste(restricted_strings, collapse = "|"), flagDF$accessRights) == TRUE,
                                  TRUE, FALSE)
  # Bind the unrestricted flags to the flag dataframe, while naming that column
  occurDF <- dplyr::bind_cols(occurDF, rlang::set_names(tibble(restrictedFlags), "restrictedFlags")) 
  
  ##### — strings ####
  # Set the flag name 
  flag = "CC_DoNotUse"
  # Set flagcol name
  flagCol = "restrictedFlags"
  # Set the description
  description = "These records may NOT be used. Contact data owner for any use."
  
  #add new flagCol to flagCols attribute
  occurDF <- updateFlagCols(occurDF, flagCol)
  #get summary of cleaning steps
  outStats <- statsOut(occurDF)
  # Set parameters
  params = paste("'rights''|'license'|'accessRights' == ", 
                 paste(restricted_strings, collapse = "|"))
  #update 'cleaning_steps' attribute
  occurDF <- updateFlagInfo(occurDF,
                            flag = flag,
                            flagCol = flagCol,
                            description = description,
                            params = params,
                            flagStats = outStats)
  
  #print summary of cleaning step
  writeLines(paste("For ", flagCol,", ", clean_summary(outStats), sep = ""))
  
  
  #### Attribution #### 
  attributionFlags <- dplyr::if_else(grepl(paste(attribution_strings, collapse = "|"), flagDF$rights) |
                               grepl(paste(attribution_strings, collapse = "|"), flagDF$license) |
                               grepl(paste(attribution_strings, collapse = "|"), flagDF$accessRights) == TRUE,
                              TRUE, FALSE)
  # Bind the unrestricted flags to the flag dataframe, while naming that column
  occurDF <- dplyr::bind_cols(occurDF, rlang::set_names(tibble(attributionFlags), "attributionFlags")) 
  
  ##### — strings ####
  # Set the flag name 
  flag = "CC_attributionRequired"
  # Set flagcol name
  flagCol = "attributionFlags"
  # Set the description
  description = "These records may be used with attribution."
  
  #add new flagCol to flagCols attribute
  occurDF <- updateFlagCols(occurDF, flagCol)
  #get summary of cleaning steps
  outStats <- statsOut(occurDF)
  # Set parameters
  params = paste("'rights''|'license'|'accessRights' == ", 
                 paste(attribution_strings, collapse = "|"))
  #update 'cleaning_steps' attribute
  occurDF <- updateFlagInfo(occurDF,
                            flag = flag,
                            flagCol = flagCol,
                            description = description,
                            params = params,
                            flagStats = outStats)
  
  #print summary of cleaning step
  writeLines(paste("For ", flagCol,", ", clean_summary(outStats), sep = ""))
  
  
  #### Non-commercial #### 
    # — NOTE: I have decided to include unknown strings as non-commercial.
  noncommercialFlags <- dplyr::if_else(grepl(paste(c(noncommercial_strings,unknown_strings), 
                                            collapse = "|"), flagDF$rights) |
                                grepl(paste(c(noncommercial_strings,unknown_strings), 
                                            collapse = "|"), flagDF$license) |
                                grepl(paste(c(noncommercial_strings,unknown_strings), 
                                            collapse = "|"), flagDF$accessRights) == TRUE,
                                TRUE, FALSE)
  # Bind the unrestricted flags to the flag dataframe, while naming that column
  occurDF <- dplyr::bind_cols(occurDF, rlang::set_names(tibble(noncommercialFlags), "noncommercialFlags")) 
  
  ##### — strings ####
  # Set the flag name 
  flag = "CC_attribution_nonCommercial"
  # Set flagcol name
  flagCol = "noncommercialFlags"
  # Set the description
  description = "These records may be used for non-commercial purposes and with attribution."
  
  #add new flagCol to flagCols attribute
  occurDF <- updateFlagCols(occurDF, flagCol)
  #get summary of cleaning steps
  outStats <- statsOut(occurDF)
  params = paste("'rights''|'license'|'accessRights' == ", 
                 paste(c(noncommercial_strings,unknown_strings), collapse = "|"))
  #update 'cleaning_steps' attribute
  occurDF <- updateFlagInfo(occurDF,
                            flag = flag,
                            flagCol = flagCol,
                            description = description,
                            params = params,
                            flagStats = outStats)
  
  #print summary of cleaning step
  writeLines(paste("For ", flagCol,", ", clean_summary(outStats), sep = ""))
  
  # Return the occurrence tibble
  return(occurDF)
}




#### ——————— ####


flagTaxonIssue <- function(occurDF,
                        flag = "Taxon_name_issue",
                        description = "Taxon name did not match any canonical (Accepted) names.",
                        flagCol = "TaxonNameIssue"){
  
  require(dplyr)
  
  cols <- attr(occurDF, 'keyCols')
  
  params = "basisOfRecord == 'fossil_specimen'"
  
  flagged <- occurDF %>%
    dplyr::select(all_of(c(cols, "basisOfRecord"))) %>%
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             basisOfRecord == "fossilspecimen" |
             basisOfRecord == "FossilSpecimen") %>%
    pull(id)
  
  
  #add new flag column to dataset
  occurDF <- addFlagCol(occurDF,
                        flagged,
                        flagCol,
                        cols)
  
  #add new flagCol to flagCols attribute
  occurDF <- updateFlagCols(occurDF, flagCol)
  
  #get summary of cleaning steps
  outStats <- statsOut(occurDF)
  
  #update 'cleaning_steps' attribute
  occurDF <- updateFlagInfo(occurDF,
                            flag = flag,
                            flagCol = flagCol,
                            description = description,
                            params = params,
                            flagStats = outStats)
  
  #print summary of cleaning step
  print(clean_summary(outStats))
  
  return(occurDF)
}