# This function, written by James Dorey builds flags from the Ascher notes column
# For queries, please contact James Dorey at jbdorey[at]me.com
# This function was started on 13th May 2022 and last updated 17th May 2022

#' @importFrom dplyr %>%
#' 
#' 
# Build a function that finds flags in the notes column and standardizes them into the flags column
flag_converter <- function(SynFile = DLdf){
  # locally bind variables to the function
  DLdf <- . <- Combined <- NULL
  
requireNamespace("dplyr")
  
  #### 1.1 doubtful ####
  # Doubtful species strings to find and matches
  doubt_sp_str <-c("nomen dubium","doubtful synonymy","doubtful synonym")
  doubt_sp_replacements <- SynFile$notes %>% 
    # Extract those strings
    stringr::str_extract(paste(doubt_sp_str,
                               collapse = "|")) %>%
    # Repalce those strings
    stringr::str_replace(pattern = paste(doubt_sp_str,
                                         collapse = "|"),
                         replacement = "doubtful species")  %>%
    as.character() %>% as.data.frame()
  
  #### 1.2 Syn Issue ####
  # Syn Issue strings to find and matches
  synIss_str <-c("partim")
  synIss_replacements <- SynFile$notes %>% 
    # Extract those strings
    stringr::str_extract(paste(synIss_str,
                               collapse = "|")) %>%
    # Repalce those strings
    stringr::str_replace(pattern = paste(synIss_str,
                                         collapse = "|"),
                         replacement = "synonym issue")  %>%
    as.character() %>% as.data.frame()
  
  #### 1.3 Auth Issue ####
  authIss_str <-c("auct", "auct , not Fabricius", "Auctorum", "Auct", "_auct",
                  "auct.", "miscitation")
  authIss_replacements <- SynFile$notes %>% 
    # Extract those strings
    stringr::str_extract(paste(authIss_str,
                               collapse = "|")) %>%
    # Repalce those strings
    stringr::str_replace(pattern = paste(authIss_str,
                                         collapse = "|"),
                         replacement = "authorship issue")  %>%
    as.character() %>% as.data.frame()
  
  #### 1.4 Auth not provided ####
  # If the authorship column is empty, flag...
  noAuth <- ifelse(is.na(SynFile$authorship), "author not provided", NA)  %>%
    as.character() %>% as.data.frame()

  
  #### 2.0 Merge flags ####
  MergedFlags <- dplyr::bind_cols(doubt_sp_replacements, synIss_replacements, authIss_replacements,
                                  noAuth) %>%
    # Concatenate the columns into one
    tidyr::unite(., col = Combined, 
                 na.rm = TRUE, sep = ", ")
    
    
  return(MergedFlags)
} # END function
