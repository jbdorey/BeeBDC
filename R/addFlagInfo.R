##### Wrapper function: addFlagInfo #####
addFlagInfo <- function(occurDF,
                        flagInfo){
  require(stringr)
  if(str_detect(class(flagInfo), 
                "character|data.frame",
                negate = TRUE)){
    stop("flagInfo must be a csv file path, a character vector of flag names, or a dataframe-like object.")
  }
  if(is.character(flagInfo)){
    if(str_detect(flagInfo, ".csv$")){
      occurDF <- addFlagInfo.csv(occurDF, flagInfo)
    } else{
      occurDF <- addFlagInfo.character(occurDF, flagInfo)
    }
  } else{
    occurDF <- addFlagInfo.df(occurDF, flagInfo)
  }
  
  return(occurDF)
}

###############################################################################-

##### Character vector: addFlagInfo.character #####
addFlagInfo.character <- function(occurDF, 
                                  flagInfo){
  # Create flag info attribute
  
  # Create dataframe for cleaning summary
  summary_attr <- data.frame(flag          = flagInfo,
                             step          = rep(as.integer(NA),   length(flag_col)),
                             flag_included = rep(as.logical(NA),   length(flag_col)),
                             flag_column   = rep(as.character(NA), length(flag_col)),
                             pts_flagged   = rep(as.integer(NA),   length(flag_col)),
                             sp_affected   = rep(as.integer(NA),   length(flag_col)),
                             pts_remaining = rep(as.integer(NA),   length(flag_col)),
                             filtered      = rep(as.logical(NA),   length(flag_col)),
                             params        = rep(as.character(NA), length(flag_col)))
  
  #add dataframe as "cleaning_steps" attribute to occurrence dataframe
  attr(occ_df, 'cleaning_steps') <- flag_attr
  
  return(occ_df)
} 

###############################################################################-

##### CSV path: addFlagInfo.csv #####

addFlagInfo.csv <- function(occurDF,
                            flagInfo){
  require(dplyr)
  flag_attr <- readr::read_csv(flagInfo) %>%
    mutate(step          = as.integer(NA),
           flag_included = as.logical(NA),
           flag_column   = as.character(NA),
           pts_flagged   = as.integer(NA),
           sp_affected   = as.integer(NA),
           pts_remaining = as.integer(NA),
           filtered      = as.logical(NA),
           params        = as.character(NA))
  
  #add dataframe as "cleaning_steps" attribute to occurrence dataframe
  attr(occurDF, 'cleaning_steps') <- flag_attr
  return(occurDF)
}

###############################################################################-

##### Date frame: addFlagInfo.df #####
addFlagInfo.df <- function(occurDF,
                           flagInfo){
  summary_attr <- dplyr::as_tibble(flagInfo) %>%
    mutate(step          = as.integer(NA),
           flag_included = as.logical(NA),
           flag_column   = as.character(NA),
           pts_flagged   = as.integer(NA),
           sp_affected   = as.integer(NA),
           pts_remaining = as.integer(NA),
           filtered      = as.logical(NA),
           params        = as.character(NA))
}


###############################################################################-

##### Get flagInfo table #####
cleaning_steps <- function(occurDF){
  flagInfo <- attr(occurDF, 'cleaning_steps')
  return(flagInfo)
}
