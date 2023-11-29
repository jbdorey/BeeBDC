


###### 0.2.1 NS func. ####

#' @importFrom dplyr %>%
# This function splits up the input name into its component elements
nameSplitR <- function(NameInput, 
                            Authority_patterns = NULL){
  requireNamespace("mgsub")

  if(is.null(Authority_patterns)){
  # split up the authority from the species name based on regular expressions
      # Help can be found testing regex here: https://www.regextester.com/95629
    Authority_patterns <- paste0("([a-zA-Z.\\-\\_]+,)", "|", # Find words ending in comma
                           "[a-zA-Z0-9.\\-\\_]+ and ","|", # Or find words before " and "
                           "\\s[:upper:][a-z]+$", "|", # Find a space then an upper-case letter (i.e., an author name after the species name)
                                                  # and at the end of the string
                           "\\s[:upper:][a-z]+\\s\\&", "|", # Find uppercase followed by "&" like "Smith &..."
                           "\\s[:upper:][a-z]+[A-Z][a-z]+\\s\\&", "|", # As above, but including multiple capitals, like "LaBerge &..."
                           "\\s[:upper:][a-z]+\\s[0-9]{4}", "|", # Find Author followed by no comma, space, and a year like " Cresson 1863"
                           "\\s[:upper:]\\'[A-Z]", "|", # Deal with names like "O'Brien"
                           "(\\([A-Z][a-z]+)(?=\\s)", "|", #, "|", # Find a capitalized word (with or without a bracket) - \\([A-Z][a-z]+) - that is followed by a space (not a bracket) - (?=\\s)
                           "(\\([A-Z]{1,2}[\\.]?)(?=\\s)", "|", # Find those few authorities that start with multiple capital letters - (SS Saunders, 1850), or with initials - (W. F. Kirby, 1900)
                           "(\\([A-Z][a-z\\u016f\\u00c0-\\u017e]+\\-[A-Z]+)(?=[\\s\\,])","|", # Match as above, but with special characters
                           "(\\([A-Z][a-z\\u016f\\u00c0-\\u017e]+)(?=[\\s\\,])","|", # Match as above, but with special characters
                           " \\(?de | \\(?van der | \\?van ", "|", # Finds the prefixs for last names "de Villers", "van der Zanden" etc.
                           "(\\([A-Z][a-z\\u016f\\u00c0-\\u017e]+\\-[A-Z][a-z\\u016f\\u00c0-\\u017e]+)(?=[\\s\\,])", "|", # Matches European/African characters with a hyphen between
                           "( not_\\(Cockerell)|\\u016fozi\\u016f|auct \\,| sensu auct not| 1914| sensu auct") # Find other haphazard matches
  }
  
  # Split the name based on Auth. Patterns
  SpSplit <-  stringr::str_split(NameInput, Authority_patterns, simplify = TRUE) %>% 
    mgsub::mgsub( c(" \\($", " $"), c("","")) # Get only the species name in ELEMENT ONE
  AuthSplit <- substr( NameInput[1],  # using the raw NameInput
                       nchar(SpSplit[[1]])+2, nchar(NameInput[1])) # find authority by removing name
  ##### subgenus test #####
  # Test for presence of subgenus
  if( grepl( "\\)",SpSplit[1], fixed = FALSE) == TRUE &
      !grepl( "\\)\\s\\(", SpSplit[1], fixed = FALSE)|
      grepl( "\\s[A-Z][a-z]", SpSplit[1], fixed = FALSE) == TRUE &
      !grepl( "\\)\\s\\(", SpSplit[1], fixed = FALSE)){ # Check to see if there is a subgenus using the presence of a a bracket
    # if there IS a subgenus present
    # SPLIT into individual words (by any white space)
    SynCols_split <- stringr::str_split(SpSplit[1],"\\s", simplify = TRUE)  # Express as matrix
    Gen_Name <- SynCols_split[1] # Get genus
    SubGen_Name <- mgsub::mgsub(SynCols_split[2], c("\\)", "\\("), c("",""),)  # Get subgenus and remove brackets from the subgenus column
    SpName <- SynCols_split[3] # Get species
    SubSpName <- ifelse(length(SynCols_split)>3, paste(c(SynCols_split[4:length(SynCols_split)]), 
                                                       sep=" ",collapse=" "), "NA")  # Get subspecies, if present
  }else{ # if there is NOT a subgenus present
    # SPLIT into individual words (by any white space)
    SynCols_split <- stringr::str_split(SpSplit[1],"\\s", simplify = TRUE)  # Express as matrix
    Gen_Name <- SynCols_split[1] # Get genus
    SubGen_Name <- "NA" # NO subgenus
    SpName <- SynCols_split[2] # Get species
    SubSpName <- ifelse(length(SynCols_split)>2, paste(c(SynCols_split[3:length(SynCols_split)]), 
                                                       sep=" ",collapse=" "), "NA")  # Get subspecies, if present
  } # END else
  
  ##### flag test ####
  flagTest <- sub( "(.*?[A-Za-z\\s]?){0,1}([\\(0-9\\)]{4}\\,?)|^[\\w]+", "", AuthSplit) %>%  # Extract the text after the year in authorship
    trimws( which = "left" , whitespace = "[\\, \\] \\) ]")  # Trim some special characters from the FRONT of the string
  
  if(flagTest != ""){ # IF there are flags present
    AuthSplit2 <- substr(AuthSplit,  # using the raw AuthSplit
                         start = 1, stop = (nchar(AuthSplit) - nchar(flagTest))) %>% # extract just the authority by removing the flag
      trimws( which = "right" , whitespace = "[\\, \\] \\) ]")
    Flags <- substr(AuthSplit,  
                    start = (nchar(AuthSplit2)+1), stop = (nchar(AuthSplit))) %>% # extract just the FLAG by removing the author
      trimws( which = "left" , whitespace = "[\\, \\] \\) ]")
  }else{ # END flag test - IF
    AuthSplit2 <- AuthSplit
    Flags <- ""
  } # END flag test - ELSE
  
  # Return values of interest
  AuthSplitOut <- list("flags" = Flags, "validName" = NameInput, 
                       "canonical" = paste( Gen_Name, "(", SubGen_Name , ")", SpName, SubSpName, sep	= " ") %>% 
                         mgsub::mgsub(c("NA","\\( "," \\)"),c("","\\(","\\)") ) %>%
                         mgsub::mgsub(c("  ","\\(\\)"),c(" ","") ), # Formerly in full - SpSplit[1] 
                       "genus" = Gen_Name,
                       "subgenus" = SubGen_Name, "species" = SpName, "infraspecies" = SubSpName,
                       "authorship" = AuthSplit2)
  
} # END the NameAuth_Splitter FUNCTION

