# This function is a modification of a bdc function and flags columns as FALSE when they are  
  # present in the strings_to_restrict. 
# This function was written on the 22nd of May 2022 by James Dorey. Email James at
  # jbdorey[at]me.com for help.#


#' Flag license protected records
#' 
#' This function will search for strings that indicate a record is restricted in its use and will 
#' flag the restricted records.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param strings_to_restrict A character vector. Should contain the strings used to detect protected records.
#' Default =  c("All Rights Reserved", "All rights reserved", "All rights reserved.", "ND", "Not for public")
#' @param excludeDataSource Optional. A character vector. A vector of the data sources (dataSource) 
#' that will not be flagged as protected, even if they are. This is useful if you have a private 
#' dataset that should be listed  as "All rights reserved" which you want to be ignored by this flag.
#'
#' @return Returns the data with a new column, .unLicensed, where FALSE = records that are protected by 
#' a license.
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#'   # Read in the example data
#' data("beesRaw")
#'   # Run the function
#' beesRaw_out <- flagLicense(data = beesRaw,
#'                         strings_to_restrict = "all",
#'                         # DON'T flag if in the following data# source(s)
#'                         excludeDataSource = NULL)

flagLicense <- function(data = NULL,
           strings_to_restrict = "all",
           excludeDataSource = NULL) {
    .data <- .unLicensed <- dataSource <-  NULL
    requireNamespace("dplyr")
    requireNamespace("rlang") 

    #### 1.0 Preperation ####
      ##### 1.1 strings_to_restrict ####
      # Flag if these strigns are present for NO USE!
    if (strings_to_restrict[1] == "all") {
      strings_to_restrict <-
        c("All Rights Reserved", 
          "All rights reserved",
          "All rights reserved.",
          "ND", # noDerivatives to be distributed
          "Not for public") # I fear this has been uploaded in error?
    }
      ##### 1.2 Missing columns ####
        ###### a. dataSource ####
      # If the dataSource column is not in the dataset, fill it in with "NA"s
    if(!any(colnames(data) %in% "dataSource")){
    data <- data %>%
      dplyr::mutate(dataSource = NA_character_)
    message("No dataSource provided. Filling this column with NAs...")
    }
    ###### b. rights ####
    # If the rights column is not in the dataset, fill it in with "NA"s
    if(!any(colnames(data) %in% "rights")){
      data <- data %>%
        dplyr::mutate(rights = NA_character_)
      message("No rights provided. Filling this column with NAs...")
    }
    
    ###### c. license ####
    # If the license column is not in the dataset, fill it in with "NA"s
    if(!any(colnames(data) %in% "license")){
      data <- data %>%
        dplyr::mutate(license = NA_character_)
      message("No license provided. Filling this column with NAs...")
    }
    
    ###### a. accessRights ####
    # If the accessRights column is not in the dataset, fill it in with "NA"s
    if(!any(colnames(data) %in% "accessRights")){
      data <- data %>%
        dplyr::mutate(accessRights = NA_character_)
      message("No accessRights provided. Filling this column with NAs...")
    }
    
    
    
    # Make a new column called .unLicensed to be TRUE when ... the restricted flags are present in 
      # any of the rights, license, or accessRights columns.
    data <-
      data %>%
      dplyr::mutate(
        .unLicensed = 
          dplyr::if_else(grepl(paste(strings_to_restrict, collapse = "|"), data$rights) |
                           grepl(paste(strings_to_restrict, collapse = "|"), data$license) |
                           grepl(paste(strings_to_restrict, collapse = "|"), data$accessRights) == TRUE,
                           dplyr::if_else(dataSource %in% excludeDataSource,
                                            # Flagged, but EXCLUDED — keep these data
                                         TRUE, 
                                            # Flagged AND included — DON'T use these data
                                         FALSE), 
                          # Not flagged — keep these data
                         TRUE))
    
    # Return user output
    message(
      paste(
        "\\.unLicensed:\n",
        "Flagged",
        format(sum(data$.unLicensed == FALSE, na.rm = TRUE), big.mark = ","),
        "records that may NOT be used.\n",
        "One column was added to the database.\n"
      )
    )
      # Return the data to the user
    return(data)
  }
