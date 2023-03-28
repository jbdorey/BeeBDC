# This function is a modification of a bdc function and flags columns as FALSE when they are  
  # present in the strings_to_restrict. 
# This function was written on the 22nd of May 2022 by James Dorey. Email James at
  # jbdorey@me.com for help.


jbd_flagLicense <-
  function(data = NULL,
           strings_to_restrict = "all",
           excludeDataSource = NULL) {
    .data <- .unLicensed <- NULL
    require(dplyr)
    require(rlang) 
    require(dplyr)
    require(tibble)
    
      # Flag if these strigns are present for NO USE!
    if (strings_to_restrict[1] == "all") {
      strings_to_restrict <-
        c("All Rights Reserved", 
          "All rights reserved",
          "All rights reserved.",
          "ND", # noDerivatives to be distributed
          "Not for public") # I fear this has been uploaded in error?
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
