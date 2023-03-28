# This function is a modification of a bdc function and flags columns as FALSE when they are marked 
  # as "ABSENT". This function was written on the 22nd of May 2022 by James Dorey. Email James at
  # jbdorey@me.com for help.


jbd_flagAbsent <-
  function(data = NULL,
           PresAbs = "occurrenceStatus") {
    .data <- .occurrenceAbsent <- NULL
    require(dplyr)
    
      # Make a new column called .occurrenceAbsent to be TRUE when occurrenceStatus is "present" or NA
    data <-
      data %>%
      dplyr::mutate(
        .occurrenceAbsent =
          !.data[[PresAbs]] %in% c("ABSENT")) 
    
    

    # Return user output
    message(
      paste(
        "\\.occurrenceAbsent:\n",
        "Flagged",
        format(sum(data$.occurrenceAbsent == FALSE, na.rm = TRUE), big.mark = ","),
        "absent records:\n",
        "One column was added to the database.\n"
      )
    )
    return(data)
  }
