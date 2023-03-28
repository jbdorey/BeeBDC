# This function is a modification of a bdc function and flags columns as FALSE when they have a  
# coordiante uncertainty over a user-set threshold. 
# This function was written on the 2nd of August 2022 by James Dorey. Email James at
# jbdorey@me.com for help.


coordUncerFlagR <-
  function(data = NULL,
           uncerColumn = "coordinateUncertaintyInMeters",
           threshold = NULL) {
    .data <- .occurrenceAbsent <- NULL
    require(dplyr)
    
    # Make a new column called .occurrenceAbsent to be TRUE when occurrenceStatus is "present" or NA
    data <-
      data %>%
      dplyr::mutate(
        .uncertaintyThreshold =
          !.data[[uncerColumn]] > threshold) 

    # Return user output
    message(
      paste(
        "\\coordUncerFlagR:\n",
        "Flagged",
        format(sum(data$.uncertaintyThreshold == FALSE, na.rm = TRUE), big.mark = ","),
        "geographically uncertain records:\n",
        "The column '.uncertaintyThreshold' was added to the database.\n"
      )
    )
    return(data)
  }
