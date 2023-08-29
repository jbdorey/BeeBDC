# This function is a modification of a bdc function and flags columns as FALSE when they are marked 
  # as "ABSENT". This function was written on the 22nd of May 2022 by James Dorey. Email James at
  # jbdorey[at]me.com for help.

#' Flags occurrences that are marked as absent
#' 
#' Flags occurrences that are "ABSENT" for the occurrenceStatus (or some other user-specified) column.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param PresAbs Character. The column in which the function will find "ABSENT" or "PRESENT" records.
#' Default = "occurrenceStatus"
#'
#' @return The input data with a new column called ".occurrenceAbsent" where FALSE == "ABSENT" records.
#' @export
#'
#'@importFrom dplyr %>%
#'
#' @examples
#'   # Bring in the data
#' data(beesRaw)
#'   # Run the function
#' beesRaw_out <- flagAbsent(data = beesRaw,
#' PresAbs = "occurrenceStatus")
#'   # See the result
#' table(beesRaw_out$.occurrenceAbsent, useNA = "always")
flagAbsent <-
  function(data = NULL,
           PresAbs = "occurrenceStatus") {
    .data <- .occurrenceAbsent <- NULL
    requireNamespace("dplyr")
    
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
