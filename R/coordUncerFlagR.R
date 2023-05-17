# This function is a modification of a bdc function and flags columns as FALSE when they have a  
# coordiante uncertainty over a user-set threshold. 
# This function was written on the 2nd of August 2022 by James Dorey. Email James at
# jbdorey[at]me.com for help.


#' Flag occurrences with an uncertainty threshold
#' 
#' To use this function, the user must choose a column, probably "coordinateUncertaintyInMeters" 
#' and a threshold above which occurrences will be flagged for geographic uncertainty.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param uncerColumn Character. The column to flag uncertainty in.
#' @param threshold Numeric. The uncertainty threshold. Values equal to, or greater than, this 
#' threshold will be flagged.
#'
#' @return The input data with a new column, .uncertaintyThreshold.
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' # Run the function
#' beesRaw_out <- coordUncerFlagR(data = beesRaw,
#'                                uncerColumn = "coordinateUncertaintyInMeters",
#'                                threshold = 1000)
#' # View the output
#' table(beesRaw_out$.uncertaintyThreshold, useNA = "always")
coordUncerFlagR <-
  function(data = NULL,
           uncerColumn = "coordinateUncertaintyInMeters",
           threshold = NULL) {
    .data <- .occurrenceAbsent <- NULL
    requireNamespace("dplyr")
    
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
