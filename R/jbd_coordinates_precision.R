# This function was written by James B Dorey on the 19th December 2022
# Its purpose is to flag rounded coordinates. This replaces bdc_coordinates_precision by flagging ONLY
# Occurrences where both lat AND lon are rounded; not just one.
# Please contact jbdorey[at]me.com for help

#' Flags coordinates for imprecision
#' 
#' This function flags occurrences where BOTH latitude and longitude values are rounded. This 
#' contrasts with the original function, bdc::bdc_coordinates_precision() that will flag 
#' occurrences where only one of latitude OR longitude are rounded. The BeeBDC approach saves 
#' occurrences that may have had terminal zeros rounded in one coordinate column.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param lat Character. The name of the column to use as latitude. Default = "decimalLatitude".
#' @param lon Character. The name of the column to use as longitude. Default = "decimalLongitude".
#' @param ndec Numeric. The number of decimal places to flag in decimal degrees. For example, 
#' argument value of 2 would flag occurrences with nothing in the hundredths place (0.0x).
#' @param quieter Logical. If TRUE, the functino will run a little quieter. Default = FALSE.
#'
#' @return Returns the input data frame with a new column, .rou, where FALSE indicates occurrences 
#' that failed the test.
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' beesRaw_out <- jbd_coordinates_precision(
#'   data = BeeBDC::beesRaw,
#'   lon = "decimalLongitude",
#'   lat = "decimalLatitude",
#'     # number of decimals to be tested
#'   ndec = 2
#' )
#' table(beesRaw_out$.rou, useNA = "always")

jbd_coordinates_precision <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           ndec = NULL,
           quieter = FALSE) {
    . <- .ndec_all <- NULL
    
    #### 0.0 Prep ####
    ##### 0.1 errors ####
    ###### a. FATAL errors ####
    if(is.null(ndec)){
      stop(paste0(" - No ndec was provided. This is the minimum number of decimal places",
                  " that the coordinates should have to be considered valid"))
    }
    
    #### 1.0 Run function ####
      ##### 1.1 Prepare data ####
        # Remove a .rou column if it already exists
    data <- data %>%
      dplyr::select(!tidyselect::any_of(".rou"))
    
    
      ##### 1.2 Tests ####
  # Select the columns that you want
    df <-
      data %>%
      dplyr::select({{ lon }}, {{ lat }}) %>%
      as.data.frame()
    
      # get a character vector of the length (number of decimal places) for each lat or lon
    ndec_lat <- (df[, lat] %>%
                   as.character() %>%
                   stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()
    ndec_lon <- (df[, lon] %>%
                   as.character() %>%
                   stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()
    
    rm(df)
    
    ndec_list <- as.list(ndec)
    names(ndec_list) <- paste0(".", "ndec", ndec)
    
    for (i in 1:length(ndec)) {
      ndec_list[[i]] <- (ndec_lat >= ndec[i] | ndec_lon >= ndec[i])
    }
    ndec_list <- dplyr::bind_cols(ndec_list)
    ndec_list$.ndec_all <- apply(ndec_list, 1, all) # all flagged as low decimal precision
    
    ndec_list <-
      ndec_list %>%
      dplyr::select(.ndec_all) %>%
      dplyr::rename(.rou = .ndec_all)
    
    
      #### 2.0 User output ####
    if(quieter == FALSE){
    message("jbd_coordinates_precision:\nFlagged ", 
            format(sum(!ndec_list[".rou"]), big.mark = ","), 
            " records\nThe '.rou' column was added to the database.\n")}else{
                # QUIETER message
              message("jbd_coordinates_precision:\nRemoved ", 
                      format(sum(!ndec_list[".rou"]), big.mark = ","), 
                      " records.")
            }
    
    res <- dplyr::bind_cols(data, ndec_list)
    
    return(res)
  }