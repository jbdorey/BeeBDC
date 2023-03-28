# This function was written by James B Dorey on the 19th December 2022
# Its purpose is to flag rounded coordinates. This replaces bdc_coordinates_precision by flagging ONLY
# Occurrences where both lat AND lon are rounded; not just one.
# Please contact jbdorey@me.com for help

jbd_coordinates_precision <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           ndec = NULL) {
    . <- .ndec_all <- NULL
    
    #### 0.0 Prep ####
    ##### 0.1 errors ####
    ###### a. FATAL errors ####
    if(is.null(ndec)){
      stop(paste0(" — No ndec was provided. This is the minimum number of decimal places",
                  " that the coordinates should have to be considered valid"))
    }
    
  # Select the columns that you want
    df <-
      data %>%
      dplyr::select({{ lon }}, {{ lat }}) %>%
      as.data.frame()
    
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
    
    message("jbd_coordinates_precision:\nFlagged ", 
            format(sum(!ndec_list[".rou"]), big.mark = ","), 
            " records\nThe '.rou' column was added to the database.\n")
    
    res <- dplyr::bind_cols(data, ndec_list)
    
    return(res)
  }