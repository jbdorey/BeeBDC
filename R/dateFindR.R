# This function was created by James Dorey on the 26th of May 2022. It will attempt to find dates
  # that dont occur in the EventDate column and restore them to avoid losing those occurrences in 
  # filtering. 
# For questions, ask James Dorey at jbdorey[at]me.com

#' Find dates in other columns
#' 
#' A function made to search other columns for dates and add them to the eventDate column. 
#' The function searches the columns locality, fieldNotes, locationRemarks, and verbatimEventDate 
#' for the relevant information.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param maxYear Numeric. The maximum year considered reasonable to find. 
#' Default = lubridate::year(Sys.Date()).
#' @param minYear Numeric. The minimum year considered reasonable to find. Default = 1700.
#'
#' @importFrom stats complete.cases setNames
#'
#' @return The function results in the input occurrence data with but with updated eventDate, year, 
#' month, and day columns for occurrences where these data were a) missing and b) located in one of the 
#' searched columns.
#' 
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' # Using the example dataset, you may not find any missing eventDates are rescued (dependent on 
#' # which version of the example dataset the user inputs.
#' beesRaw_out <- dateFindR(data = beesRaw,
#'                          # Years above this are removed (from the recovered dates only)
#'                          maxYear = lubridate::year(Sys.Date()),
#'                          # Years below this are removed (from the recovered dates only)
#'                          minYear = 1700)

dateFindR <-
  function(data = NULL,
           maxYear = lubridate::year(Sys.Date()),
           minYear = 1700) {
    # locally bind variables to the function
    eventDate<-database_id<-.<-verbatimEventDate<-fieldNotes<-locationRemarks<-ymd_vEV<-
      ymd_fieldNotes<-ymd_locationRemarks<-locality<-dmy_vEV<-dmy_locality<-dmy_fieldNotes<-
      dmy_locationRemarks<-mdy_vEV<-mdy_locality<-mdy_fieldNotes<-mdy_locationRemarks<-my_vEV<-
      my_locality<-my_fieldNotes<-my_locationRemarks<-amb_vEV<-amb_locality<-amb_fieldNotes<-
      amb_locationRemarks<-year <- endTime <- startTime <- originalDateCount <- NULL
    
      # load required packages
    requireNamespace("dplyr")
    requireNamespace("lubridate")
    requireNamespace("mgsub")
    
    timeStart <- Sys.time()
    
    #### 0.0 prep ####
    writeLines(" - Preparing data...")
      # Get a count of how many eventDate rows are full
    originalDateCount <- sum(complete.cases(data$eventDate))
      # Create a new running dataset
    noDATEa <- data
      # Convert eventDate to date format 
        # - might also lose some records
    noDATEa$eventDate <- lubridate::ymd_hms(noDATEa$eventDate,
                                            truncated = 5, quiet = TRUE)
      # Find all of the records without dates
    noDATEa <- data %>% 
      dplyr::filter(any(is.na(eventDate) | eventDate %>% as.character() == ""))

    
    #### 1.0 easyDates ####
      # Retrieve dates that are much easier to recover...
    writeLines(" - Extracting dates from year, month, day columns...")
    ##### 1.1 year month day ####
      # Some records have date information in the dmy columns
    noDATEa$eventDate <- lubridate::ymd(paste(noDATEa$year, noDATEa$month, noDATEa$day, sep = "-"),
                                        quiet = TRUE, truncated = 2)
      # Save this dataset to be merged at the end...
    dmy_1 <- noDATEa %>%
      dplyr::filter(complete.cases(eventDate)) %>%
      dplyr::select(database_id, eventDate) %>%
      setNames(object = ., c("database_id", "date"))

      # Filter down to the records that again have no eventDate
    noDATEa <- noDATEa %>%
      dplyr::filter(is.na(eventDate))
    if("occurrenceYear" %in% colnames(noDATEa)){
      # Copy across the occurrenceYear column into the eventDate column
    noDATEa$eventDate <- noDATEa$occurrenceYear}
      # Save this dataset to be merged at the end...
    occYr_2 <- noDATEa %>%
      dplyr::filter(complete.cases(eventDate))%>%
      dplyr::select(database_id, eventDate) %>%
      setNames(object = ., c("database_id", "date"))
    
      ##### 1.1 Sept ####
        # Because some people write "Sept" which cannot be read by lubridate, it needs to be 
        # replaced in these columns
    noDATEa$locality <- noDATEa$locality %>%
      stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                           replacement = "Sep ")
    noDATEa$fieldNotes <- noDATEa$fieldNotes %>%
      stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                           replacement = "Sep ")
    noDATEa$locationRemarks <- noDATEa$locationRemarks %>%
      stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                           replacement = "Sep ")
    noDATEa$verbatimEventDate <- noDATEa$verbatimEventDate %>%
      stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                           replacement = "Sep ")
    

    #### 2.0 unAmb. str. dates ####
    writeLines(paste(
      " - Extracting dates from fieldNotes, locationRemarks, and verbatimEventDate ",
      "columns in unambiguous ymd, dmy, mdy, and my formats...", sep = ""))
      # Filter down to the records that again have no eventDate
    noDATEa <- noDATEa %>%
      dplyr::filter(is.na(eventDate))
      
    monthStrings <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                      "jan", "feb", "mar", "apr", "may", "jun",
                      "jul", "aug", "sep", "oct","nov", "dec",
                      "January", "February", "March", "April",
                      "May", "June","July","August",
                      "September","October","November","December",
                      "january", "february", "march", "april",
                      "may", "june","july","august",
                      "september","october","november","december",
                      "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                      "JUL", "AUG", "SEP", "OCT","NOV", "DEC",
                      "JANUARY", "FEBRUARY", "MARCH", "APRIL",
                      "MAY", "JUNE","JULY","AUGUST",
                      "SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER",
                      "sept")
    romanNumerals <- c("i","ii","iii","iv","v","vi","vii","viii","ix","x","xi","xii",
                       "I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")
    numeralConversion <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                           "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
    
      ##### 2.1 ymd ####
      # enter ymd strings...
    ymd_strings <- c(
      "[0-9]{4}[\\s- /]+[0-9]{2}[\\s- /]+[0-9]{2}",
      paste("[0-9]{4}", monthStrings,"[0-9]{2}", collapse = "|", sep = "[\\s-/]+"),
      paste("[0-9]{4}", romanNumerals,"[0-9]{2}", collapse = "|", sep = "[\\s-/]+"))
    
    # Extract the matching strings
    ymd_unambiguous <- noDATEa %>%
      dplyr::mutate(
        ymd_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = paste(ymd_strings, 
                                                              collapse = "|", sep = "")),
        ymd_fieldNotes = stringr::str_extract(fieldNotes,
                                  pattern = paste(ymd_strings, 
                                  collapse = "|", sep = "")),
        ymd_locationRemarks = stringr::str_extract(locationRemarks,
                                  pattern = paste(ymd_strings, 
                                  collapse = "|", sep = ""))
        ) %>% # END mutate
      dplyr::select(database_id, 
                    ymd_vEV,
                    ymd_fieldNotes, 
                    ymd_locationRemarks)  
    
    # FORMAT the ymd_vEV column
    ymd_unambiguous$ymd_vEV <- ymd_unambiguous$ymd_vEV %>%
      # Remove 00 values to truncate the lubridate 
      stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
      lubridate::ymd(truncated = 2, quiet = TRUE) 
      #
        # FORMAT the ymd_fieldNotes column
      ymd_unambiguous$ymd_fieldNotes <- ymd_unambiguous$ymd_fieldNotes %>%
          # Remove 00 values to truncate the lubridate 
      stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
      lubridate::ymd(truncated = 2, quiet = TRUE) 
      #
          # FORMAT the ymd_locationRemarks column
      ymd_unambiguous$ymd_locationRemarks <- ymd_unambiguous$ymd_locationRemarks %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
        lubridate::ymd(truncated = 2, quiet = TRUE) 
      #
        # Combine the columns
      ymd_keepers_21 <- ymd_unambiguous %>%
        dplyr::filter(complete.cases(ymd_vEV)|
          complete.cases(ymd_locationRemarks) | complete.cases(ymd_fieldNotes)) %>%
        tidyr::unite(col = date,
                     ymd_vEV, ymd_locationRemarks, ymd_fieldNotes, na.rm = TRUE)
        # add ymd_keepers_21 at the end
      


    ##### 2.2 dmy ####
    dmy_strings <- c(
      # 12-JUL-2002; 12 Jul 2002; 12/July/2002
    paste("[0-9]{1,2}[\\s-/ ]+", monthStrings,"[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""),
    paste("[0-9]{1,2}[\\s-/ ]+", monthStrings,"[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""),
      # 12-XII-2022; 12 XII 2022; 12 xii 2022;
    paste("[0-9]{1,2}[\\s-/ ]+", romanNumerals,"[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""),
    paste("[0-9]{1,2}[\\s-/ ]+", romanNumerals,"[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""),
      # >12 <12 1992 - dmy
    "([1][3-9]|[2-3][0-9])[\\s-/ ]+([1-9]|1[0-2])[\\s-/ ]+[0-9]{2}",
    "([1][3-9]|[2-3][0-9])[\\s-/ ]+([1-9]|1[0-2])[\\s-/ ]+[0-9]{4}")
    
        # Extract the matching strings
      dmy_unambiguous <- noDATEa %>%
          # First, remove the strings matched prior
        dplyr::filter(!database_id %in% ymd_keepers_21$database_id) %>%
        dplyr::mutate(
          dmy_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = paste(dmy_strings, 
                                                              collapse = "|", sep = "")),
          dmy_locality = stringr::str_extract(locality,
                                              pattern = paste(dmy_strings, 
                                                              collapse = "|", sep = "")),
          dmy_fieldNotes = stringr::str_extract(fieldNotes,
                                                pattern = paste(dmy_strings, 
                                                                collapse = "|", sep = "")),
          dmy_locationRemarks = stringr::str_extract(locationRemarks,
                                                     pattern = paste(dmy_strings, 
                                                                     collapse = "|", sep = ""))
        ) %>% # END mutate
        dplyr::select(database_id, dmy_vEV, dmy_locality,
                      dmy_fieldNotes, dmy_locationRemarks)  
      
      # FORMAT the dmy_vEV column
      dmy_unambiguous$dmy_vEV <- dmy_unambiguous$dmy_vEV %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("[-/ ]", romanNumerals, "[ -/]", sep = ""),
          replacement = numeralConversion) %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$|^00", replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the dmy_locality column
      dmy_unambiguous$dmy_locality <- dmy_unambiguous$dmy_locality %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("[-/ ]", romanNumerals, "[ -/]", sep = ""),
          replacement = numeralConversion) %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the dmy_fieldNotes column
      dmy_unambiguous$dmy_fieldNotes <- dmy_unambiguous$dmy_fieldNotes %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("[-/ ]", romanNumerals, "[ -/]", sep = ""),
          replacement = numeralConversion) %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the dmy_locationRemarks column
      dmy_unambiguous$dmy_locationRemarks <- dmy_unambiguous$dmy_locationRemarks %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("[-/ ]", romanNumerals, "[ -/]", sep = ""),
          replacement = numeralConversion) %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      #
      # Combine the columns
      dmy_keepers_22 <- dmy_unambiguous %>%
        dplyr::filter(complete.cases(dmy_vEV) |
                      complete.cases(dmy_locality) | 
                        complete.cases(dmy_locationRemarks) | 
                        complete.cases(dmy_fieldNotes)) %>%
        tidyr::unite(col = date, 
                     dmy_vEV, dmy_locality, dmy_locationRemarks, dmy_fieldNotes, 
                     na.rm = TRUE)
      # add dmy_keepers_22 at the end
    
    ##### 2.3 mdy ####
    mdy_strings <- c(
      # Aug 2, 2019
    paste(monthStrings,"[\\s-/ ]+[0-9]{1,2}[\\s-/, ]+[0-9]{4}", collapse = "|", sep = ""),
    paste(monthStrings,"[\\s-/ ]+[0-9]{1,2}[\\s-/, ]+[0-9]{2}", collapse = "|", sep = ""),
     # Aug 1-10 2019
    paste(monthStrings,"[0-9]+[-\\u2013][0-9]+[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""),
    paste(monthStrings,"[0-9]+[-\\u2013][0-9]+[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""),
      # V. 17 1901
    paste(romanNumerals,"[\\s-/\\. ]+[0-9]{1,2}[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""),
    paste(romanNumerals,"[\\s-/\\. ]+[0-9]{1,2}[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""),
     # <12 >12 1992 - mdy
    "([1-9]|1[0-2])[\\s- /]+([1][3-9])[\\s- /]+[0-9]{2}",
    "([1-9]|1[0-2])[\\s- /]+([2-3][0-9])[\\s- /]+[0-9]{2}",
    "([1-9]|1[0-2])[\\s- /]+([1][3-9])[\\s- /]+[0-9]{4}",
    "([1-9]|1[0-2])[\\s- /]+([2-3][0-9])[\\s- /]+[0-9]{4}")
      
      # Get the IDs to remove...
      id2remove_23 <- c(ymd_keepers_21$database_id, dmy_keepers_22$database_id)
    
      # Extract the matching strings to three columns
      mdy_unambiguous <- noDATEa %>%
        # First, remove the strings matched prior
        dplyr::filter(!database_id %in% id2remove_23) %>%
        dplyr::mutate(
          mdy_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = paste(mdy_strings, 
                                                              collapse = "|", sep = "")),
          mdy_locality = stringr::str_extract(locality,
                                              pattern = paste(mdy_strings, 
                                                              collapse = "|", sep = "")),
          mdy_fieldNotes = stringr::str_extract(fieldNotes,
                                                pattern = paste(mdy_strings, 
                                                                collapse = "|", sep = "")),
          mdy_locationRemarks = stringr::str_extract(locationRemarks,
                                                     pattern = paste(mdy_strings, 
                                                                     collapse = "|", sep = ""))
        ) %>% # END mutate
          # select a subset of columns
        dplyr::select(database_id, mdy_vEV, mdy_locality,
                      mdy_fieldNotes, mdy_locationRemarks)  
      
      # FORMAT the mdy_vEV column
      mdy_unambiguous$mdy_vEV <- mdy_unambiguous$mdy_vEV %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
          replacement = numeralConversion) %>%
        lubridate::mdy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the mdy_locality column
      mdy_unambiguous$mdy_locality <- mdy_unambiguous$mdy_locality %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
          replacement = numeralConversion) %>%
        stringr::str_replace( pattern = "^The ", replacement = "") %>%
        lubridate::mdy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the mdy_fieldNotes column
      mdy_unambiguous$mdy_fieldNotes <- mdy_unambiguous$mdy_fieldNotes %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
          replacement = numeralConversion) %>%
        lubridate::mdy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the mdy_locationRemarks column
      mdy_unambiguous$mdy_locationRemarks <- mdy_unambiguous$mdy_locationRemarks %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
          replacement = numeralConversion) %>%
        lubridate::mdy(truncated = 2, quiet = TRUE) 
      #
      # Combine the columns
      mdy_keepers_23 <- mdy_unambiguous %>%
        dplyr::filter( complete.cases(mdy_vEV) |
                       complete.cases(mdy_locality) | 
                        complete.cases(mdy_locationRemarks) | 
                        complete.cases(mdy_fieldNotes)) %>%
        tidyr::unite(col = date, 
                     mdy_vEV, mdy_locality, mdy_locationRemarks, mdy_fieldNotes, 
                     na.rm = TRUE, sep = "")
        # KEEP mdy_keepers_23 at the end
    
    
    ##### 2.4 my ####
    my_strings <- c(
      # VIII-1946
    paste(romanNumerals,"[\\s-/ \\.]+[0-9]{4}", collapse = "|", sep = ""),
      # July 1995; July, 1995
    paste(monthStrings,"[\\s-/ \\.]+[0-9]{2}", collapse = "|", sep = ""),
    paste(monthStrings,"[\\s-/ \\.]+[0-9]{4}", collapse = "|", sep = ""),
      # April 1899
    paste(monthStrings,"[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""),
    paste(monthStrings,"[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""),
      # 1899 April 
    paste("[\\s- /]+[0-9]{2}", monthStrings, collapse = "|", sep = ""),
    paste("[\\s- /]+[0-9]{4}", monthStrings, collapse = "|", sep = ""),
     # 4/1957
    "([1-9]|1[0-2])[\\s- /]+[0-9]{4}"
    )
    
      # Get the IDs to remove...
      id2remove_24 <- c(mdy_keepers_23$database_id, id2remove_23)
      
      # Extract the matching strings to three columns
      my_unambiguous <- noDATEa %>%
        # First, remove the strings matched prior
        dplyr::filter(!database_id %in% id2remove_24) %>%
        dplyr::mutate(
          my_vEV = stringr::str_extract(verbatimEventDate,
                                             pattern = paste(my_strings, 
                                                             collapse = "|", sep = "")),
          my_locality = stringr::str_extract(locality,
                                              pattern = paste(my_strings, 
                                                              collapse = "|", sep = "")),
          my_fieldNotes = stringr::str_extract(fieldNotes,
                                                pattern = paste(my_strings, 
                                                                collapse = "|", sep = "")),
          my_locationRemarks = stringr::str_extract(locationRemarks,
                                                     pattern = paste(my_strings, 
                                                                     collapse = "|", sep = ""))
        ) %>% # END mutate
        # select a subset of columns
        dplyr::select(database_id, my_vEV, my_locality,
                      my_fieldNotes, my_locationRemarks) 
      
      # FORMAT the my_vEV column
      my_unambiguous$my_vEV <- my_unambiguous$my_vEV %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, sep = ""),
          replacement = numeralConversion) %>%
        # format
        lubridate::my( quiet = TRUE) 
      #
      # FORMAT the my_locality column
      my_unambiguous$my_locality <- my_unambiguous$my_locality %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, sep = ""),
          replacement = numeralConversion) %>%
          # format
        lubridate::my(quiet = TRUE) 
      #
      # FORMAT the my_fieldNotes column
      my_unambiguous$my_fieldNotes <- my_unambiguous$my_fieldNotes %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, sep = ""),
          replacement = numeralConversion) %>%
        # format
        lubridate::my(quiet = TRUE) 
      #
      # FORMAT the my_locationRemarks column
      my_unambiguous$my_locationRemarks <- my_unambiguous$my_locationRemarks %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste("^",romanNumerals, sep = ""),
          replacement = numeralConversion) %>%
        # format
        lubridate::my(quiet = TRUE) 
      #
      
      # Combine the columns
      my_keepers_24 <- my_unambiguous %>%
        dplyr::filter(complete.cases(my_vEV) |
                      complete.cases(my_locality) | 
                        complete.cases(my_locationRemarks) | 
                        complete.cases(my_fieldNotes)) %>%
        tidyr::unite(col = date, 
                     my_vEV, my_locality, my_locationRemarks, my_fieldNotes, 
                     na.rm = TRUE, sep = "-") 
          # Remove double-ups
      my_keepers_24$date <- stringr::str_replace(my_keepers_24$date,
                                                 pattern = "-[0-9]+-[0-9]+-[0-9]+", 
                                                 replacement = "")

      
      #### 3.0 Amb. str. dates ####
      writeLines(paste(
        " - Extracting year from fieldNotes, locationRemarks, and verbatimEventDate ",
        "columns in ambiguous formats...", sep = ""))
      ambiguousDateStrings <- c(
        # dmy or mdy; 10 02 1946
        "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{2}",
        "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{2}",
        "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{4}",
        "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{4}"
      )
      
      # Get the IDs to remove...
      id2remove_30 <- c(my_keepers_24$database_id, id2remove_24)
      
      # Extract the matching strings to three columns
      ambiguousNames <- noDATEa %>%
        # First, remove the strings matched prior
        dplyr::filter(!database_id %in% id2remove_30) %>%
        dplyr::mutate(
          amb_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = paste(ambiguousDateStrings, 
                                                              collapse = "|", sep = "")),
          amb_locality = stringr::str_extract(locality,
                                             pattern = paste(ambiguousDateStrings, 
                                                             collapse = "|", sep = "")),
          amb_fieldNotes = stringr::str_extract(fieldNotes,
                                               pattern = paste(ambiguousDateStrings, 
                                                               collapse = "|", sep = "")),
          amb_locationRemarks = stringr::str_extract(locationRemarks,
                                                    pattern = paste(ambiguousDateStrings, 
                                                                    collapse = "|", sep = ""))
        ) %>% # END mutate
        # select a subset of columns
        dplyr::select(database_id, amb_vEV, amb_locality,
                      amb_fieldNotes, amb_locationRemarks) 
      
      # FORMAT the amb_vEV column
      ambiguousNames$amb_vEV <- ambiguousNames$amb_vEV %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                             replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      # FORMAT the amb_locality column
      #
      ambiguousNames$amb_locality <- ambiguousNames$amb_locality %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                             replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      # FORMAT the amb_fieldNotes column
      ambiguousNames$amb_fieldNotes <- ambiguousNames$amb_fieldNotes %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                             replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      #
      # FORMAT the amb_locationRemarks column
      ambiguousNames$amb_locationRemarks <- ambiguousNames$amb_locationRemarks %>%
      # Remove 00 values to truncate the lubridate 
      stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                             replacement = "") %>%
        lubridate::dmy(truncated = 2, quiet = TRUE) 
      
      
      
      # Combine the columns
      amb_keepers_30 <- ambiguousNames %>%
        dplyr::filter(complete.cases(amb_vEV)|
                      complete.cases(amb_locality) | 
                        complete.cases(amb_locationRemarks) | 
                        complete.cases(amb_fieldNotes)) %>%
        tidyr::unite(col = date, amb_vEV,  
                     amb_locality, amb_locationRemarks, amb_fieldNotes, 
                     na.rm = TRUE)
      # KEEP amb_keepers_30 at the end
      
      #### 4.0 Format+combine ####
      writeLines(paste(
        " - Formating and combining the new data..", sep = ""))
        ##### 4.1 formatting... ####
        # Extract only the date from occYr_2
      occYr_2$date <- as.character(occYr_2$date) %>% lubridate::ymd_hms() %>% lubridate::date()
        # Set as date format...
      ymd_keepers_21$date <- lubridate::ymd(ymd_keepers_21$date)
      dmy_keepers_22$date <- lubridate::ymd(dmy_keepers_22$date)
      mdy_keepers_23$date <- lubridate::ymd(mdy_keepers_23$date)
      my_keepers_24$date <- lubridate::ymd(my_keepers_24$date)
        # merge these data...
      
      saveTheDates <- dplyr::bind_rows(dmy_1, occYr_2, 
        ymd_keepers_21, dmy_keepers_22, mdy_keepers_23) %>%
        dplyr::select(database_id, date) 
      
      
      ##### 4.2 Full dates ####
        # Join these dates to the original rows...
      datesOut_full <- data %>% 
        dplyr::right_join(saveTheDates, 
                          by = "database_id")
        # Fill the eventDate column
      datesOut_full$eventDate <- lubridate::ymd(datesOut_full$date)
        # Fill the year, month, and day columns 
      datesOut_full$year <- lubridate::year(datesOut_full$date)
      datesOut_full$month <- lubridate::month(datesOut_full$date)
      datesOut_full$day <- lubridate::day(datesOut_full$date)
        # Remove records with non-sensical years
      datesOut_full <- datesOut_full %>%
          # remove FUTURE dates
        dplyr::filter(!year > maxYear) %>%
          # Remove PAST dates 
        dplyr::filter(!year < minYear) %>%
         # remove the date column
        dplyr::select(!date)
        
      
      ##### 4.3 No day ####
        # Join these dates to the original rows...
      datesOut_noDay <- data %>% 
        dplyr::right_join(my_keepers_24, 
                          by = "database_id")
        # Fill the eventDate column
      datesOut_noDay$eventDate <- lubridate::ymd(datesOut_noDay$date, quiet = TRUE)
        # Fill the year, month, and day columns 
      datesOut_noDay$year <- lubridate::year(datesOut_noDay$date)
      datesOut_noDay$month <- lubridate::month(datesOut_noDay$date)
        # Remove records with non-sensical years
      datesOut_noDay <- datesOut_noDay %>%
         # remove FUTURE dates
        dplyr::filter(!year > maxYear) %>%
         # Remove PAST dates 
        dplyr::filter(!year < minYear) %>%
         # remove the date column
        dplyr::select(!date)
      
      
      
      ##### 4.4 No month ####
        # Join these dates to the original rows...
      datesOut_noMonth <- data %>% 
        dplyr::right_join(amb_keepers_30, 
                          by = "database_id")
        # Fill the eventDate column
      datesOut_noMonth$eventDate <- lubridate::ymd(datesOut_noMonth$date, quiet = TRUE)
        # Fill the year, month, and day columns 
      datesOut_noMonth$year <- lubridate::year(datesOut_noMonth$date)
        # Remove records with non-sensical years
      datesOut_noMonth <- datesOut_noMonth %>%
          # remove FUTURE dates
        dplyr::filter(!year > maxYear) %>%
         # Remove PAST dates 
        dplyr::filter(!year < minYear) %>%
          # remove the date column
        dplyr::select(!date)
      

    #### 5.0 Merge ####
      writeLines(paste(
        " - Merging all data, nearly there...", sep = ""))
        # Get all of the changed rows together
      datesMerged <- dplyr::bind_rows(
        datesOut_full, datesOut_noDay, datesOut_noMonth)
        
        # Format the original eventDate column into a new sheet - datesOut
      datesOut <- data
      datesOut$eventDate <- lubridate::ymd_hms(datesOut$eventDate,
                                 truncated = 5, quiet = TRUE)
      # Replace these in the original dataset
      datesOut <- datesOut %>% 
          # Remove the dates that are to be replaced
        dplyr::filter(!database_id %in% datesMerged$database_id) 
      
      # Extract year, month, and day where possible
        # year
      datesOut$year <- ifelse(is.na(datesOut$year),
                              lubridate::year(datesOut$eventDate),
                              datesOut$year)
        # month
      datesOut$month <- ifelse(is.na(datesOut$month),
                               lubridate::month(datesOut$eventDate),
                               datesOut$month)
        # day
      datesOut$day <- ifelse(is.na(datesOut$day),
                               lubridate::day(datesOut$eventDate),
                               datesOut$day)
      
      # Remove the months and days where the year is incorrect.
      datesOut$month <- ifelse(datesOut$year > maxYear | datesOut$year < minYear,
                               NA,
                               datesOut$month)
      datesOut$day <- ifelse(datesOut$year > maxYear | datesOut$year < minYear,
                             NA,
                             datesOut$day)
      # Remove non-sensical years now
      datesOut$year <- ifelse(datesOut$year > maxYear | datesOut$year < minYear,
                     NA,
                     datesOut$year)
      # Now check and replace the eventDate column if it's out of range
      datesOut$eventDate <- ifelse(lubridate::year(datesOut$eventDate) > maxYear | 
                                     lubridate::year(datesOut$eventDate) < minYear,
                                     NA,
                                     as.character(datesOut$eventDate))
      

        # For simplicity's sake, return the date columns as character...
      datesOut$eventDate  <- as.character(datesOut$eventDate)
      datesMerged$eventDate  <- as.character(datesMerged$eventDate)
      
      # MERGE all datsets
      dates_complete <- data %>%
        dplyr::mutate(eventDate = as.character(eventDate)) %>%
          # REMOVE the meddled-with rows
        dplyr::filter(!database_id %in% c( datesMerged$database_id, datesOut$database_id)) %>%
          # Merge the new rows back in
        dplyr::bind_rows(datesMerged, datesOut)
      
        # Return to date format <3 
      dates_complete$eventDate <- lubridate::ymd_hms(dates_complete$eventDate,
                         truncated = 5, quiet = TRUE)
      
      # Plot the dates
      # graphics::hist(dates_complete$eventDate, breaks = 100,
      #      main = "Histogram of eventDate output")
      
      timeEnd <- Sys.time()
    
    # Return user output
    writeLines(
      paste(
        " - Finished. \n",
        "We now have ",
        format((sum(complete.cases(dates_complete$eventDate)) - originalDateCount),
               big.mark = ","), 
        " more full eventDate cells than in the input data.\n",
        "We modified dates in \n",
        format(nrow(datesMerged), big.mark = ","), " occurrences.\n",
        " - As it stands, there are ", 
        format( sum(complete.cases(dates_complete$eventDate)), big.mark = ","),
        " complete eventDates and ", 
        format( sum(is.na(dates_complete$eventDate)), big.mark = ","), 
        " missing dates.\n", 
        " - There are also ", 
        format( sum(complete.cases(dates_complete$year)), big.mark = ","), 
        " complete year occurrences to filter from. This is up from an initial count of ",
        format( sum(complete.cases(data$year)), big.mark = ","),
        " At this rate, you will stand to lose ",
        format( sum(is.na(dates_complete$year)), big.mark = ","), 
        " occurrences on the basis of missing",
        " year",
        " - Operation time: ", (timeEnd - timeStart)," ",
        units(round(timeEnd - timeStart, digits = 2)),
      sep = "")
    )
    return(dates_complete)
  }
