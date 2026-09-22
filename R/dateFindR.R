# This function was created by James Dorey on the 26th of May 2022. It will attempt to find dates
  # that dont occur in the EventDate column and restore them to avoid losing those occurrences in 
  # filtering. 
# For questions, ask James Dorey at jbdorey[at]me.com

#' Find dates in other columns
#' 
#' A function made to search other columns for dates and add them to the eventDate column. 
#' The function searches the columns locality, fieldNotes, locationRemarks, and verbatimEventDate 
#' for the relevant information. Additionally, for date ranges in the eventDate column, these will
#' Be translated into startDayOfYear and endDayOfYear and the eventDate will be kept as the last 
#' date, but formatted in the correct format. Ambiguous dates that can't be parsed reliably as 
#' day-month-year or month-day-year will be rounded to the nearest certainty; if month or day can
#' be identified with a spelled-out month or a day >12, these wil be correctly formatted. 
#' Year-month-day... is the most-reliable format
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

dateFindR <- function(data = NULL,
           maxYear = lubridate::year(Sys.Date()),
           minYear = 1700) {
    # locally bind variables to the function
    eventDate<-database_id<-.<-verbatimEventDate<-fieldNotes<-locationRemarks<-ymd_vEV<-
      ymd_fieldNotes<-ymd_locationRemarks<-locality<-dmy_vEV<-dmy_locality<-dmy_fieldNotes<-
      dmy_locationRemarks<-mdy_vEV<-mdy_locality<-mdy_fieldNotes<-mdy_locationRemarks<-my_vEV<-
      my_locality<-my_fieldNotes<-my_locationRemarks<-amb_vEV<-amb_locality<-amb_fieldNotes<-
      amb_locationRemarks<-year <- endTime <- startTime <- originalDateCount <- 
      eventDate_in <- day <- startDayOfYear <- endDayOfYear <- ymd_eventDate_in <- 
      dmy_eventDate_in <- mdy_eventDate_in <- NULL
    
      # load required packages
    requireNamespace("dplyr")
    requireNamespace("lubridate")
    requireNamespace("mgsub")
    
    timeStart <- Sys.time()
    
    #### 0.0 prep ####
      ##### 0.1 Functions ####
      # Make a function to deal with ranges
    rangeFinder <- function(rangeData = NULL, rangeColumn = "eventDate_in",
                            timeStrings_in = timeStrings_robust){
      dateCol <- dateStart <- dateEnd <- startDayOfYear <- startDate <- endDayOfYear <- endDate <- NULL
      
      # Check and add startDayOfYear and endDayOfYear
      if(!"startDayOfYear" %in% colnames(rangeData)){
        rangeData <- rangeData %>%
          dplyr::mutate(startDayOfYear = NA_integer_)
      }
      if(!"endDayOfYear" %in% colnames(rangeData)){
        rangeData <- rangeData %>%
          dplyr::mutate(endDayOfYear = NA_integer_)
      }
      
      # Temporarily rename the rangeColumn to "dateCol" within the function
      rangeData <- rangeData %>%
        dplyr::rename("dateCol" = tidyselect::any_of(rangeColumn)) %>% 
          # Make sure that dateCol is a character column
        dplyr::mutate(dateCol = as.character(dateCol))
      
        # Sort out the ranges
      rangeData <- rangeData %>% 
      # DATE START
      dplyr::mutate(
        dateStart = dplyr::if_else(# Extract first for...
          # /
          stringr::str_detect(dateCol, dateRangeStrings_forwardSlash),
          stringr::str_extract(dateCol, ".*/") %>% stringr::str_remove("\\/"),
          # , 
          dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_comma),
                         stringr::str_extract(dateCol, ".*,") %>% stringr::str_remove("\\,"),
                         # to
                         dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_to),
                                        stringr::str_extract(dateCol, ".*to ") %>% stringr::str_remove("to"),
                                        # -
                                        dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_dash),
                                                       stringr::str_extract(dateCol, ".*-") %>% stringr::str_remove("\\-"),
                                                       # startDateTime:
                                                       dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_StEn),
                                                                      stringr::str_extract(dateCol, ".*EndDateTime:|.*endDateTime:") %>% 
                                                                        stringr::str_remove_all("[a-zA-Z]+|\\:|\\;|\\s"),
                                                                      dateCol))))),
        dateStart = stringr::str_squish(dateStart),
        .after = dateCol) %>%
        # DATE END
        dplyr::mutate(
          dateEnd = dplyr::if_else(
            # Extract first for...
            # /
            stringr::str_detect(dateCol, dateRangeStrings_forwardSlash),
            stringr::str_extract(dateCol, "/.*") %>% stringr::str_remove("\\/"),
            # , 
            dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_comma),
                           stringr::str_extract(dateCol, ",.*") %>% stringr::str_remove("\\,"),
                           # to
                           dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_to),
                                          stringr::str_extract(dateCol, " to.*") %>% stringr::str_remove("to"),
                                          # -
                                          dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_dash),
                                                         stringr::str_extract(dateCol, "-.*") %>% stringr::str_remove("\\-"),
                                                         # EndDateTime:
                                                         dplyr::if_else(stringr::str_detect(dateCol, dateRangeStrings_StEn),
                                                                        stringr::str_extract(dateCol, "EndDateTime:.*|endDateTime:.*") %>%
                                                                          stringr::str_remove_all("[a-zA-Z]+|\\:|\\;|\\s"),
                                                                        dateCol))))),
          dateEnd = stringr::str_squish(dateEnd),
          .after = dateStart) %>%
        # IF the dateStart date has only day or only day month...
        dplyr::mutate(# DAY ONLY 
          dateStart = dplyr::if_else(stringr::str_detect(dateStart, "^[0-9]{1,2}$"),
                                     # Add in the end of dateEnd — TRUE1
                                     stringr::str_c(dateStart, 
                                                    stringr::str_remove(dateEnd,"^[0-9]{1,2}")),
                                     # DAY MONTH — FALSE
                                     dplyr::if_else(stringr::str_detect(dateStart, 
                                                                        stringr::str_c("^[0-9]{1,2}[\\s\\/\\-]", monthStrings,"$",
                                                                                       collapse = "|")),
                                                    # Add in the end of dateEnd — TRUE
                                                    stringr::str_c(dateStart, 
                                                                   stringr::str_remove(dateEnd,
                                                                                       stringr::str_c("^[0-9]{1,2}[\\s\\/\\-]", monthStrings,
                                                                                                      collapse = "|"))),
                                                    # FALSE
                                                    dateStart)
          ),
          .after = dateStart) %>% # END mutate
        # swap Roman numerals
        dplyr::mutate(
          dateEnd = mgsub::mgsub(dateEnd,
                                 pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
                                 replacement = paste0(" ", numeralConversion, " "))  %>% 
            stringr::str_squish() %>% 
            # remove "Sept."
            stringr::str_replace(pattern = "[Ss]ept[\\s-/\\.]",replacement = "Sep "),
          dateStart = mgsub::mgsub(dateStart,
                                   pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
                                   replacement = paste0(" ", numeralConversion, " "))  %>% 
            stringr::str_squish() %>% 
            # remove "Sept."
            stringr::str_replace(pattern = "[Ss]ept[\\s-/\\.]",replacement = "Sep "),
          eventDate = mgsub::mgsub(dateEnd,
                                   pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
                                   replacement = paste0(" ", numeralConversion, " "))  %>% 
            stringr::str_squish() %>% 
            # remove "Sept."
            stringr::str_replace(pattern = "[Ss]ept[\\s-/\\.]", replacement = "Sep ")
        ) %>% 
        # Guess the formats
        dplyr::mutate(
          endDate = dateEnd %>% lubridate::parse_date_time(., timeStrings_in, quiet = TRUE) %>%
            lubridate::yday(),
          startDate = dateStart %>% lubridate::parse_date_time(., timeStrings_in, quiet = TRUE) %>%
            lubridate::yday(),
          eventDate = eventDate %>% lubridate::parse_date_time(., timeStrings_in, quiet = TRUE)
        ) %>%
        dplyr::mutate(
          startDayOfYear = dplyr::if_else(is.na(startDayOfYear), startDate, startDayOfYear),
          endDayOfYear = dplyr::if_else(is.na(endDayOfYear), endDate, startDayOfYear)
        ) %>%
        dplyr::select(!tidyselect::any_of(c("endDate", "startDate", "dateEnd", "dateStart")))
      # Return the rangeColumn name to it's original state
      names(rangeData)[names(rangeData) == "dateCol"] <- rangeColumn
      return(rangeData)
    }
    
      ##### 0.2 Strings ####
    ###### a. month strings ####
    monthStrings <- c("september","october","november","december",
                      "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                      "JUL", "AUG", "SEP", "OCT","NOV", "DEC",
                      "JANUARY", "FEBRUARY", "MARCH", "APRIL",
                      "MAY", "JUNE","JULY","AUGUST",
                      "SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER",
                      "January", "February", "March", "April",
                      "May", "June","July","August",
                      "September","October","November","December",
                      "january", "february", "march", "april",
                      "may", "june","july","august",
                      "sept\\.","Sept\\.",
                      "sept",
                      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                      "jan", "feb", "mar", "apr", "may", "jun",
                      "jul", "aug", "sep", "oct","nov", "dec")
    romanNumerals <- c("i","ii","iii","iv","v","vi","vii","viii","ix","x","xi","xii",
                       "I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")
    numeralConversion <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                           "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
    
          ###### b. date ranges ####
    writeLines(" - Preparing data...")
      # Get a count of how many eventDate rows are full
    originalDateCount <- sum(stats::complete.cases(data$eventDate))
    
      # Create sets of strings that can be used to match date ranges based on delimiter
    dateRangeStrings_forwardSlash <- stringr::str_c(
        # "2015-08-14T16:00:00/2015-08-15T16:00:00"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\/[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",
        # "2019-07-29T14:00:00+02:00/2019-07-29T15:00:00+02:00"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\+[0-9]{2}:[0-9]{2}\\/[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\+[0-9]{2}:[0-9]{2}",
       # "2015-08-14/2015-08-15"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\/[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      # 9.viii.2000/11.viii.2000
      stringr::str_c("[0-9]{1,2}[-\\.\\s]",
                     "(", stringr::str_c(romanNumerals, collapse = "|"), ")",
                     "[-\\.\\s][0-9]{4}\\/[0-9]{1,2}[-\\.\\s]",
                     "(", stringr::str_c(romanNumerals, collapse = "|"), ")",
                     "[-\\.\\s][0-9]{4}", collapse = "|"),
      # 28 Jun 2005/29 Jun 2005
      stringr::str_c("[0-9]{1,2}[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"), ")",
                     "[\\s\\/-][0-9]{4}\\/[0-9]{1,2}[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"), ")",
                     "[\\s\\/-][0-9]{4}", collapse = "|"),
      sep = "|"
    )
    dateRangeStrings_comma <- stringr::str_c(
      # "2015-03-02T17:37:47Z,2015-03-02 05:37 PM"  
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z\\,[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z\\,[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z",
      # "2015-08-14,2015-08-15"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\,[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      sep = "|"
    )
    dateRangeStrings_to <- stringr::str_c(
      # "2004-08-05 to 2004-08-06"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} to [0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      # 1940-06-26 to 1940-06-28
      "[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}\\sto\\s[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}",
      # "07 to 21 September 2009"
      stringr::str_c("[0-9]{1,2}\\sto\\s[0-9]{1,2}\\s",
                     monthStrings,"[\\s\\/-][0-9]{4}", collapse = "|"),
      # 20 April to 04 May 2009
      stringr::str_c("[0-9]{1,2}[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-]to[\\s\\/-][0-9]{1,2}[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{4}",  collapse = "|"),
      # 13-Jun-2018 to 13-Jul-2018
      stringr::str_c("[0-9]{1,2}[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{4}",
                     "[\\s\\/-]to[\\s\\/-]","[0-9]{1,2}[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{4}",  collapse = "|"),
      # Nov 15 to Jan 10.1918
      stringr::str_c("(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{1,2}",
                     "[\\s\\/-]to[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{1,2}",
                     "[\\s\\/-][0-9]{4}",  collapse = "|"),
      # Aug 30 1921 to Sept. 5 1921
      stringr::str_c( "(",stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{1,2}[\\s\\/-][0-9]{4}",
                     "[\\s\\/-]to[\\s\\/-]",
                     "(", stringr::str_c(monthStrings, collapse = "|"),")",
                     "[\\s\\/-][0-9]{1,2}",
                     "[\\s\\/-][0-9]{4}",  collapse = "|"),
      sep = "|"
    )
    dateRangeStrings_dash <- stringr::str_c(
      # "28 September - 4 October 2005"
      stringr::str_c( "[0-9]{1,2}\\s",
                      "(",stringr::str_c(monthStrings, collapse = "|"),")",
                      "[\\s\\/-]-[\\s\\/-]", "[0-9]{1,2}\\s",
                      "(", stringr::str_c(monthStrings, collapse = "|"),")",
                      "[\\s\\/-][0-9]{4}",  collapse = "|"),
      # 2 October 2003 - 3 October 2003
      stringr::str_c( "[0-9]{1,2}\\s",
                      "(",stringr::str_c(monthStrings, collapse = "|"),")",
                      "[\\s\\/-]","[0-9]{4}",
                      "[\\s\\/-]-[\\s\\/-]", "[0-9]{1,2}\\s",
                      "(", stringr::str_c(monthStrings, collapse = "|"),")",
                      "[\\s\\/-][0-9]{4}",  collapse = "|"),
      # 15-17 OCTOBER 1976 Or 15-17 October 1976
      stringr::str_c( "[0-9]{1,2}-[0-9]{1,2}","[\\s\\/-]",
                      "(",stringr::str_c(monthStrings, collapse = "|"),")",
                      "[\\s\\/-][0-9]{4}",  collapse = "|"),
      sep = "|"
    )
    dateRangeStrings_StEn <- stringr::str_c(
      # "startDateTime:201908301000xx; EndDateTime:201908301000xx"
      "startDateTime:",
      "EndDateTime:",
      sep = "|"
    )
    
    
      # Testing for exceptions
    if(FALSE){
               TEST <- c("2015-08-14T16:00:00/2015-08-15T16:00:00",
                         "2015-08-14/2015-08-15",
                           # comma 
                         "2015-03-02T17:37:47Z,2015-03-02 05:37 PM",
                           # To
                         "2004-08-05 to 2004-08-06",
                         "07 to 21 September 2009",
                         "28 September - 4 October 2005",
                         "20 April to 04 May 2009",
                         "2 October 2003 - 3 October 2003",
                         "13-Jun-2018 to 13-Jul-2018",
                         "1940-06-26 to 1940-06-28",
                         "Nov 15 to Jan 10.1918",
                         "15-17 October 1976",
                         "15-17 OCTOBER 1976",
                         "Aug 30 1921 to Sept. 5 1921",
                         "startDateTime:201908301000xx; EndDateTime:201908301000xx",
                         "2019-07-29T14:00:00+02:00/2019-07-29T15:00:00+02:00",
                         "9.viii.2000/11.viii.2000",
                         "28 Jun 2005/29 Jun 2005")
               
               stringr::str_detect(TEST, stringr::str_c(
                 dateRangeStrings_forwardSlash, dateRangeStrings_comma,
                 dateRangeStrings_StEn, 
                 dateRangeStrings_to, 
                 dateRangeStrings_dash,
                 sep = "|"
               ))
               stringr::str_detect("15-17 OCTOBER 1976", stringr::str_c(
                 #dateRangeStrings_forwardSlash, dateRangeStrings_comma,
                 #dateRangeStrings_StEn, 
                 #dateRangeStrings_to, 
                 dateRangeStrings_dash,
                 sep = "|"
               ))
    } # END testing
    
    
      ###### c. time parse ####
      # The robust time strings
    timeStrings_robust <- c("Ymd", "YmdHM", "YmdH","YmdHMS","YmdHMSz",
                      # Explicit day MONTH year
                     "dBy","dBY HMS",
                      # Explicit MONTH day year
                     "Bdy","BdY HMS")
    
    
    ##### 0.3 Prep eventDate and find ranges ####
      # Create a new running dataset
    noDATEa <- data %>%
        # Save the original eventDate column
      dplyr::mutate(eventDate_in = eventDate,
                    .before = eventDate) %>%
      rangeFinder(rangeData = .,
                  rangeColumn = "eventDate_in")

    
      # Save this dataset to be merged at the end...
    ymd_hms_0 <- noDATEa %>%
      dplyr::filter(stats::complete.cases(eventDate)) %>%
      dplyr::select(database_id, eventDate, startDayOfYear, endDayOfYear) %>%
      stats::setNames(object = ., c("database_id", "date", "startDayOfYear", "endDayOfYear"))
    
    #### 1.0 easyDates ####
      # Retrieve dates that are much easier to recover...
    writeLines(" - Extracting dates from year, month, day columns...")
    ##### 1.1 year month day ####
    # Filter down to the records that again have no eventDate
    noDATEa <- noDATEa %>%
      dplyr::filter(is.na(eventDate))
      # Some records have date information in the dmy columns that can easily be retrieved
    noDATEa <- noDATEa %>%
      dplyr::mutate(eventDate = dplyr::if_else(is.na(as.character(eventDate)),
                                               lubridate::ymd(stringr::str_c(year, month, day,
                                                                             sep = "-"),
                                                              quiet = TRUE, truncated = 2),
                                               eventDate))

          # Save this dataset to be merged at the end...
    dmy_1 <- noDATEa %>%
      dplyr::filter(stats::complete.cases(eventDate)) %>%
      dplyr::select(database_id, eventDate) %>%
      stats::setNames(object = ., c("database_id", "date"))

      # Filter down to the records that again have no eventDate
    noDATEa <- noDATEa %>%
      dplyr::filter(is.na(eventDate))
    if("occurrenceYear" %in% colnames(noDATEa)){
      # Copy across the occurrenceYear column into the eventDate column
    noDATEa$eventDate <- noDATEa$occurrenceYear}
      # Save this dataset to be merged at the end...
    occYr_2 <- noDATEa %>%
      dplyr::filter(stats::complete.cases(eventDate))%>%
      dplyr::select(database_id, eventDate) %>%
      stats::setNames(object = ., c("database_id", "date"))
    
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
    writeLines(paste0(
      " - Extracting dates from fieldNotes, locationRemarks, and verbatimEventDate ",
      "columns in unambiguous ymd, dmy, mdy, and my formats..."))
      # Filter down to the records that again have no eventDate
    noDATEa <- noDATEa %>%
      dplyr::filter(is.na(eventDate))
    
      ##### 2.1 ymd ####
      # enter ymd strings...
    ymd_strings <- c(
      "[0-9]{4}[\\s- /]+[0-9]{2}[\\s- /]+[0-9]{2}",
      paste("[0-9]{4}", monthStrings,"[0-9]{2}", collapse = "|", sep = "[\\s-/]+"),
      paste("[0-9]{4}", romanNumerals,"[0-9]{2}", collapse = "|", sep = "[.\\s-/]+"),
      # "2015-08-14T16:00:00/2015-08-15T16:00:00"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\/[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",
      # "2019-07-29T14:00:00+02:00/2019-07-29T15:00:00+02:00"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\+[0-9]{2}:[0-9]{2}\\/[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\+[0-9]{2}:[0-9]{2}",
      # "2015-08-14/2015-08-15"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\/[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      # "2015-03-02T17:37:47Z,2015-03-02 05:37 PM"  
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z\\,[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z\\,[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z",
      # "2015-08-14,2015-08-15"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\,[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      # "2004-08-05 to 2004-08-06"
      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} to [0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
      # 1940-06-26 to 1940-06-28
      "[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}\\sto\\s[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}")
    
    # Extract the matching strings
    ymd_unambiguous <- noDATEa %>%
      dplyr::mutate(
        ymd_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = paste0(ymd_strings, 
                                                              collapse = "|")),
        ymd_fieldNotes = stringr::str_extract(fieldNotes,
                                  pattern = paste0(ymd_strings, 
                                  collapse = "|")),
        ymd_locationRemarks = stringr::str_extract(locationRemarks,
                                  pattern = paste0(ymd_strings, 
                                  collapse = "|")),
        ymd_eventDate_in = stringr::str_extract(eventDate_in,
                                                   pattern = paste0(ymd_strings, 
                                                                    collapse = "|"))
        ) %>% # END mutate
      dplyr::select(database_id, 
                    ymd_vEV,
                    ymd_fieldNotes, 
                    ymd_locationRemarks,
                    ymd_eventDate_in)  
    
    # FORMAT the ymd_vEV column
    ymd_unambiguous$ymd_vEV <- ymd_unambiguous$ymd_vEV %>%
      # Remove 00 values to truncate the lubridate 
      stringr::str_replace(pattern = "/00$|/00/00$", replacement = "")
      #
        # FORMAT the ymd_fieldNotes column
      ymd_unambiguous$ymd_fieldNotes <- ymd_unambiguous$ymd_fieldNotes %>%
          # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 
      #
          # FORMAT the ymd_locationRemarks column
      ymd_unambiguous$ymd_locationRemarks <- ymd_unambiguous$ymd_locationRemarks %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 
      #
      # FORMAT the ymd_eventDate column
      ymd_unambiguous$ymd_eventDate_in <- ymd_unambiguous$ymd_eventDate_in %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 

        # Combine the columns
      ymd_keepers_21 <- ymd_unambiguous %>%
        dplyr::filter(stats::complete.cases(ymd_vEV)|
                        stats::complete.cases(ymd_locationRemarks) | stats::complete.cases(ymd_fieldNotes) |
                        stats::complete.cases(ymd_eventDate_in)) %>%
        tidyr::unite(col = date,
                     ymd_vEV, ymd_locationRemarks, ymd_fieldNotes, ymd_eventDate_in, na.rm = TRUE) %>% 
        rangeFinder(rangeData = .,
                    rangeColumn = "date")
        # add ymd_keepers_21 at the end
      

    ##### 2.2 dmy ####
    dmy_strings <- stringr::str_c(
      # 12-JUL-2002; 12 Jul 2002; 12/July/2002
    paste0("[0-9]{1,2}[\\s-/]+", monthStrings,"[\\s-/]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # 12-XII-2022; 12 XII 2022; 12 xii 2022;
    paste0("[0-9]{1,2}[.\\s-/]+", romanNumerals,"[.\\s-/]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # >12 <12 1992 - dmy
    "([1][3-9]|[2-3][0-9])[\\s-/ ]+(0[1-9]|[1-9]|1[0-2])[\\s-/]+([0-9]{4}|[0-9]{2})",
    stringr::str_c("[0-9]{1,2}[-\\.\\s]",romanNumerals,
                   "[-\\.\\s][0-9]{4}\\/[0-9]{1,2}[-\\.\\s]",
                   romanNumerals,"[-\\.\\s][0-9]{4}", collapse = "|"),
    # 28 Jun 2005/29 Jun 2005
    stringr::str_c("[0-9]{1,2}[\\s\\/-]",monthStrings,
                   "[\\s\\/-][0-9]{4}\\/[0-9]{1,2}[\\s\\/-]",
                   monthStrings,"[\\s\\/-][0-9]{4}", collapse = "|"),
    # "07 to 21 September 2009"
    stringr::str_c("[0-9]{1,2}\\sto\\s[0-9]{1,2}\\s",
                   monthStrings,"[\\s\\/-][0-9]{4}", collapse = "|"),
    # 20 April to 04 May 2009
    stringr::str_c("[0-9]{1,2}[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-]to[\\s\\/-][0-9]{1,2}[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{4}",  collapse = "|"),
    # 13-Jun-2018 to 13-Jul-2018
    stringr::str_c("[0-9]{1,2}[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{4}",
                   "[\\s\\/-]to[\\s\\/-]","[0-9]{1,2}[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{4}",  collapse = "|"),
    # Nov 15 to Jan 10.1918
    stringr::str_c("(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{1,2}",
                   "[\\s\\/-]to[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{1,2}",
                   "[\\s\\/-][0-9]{4}",  collapse = "|"),
    # Aug 30 1921 to Sept. 5 1921
    stringr::str_c( "(",stringr::str_c(monthStrings, collapse = "|"),")",
                    "[\\s\\/-][0-9]{1,2}[\\s\\/-][0-9]{4}",
                    "[\\s\\/-]to[\\s\\/-]",
                    "(", stringr::str_c(monthStrings, collapse = "|"),")",
                    "[\\s\\/-][0-9]{1,2}",
                    "[\\s\\/-][0-9]{4}",  collapse = "|"),
    # 9.viii.2000/11.viii.2000
    stringr::str_c("[0-9]{1,2}[-\\.\\s]",
                   "(", stringr::str_c(romanNumerals, collapse = "|"), ")",
                   "[-\\.\\s][0-9]{4}\\/[0-9]{1,2}[-\\.\\s]",
                   "(", stringr::str_c(romanNumerals, collapse = "|"), ")",
                   "[-\\.\\s][0-9]{4}", collapse = "|"),
    # 28 Jun 2005/29 Jun 2005
    stringr::str_c("[0-9]{1,2}[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"), ")",
                   "[\\s\\/-][0-9]{4}\\/[0-9]{1,2}[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"), ")",
                   "[\\s\\/-][0-9]{4}", collapse = "|"),
     sep = "|"
    )

        # Extract the matching strings
      dmy_unambiguous <- noDATEa %>%
          # First, remove the strings matched prior
        dplyr::filter(!database_id %in% ymd_keepers_21$database_id) %>%
        dplyr::mutate(
          dmy_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = dmy_strings),
          dmy_locality = stringr::str_extract(locality,
                                              pattern = dmy_strings),
          dmy_fieldNotes = stringr::str_extract(fieldNotes,
                                                pattern = dmy_strings),
          dmy_locationRemarks = stringr::str_extract(locationRemarks,
                                                     pattern = dmy_strings),
          dmy_eventDate_in = stringr::str_extract(eventDate_in,
                                                pattern = dmy_strings)
        ) %>% # END mutate
        dplyr::select(database_id, dmy_vEV, dmy_locality,
                      dmy_fieldNotes, dmy_locationRemarks, dmy_eventDate_in)  

      # FORMAT the dmy_vEV column
      dmy_unambiguous$dmy_vEV <- dmy_unambiguous$dmy_vEV %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish() %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$|^00", replacement = "")
      #
      # FORMAT the dmy_locality column
      dmy_unambiguous$dmy_locality <- dmy_unambiguous$dmy_locality %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish() %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 
      #
      # FORMAT the dmy_fieldNotes column
      dmy_unambiguous$dmy_fieldNotes <- dmy_unambiguous$dmy_fieldNotes %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish() %>%
        # Remove 00 values to truncate pthe lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 
      #
      # FORMAT the dmy_locationRemarks column
      dmy_unambiguous$dmy_locationRemarks <- dmy_unambiguous$dmy_locationRemarks %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish() %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 
      # FORMAT the dmy_eventDate_in column
      dmy_unambiguous$dmy_eventDate_in <- dmy_unambiguous$dmy_eventDate_in %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("[-/ \\.]", romanNumerals, "[ -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish() %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") 
      #
      # Combine the columns
      dmy_keepers_22 <- dmy_unambiguous %>%
        dplyr::filter(stats::complete.cases(dmy_vEV) |
                        stats::complete.cases(dmy_locality) | 
                        stats::complete.cases(dmy_locationRemarks) | 
                        stats::complete.cases(dmy_fieldNotes) |
                        stats::complete.cases(dmy_eventDate_in)) %>%
        tidyr::unite(col = date, 
                     dmy_vEV, dmy_locality, dmy_locationRemarks, dmy_fieldNotes, dmy_eventDate_in,
                     na.rm = TRUE) %>% 
        rangeFinder(rangeData = .,
                    rangeColumn = "date",
                    timeStrings_in = c(timeStrings_robust, "dmy"))
      # add dmy_keepers_22 at the end
    
    ##### 2.3 mdy ####
    mdy_strings <- c(
      # Aug 2, 2019
    paste0(monthStrings,"[\\s-/ ]+[0-9]{1,2}[\\s-/, ]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # Aug 1-10 2019
    paste0(monthStrings,"[0-9]+[-\\u2013][0-9]+[\\s-/ ]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # V. 17 1901
    paste0("(?:^|\\s|\\(|\"\')", romanNumerals,"[\\s-/\\.]+[0-9]{1,2}[\\s-/ ]+([0-9]{4}|[0-9]{2})", collapse = "|"),
    
    # <12 >12 1992 - mdy
    "(0?[1-9]|1[0-2])[\\s- /]+(1[3-9]|2[0-9]|3[01])[\\s- /]+([0-9]{4})",
    "(0?[1-9]|1[0-2])[\\s- /]+(1[3-9]|2[0-9]|3[01])[\\s- /]+([0-9]{2})",
    "(0?[1-9])[\\s- /]+(1[3-9]|2[0-9]|3[01])[\\s- /]+([0-9]{4})",
    "(0?[1-9])[\\s- /]+(1[3-9]|2[0-9]|3[01])[\\s- /]+([0-9]{2})",
    # Nov 15 to Jan 10.1918
    stringr::str_c("(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{1,2}",
                   "[\\s\\/-]to[\\s\\/-]",
                   "(", stringr::str_c(monthStrings, collapse = "|"),")",
                   "[\\s\\/-][0-9]{1,2}",
                   "[\\s\\/-][0-9]{4}",  collapse = "|"),
    # Aug 30 1921 to Sept. 5 1921
    stringr::str_c( "(",stringr::str_c(monthStrings, collapse = "|"),")",
                    "[\\s\\/-][0-9]{1,2}[\\s\\/-][0-9]{4}",
                    "[\\s\\/-]to[\\s\\/-]",
                    "(", stringr::str_c(monthStrings, collapse = "|"),")",
                    "[\\s\\/-][0-9]{1,2}",
                    "[\\s\\/-][0-9]{4}",  collapse = "|")
    )
      # Get the IDs to remove...
      id2remove_23 <- c(ymd_keepers_21$database_id, dmy_keepers_22$database_id)
    
      # Extract the matching strings to three columns
      mdy_unambiguous <- noDATEa %>%
        # First, remove the strings matched prior
        dplyr::filter(!database_id %in% id2remove_23) %>%
        dplyr::mutate(
          mdy_vEV = stringr::str_extract(verbatimEventDate,
                                              pattern = paste0(mdy_strings, 
                                                               collapse = "|")),
          mdy_locality = stringr::str_extract(locality,
                                              pattern = paste0(mdy_strings, 
                                                               collapse = "|")),
          mdy_fieldNotes = stringr::str_extract(fieldNotes,
                                                pattern = paste0(mdy_strings, 
                                                                 collapse = "|")),
          mdy_locationRemarks = stringr::str_extract(locationRemarks,
                                                     pattern = paste0(mdy_strings, 
                                                                      collapse = "|")),
          mdy_eventDate_in = stringr::str_extract(eventDate_in,
                                                     pattern = paste0(mdy_strings, 
                                                                      collapse = "|"))
        ) %>% # END mutate
          # select a subset of columns
        dplyr::select(database_id, mdy_vEV, mdy_locality,
                      mdy_fieldNotes, mdy_locationRemarks, mdy_eventDate_in)  

      # FORMAT the mdy_vEV column
      mdy_unambiguous$mdy_vEV <- mdy_unambiguous$mdy_vEV %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("^", romanNumerals, "[ -/\\.]"),
          replacement = paste0(numeralConversion, " ") ) %>% stringr::str_squish()

      #
      # FORMAT the mdy_locality column
      mdy_unambiguous$mdy_locality <- mdy_unambiguous$mdy_locality %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("^", romanNumerals, "[ -/\\.]"),
          replacement = paste0(numeralConversion, " ") ) %>% stringr::str_squish() %>%
        stringr::str_replace( pattern = "^The ", replacement = "")  
      #
      # FORMAT the mdy_fieldNotes column
      mdy_unambiguous$mdy_fieldNotes <- mdy_unambiguous$mdy_fieldNotes %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("^", romanNumerals, "[ -/\\.]"),
          replacement = paste0(numeralConversion, " ") ) %>% stringr::str_squish()

      #
      # FORMAT the mdy_locationRemarks column
      mdy_unambiguous$mdy_locationRemarks <- mdy_unambiguous$mdy_locationRemarks %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("^", romanNumerals, "[-/ \\.]"),
          replacement = paste0(numeralConversion, " ") ) %>% stringr::str_squish() %>%
        lubridate::mdy(truncated = 2, quiet = TRUE) 
      # FORMAT the mdy_eventDate_in column
      mdy_unambiguous$mdy_eventDate_in <- mdy_unambiguous$mdy_eventDate_in %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("^", romanNumerals, "[ -/\\.]"),
          replacement = paste0(numeralConversion, " ") ) %>% stringr::str_squish()
      #

      # Combine the columns
      mdy_keepers_23 <- mdy_unambiguous %>%
        dplyr::filter( stats::complete.cases(mdy_vEV) |
                         stats::complete.cases(mdy_locality) | 
                        stats::complete.cases(mdy_locationRemarks) | 
                         stats::complete.cases(mdy_fieldNotes) | 
                         stats::complete.cases(mdy_eventDate_in)) %>%
        tidyr::unite(col = date, 
                     mdy_vEV, mdy_locality, mdy_locationRemarks, mdy_fieldNotes, mdy_eventDate_in,
                     na.rm = TRUE, sep = "") %>% 
        rangeFinder(rangeData = .,
                    rangeColumn = "date",
                    timeStrings_in = c(timeStrings_robust, "mdy"))
        # KEEP mdy_keepers_23 at the end
    
    
    ##### 2.4 my ####
    my_strings <- c(
      # VIII-1946
    paste0("(?:^|\\s|\\(|\"|\')", romanNumerals, "[\\s-/ \\.]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # July 1995; July, 1995
    paste0(monthStrings, "[\\s-/ \\.]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # April 1899
    paste0(monthStrings, "[\\s-/ ]+([0-9]{4}|[0-9]{2})", collapse = "|"),
      # 1899 April 
    paste0("([0-9]{4}|[0-9]{2})[\\s-/ ]+", monthStrings, collapse = "|"),
      # 4/1957
    "(?:^|\\s|\\(|\"\')(0?[1-9]|1[0-2])[\\s- /]+[0-9]{4}"
    )
    
      # Get the IDs to remove...
      id2remove_24 <- c(mdy_keepers_23$database_id, id2remove_23)

      # Extract the matching strings to three columns
      my_unambiguous <- noDATEa %>%
        # First, remove the strings matched prior
        dplyr::filter(!database_id %in% id2remove_24) %>%
        dplyr::mutate(
          my_vEV = stringr::str_extract(verbatimEventDate,
                                             pattern = paste0(my_strings, 
                                                              collapse = "|")),
          my_locality = stringr::str_extract(locality,
                                              pattern = paste0(my_strings, 
                                                               collapse = "|")),
          my_fieldNotes = stringr::str_extract(fieldNotes,
                                                pattern = paste0(my_strings, 
                                                                 collapse = "|")),
          my_locationRemarks = stringr::str_extract(locationRemarks,
                                                     pattern = paste0(my_strings, 
                                                                      collapse = "|"))
        ) %>% # END mutate
        # select a subset of columns
        dplyr::select(database_id, my_vEV, my_locality,
                      my_fieldNotes, my_locationRemarks) 

      # FORMAT the my_vEV column
      my_unambiguous$my_vEV <- my_unambiguous$my_vEV %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("(^|[-/ \\.])", romanNumerals, "[\\s -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish()  %>%
        # format
        lubridate::my( quiet = TRUE) 
      #
      # FORMAT the my_locality column
      my_unambiguous$my_locality <- my_unambiguous$my_locality %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("(^|[-/ \\.])", romanNumerals, "[\\s -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish() %>%
          # format
        lubridate::my(quiet = TRUE) 
      #
      # FORMAT the my_fieldNotes column
      my_unambiguous$my_fieldNotes <- my_unambiguous$my_fieldNotes %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("(^|[-/ \\.])", romanNumerals, "[\\s -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish()  %>%
        # format
        lubridate::my(quiet = TRUE) 
      #
      # FORMAT the my_locationRemarks column
      my_unambiguous$my_locationRemarks <- my_unambiguous$my_locationRemarks %>%
        # Convert roman numerals to readable by lubridate
        mgsub::mgsub(
          pattern = paste0("(^|[-/ \\.])", romanNumerals, "[\\s -/\\.]"),
          replacement = paste0(" ", numeralConversion, " ") ) %>% stringr::str_squish()  %>%
        # format
        lubridate::my(quiet = TRUE) 
      #

      # Combine the columns
      my_keepers_24 <- my_unambiguous %>%
        dplyr::filter(stats::complete.cases(my_vEV) |
                        stats::complete.cases(my_locality) | 
                        stats::complete.cases(my_locationRemarks) | 
                        stats::complete.cases(my_fieldNotes)) %>%
        tidyr::unite(col = date, 
                     my_vEV, my_locality, my_locationRemarks, my_fieldNotes, 
                     na.rm = TRUE, sep = "-")  %>% 
        rangeFinder(rangeData = .,
                    rangeColumn = "date") 
          # Remove double-ups
      my_keepers_24$date <- stringr::str_replace(my_keepers_24$date,
                                                 pattern = "-[0-9]+-[0-9]+-[0-9]+", 
                                                 replacement = "")
    
      #### 3.0 Amb. str. dates ####
      writeLines(paste0(
        " - Extracting year from fieldNotes, locationRemarks, and verbatimEventDate ",
        "columns in ambiguous formats..."))
      ambiguousDateStrings <- c(
        # dmy or mdy; 10 02 1946
        "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{4}",
        "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{4}",
        "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{2}",
        "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{2}"
      )

      ambiguousDatePattern = paste0(ambiguousDateStrings, collapse="|")

      # Get the IDs to remove...
      id2remove_30 <- c(my_keepers_24$database_id, id2remove_24)
      
      # Extract the matching strings to three columns
      ambiguousNames <- noDATEa %>%
        # First, remove the strings matched prior
        dplyr::filter(!database_id %in% id2remove_30) %>%
        dplyr::mutate(
          amb_vEV = stringr::str_extract(verbatimEventDate, pattern=ambiguousDatePattern),
          amb_locality = stringr::str_extract(locality, pattern=ambiguousDatePattern),
          amb_fieldNotes = stringr::str_extract(fieldNotes, pattern=ambiguousDatePattern),
          amb_locationRemarks = stringr::str_extract(locationRemarks, pattern=ambiguousDatePattern)
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
        # Attempt to build date from remaining string
        lubridate::dmy(truncated = 2, quiet = TRUE) %>%
        # Safely extract just the year
        lubridate::year() %>%
        # Build date from Jan 1st
        lubridate::make_date(1, 1)
      #
      # FORMAT the amb_locality column
      ambiguousNames$amb_locality <- ambiguousNames$amb_locality %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        # Remove any remaining leading/trailing zeroes
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                             replacement = "") %>%
        # Attempt to build date from remaining string
        lubridate::dmy(truncated = 2, quiet = TRUE) %>%
        # Extract just the year
        lubridate::year() %>%
        # Build date from Jan 1st
        lubridate::make_date(1, 1)
      #
      # FORMAT the amb_fieldNotes column
      ambiguousNames$amb_fieldNotes <- ambiguousNames$amb_fieldNotes %>%
      # Remove 00 values to truncate the lubridate 
      stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                             replacement = "") %>%
        # Attempt to build date from remaining string
        lubridate::dmy(truncated = 2, quiet = TRUE) %>%
        # Extract just the year
        lubridate::year() %>%
        # Build date from Jan 1st
        lubridate::make_date(1, 1)
      #
      # FORMAT the amb_locationRemarks column
      ambiguousNames$amb_locationRemarks <- ambiguousNames$amb_locationRemarks %>%
        # Remove 00 values to truncate the lubridate 
        stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>%
        stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>%
        stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]",
                            replacement = "") %>%
        # Attempt to build date from remaining string
        lubridate::dmy(truncated = 2, quiet = TRUE) %>%
        # Extract just the year
        lubridate::year() %>%
        # Build date from Jan 1st
        lubridate::make_date(1, 1)


      # Combine the columns
      amb_keepers_30 <- ambiguousNames %>%
        dplyr::filter(stats::complete.cases(amb_vEV)|
                        stats::complete.cases(amb_locality) | 
                        stats::complete.cases(amb_locationRemarks) | 
                        stats::complete.cases(amb_fieldNotes)) %>%
        tidyr::unite(col = date, amb_vEV,  
                     amb_locality, amb_locationRemarks, amb_fieldNotes, 
                     na.rm = TRUE) %>% 
        rangeFinder(rangeData = .,
                    rangeColumn = "date")
      # KEEP amb_keepers_30 at the end

      #### 4.0 Format+combine ####
      writeLines(paste0(
        " - Formating and combining the new data.."))
        ##### 4.1 formatting... ####
        # Extract only the date from occYr_2
      occYr_2$date <- as.character(occYr_2$date) %>% lubridate::ymd_hms() %>% lubridate::date()
        # Set as date format...
      ymd_keepers_21$date <- lubridate::ymd(ymd_keepers_21$eventDate)
      dmy_keepers_22$date <- lubridate::ymd(dmy_keepers_22$eventDate)
      mdy_keepers_23$date <- lubridate::ymd(mdy_keepers_23$eventDate)
      my_keepers_24$date <- lubridate::ymd(my_keepers_24$eventDate)
        # merge these data...

      saveTheDates <- dplyr::bind_rows(ymd_hms_0, dmy_1, occYr_2, 
        ymd_keepers_21, dmy_keepers_22, mdy_keepers_23) %>%
        dplyr::select(database_id, date, startDayOfYear, endDayOfYear) 


      ##### 4.2 Full dates ####
        # Join these dates to the original rows...
      datesOut_full <- data %>% 
        dplyr::right_join(saveTheDates, 
                          by = "database_id")
        # Fill the eventDate column
      datesOut_full$eventDate <- lubridate::ymd_hms(datesOut_full$date,
                                                    truncated = 5)
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
        dplyr::right_join(my_keepers_24 %>%
                            dplyr::mutate(date = eventDate) %>%
                            dplyr::select(!eventDate), 
                          by = "database_id")
        # Fill the eventDate column
      datesOut_noDay$eventDate <- lubridate::ymd_hms(datesOut_noDay$date, quiet = TRUE,
                                                     truncated = 5)
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
        dplyr::right_join(amb_keepers_30 %>%
                            dplyr::mutate(date = eventDate) %>%
                            dplyr::select(!eventDate), 
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
      writeLines(paste0(
        " - Merging all data, nearly there..."))
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
        paste0(
        " - Finished. \n",
        "We now have ",
        format((sum(stats::complete.cases(dates_complete$eventDate)) - originalDateCount),
               big.mark = ","), 
        " more full eventDate cells than in the input data.\n",
        "We modified dates in \n",
        format(nrow(datesMerged), big.mark = ","), " occurrences.\n",
        " - As it stands, there are ", 
        format( sum(stats::complete.cases(dates_complete$eventDate)), big.mark = ","),
        " complete eventDates and ", 
        format( sum(is.na(dates_complete$eventDate)), big.mark = ","), 
        " missing dates.\n", 
        " - There are also ", 
        format( sum(stats::complete.cases(dates_complete$year)), big.mark = ","), 
        " complete year occurrences to filter from. This is up from an initial count of ",
        format( sum(stats::complete.cases(data$year)), big.mark = ","),
        " At this rate, you will stand to lose ",
        format( sum(is.na(dates_complete$year)), big.mark = ","), 
        " occurrences on the basis of missing",
        " year",
        " - Operation time: ", (timeEnd - timeStart)," ",
        units(round(timeEnd - timeStart, digits = 2)))
    )
    return(dates_complete)
  }
