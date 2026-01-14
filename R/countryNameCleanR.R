
# This function was written by James B Dorey on the 5th of August 2022 to fix up some
  # Country names in occurrence records and extract country names from ISO2 codes


#' Fix country name issues using a user-input list
#' 
#' This function is basic for a user to manually fix some country name inconsistencies.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param ISO2_table A data frame or tibble with the columns ISO2 and long names for country names. Default 
#' is a static version from Wikipedia.
#' @param commonProblems A data frame or tibble. It must have two columns: 
#' one containing the user-identified problem and one with a user-defined fix
#'
#' @return Returns the input data, but with countries occurring in the user-supplied problem 
#' column ("commonProblems") replaced with those in the user-supplied fix column
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom stats complete.cases
#'
#' @examples
#' beesFlagged_out <- countryNameCleanR(
#' data = BeeBDC::beesFlagged,
#' commonProblems = dplyr::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
#'                         'United States','U.S.A','MX','CA','Bras.','Braz.',
#'                         'Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
#'                         fix = c('United States of America','United States of America',
#'                                 'United States of America','United States of America',
#'                                 'United States of America','United States of America',
#'                                 'United States of America','Mexico','Canada','Brazil',
#'                                 'Brazil','Brazil','Northern Mariana Islands','PUERTO.RICO')))

countryNameCleanR <- function(
    data = NULL,
    ISO2_table = NULL,
    commonProblems = NULL){
  # locally bind variables to the function
  database_id <- decimalLatitude <- decimalLongitude <- country <- countryCode <- scientificName <-
    dataSource <- FullName <- . <- fix <- NULL
  
  #### 0.0 Prep ####
  
  ##### 0.1 warnings ####
  if(is.null(data)){
    stop(" - Please provide input data.")
  }
  if(!"countryCode" %in% colnames(data)){
    stop(" - No countryCode column in data.")
  }
  
  ##### 0.2 Data defaults ####
  if(is.null(ISO2_table)){
    writeLines(" - Using default country names and codes from https:en.wikipedia.org/wiki/ISO_3166-1_alpha-2 - static version from July 2022.")
    # Use countryCode to extract country name using the Wikipedia table - https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
    # Make a tibble with 
    ISO2_table <- dplyr::tibble(
      ISO2 = c("AD","AE","AF","AG","AI","AL","AM","AO","AQ","AR","AS","AT","AU","AW","AX","AZ","BA","BB","BD","BE","BF","BG","BH","BI","BJ","BL","BM","BN","BO","BQ","BR","BS","BT","BV","BW","BY","BZ","CA","CC","CD","CF","CG","CH","CI","CK","CL","CM","CN","CO","CR","CU","CV","CW","CX","CY","CZ","DE","DJ","DK","DM","DO","DZ","EC","EE","EG","EH","ER","ES","ET","FI","FJ","FK","FM","FO","FR","GA","GB","GD","GE","GF","GG","GH","GI","GL","GM","GN","GP","GQ","GR","GS","GT","GU","GW","GY","HK","HM","HN","HR","HT","HU","ID","IE","IL","IM","IN","IO","IQ","IR","IS","IT","JE","JM","JO","JP","KE","KG","KH","KI","KM","KN","KP","KR","KW","KY","KZ","LA","LB","LC","LI","LK","LR","LS","LT","LU","LV","LY","MA","MC","MD","ME","MF","MG","MH","MK","ML","MM","MN","MO","MP","MQ","MR","MS","MT","MU","MV","MW","MX","MY","MZ","NA","NC","NE","NF","NG","NI","NL","NO","NP","NR","NU","NZ","OM","PA","PE","PF","PG","PH","PK","PL","PM","PN","PR","PS","PT","PW","PY","QA","RE","RO","RS","RU","RW","SA","SB","SC","SD","SE","SG","SH","SI","SJ","SK","SL","SM","SN","SO","SR","SS","ST","SV","SX","SY","SZ","TC","TD","TF","TG","TH","TJ","TK","TL","TM","TN","TO","TR","TT","TV","TW","TZ","UA","UG","UM","US","UY","UZ","VA","VC","VE","VG","VI","VN","VU","WF","WS","YE","YT","ZA","ZM","ZW"),
      FullName = c("Andorra","United Arab Emirates","Afghanistan","Antigua and Barbuda","Anguilla","Albania","Armenia","Angola","Antarctica","Argentina","American Samoa","Austria","Australia","Aruba","\\u00c5land Islands","Azerbaijan","Bosnia and Herzegovina","Barbados","Bangladesh","Belgium","Burkina Faso","Bulgaria","Bahrain","Burundi","Benin","Saint Barth\\u00e9lemy","Bermuda","Brunei Darussalam","Bolivia (Plurinational State of)","Bonaire, Saint Eustatius and Saba","Brazil","Bahamas","Bhutan","Bouvet Island","Botswana","Belarus","Belize","Canada","Cocos (Keeling) Islands","Congo, Democratic Republic of the","Central African Republic","Congo","Switzerland","C\\u00f4te d'Ivoire","Cook Islands","Chile","Cameroon","China","Colombia","Costa Rica","Cuba","Cabo Verde","Cura\\u00e7ao","Christmas Island","Cyprus","Czechia","Germany","Djibouti","Denmark","Dominica","Dominican Republic","Algeria","Ecuador","Estonia","Egypt","Western Sahara","Eritrea","Spain","Ethiopia","Finland","Fiji","Falkland Islands (Malvinas)","Micronesia (Federated States of)","Faroe Islands","France","Gabon","United Kingdom of Great Britain and Northern Ireland","Grenada","Georgia","French Guiana","Guernsey","Ghana","Gibraltar","Greenland","Gambia","Guinea","Guadeloupe","Equatorial Guinea","Greece","South Georgia and the South Sandwich Islands","Guatemala","Guam","Guinea-Bissau","Guyana","Hong Kong","Heard Island and McDonald Islands","Honduras","Croatia","Haiti","Hungary","Indonesia","Ireland","Israel","Isle of Man","India","British Indian Ocean Territory","Iraq","Iran (Islamic Republic of)","Iceland","Italy","Jersey","Jamaica","Jordan","Japan","Kenya","Kyrgyzstan","Cambodia","Kiribati","Comoros","Saint Kitts and Nevis","Korea (Democratic People's Republic of)","Korea, Republic of","Kuwait","Cayman Islands","Kazakhstan","Lao People's Democratic Republic","Lebanon","Saint Lucia","Liechtenstein","Sri Lanka","Liberia","Lesotho","Lithuania","Luxembourg","Latvia","Libya","Morocco","Monaco","Moldova, Republic of","Montenegro","Saint Martin (French part)","Madagascar","Marshall Islands","North Macedonia","Mali","Myanmar","Mongolia","Macao","Northern Mariana Islands","Martinique","Mauritania","Montserrat","Malta","Mauritius","Maldives","Malawi","Mexico","Malaysia","Mozambique","Namibia","New Caledonia","Niger","Norfolk Island","Nigeria","Nicaragua","Netherlands","Norway","Nepal","Nauru","Niue","New Zealand","Oman","Panama","Peru","French Polynesia","Papua New Guinea","Philippines","Pakistan","Poland","Saint Pierre and Miquelon","Pitcairn","Puerto Rico","Palestine, State of","Portugal","Palau","Paraguay","Qatar","R\\u00e9union","Romania","Serbia","Russian Federation","Rwanda","Saudi Arabia","Solomon Islands","Seychelles","Sudan","Sweden","Singapore","Saint Helena, Ascension and Tristan da Cunha","Slovenia","Svalbard and Jan Mayen","Slovakia","Sierra Leone","San Marino","Senegal","Somalia","Suriname","South Sudan","Sao Tome and Principe","El Salvador","Sint Maarten (Dutch part)","Syrian Arab Republic","Eswatini","Turks and Caicos Islands","Chad","French Southern Territories","Togo","Thailand","Tajikistan","Tokelau","Timor-Leste","Turkmenistan","Tunisia","Tonga","Turkey","Trinidad and Tobago","Tuvalu","Taiwan, Province of China","Tanzania, United Republic of","Ukraine","Uganda","United States Minor Outlying Islands","United States of America","Uruguay","Uzbekistan","Holy See","Saint Vincent and the Grenadines","Venezuela (Bolivarian Republic of)","Virgin Islands (British)","Virgin Islands (U.S.)","Viet Nam","Vanuatu","Wallis and Futuna","Samoa","Yemen","Mayotte","South Africa","Zambia","Zimbabwe"))
  }
  
###### a. prep data ####
# Remove NA for lat and long
data_noNa <- data %>% 
  # Drop na in lat and long
  tidyr::drop_na(c("decimalLatitude", "decimalLongitude")) %>%
  # select a subset of columns to save RAM
  dplyr::select(c(database_id, decimalLatitude, decimalLongitude, country, countryCode,
                  scientificName, dataSource))

  #### 1.0 GBIF ####
# Now, because GBIF doesn't have a country column (weird, right?!), 
# Create country from countryCode
GBIF_occs <- data_noNa %>%
  dplyr::filter(stringr::str_detect(dataSource, pattern = "GBIF"))
  # # Remove NA values from data_noNa
  # GBIF_occs$countryCode <- GBIF_occs$countryCode %>% as.character() %>%
  #   stringr::str_replace(pattern = "NA", replacement = "")

# Join the ISO2_table names with the countryCode df
GBIF_occs <- GBIF_occs %>%
  # If there is NO countryCode, but there IS a country, add this to countryCode in case
  dplyr::mutate(countryCode = dplyr::if_else(countryCode == "" & stats::complete.cases(as.character(country)),
                                             country, as.character(countryCode))) %>%
  dplyr::left_join(ISO2_table, by = c("countryCode" = "ISO2")) %>%
  # Insert the country information to the correct spot
  dplyr::mutate(country = FullName) %>%
  dplyr::select(!FullName)
# Re-join datasets
data_noNa <- data_noNa %>%
  # Remove GBIF databse_ids
  dplyr::filter(!database_id %in% GBIF_occs$database_id) %>%
  # Re-bind those occs
  dplyr::bind_rows(GBIF_occs)
rm(GBIF_occs)


#### 2.0 All ####
# Join the ISO2_table names with the countryCode df
data_all <- data_noNa %>%
  # If there is NO countryCode, but there IS a country, add this to countryCode in case
  dplyr::mutate(countryCode = dplyr::if_else(countryCode == "" | is.na(countryCode)
                                               & stats::complete.cases(country) & 
                                               tolower(country) != "namibia",
                                             country, countryCode)) %>%
  dplyr::left_join(ISO2_table, by = c("countryCode" = "ISO2")) %>%
  # Insert the country information to the correct spot
  dplyr::mutate(country = FullName) %>%
  dplyr::select(!FullName)
# Re-join datasets
data_all <- data_all %>%
  # Remove GBIF databse_ids
  dplyr::filter(!database_id %in% data_noNa$database_id) %>%
  # Re-bind those occs
  dplyr::bind_rows(data_noNa)
rm(data_noNa)

# Remove illegal characters
data_all$country <- data_all$country %>%
  stringr::str_replace(., pattern = paste("\\[", "\\]", "\\?",
                                          sep=  "|"), replacement = "")
# Replace the problems as they occur
data_all <- data_all %>%
  dplyr::left_join(commonProblems, by = c("country" = "problem")) %>%
  dplyr::mutate(country = 
                  dplyr::if_else(country %in% as.character(commonProblems$problem),
                                 fix, country))
return(data_all)

}
