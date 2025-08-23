# This function was started on the 20th of January 2025 by James Dorey in order to make some 
# country countryColumns more consistent for his paper titled "How many bee species are there? A 
# quantitative global estimate".
  # For questions, email him on jbdorey@me.com



#' Match and harmonise country names
#' 
#' A small function that is useful for harmonising country names so that they mathc between datasets.
#' This relies on the country names matching one of the exceptions pre-coded in and is so far used 
#' internally.
#'
#' @param data A data frame or tibble. 
#' @param countryColumn Character. The column that contains country names to be harmonised.
#' @param continentAnalysis Logical. Set to TRUE in order to match small entities to continents;
#' limited use for the moment.
#' @param shorterNames Logical. If TRUE, some very long country names will be shortened. This can be
#' helpful for writing country names in plots where long names are annoying.
#' 
#'
#' @return Returns the original data frame or tibble but with harmonised country names
#' 
#' @importFrom dplyr %>%
#' 
#' 
#' @export
#'
#' @examples
#' # load in the test dataset
#' system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |> load()
#' harmonisedCountries <- countryHarmoniseR(
#'   data = testTaxonomy,
#'   countryColumn = "country_suggested"
#' )


countryHarmoniseR <- function(
    data = NULL,
    countryColumn = NULL,
    shorterNames = TRUE,
      # set to TRUE in order to match small entities to continents
    continentAnalysis = FALSE){
  . <- NULL
    
  
  #### 1.0 Main changes ####
  # Change country names based on exceptions
  data <- data %>%
    dplyr::rename(countryColumn = tidyselect::any_of(countryColumn)) %>%
    # Shorten some country countryColumns
    dplyr::mutate(countryColumn = dplyr::if_else(countryColumn %in% c("United States of America","United States"),
                                        "United States",countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Russia", "Russian Federation", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Lao People's Democratic Republic", "Lao PDR"),
                                        "Lao",countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Republic of Cabo Verde","Cape Verde"),
                                        "Cabo Verde",countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("S\u00e3o Tom\u00e9 and Principe",
                                                                      "Sao Tome and Principe",
                                                                      "S\u00e3o Tom\u00e9 and Pr\u00edncipe"),
                                        "S\u00e3o Tom\u00e9",countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Republic of the Congo","Republic of Congo"),
                                        "Republic of Congo",countryColumn),
                  countryColumn = dplyr::if_else(stringr::str_detect(countryColumn, "Northern") & stringr::str_detect(countryColumn, "Cyprus"),
                                        "Northern Cyprus", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("N. Cyprus"),
                                        "Northern Cyprus",countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "The Gambia", "Gambia", countryColumn),
                  countryColumn = dplyr::if_else(stringr::str_detect(countryColumn, "North") & stringr::str_detect(countryColumn, "Macedonia"),
                                        "Macedonia", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Kingdom of eSwatini","Swaziland", "eSwatini"), 
                                        "Eswatini", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Barbuda", "Antigua"),
                                        "Antigua and Barbuda", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Brussels","Flemish Region","Walloon Region"),
                                        "Belgium", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Caribbean Netherlands","Martinique"),
                                                      # RecountryColumnd Cura\u00e7ao to match to continent
                                                      "Cura\u00e7ao", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Cocos Islands","Indian Ocean Territories", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Federation of Bosnia and Herzegovina",
                                                    "Republic Srpska","Vojvodina", "Bosnia"),
                                        "Bosnia and Herzegovina", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Gibraltar","Spain", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Guadeloupe","Barbados", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Mayotte","Madagascar", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("R\u00e9union", "Reunion"),"Mauritius", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Svalbard Islands","Bouvet"),
                                        "Norway", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("West Bank","Gaza"),"Palestine", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "French Southern Territories",
                                        "French Southern and Antarctic Lands", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "South Georgia and South Sandwich Islands","South Georgia and the Islands", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Aland Islands"),
                                                 "\u00c5land Islands", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Kosovo",
                                                      "Serbia", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Falkland Islands","Falkland Islands / Malvinas", countryColumn),
                  countryColumn = dplyr::if_else(stringr::str_detect(countryColumn, "Darussalam"),"Brunei Darussalam", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn %in% c("Scotland","Wales","England"),
                                        "United Kingdom", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Somaliland",
                                                      "Somalia", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Tokelau","New Zealand", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Czech Republic", "Czechia", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "South Korea", "Republic of Korea", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Wallis and Futuna Islands", "Wallis and Futuna", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Faeroe Islands","Faroe Islands", countryColumn),
                  countryColumn = dplyr::if_else(countryColumn == "Saint Martin","Saint-Martin", countryColumn),
                  )
  
  #### 2.0 Shorter names ####
  if(shorterNames == TRUE){
    data <- data %>%
      dplyr::rename(countryColumn = tidyselect::any_of(countryColumn)) %>%
      # Shorten some country countryColumns
      dplyr::mutate(countryColumn = dplyr::if_else(countryColumn %in% c("United States of America","United States"),
                                                   "USA",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn == "Democratic Republic of the Congo","DRC",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn %in% c("Lao People's Democratic Republic", "Lao PDR"),
                                                   "Lao",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn == "United Arab Emirates","UAE",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn == "Central African Republic","CAR",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn == "Saint Vincent and the Grenadines","SV & Grenadines",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn == "Northern Mariana Islands","N. Mariana",countryColumn),
                    countryColumn = dplyr::if_else(countryColumn %in% c("Republic of the Congo","Republic of Congo"),
                                                   "Rep. Congo",countryColumn)
      )
  }
  
  
  #### 3.0 Continent changes ####
  if(continentAnalysis == TRUE){
    data = data %>% 
      dplyr::mutate(
        countryColumn = dplyr::if_else(countryColumn == "French Guiana","Brazil", countryColumn),
      )
  }
  
    # Re-apply the original column name
  names(data)[names(data) == "countryColumn"] <- countryColumn
  
  # Return the data
  return(data)
  
}



