   # This function was written by James B Dorey starting from the 3rd-7th of June 2022.
# It is intended to replace the bdc function bdc_coordinates_country_inconsistent for larger datasets
# where it is simply not feasible. 
  # Initial function finished: _____
  # For questions, please ask James at jbdorey[at]me.com

#' Flags coordinates that are inconsistent with the stated country name
#' 
#' Compares stated country name in an occurrence record with record’s coordinates using 
#' rnaturalearth data. The prefix, jbd_ is meant
#' to distinguish this function from the similar [bdc::bdc_coordinates_country_inconsistent()].
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param lon Character. The name of the column to use as longitude. Default = "decimalLongitude".
#' @param lat Character. The name of the column to use as latitude. Default = "decimalLatitude".
#' @param mapResolution Numeric or character. To be passed to [rnaturalearth::ne_countries()]'s scale.
#' Scale of map to return, one of 110, 50, 10 or “small”, “medium”, “large”. 
#' Smaller values return higher-resolution maps.
#' @param pointBuffer Numeric. Amount to buffer points, in decimal degrees. If the point is outside 
#' of a country, but within this point buffer, it will not be flagged.
#'
#' @return The input occurrence data with a new column, .coordinates_country_inconsistent
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' 
#' beesRaw_out <- jbd_coordCountryInconsistent(
#'   data = BeeDC::beesRaw,
#'   lon = "decimalLongitude",
#'   lat = "decimalLatitude",
#'   mapResolution = 50,
#'   pointBuffer = 0.01)

jbd_coordCountryInconsistent <- function(
    data = NULL,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    mapResolution = 50,
    pointBuffer = 0.01){
  
  database_id <- decimalLatitude <- decimalLongitude <- country <- name_long <- iso_a2 <- 
    geometry <- admin <- sovereignt <- name <- . <- NULL
  
startTime <- Sys.time()
requireNamespace("rnaturalearth")
requireNamespace("dplyr")
requireNamespace("ggspatial")
requireNamespace("mgsub")
requireNamespace("terra")

  #### 0.0 Prep ####
    # Reduce dataset 
  dataR <- data %>%
    dplyr::select(database_id, decimalLatitude, decimalLongitude, country) %>%
      # Remove lat/lon NAs
    dplyr::filter(!is.na(decimalLatitude)) %>% dplyr::filter(!is.na(decimalLongitude))
  
  #### 1.1 Terrestrial map ####
      ##### 1.1 rnaturalearth DL ####
  writeLines(" - Downloading naturalearth map...")
  # Download the rnaturalearth countries
vectEarth <- rnaturalearth::ne_countries(scale = mapResolution, type = "countries", 
                                    returnclass = "sf" )%>%
  dplyr::select(name_long, iso_a2, geometry, admin, sovereignt, name)
  # Repair gemoetries
sf::sf_use_s2(FALSE)


  #### 2.0 Extractions ####
    ##### 2.1 Country name ####
writeLines(" - Extracting initial country names without buffer...")
      # Turn the points into an sf object
sp <- sf::st_as_sf(dataR, coords = c(lon, lat),
                   crs = terra::crs(vectEarth))
  # Extract the country for the points from the vectEarth map
country_extracted <- sf::st_intersection(vectEarth, sp)
  
    ##### 2.2 Failures ####
  # Find those records that don't match.
failed_extract <- country_extracted %>%
    # Find the mis-matched countries
  dplyr::filter(!tolower(country) %in% c(tolower(name_long), tolower(admin),
                                                   tolower(sovereignt), tolower(name))) %>%
  dplyr::filter(!tolower(iso_a2) %in% tolower(country)) %>%
    # Remove NA countries
  dplyr::filter(!is.na(country)) %>%
  dplyr::tibble() %>%
  sf::st_as_sf(crs = terra::crs(vectEarth))
    # Replace some country names as needed and re-remove
failed_extract$country <-
  mgsub::mgsub(failed_extract$country, 
               pattern = c("Martinique", "French guiana", "Federated States of Micronesia"),
               replacement = c("France", "France", "Micronesia"))
  # Remove new matches
failed_extract <- failed_extract %>%
  # Find the mis-matched countries
  dplyr::filter(!tolower(country) %in% c(tolower(name_long), tolower(admin),
                                                   tolower(sovereignt), tolower(name))) %>%
  dplyr::filter(!tolower(iso_a2) %in% tolower(country)) %>%
  # Remove NA countries
  dplyr::filter(!is.na(country)) %>%
  dplyr::tibble() %>%
  sf::st_as_sf(crs = terra::crs(vectEarth))

  # Find the unique combination of failures
failed_unique <- failed_extract %>% dplyr::distinct(admin, country, iso_a2)

    ##### 2.3 Buffer fails ####
writeLines(" - Buffering natualearth map by pointBuffer...")
  # Buffer the natural earth map
suppressWarnings({
  vectEarth_buff <- vectEarth %>% 
  sf::st_buffer(dist = pointBuffer)
})

writeLines(" - Extracting FAILED country names WITH buffer...")
# Extract the country for the points from the vectEarth map
suppressWarnings({
  failed_extract_2 <- failed_extract %>%
  dplyr::select(database_id, country, geometry) %>%
  sf::st_intersection(vectEarth_buff, .)
})
    # Find MATCHES #
  # With country
fExtr_1 <- failed_extract_2 %>% 
  dplyr::filter(tolower(country) %in% c(tolower(name_long), tolower(admin),
                                                  tolower(sovereignt), tolower(name)))
  # With iso_a2
fExtr_2 <- failed_extract_2 %>% 
  dplyr::filter(tolower(iso_a2) %in% tolower(country))

ids2keep <- dplyr::bind_rows(fExtr_1, fExtr_2) %>%
  # Find the mis-matched countries
  dplyr::filter(tolower(country) %in% c(tolower(name_long), tolower(admin),
                                                   tolower(sovereignt), tolower(name))) %>%
  dplyr::filter(tolower(iso_a2) %in% tolower(country)) %>%
    # Keep only the database id
  dplyr::select(database_id)

  #### 3.0 Final fails ####
# Get the final fails by removing those to keep from the failed list
ids2remove <- failed_extract_2 %>%
  dplyr::filter(!database_id %in% ids2keep$database_id) %>%
  dplyr::select(database_id)


  #### 4.0 Finals ####
    # Create new column
data <- data %>% 
  dplyr::mutate(.coordinates_country_inconsistent = !database_id %in% ids2remove$database_id)

    # return message
message(paste("\njbd_coordinates_country_inconsistent:\nFlagged", 
              format(sum(data$.coordinates_country_inconsistent == FALSE, na.rm = TRUE), big.mark = ","),
              "records.\nThe column, '.coordinates_country_inconsistent',",
              "was added to the database.\n"), sep = "")
 endTime <- Sys.time()
    # Time output
 message(paste(
   " - Completed in ", 
   round(difftime(endTime, startTime, units = "mins"), digits = 2 ),
   " minutes.",
   sep = ""))

  # Return the data
return(data)
} # END function
  