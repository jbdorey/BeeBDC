#' Internal function. Get a world map with country names and iso code
#'
#' This is a helper function used to obtain names, iso code, and the limits
#' (polygon) of world countries. Data from the package 'rnaturalearth'. The prefix, jbd_ is meant
#' to distinguish this function from the original bdc::bdc_get_world_map().
#' 
#' @param scale Passed to rnaturalearth's ne_countries().
#' Scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large'. Default = "large".
#'
#' @noRd
#' @importFrom dplyr %>%
#'
#' @examples
#' \donttest{
#'   # This is an internal function.
#' worldmap <- jbd_get_world_map(scale = "large")
#' }
jbd_get_world_map <- function(scale = "large") {
  name_en <- iso_n3 <- iso2c <- iso3c <- NULL

  check_require_cran("rnaturalearth")

  suppressWarnings({
    worldmap <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf") 
    
      # For large scales
    if(scale %in% c("large", "10")){
      worldmap <- worldmap %>%
        dplyr::mutate(iso2c = countrycode::countrycode(worldmap$name_en,
                                                       origin = "country.name.en",
                                                       destination = "iso2c"),
                      iso3c = countrycode::countrycode(worldmap$name_en,
                                                       origin = "country.name.en",
                                                       destination = "iso3c")
        ) %>% # END mutate
        dplyr::select(name_en, tidyselect::starts_with("iso")) 
    }else{ # For other scales...
      # Add some iso code to some countries polygons
        # Also, add the name column to be used
      worldmap <- worldmap %>%
        dplyr::mutate(name_en = countrycode::countrycode(iso_n3 %>% as.numeric(),
                                                         origin = "iso3n",
                                                         destination = "country.name.en"),
                      iso2c = countrycode::countrycode(worldmap$iso_n3 %>% as.numeric(),
                                                       origin = "iso3n",
                                                       destination = "iso2c"),
                      iso3c = countrycode::countrycode(worldmap$iso_n3 %>% as.numeric(),
                                                       origin = "iso3n",
                                                       destination = "iso3c")
                      ) %>%# END mutate
        dplyr::select(name_en, tidyselect::starts_with("iso")) 
    }# END else

    is.na(worldmap) %>% colSums() # number of polygons without isocode

    worldmap <- worldmap %>%
      dplyr::select(iso2c, iso3c) %>%
      sf::st_make_valid() 
  })
  return(worldmap)
}
