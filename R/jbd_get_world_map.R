#' Internal function. Get a world map with country names and iso code
#'
#' This is a helper function used to obtain names, iso code, and the limits
#' (polygon) of world countries. Data from the package 'rnaturalearth'.
#' 
#' @param scale Passed to rnaturalearth's ne_countries().
#' Scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large'. Default = "large".
#'
#' @noRd
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' worldmap <- jbd_get_world_map()
#' }
jbd_get_world_map <- function(scale = "large") {
  name_en <- NULL

  check_require_cran("rnaturalearth")
  # check_require_github("ropensci/rnaturalearthdata")
  # loadNamespace("rnaturalearthdata")
  
  suppressWarnings({
    worldmap <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")

    # Add some iso code to some countries polygons
    iso2c <- countrycode::countrycode(unique(worldmap$name_en),
      origin = "country.name.en",
      destination = "iso2c"
    )

    iso3c <- countrycode::countrycode(unique(worldmap$name_en),
      origin = "country.name.en",
      destination = "iso3c"
    )

    iso <- dplyr::bind_cols(
      worldmap %>%
        dplyr::select(name_en, tidyselect::starts_with("iso")),
      "iso2c" = iso2c,
      "iso3c" = iso3c
    ) %>% dplyr::select(!"geometry") %>% sf::st_drop_geometry()

    filt <- !is.na(iso$iso_a2) & is.na(iso$iso2c)
    iso$iso2c[filt] <- iso$iso_a2[filt]

    filt <- !is.na(iso$iso_a3) & is.na(iso$iso3c)
    iso$iso3c[filt] <- iso$iso_a3[filt]

    worldmap <-  worldmap %>%
      dplyr::select(!c(name_en, tidyselect::starts_with("iso"))) %>%
    dplyr::bind_cols(iso)
      
    is.na(iso) %>% colSums() # number of polygons without isocode

    worldmap <- worldmap %>%
      dplyr::select(iso2c, iso3c) %>%
      sf::st_make_valid()
  })
  return(worldmap)
}
