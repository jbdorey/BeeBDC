# This function was written by James B Dorey on the 29th of September 2022
# Its purpose is to visualise some data spatially by country
# Please contact jbdorey[at]me.com for help


#' Create country-level summary maps of species and occurrence numbers
#' 
#' Builds an output figure that shows the number of species and the number of occurrences per 
#' country. Breaks the data into classes for visualisation. Users may filter data to their taxa 
#' of interest to produce figures of interest.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param class_n Numeric. The number of categories to break the data into.
#' @param class_Style Character. The class style passed to [classInt::classIntervals()]. Options are chosen 
#' style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", 
#' "fisher", "jenks", "dpih", "headtails", or "maximum". Default = "fisher"
#' @param outPath A character vector the path to the save location for the output figure.
#' @param fileName A character vector with file name 
#' for the output figure, ending with '.pdf'.
#' @param width Numeric. The width, in inches, of the resulting figure. Default = 10.
#' @param height Numeric. The height, in inches, of the resulting figure. Default = 5.
#' @param dpi Numeric. The resolution of the resulting plot. Default = 300.
#' @param returnPlot Logical. If TRUE, return the plot to the environment. Default = FALSE.
#' @param scale Numeric or character. Passed to rnaturalearth's ne_countries().
#' Scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large'. Default = 110.
#' @param pointBuffer Numeric. Amount to buffer points, in decimal degrees. If the point is outside 
#' of a country, but within this point buffer, it will count towards that country. It's a good idea
#' to keep this value consistent with the prior flags applied. Default = 0.01.
#'
#' @return Saves a figure to the user-specified outpath and name with a global map of bee 
#' occurrence species and count data from the input dataset. 
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom sf sf_use_s2
#' @importFrom ggspatial north_arrow_fancy_orienteering
#' @importFrom grDevices gray
#' @importFrom ggplot2 xlab ylab ggtitle
#'
#' @examples
#' # Read in data
#' data(beesFlagged)
#' OutPath_Figures <- tempdir()
#' # This simple example using the test data has very few classes due to the small amount of input 
#' # data.
#' summaryMaps(
#' data = beesFlagged,
#' width = 10, height = 10,
#' class_n = 4,
#' class_Style = "fisher",
#' outPath = OutPath_Figures,
#' fileName = paste0("CountryMaps_fisher_TEST.pdf"),
#' )
#' 
#' 
summaryMaps <- function(
    data = NULL,
    class_n = 15,
    class_Style = "fisher",
    outPath = NULL,
    fileName = NULL,
    width = 10, 
    height = 5,
    dpi = 300,
    returnPlot = FALSE,
    scale = 110,
    pointBuffer = 0.01
){
  # locally bind variables to the function
  name_long<-iso_a3<-name<-geometry<-decimalLongitude<-decimalLatitude<-database_id<-
    scientificName<-species<-country<-stateProvince<-dataSource<-count<-class_count<-
    class_count2<-occCount <- indexMatch <- . <- iso_a2 <- NULL
  
  requireNamespace("dplyr")
  requireNamespace("classInt")
  requireNamespace("rnaturalearth")
  requireNamespace("ggspatial")
  
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data I'm a program not a magician.")
  }
  if(is.null(fileName)){
    stop(" - No argument provided for fileName. Please provide a fileName.")
  }
  if(is.null(outPath)){
    stop(" - No argument provided for outPath Please provide an outPath.")
  }
  
  

  #### 1.0 Download base map ####
    ###### 1.1 naturalEarth ####
  # Download world map using rnaturalearth packages
  worldMap <- rnaturalearth::ne_countries(returnclass = "sf", country = NULL,
                                          type = "map_units", scale = scale) %>%
    # Select only a subset of the naturalearthdata columns to extract
    dplyr::select(name_long, iso_a3, iso_a2, name, name_long, geometry) %>%
    sf::st_make_valid()

  # This stops a plotting error
  sf::sf_use_s2(FALSE)
  
  
    ###### 1.2 occurrences ####
    # Filter the data columns
  data <- data %>%
    # Use a subset of columns
    dplyr::select(tidyselect::any_of(c("database_id", "scientificName", "species", 
                  "country", "stateProvince", "dataSource", "geometry",
                  "decimalLongitude", "decimalLatitude"))) %>% 
      # Drop the points without coordinates
    tidyr::drop_na(tidyselect::all_of(c("decimalLongitude", "decimalLatitude")))
  
  # Make all of the US virgin islands species into US species
    #  data$countryCode <- stringr::str_replace(string = data$countryCode, 
    #                                              pattern = "VI", replacement = "US")
  # Turn occData into a simple point feature
  dataPoints <- sf::st_as_sf(data,
                         coords = c("decimalLongitude", "decimalLatitude"),
                         na.fail = TRUE,
                         # Assign the CRS from the rnaturalearth map to the point data
                         crs = sf::st_crs(worldMap)) 
  
    ##### 1.3 Extraction ####
  writeLines(" - Extracting country data from points...")
  suppressWarnings({
    # Set geometries to constant for the sake of the map
  sf::st_agr(worldMap) = "constant"
  sf::st_agr(dataPoints) = "constant"
  
  # Simplify the world map ONCE to be used later
  simplePoly <- worldMap %>% sf::st_drop_geometry() %>%
    dplyr::mutate(indexMatch = dplyr::row_number())
  
    #Extract polygon information to points
  extracted <- sf::st_intersects(dataPoints, worldMap, sparse = TRUE) %>% 
    # return a tibble with the index of each match or NA where there was no match
    dplyr::tibble(indexMatch = .) 
  # If first element is full, unlist each one
  extracted <- extracted %>%
    dplyr::mutate(indexMatch = indexMatch %>% as.character() %>%
                      # deal with problems — Take the first number where two are provided
                    stringr::str_extract("[0-9]+") %>% 
                      # Remove zero to NA
                    stringr::str_replace("^[0]$", NA_character_) %>%
                      # Make numeric
                    as.numeric()
                    ) %>%
    # drop geometry
    sf::st_drop_geometry() 
})
# rejoin
data <- extracted %>%
  dplyr::left_join(simplePoly,
                   by = "indexMatch") %>%
  # Add in the database_id
  dplyr::bind_cols(data)

rm(extracted)
  
  writeLines("Extraction complete.")
  
  ##### 1.4 Buffer fails ####
  writeLines(" - Buffering naturalearth map by pointBuffer...")
    ###### a. buffer map ####
  # Buffer the natural earth map
  suppressWarnings({
    worldMap <- worldMap %>% 
      sf::st_buffer(dist = pointBuffer)
  })
    ###### b. extract fails ####
  
  #Extract polygon information to points
  suppressWarnings({
  extracted2 <- sf::st_intersects(dataPoints %>% dplyr::filter(
    database_id %in% (data %>% dplyr::filter(is.na(name_long)) %>% pull(database_id))),
                                 worldMap %>% sf::st_make_valid(), sparse = TRUE) %>% 
    # return a tibble with the index of each match or NA where there was no match
    dplyr::tibble(indexMatch = .) 
  # If first element is full, unlist each one
  extracted2 <- extracted2 %>%
    dplyr::mutate(indexMatch = indexMatch %>% as.character() %>%
                    # deal with problems — Take the first number where two are provided
                    stringr::str_extract("[0-9]+") %>% 
                    # Remove zero to NA
                    stringr::str_replace("^[0]$", NA_character_) %>%
                    # Make numeric
                    as.numeric()
    ) %>%
    # drop geometry
    sf::st_drop_geometry() 
})
# rejoin
extracted2 <- extracted2 %>%
  dplyr::left_join(simplePoly,
                   by = "indexMatch") %>%
  # Add in the database_id
  dplyr::bind_cols(data %>% dplyr::filter(is.na(name_long)) %>% 
                     dplyr::select(!tidyselect::any_of(c("name_long", "iso_a3", "indexMatch",
                                                         "iso_a2", "name"))))

  # Rejoin with data
data <- data %>%
  dplyr::filter(!database_id %in% extracted2$database_id) %>%
  dplyr::bind_rows(extracted2)

rm(extracted2)
  
  
  #### 2.0 Species map ####
    ##### 2.1 Data prep ####
  # Get the unique country-species pairs
  spMapData <- data %>%
    dplyr::distinct(scientificName, name_long, .keep_all = TRUE) %>%
    # Group by the country
    dplyr::group_by(name_long) %>%
    # Get a count of the records per country
    dplyr::mutate(count = n()) %>% dplyr::ungroup() %>%
    # Get unique
    dplyr::distinct(name_long, .keep_all = TRUE) 
  
  ##### 2.2 Breaks ####
  # make class intervals.
  # Class intervals from ?classIntervals: fixed", "sd", "equal", "pretty", "quantile", 
  # "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  classes <- classInt::classIntervals(spMapData$count, n = class_n, 
                                      style = class_Style, dig.lab=20,
                                      dataPrecision=0)
  # Next we’ll create a new column in our sf object using the base R cut() function to cut up our 
  # percent variable into distinct groups:
  spMapData <- spMapData %>%
    dplyr::mutate(class_count = cut(count, 
                                    classes$brks %>% round(digits = 0),
                                    include.lowest = T, dig.lab = 10)) %>%
      # format the class_count column to remove spaces, add comma break, and join min and max
    dplyr::mutate(class_count2 = class_count %>%
                    stringr::str_remove("\\[|\\]|\\(|\\)") %>%
                    stringr::str_remove("\\]") %>%
                    stringr::str_replace(",", "-")) %>%
    tidyr::separate(col = class_count2, into = c("min", "max"), sep = "-") %>%
    dplyr::mutate(min = min %>% as.numeric() %>% format(big.mark = ",") %>% 
                    stringr::str_remove("\\s+"),
                  max = max %>% as.numeric() %>% format(big.mark = ",") %>% 
                    stringr::str_remove("\\s+"),
                  class_count2 = stringr::str_c(min, max, sep = "-") ) 
  
  # Join the map and occurrence data
  fullMap <-  dplyr::full_join(worldMap,  spMapData %>% sf::st_drop_geometry(),
                               by = c("name_long" = "name_long")) %>%
    # Remove na rows
    tidyr::drop_na(count)
  
  ##### 2.3 Draw map ####
  # Make the map
  (spCountryMap <- ggplot2::ggplot(data = fullMap, ) +
     # Add in a blank base-map to highlight countries with no data
     ggplot2::geom_sf(data = worldMap, size = 0.15, fill = "white")+ 
      # Plot and colour the terrestrial base map
      ggplot2::geom_sf(ggplot2::aes(fill = class_count), size = 0.15)+ 
      # Set map limits, if wanted
      ggplot2::coord_sf(expand = FALSE, ylim = c(-60,90), lims_method = "geometry_bbox") + 
      # Map formatting
      # Add in the map's north arrow
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                             pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"), 
                             style = ggspatial::north_arrow_fancy_orienteering()) + # Add in NORTH ARROW
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grDevices::gray(.1, alpha = 0.1), 
                                            linetype = "dashed", linewidth = 0.5), # Add grid lines
            panel.border = ggplot2::element_rect(color = grDevices::gray(.1, alpha = 1), 
                                        linetype = "solid", linewidth = 0.5,
                                        fill = NA), # add panel border
            # Add background - colour in the ocean
            panel.background = ggplot2::element_rect(fill = "aliceblue") )+ 
      # Change map colour scheme - CHOOSE YOUR OWN ADVENTURE
      # For Dorey colour scheme use the below
      ggplot2::scale_fill_viridis_d(option = "inferno",
                           na.value = "grey50",
                           name = "Class count",
                           labels = fullMap %>%
                             dplyr::arrange(count) %>% 
                             dplyr::distinct(class_count2) %>%
                             # options = "magma", "inferno", "plasma", "cividis"
                             dplyr::pull(class_count2)) + 
      # Add in X and Y labels
     ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") + 
      # Add in the title
     ggplot2::ggtitle( "Number of species per country")  )
  
  rm(spMapData)
  
  
  #### 3.0 occurrence map ####
  ##### 2.1 data prep ####
  # Get the unique county-database_id pairs
  mapTable <- data %>%
      # Group by the country
    dplyr::group_by(name_long) %>%
      # Get a count of the records per country
    dplyr::mutate(occCount = dplyr::n()) %>%
    # Get unique
    dplyr::distinct(name_long, .keep_all = TRUE) %>%
      # Select only these columns
    dplyr::select(name_long, occCount)
  
  # Join the map and occurrence data
  fullMap <-  dplyr::full_join(worldMap,  mapTable  %>% sf::st_drop_geometry(),
                               by = c("name_long" = "name_long")) %>%
    # Remove na rows
    tidyr::drop_na(occCount)
  
  ##### 2.2 Breaks ####
  # make class intervals.
  # Class intervals from ?classIntervals: fixed", "sd", "equal", "pretty", "quantile", 
  # "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  classes <- classInt::classIntervals(fullMap$occCount, n = class_n, 
                                      style = class_Style, dig.lab=20,
                                      dataPrecision=0)
  # Next we’ll create a new column in our sf object using the base R cut() function to cut up our 
  # percent variable into distinct groups:
  fullMap <- fullMap %>%
    dplyr::mutate(class_count = cut(occCount, 
                                    classes$brks %>% round(digits = 0),
                                    include.lowest = T, dig.lab = 10)) %>%
  # format the class_count column to remove spaces, add comma break, and join min and max
  dplyr::mutate(class_count2 = class_count %>%
                  stringr::str_remove("\\[|\\]|\\(|\\)") %>%
                  stringr::str_remove("\\]") %>%
                  stringr::str_replace(",", "-")) %>%
    tidyr::separate(col = class_count2, into = c("min", "max"), sep = "-") %>%
    dplyr::mutate(min = min %>% as.numeric() %>% format(big.mark = ",") %>% 
                    stringr::str_remove("\\s+"),
                  max = max %>% as.numeric() %>% format(big.mark = ",") %>% 
                    stringr::str_remove("\\s+"),
                  class_count2 = stringr::str_c(min, max, sep = "-") ) 
  
  ##### 2.3 Draw map ####
  # Make the map
  (occCountryMap <- ggplot2::ggplot(data = fullMap) +
      # Add in a blank base-map to highlight countries with no data
     ggplot2::geom_sf(data = worldMap, size = 0.15, fill = "white")+ 
      # Plot and colour the terrestrial base map
      ggplot2::geom_sf(ggplot2::aes(fill = class_count), size = 0.15)+ 
      # Set map limits, if wanted
     ggplot2::coord_sf(expand = FALSE, ylim = c(-60,90), lims_method = "geometry_bbox") + 
     # Map formatting
      # Add in the map's north arrow
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"), 
                                        style = ggspatial::north_arrow_fancy_orienteering()) + # Add in NORTH ARROW
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grDevices::gray(.1, alpha = 0.1), 
                                                              linetype = "dashed", 
                                                              linewidth = 0.5), # Add grid lines
                     panel.border = ggplot2::element_rect(color = grDevices::gray(.1, alpha = 1), 
                                                          linetype = "solid", linewidth = 0.5,
                                                          fill = NA), # add panel border
                     # Add background - colour in the ocean
                     panel.background = ggplot2::element_rect(fill = "aliceblue") )+ 
      # Change map colour scheme - CHOOSE YOUR OWN ADVENTURE
      # For Dorey colour scheme use the below
      ggplot2::scale_fill_viridis_d(option = "inferno",
                                    na.value = "grey50",
                                    name = "Class count",
                                    labels = fullMap %>%
                                      dplyr::arrange(occCount) %>% 
                                      dplyr::distinct(class_count2) %>%
                                      # options = "magma", "inferno", "plasma", "cividis"
                                      dplyr::pull(class_count2)) + 
      # Add in X and Y labels
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") + 
      # Add in the title
     ggplot2::ggtitle( "Number of occurrences per country")  )
  
  #### 4.0 combine + save ####
  # plot the figures together
  (combinedPlot <- cowplot::plot_grid(spCountryMap,
                                  #    + 
                                  # theme(legend.position = legend.position,
                                  #       legend.title = element_blank()),
                                  occCountryMap, 
                                  labels = c("(a)","(b)"),
                                 ncol = 1, align = 'v', axis = 'l'))
  # Save the plot
  cowplot::save_plot(filename = paste(outPath, "/", fileName, sep = ""),
                     plot = combinedPlot,
                     base_width = width,
                     base_height = height, dpi = dpi)
  if(returnPlot == TRUE){
  return(combinedPlot)}
  
  
} # END function
