# This function was written by James B Dorey on the 29th of September 2022
# Its purpose is to visualise all flags for each dataSource (simplified to the text before the 
  # first underscore)
# Please contact jbdorey[at]me.com for help
#' Generate a plot summarising flagged data
#' 
#' Creates a compound bar plot that shows the proportion of records that pass or fail each flag (rows)
#' and for each data source (columns). The function can also optionally return a point map for 
#' a user-specified species when plotMap = TRUE. This function requires that your dataset has been
#' run through some filtering functions - so that is can display logical columns starting with
#' ".".
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param flagColours A character vector. Colours in order of pass (TRUE), fail (FALSE), and NA.
#' Default = c("#127852", "#A7002D", "#BDBABB").
#' @param fileName Character. The name of the file to be saved, ending in ".pdf".
#' If saving as a different file type, change file type suffix - See `device`.
#' @param outPath A character path. The path to the directory in which the figure will be saved.
#' Default = OutPath_Figures.
#' @param width Numeric. The width of the output figure in user-defined units Default = 15.
#' @param height Numeric. The height of the output figure in user-defined units Default = 9.
#' @param units Character. The units for the figure width and height passed to [ggplot2::ggsave()] 
#' ("in", "cm", "mm", or "px"). Default = "in".
#' @param dpi Numeric. Passed to [ggplot2::ggsave()]. Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or 
#' "screen" (72). Applies only to raster output types. Default = 300.
#' @param bg Character. Passed to [ggplot2::ggsave()]. Background colour. If NULL, uses the plot.background fill value from the plot theme.
#' Default = "white."
#' @param device Character. Passed to [ggplot2::ggsave()]. Device to use. Can either be a device function (e.g. png), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' Default = "pdf". If not using default, change file name suffix in fileName argument.
#' @param speciesName Optional. Character. A species name, as it occurs in the user-input nameColumn.
#' If provided, the data will be filtered to this species for the plot.
#' @param nameColumn Optional. Character. If speciesName is not NULL, enter the column to look 
#' for the species in. A User might realise that, combined with speciesName, figures can be made for
#' a variety of factors.
#' @param saveFiltered Optional. Logical. If TRUE, the filtered data will be saved to the computer 
#' as a .csv file.
#' @param plotMap Logical. If TRUE, the function will produce a point map. Tested for use with one
#' species at a time; i.e., with speciesName is not NULL.
#' @param filterColumn Optional. The flag column to display on the map. Default = .summary.
#' @param mapAlpha Optional. Numeric. The opacity for the points on the map.
#' @param xbuffer Optional. Numeric vector. A buffer in degrees of the amount to increase the
#' min and max bounds along the
#' x-axis. This may require some experimentation, keeping in mind
#' the negative and positive directionality of hemispheres. Default = c(0,0).
#' @param ybuffer Optional. Numeric vector. A buffer in degrees of the amount to increase the
#' min and max bounds along the y-axis. This may require some experimentation, keeping in mind
#' the negative and positive directionality of hemispheres. Default = c(0,0).
#' @param ptSize Optional. Numeric. The size of the points as passed to ggplot2. Default = 1.
#' @param saveTable Optional. Logical. If TRUE, the function will save the data used to produce the 
#' compound bar plot.
#' @param jitterValue Optional. Numeric. The value to jitter points by in the map in decimal degrees.
#' @param returnPlot Logical. If TRUE, return the plot to the environment. Default = FALSE.
#' @param ... Optional. Extra variables to be fed into [forcats::fct_recode()] to change names on plot.
#' For example... 'B. Mont.' = "BMont", 'B. Minkley' = "BMin", Ecd = "Ecd", Gaiarsa = "Gai"
#'
#' @return Exports a compound bar plot that summarises all flag columns. Optionally can also return 
#' a point map for a particular species in tandem with the summary plot.
#' @export
#' 
#' @importFrom dplyr across desc %>%
#' @importFrom ggplot2 geom_sf geom_point geom_jitter scale_color_manual coord_sf 
#'   element_rect scale_fill_viridis_d
#' xlab ylab ggtitle 
#' @importFrom ggspatial north_arrow_fancy_orienteering annotation_north_arrow
#' @importFrom grDevices gray
#'
#' @examples
#' # import data
#' data(beesFlagged)
#' OutPath_Figures <- tempdir()
#'  # Visualise all flags for each dataSource (simplified to the text before the first underscore)
#' plotFlagSummary(
#'   data = beesFlagged,
#'   # Colours in order of pass (TRUE), fail (FALSE), and NA
#'   flagColours = c("#127852", "#A7002D", "#BDBABB"),
#'   fileName = paste0("FlagsPlot_TEST_", Sys.Date(),".pdf"),
#'   outPath = OutPath_Figures,
#'   width = 15, height = 9,
#'   # OPTIONAL:
#'   #\   #  # Filter to species
#'   #\   speciesName = "Holcopasites heliopsis",
#'   #\   # column to look in
#'   #\   nameColumn = "species",
#'   #\   # Save the filtered data
#'   #\   saveFiltered = TRUE,
#'   #\   # Filter column to display on map
#'   #\   filterColumn = ".summary",
#'   #\   plotMap = TRUE,
#'   #\   # amount to jitter points if desired, e.g. 0.25 or NULL
#'   #\   jitterValue = NULL,
#'   #\   # Map opacity value for points between 0 and 1
#'   #\   mapAlpha = 1,
#'   # Extra variables can be fed into forcats::fct_recode() to change names on plot
#'   GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
#'   ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minkley' = "BMin", Ecd = "Ecd",
#'   Gaiarsa = "Gai", EPEL = "EPEL"
#' )
#' 
#' 
#' 
plotFlagSummary <- function(
    data = NULL,
    flagColours = c("#127852", "#A7002D", "#BDBABB"),
    fileName = NULL,
    outPath = OutPath_Figures,
    width = 15, height = 9, units = "in",
    dpi = 300,
    bg = "white", device = "pdf",
      # OPTIONAL:
    speciesName = NULL,
    saveFiltered = FALSE,
    filterColumn = ".summary",
    nameColumn = NULL,
    plotMap = FALSE,
    mapAlpha = 0.5,
    xbuffer = c(0,0),
    ybuffer = c(0,0),
    ptSize = 1,
    saveTable = FALSE,
    # Jitter map? enter jitter amount
    jitterValue = NULL,
    returnPlot = FALSE,
    ...
){
  # locally bind variables to the function
  OutPath_Figures <- decimalLatitude <- decimalLongitude <- . <- dataSource <- NULL
  database <- flags <- value <- count <- .data <- NULL
  
  requireNamespace("ggspatial")
  requireNamespace("dplyr")
  requireNamespace("bdc")
  requireNamespace("forcats")
  
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data I'm a program not a magician.")
  }
  if(is.null(outPath)){
    stop(" - Please provide an argument for outPath Seems reckless to let me just guess.")
  }
  if(is.null(speciesName) & saveFiltered == TRUE){
    stop(" - saveFiltered cannot be TRUE if no speciesName is provided to filter occurrences.\n",
         "This functionality is provided to save the filtered dataset for examination.")
  }
  ###### b. warnings ####
  if(is.null(speciesName) & plotMap == TRUE){
    warning(" - plotMap is not tested with no speciesName provided to filter occurrences.\n",
            "This functionality is provided to check the filtered dataset for examination and I fear ",
            "that this might result in an intense task to run... Maybe not... Enjoy!")
  }
  if(is.null(fileName)){
    writeLines(" - No argument provided for fileName. Using default of 'FlagsPlot_DATE.pdf'")
    fileName = paste0("FlagsPlot_", Sys.Date(),".pdf")
  }
  if(is.null(filterColumn)){
    writeLines(" - No argument provided for filterColumn Using default of '.summary'")
    filterColumn = ".summary"
  }
  if(!is.null(speciesName) & is.null(nameColumn)){
    writeLines(" - nameColumn is not provided. Defaulting to scientificName.\n")
    nameColumn = "scientificName"
  }

  
  
#### 1.0 Prepare data ####
  ##### 1.1 Optional species filter ####
    # If a species name is provided then filter to ONLY that/those species
  if(!is.null(speciesName)){
    writeLines(" - Filtering to selected species...")
      ###### a. filter ####
      # Filter data
    data <- data %>%
      dplyr::filter( data[[nameColumn]] %in% speciesName)
    writeLines(paste0(" - Selected species has ",
               format(nrow(data), big.mark = ","),
               " occurrences."))
    # OPTIONAL save filtered data
    ##### b. save ####
      # If a save location is provided, then save the filtered dataset
    if(saveFiltered == TRUE){
      data %>%
        readr::write_excel_csv(paste0(outPath, "/FlagsPlot_", speciesName,".csv"))
    }
    ##### c. map data ####
      # Save a version of the data for mapping, if asked by user
    if(plotMap == TRUE){
      mapData <- data %>%
          # Select the columns to use
        dplyr::select(c(decimalLatitude, decimalLongitude,
                        tidyselect::all_of(c(nameColumn, filterColumn)))) 
          # Sort the filterColumn to have TRUE on top of FALSE
      mapData <- mapData %>%
        dplyr::select(tidyselect::all_of(filterColumn)) %>%
        dplyr::mutate(dplyr::across(1, as.character)) %>%
        dplyr::bind_cols(mapData %>% dplyr::select(!tidyselect::all_of(filterColumn))) %>% 
        dplyr::arrange(dplyr::desc(.))
    }
  } # END !is.null(speciesName)
  

  ##### 1.3 Prepare for plot ####
  writeLines(" - Preparing data to plot...")
  # Make a column with the dataSource without numbers
  data <- data %>%
    # Make a new column with the dataSource names but not the specifics
    dplyr::mutate(database = stringr::str_replace(dataSource,
                                                  pattern = "_.*",
                                                  replacement = "")) %>%
    # Group by the new column (i.e. database)
    dplyr::group_by(database) %>%
    # Select only the filter columns (starting with ".") and the database column
    dplyr::select(database, tidyselect::starts_with(".")) %>%
    #mutate(across(is.logical, ~as.numeric(.x))) %>%
    #dplyr::filter(database %in% c("ASP", "Ecd")) %>%
    # Pivot the data to a longer format for plotting in ggplot2.
    tidyr::pivot_longer(cols = tidyselect::starts_with("."),
                        names_to = "flags",
                        values_to = "value") %>%
    dplyr::group_by(database, flags, value) %>%
    dplyr::summarise(count = dplyr::n())
  
  # Make flag type
  data$flagType <- data$flags %>%
    dplyr::recode(.coordinates_empty = "Initial", .coordinates_outOfRange = "Initial", 
                  .basisOfRecords_notStandard = "Initial", .coordinates_country_inconsistent = "Initial",
                  .occurrenceAbsent = "Initial", .unLicensed = "Initial", .GBIFflags = "Initial",
                  # Taxonomy
                  .scientificName_empty = "Taxonomy",.invalidName = "Taxonomy", .uncer_terms = "Taxonomy",
                  #Space
                  .rou = "Space", .uncertaintyThreshold = "Space", .cap = "Space", .cen = "Space",
                  .equ = "Space",.gbf = "Space", .inst = "Space", .zer = "Space",.sea = "Space",
                  .val = "Space",
                  .countryOutlier = "Space", .continentOutlier = "Space", .expertOutlier = "Space",
                  .sequential = "Space", .lonFlag = "Space", .latFlag = "Space", .gridSummary = "Space", 
                  # Time
                  .eventDate_empty = "Time", .year_outOfRange = "Time", 
                  # Summary
                  .duplicates = "Summary", .summary = "Summary") %>%
    factor(levels = c("Initial",
                         "Taxonomy","Space",
                         "Time","Summary")) %>% 
    forcats::fct_relevel("Initial",
                         "Taxonomy","Space",
                         "Time","Summary")
    
  
  # You can turn the flag columns into factors and order them here
  data$flags <- data$flags %>%
    dplyr::recode_factor(.coordinates_empty = "No coordinates", 
                         .coordinates_outOfRange = "Point off map", 
                         .basisOfRecords_notStandard = "Excluded basis of record", 
                         .coordinates_country_inconsistent = "Coords. & country inconsistent",
                         .occurrenceAbsent = "Absent record", 
                         .unLicensed = "Protected by license", 
                         .GBIFflags = "GBIF flags",
                         # Taxonomy
                         .scientificName_empty = "No scientific name",
                         .invalidName = "Name didn't match", 
                         .uncer_terms = "Taxonomy qualifier",
                         #Space
                         .rou = "Coordinates rounded",
                         .uncertaintyThreshold = "High coordinate uncertainty",
                         .cap = "Capital centroid", 
                         .cen = "Country centroid",
                         .equ = "Coordinates equal",
                         .gbf = "Point on GBIF HQ", 
                         .inst = "Point on institution", 
                         .zer = "Coordinates zero",
                         .sea = "Point in sea", 
                         .val = "Coordinates invalid",
                         .countryOutlier = "Country outliers",
                         .continentOutlier = "Continent outliers",
                         .expertOutlier = "Expert outliers",
                         .sequential = "Coordinate fill-down", 
                         .lonFlag = "Gridded longitudes", .latFlag = "Gridded latitudes",
                         .gridSummary = "Gridded lat & lon", 
                         # Time
                         .eventDate_empty = "No event date", .year_outOfRange = "Year out of range", 
                         # Summary
                         .duplicates = "Duplicate", .summary = "Summary") %>%
    forcats::fct_rev() 
  
  # Factorise and order by database
  data$database <- data$database %>%
    dplyr::recode_factor(...)
  
  ##### 1.4 Save table ####
    # If user choses to save the table that makes the plot, do so here.
  if(saveTable == TRUE){
    data %>%
      readr::write_excel_csv(paste0(outPath, "/",
                              stringr::str_replace(fileName,
                                                   pattern = "\\.pdf",
                                                   replacement = ".csv")))
  }

  # Make data$value into a character type
  data$value <- as.character(data$value)
  
  # NA levels may be removed from the plot here, but I prefer to keep them.
    # data <- data %>%
    #   tidyr::drop_na()
  
#### 2.0 Plot ####
    ##### 2.1 Build plot ####
  writeLines(" - Building plot...")
  plot <-  ggplot2::ggplot(data = data) +
    # Set up the plot facets
    ggplot2::facet_grid( flagType~database, scales = "free", space= "free_y") + 
    # Make the bar plots
    ggplot2::geom_bar(ggplot2::aes(y = flags, x = count, fill = as.factor(value)), 
                      position="fill", stat='identity') +
    # Colour and label the plots
    ggplot2::scale_fill_manual(labels = c("TRUE" = "Pass",
                                 "FALSE" = "Fail",
                                 "NA" = "NA"),
                      values = c("TRUE" = flagColours[1],
                                 "FALSE" = flagColours[2],
                                 "NA" = flagColours[3])
                      ) +
    # Set up the theme stuff
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), # element_text(angle = 35, vjust = 0.5, hjust=0.5),
          #axis.text.y = element_text(colour = as.character(flagCols)),
          strip.placement = "outside",
          panel.background = ggplot2::element_rect(fill = "white"),
          plot.title = ggplot2::element_text(size = 20, face = "bold"))  + 
    ggplot2::labs(x ="Proportion flagged", y = "Flag columns", fill = "Filter") 
    
  ##### 2.2 Filtered title+map+save ####
  if(!is.null(speciesName)){
    ###### a. title ####
      # Add title if filtered by species name
    plot <- plot +
      ggplot2::ggtitle(paste0("Species: ", speciesName))
    
    ##### b. map ####
    if(plotMap == TRUE){
        # Make into an ordered factor
      mapData <- mapData %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(filterColumn),
                        ~factor(.x, levels = c(FALSE, TRUE), ordered = TRUE))) %>%
        tidyr::drop_na(decimalLatitude, decimalLongitude)
      
      # Download world map using rnaturalearth packages
      WorldMap_layer <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf", 
                                     country = NULL, type="map_units") 
      # Create the checklist map
      (PointMap <- ggplot2::ggplot(data = WorldMap_layer ) +
          # CORE plotting of map and data
          # Plot and colour the terrestrial base map
          ggplot2::geom_sf(ggplot2::aes(fill = NULL), size = 0.15)+ 
          # plot point data
            # POINTS IF IS NULL; i.e. DON'T jitter
          {if(is.null(jitterValue))
            ggplot2::geom_point(data = mapData %>% dplyr::filter(.[[1]] == "FALSE"),
                     mapping = ggplot2::aes(x = decimalLongitude, y = decimalLatitude,
                                   colour = .data[[filterColumn]]),
                                   size = ptSize,
                     alpha = mapAlpha)} +
          {if(is.null(jitterValue))
            ggplot2::geom_point(data = mapData %>% dplyr::filter(.[[1]] == "TRUE"),
                       mapping = ggplot2::aes(x = decimalLongitude, y = decimalLatitude,
                                     colour = .data[[filterColumn]]),
                       size = ptSize,
                       alpha = mapAlpha)} +
        # POINTS IF IS NOT NULL; i.e. jitter
          {if(!is.null(jitterValue))ggplot2::geom_jitter(mapData %>% 
                                                           dplyr::filter(.[[1]] == "FALSE"), 
                           mapping = ggplot2::aes(x = decimalLongitude, y = decimalLatitude,
                                         colour = .data[[filterColumn]]),
                           size = ptSize,
                           alpha = mapAlpha, width = jitterValue, height = jitterValue)}+ 
          {if(!is.null(jitterValue))ggplot2::geom_jitter(mapData %>% 
                                                           dplyr::filter(.[[1]] == "TRUE"), 
                                                mapping = ggplot2::aes(x = decimalLongitude, 
                                                                       y = decimalLatitude,
                                                              colour = .data[[filterColumn]]),
                                                size = ptSize,
                                                alpha = mapAlpha, width = jitterValue, 
                                                height = jitterValue)}+ 
          ggplot2::scale_color_manual(values = c(
                                        "TRUE" = flagColours[[1]], # "#013766",
                                        "FALSE" = flagColours[[2]]), #"#ac0e28"),
                             name = "Passed occ.") +
          # Set map limits, if wanted
          ggplot2::coord_sf(expand = TRUE, 
                   ylim = c(min(mapData$decimalLatitude, na.rm = TRUE)+ybuffer[[1]], 
                            max(mapData$decimalLatitude, na.rm = TRUE)+ybuffer[[2]]),
                   xlim = c(min(mapData$decimalLongitude, na.rm = TRUE)+xbuffer[[1]], 
                            max(mapData$decimalLongitude, na.rm = TRUE)+xbuffer[[2]]),
                   lims_method = "box") + 
          # Map formatting
          # Add in the map's north arrow
          ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                 pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"), 
                                 style = ggspatial::north_arrow_fancy_orienteering()) + # Add in NORTH ARROW
          ggplot2::theme(panel.grid.major = ggplot2::element_line(color = gray(.1, alpha = 0.1), 
                                                linetype = "dashed", linewidth = 0.5), # Add grid lines
                panel.border = ggplot2::element_rect(color = gray(.1, alpha = 1), 
                                            linetype = "solid", linewidth = 0.5,
                                            fill = NA), # add panel border
                panel.background = ggplot2::element_rect(fill = "aliceblue") ,
                plot.title = ggplot2::element_text(face = "italic"))+ # Add background - colour in the ocean
          # Change map colour scheme
          ggplot2::scale_fill_viridis_d(option = "magma") + # options = "magma", "inferno", "plasma", "cividis"
          # Add in X and Y labels
          ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") + 
          # Add in the title
          ggplot2::ggtitle( speciesName) )
      # save as the map as 10*6"
      ggplot2::ggsave(paste0("/Map_FlagsPlot_", speciesName, ".pdf"), plot = PointMap, device = "pdf", 
              width = 10, height = 5, dpi = 300, path = outPath)
    }
    
    ##### c. save ####
    # Filtered save plot
    if(!is.null(speciesName)){
      ggplot2::ggsave(filename = paste0("/FlagsPlot_", speciesName,".pdf"),
                      path = outPath,
                      plot = plot, dpi = dpi, bg = bg, device = device,
                      width = width, height = height, units = units)}
  }

  # Save the figure
  ##### 2.2 Save all option ####
  if(is.null(speciesName)){
  ggplot2::ggsave(filename = fileName,
                  path = outPath,
                  plot = plot, dpi = dpi, bg = bg, device = device,
                  width = width, height = height, units = units)}
 
  if(returnPlot == TRUE){
    return(plot)}
  
} # END function

