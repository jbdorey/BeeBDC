# This function was written by James B Dorey and Angela Nava Bolańos from the 12 th of October 2022
# Its purpose is to create series of output figures or csv files for specified species
# Please contact jbdorey[at]me.com for help
#' Creates interactive html maps for species
#' 
#' Uses the occurrence data (preferably uncleaned) and outputs interactive .html maps that can be opened
#' in your browser to a specific directory. The maps can highlight if an occurrence has passed all filtering
#' (.summary == TRUE) or failed at least one filter (.summary == FALSE). This can be modified by first running
#' [BeeBDC::summaryFun()] to set the columns that you want to be highlighted. It can also highlight occurrences
#' flagged as expert-identified or country outliers.
#' 
#' @param data A data frame or tibble. Occurrence records to use as input.
#' @param outPath A directory as character. Directory where to save output maps.
#' @param lon Character. The name of the longitude column. Default = "decimalLongitude".
#' @param lat Character. The name of the latitude column. Default = "decimalLatitude".
#' @param speciesColumn Character. The name of the column containing species names (or another factor)
#' to build individual maps from. Default = "scientificName".
#' @param speciesList A character vector. Should contain species names as they appear in the 
#' speciesColumn to make maps of. User can also specify "ALL" in order to make maps of all 
#' species present in the data. Hence, a user may first filter their data and then use "ALL".
#' @param countryList A character vector. Country names to map, or NULL for to map ALL countries.
#' @param jitterValue Numeric. The amount, in decimal degrees, to jitter the map points by - this 
#' is important for separating stacked points with the same coordinates.
#' @param onlySummary Logical. If TRUE, the function will not look to plot country or 
#' expert-identified outliers in different colours.
#' @param overWrite Logical. If TRUE, the function will overwrite existing files in the provided
#' directory that have the same name.
#' Default = TRUE.
#' @param TrueAlwaysTop If TRUE, the quality (TRUE) points will always be displayed on top of other points. 
#' If FALSE, then whichever layer was turned on most-recently will be displayed on top.
#' @param excludeApis_mellifera Logical. If TRUE, will not map records for Apis mellifera. Note: in most cases 
#' A. mellifera has too many points, and the resulting map will take a long time to make and be difficult to open.
#' Default = TRUE.
#' @param pointColours A character vector of colours. In order provide colour for TRUE, FALSE, countryOutlier, and customOutlier.
#' Default = c("blue", "darkred","#ff7f00", "black").
#'
#' @return Exports .html interactive maps of bee occurrences to the specified directory.
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr across where
#'
#' @examples
#' OutPath_Figures <- tempdir()
#' 
#' interactiveMapR(
#' # occurrence data - start with entire dataset, filter down to these species
#' data = BeeBDC::bees3sp, # %>%
#'   # Select only those species in the 100 randomly chosen
#'   # dplyr::filter(scientificName %in% beeData_interactive$scientificName),
#'   # Select only one species to map
#'   # dplyr::filter(scientificName %in% "Agapostemon sericeus (Forster, 1771)"),
#' # Directory where to save files
#' outPath = paste0(OutPath_Figures, "/interactiveMaps_TEST"),
#' # lat long columns
#' lon = "decimalLongitude",
#' lat = "decimalLatitude",
#' # Occurrence dataset column with species names
#' speciesColumn = "scientificName",
#' # Which species to map - a character vector of names or "ALL"
#' # Note: "ALL" is defined AFTER filtering for country
#' speciesList = "ALL",
#' # studyArea
#' countryList = NULL, 
#' # Point jitter to see stacked points - jitters an amount in decimal degrees
#' jitterValue = 0.01,
#' # If TRUE, it will only map the .summary column. Otherwise, it will map .summary
#' # which will be over-written by countryOutliers and manualOutliers
#' onlySummary = TRUE,
#' excludeApis_mellifera = TRUE,
#' overWrite = TRUE,
#'   # Colours for points which are flagged as TRUE, FALSE, countryOutlier, and customOutlier
#' pointColours = c("blue", "darkred","#ff7f00", "black")
#' )

interactiveMapR <- function(
      # occurrence data
    data = NULL,
      # Directory where to save files
    outPath = NULL,
      # lat long columns
    lon = "decimalLongitude",
    lat = "decimalLatitude",
      # Occurrence dataset column with species names
    speciesColumn = "scientificName",
      # Which species to map - a character vector of names or "ALL"
    speciesList = NULL,
    countryList = NULL,
    jitterValue = NULL,
    onlySummary = TRUE,
    overWrite = TRUE,
    TrueAlwaysTop = FALSE,
    excludeApis_mellifera = TRUE,
    pointColours = c("blue", "darkred","#ff7f00", "black")
    ){
  # locally bind variables to the function
  country <- .data <- scientificName <- expertOutlier <- .countryOutlier <- .summary <-
    providers <- databaseSpp <- .expertOutlier <- NULL
    
  
  requireNamespace("htmlwidgets")
  requireNamespace("leaflet")
  requireNamespace("dplyr")
  
  # Ensure that working directories are maintain on exit from function
  oldwd <- getwd()           # code line i 
  on.exit(setwd(oldwd))        # code line i+1 
  
  
#### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(paste0(" - No data was given. Please specify the data that you want to map ",
                "for your data-cleaning adventures. I'll do the rest."))
  }
  if(is.null(outPath)){
    stop(paste0(" - No outPath was given. Please specify the directory to save the maps to."))
  }
  
  ##### 0.2 Packages ####
  # Save the original directory 
  olddir <- getwd()
  

  ##### 0.3 Directories ####
  # Create directory if it does not exist
if (!dir.exists(outPath)) {
  dir.create(outPath, recursive = TRUE)}
  # Set directory
setwd(outPath) #directory of work

  # data$IDall <- paste0(1:nrow(data)) #to add an ID by row

  #### 1.0 Data prep ####
    ##### 1.1 Remove na+ ####
data <- data %>%
  tidyr::drop_na(tidyselect::any_of(c(lon, lat)))  %>%
    # Rename the lat and lon to darwincore
  dplyr::rename("decimalLongitude" = tidyselect::all_of(lon),
                "decimalLatitude" = tidyselect::all_of(lat) )

# Stop if no lat/lon
if(nrow(data) == 0){
  stop("It looks like there may be no lat and lon data. Check that it exists and is not NA.")
}

# If there is no .expertOutlier then add one as all NA
if(!".expertOutlier" %in% colnames(data)){
  message("The column .expertOutlier was not found. One will be created with all values = TRUE.")
  data <- data %>% 
    dplyr::mutate(.expertOutlier = TRUE)
}

##### 1.2 Country list ####
# Select only the countries user provides
if(!is.null(countryList)){
  data <- data %>%
    dplyr::filter(country %in% countryList)
}

    ##### 1.3 Species list ####
if(any(stringr::str_detect(speciesList, "ALL")) == FALSE){
# Prepare the data for the loop
  data <- data %>% 
  # Select ONLY the species requested
  dplyr::filter(.data[[speciesColumn]] %in% speciesList)
}else{
  speciesList <- unique(data[[speciesColumn]])
} # END if else statement


##### 1.4 excludeApis_mellifera ####
if(excludeApis_mellifera == TRUE){
  data <- data %>%
    dplyr::filter(!scientificName == "Apis mellifera Linnaeus, 1758")
  speciesList <- setdiff(speciesList, "Apis mellifera Linnaeus, 1758")
}


##### 1.5 Overwrite ####
if(overWrite == FALSE){
    # Find completed species
  existingFiles <- list.files(path = outPath) %>%
    stringr::str_remove("\\.html")
    # remove them from the to-do list
  speciesList <- setdiff(speciesList, existingFiles)
    # STOP if no maps will be produced
  if(length(speciesList) == 0){
    stop("With overWrite = FALSE, there are no new maps to produce.")
  }
    # Re-filter the data to use only wanted species
  data <- data %>% 
    # Select ONLY the species requested
    dplyr::filter(.data[[speciesColumn]] %in% speciesList)
}

    ##### 1.6 Jitter ####
  # If the user specifies a jitter value, add that calue
if(!is.null(jitterValue)){
  data <- data %>%
    dplyr::mutate(
      decimalLongitude = base::jitter(data[[lon]], amount = jitterValue),
      decimalLatitude = base::jitter(data[[lat]], amount = jitterValue)
    )
}else{
    # If no jitter, ensure that the lat lon columns are the same
  data <- data %>%
    dplyr::mutate(
      decimalLongitude = data[[lon]],
      decimalLatitude = data[[lat]])
} # END Jitter


  # Make a new column to colour by if onlySummary == FALSE
if(onlySummary == FALSE){
  data <- data %>% 
    dplyr::mutate(mapLevels = dplyr::if_else(.expertOutlier == FALSE,
                                             "expertOutlier",
                    dplyr::if_else(.countryOutlier == FALSE | is.na(.countryOutlier),
      "countryOutlier", dplyr::if_else(.summary == FALSE,
                                       "FALSE", "TRUE"))) %>%
        factor(c("TRUE", "FALSE", "countryOutlier", "expertOutlier"), 
               levels = c("TRUE", "FALSE", "countryOutlier", "expertOutlier"), 
               ordered = TRUE)
      )
  colPal = leaflet::colorFactor(pointColours, 
                             levels = c("TRUE", "FALSE",
                                        "countryOutlier", "expertOutlier"))
  
}else{ # Make colour palette for == TRUE
    # Only take the first two, if more are provided
  pointColours <- pointColours[1:2]
  colPal = leaflet::colorFactor(pointColours, 
                             levels = c("TRUE", "FALSE"))
}
  


# ensure UTF-8 encoding
old <- options()         # code line i 
on.exit(options(old))      # code line i+1 

options(encoding = "UTF-8")

data <- data %>% mutate(across(where(is.character), 
                              function(x){iconv(x, 
                                                to = "UTF-8",
                                                sub = "")}))

#### 2.0 produce maps ####
#function for leaflet maps
for (x in 1:length(speciesList)){
    # Filter to the xth species
  databaseLoop <- data %>% 
    dplyr::filter(.data[[speciesColumn]] == speciesList[[x]] %>% iconv(x,
                                                                       from = "UTF-8",
                                                                       to = "UTF-8",
                                                                       sub = ""))
    # Split data into classes
  if(onlySummary == FALSE){
    databaseLoop <- split(databaseLoop, f= databaseLoop$mapLevels, drop = TRUE)
  }else{
  databaseLoop <- split(databaseLoop, databaseLoop$.summary)}
                                    #here you can change the number of spp
  
  # Make the base map
  mdatabaseSpp <- leaflet::leaflet(data = databaseLoop ) %>% 
      # Add map panes
    leaflet::addMapPane(name = "maplabels_FALSE", zIndex = 410) %>% 
    leaflet::addMapPane(name = "maplabels_TRUE", zIndex = 420) %>% # higher zIndex rendered on top
    # Base groups
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles("Stamen.TonerLite", group = "Toner Lite",
                              layerId = 300,
                              options = leaflet::providerTileOptions(zIndex = 500))
    # For the names in the list, apply the points function
      # Apply each walkName in a for loop to add to the map.
for(i in 1:length(names(databaseLoop))){
  walkName <- names(databaseLoop)[[i]]
  databaseSpp <- databaseLoop[[walkName]]
  mdatabaseSpp <- databaseLoop[[walkName]] %>%
    leaflet::addCircleMarkers(map = mdatabaseSpp,
                              data = databaseSpp,
                              lng = ~decimalLongitude, lat = ~decimalLatitude, ###then you can specify what do you want in the popup window from your data
                              group = walkName,
                              if(TrueAlwaysTop == TRUE){
                              options = leaflet::leafletOptions(
                                pane = if(walkName == TRUE){"maplabels_TRUE"}else{
                                  "maplabels_FALSE"})},
                              popup = stringr::str_c(
                                sep = "",
                                ###### a. basic data ####
                                "<b>Basic data </b> - ",
                                "ID: ", databaseSpp$database_id, " ", #databaseSpp is the name of data and ID the name of the column
                                if("family" %in% colnames(databaseSpp)){
                                  paste0("Family: ", databaseSpp$family, 
                                         ";   ")},
                                if("scientificName" %in% colnames(databaseSpp)){
                                  paste0("Species: ", databaseSpp$scientificName, 
                                         ";   ")},
                                            if("institutionCode" %in% colnames(databaseSpp)){
                                                           paste0(" institutionCode: ", databaseSpp$institutionCode, 
                                                          ";   ")},
                                            if("catalogNumber" %in% colnames(databaseSpp)){
                                                           paste0("catalogNumber: ", databaseSpp$catalogNumber, 
                                                                  ";   ")},
                                            if("verbatimScientificName" %in% colnames(databaseSpp)){
                                                           paste0("Original name: ", databaseSpp$verbatimScientificName, 
                                                                  ";   ")},
                                            if("scientificNameAuthorship" %in% colnames(databaseSpp)){
                                                           paste0("Authority: ", databaseSpp$scientificNameAuthorship, 
                                                                  ";   ")},
                                ###### b. summary data ####
                                "<p></p> <b>Summary flag</b> - ", databaseSpp$.summary,
                                ###### c. initial data ####
                                "<p></p><b>Initial flags</b> - ",
                                            if(".coordinates_empty" %in% colnames(databaseSpp)){
                                                           paste0("No coordinates: ", databaseSpp$.coordinates_empty, 
                                                                  ";   ")},
                                            if(".coordinates_outOfRange" %in% colnames(databaseSpp)){
                                                           paste0("Point off map: ", databaseSpp$.coordinates_outOfRange, 
                                                                  ";   ")},
                                            if(".basisOfRecords_notStandard" %in% colnames(databaseSpp)){
                                                           paste0("Excluded basis of record: ", databaseSpp$.basisOfRecords_notStandard, 
                                                                  ";   ")},
                                            if(".coordinates_country_inconsistent" %in% colnames(databaseSpp)){
                                                           paste0("Coords. & country inconsistent: ", databaseSpp$.coordinates_country_inconsistent, 
                                                                  ";   ")},
                                            if(".occurrenceAbsent" %in% colnames(databaseSpp)){
                                                           paste0("Absent record: ", databaseSpp$.occurrenceAbsent, 
                                                                  ";   ")},
                                            if(".unLicensed" %in% colnames(databaseSpp)){
                                                           paste0("Protected by license: ", databaseSpp$.unLicensed, 
                                                                  ";   ")},
                                ###### d. taxonomy data ####
                                              # Taxonomy
                                "<p></p><b>Taxonomy flags</b> - ",
                                            if(".scientificName_empty" %in% colnames(databaseSpp)){
                                                           paste0("No scientific name: ", databaseSpp$.scientificName_empty, 
                                                                  ";   ")},
                                            if(".invalidName" %in% colnames(databaseSpp)){
                                                           paste0("Name didn't match: ", databaseSpp$.invalidName, 
                                                                  ";   ")},
                                            if(".uncer_terms" %in% colnames(databaseSpp)){
                                                           paste0("Taxonomy qualifier: ", databaseSpp$.uncer_terms, 
                                                                  ";   ")},
                                ###### e. space data ####
                                              # space
                                "<p></p><b>Space flags</b> - ",
                                            if(".rou" %in% colnames(databaseSpp)){
                                                           paste0("Coordinates rounded: ", databaseSpp$.rou, 
                                                                  ";   ")},
                                            if(".uncertaintyThreshold" %in% colnames(databaseSpp)){
                                              paste0("High coordinate uncertainty: ", databaseSpp$.uncertaintyThreshold, 
                                                     ";   ")},
                                            if(".cap" %in% colnames(databaseSpp)){
                                             paste0("Capital centroid: ", databaseSpp$.cap, 
                                                    ";   ")},
                                            if(".cen" %in% colnames(databaseSpp)){
                                                           paste0("Country centroid: ", databaseSpp$.cen, 
                                                                  ";   ")},
                                            if(".gbf" %in% colnames(databaseSpp)){
                                                           paste0("Point on GBIF HQ: ", databaseSpp$.gbf, 
                                                                  ";   ")},
                                            if(".equ" %in% colnames(databaseSpp)){
                                                           paste0("Coordinates equal: ", databaseSpp$.equ, 
                                                                  ";   ")},
                                            if(".inst" %in% colnames(databaseSpp)){
                                                           paste0("Point on institution: ", databaseSpp$.inst, 
                                                                  ";   ")},
                                            if(".zer" %in% colnames(databaseSpp)){
                                                           paste0("Coordinates zero: ", databaseSpp$.zer, 
                                                                  ";   ")},
                                            if(".val" %in% colnames(databaseSpp)){
                                                           paste0("Coordinates zero: ", databaseSpp$.val, 
                                                                  ";   ")},
                                           if(".sea" %in% colnames(databaseSpp)){
                                             paste0("In sea: ", databaseSpp$.sea, 
                                                    ";   ")},
                                           
                                            if(".countryOutlier" %in% colnames(databaseSpp)){
                                                           paste0("Country outliers: ", databaseSpp$.countryOutlier, 
                                                                  ";   ")},
                                            if(".stateOutlier" %in% colnames(databaseSpp)){
                                              paste0("State outliers: ", databaseSpp$.stateOutlier, 
                                                     ";   ")},
                                           if(".expertOutlier" %in% colnames(databaseSpp)){
                                             paste0("Expert-identified outliers: ", databaseSpp$.expertOutlier, 
                                                    ";   ")},
                                            if(".sequential" %in% colnames(databaseSpp)){
                                                           paste0("Coordinate fill-down: ", databaseSpp$.sequential, 
                                                                  ";   ")},
                                            if(".latFlag" %in% colnames(databaseSpp)){
                                                           paste0("Gridded latitudes: ", databaseSpp$.latFlag, 
                                                                  ";   ")},
                                            if(".lonFlag" %in% colnames(databaseSpp)){
                                                           paste0("Gridded latitudes: ", databaseSpp$.lonFlag, 
                                                                  ";   ")},
                                            if(".gridSummary" %in% colnames(databaseSpp)){
                                                           paste0("Gridded lat & lon: ", databaseSpp$.gridSummary, 
                                                                  ";   ")},
                                ###### f. time data ####
                                            # Time
                                "<p></p><b>Time flags</b> - ",
                                            if(".eventDate_empty" %in% colnames(databaseSpp)){
                                                           paste0("No event date: ", databaseSpp$.eventDate_empty, 
                                                                  ";   ")},
                                            if(".year_outOfRange" %in% colnames(databaseSpp)){
                                                           paste0("Year out of range: ", databaseSpp$.year_outOfRange, 
                                                                  ";   ")},
                                ###### g. duplicate data ####
                                            # Duplicate
                                if(".duplicates" %in% colnames(databaseSpp)){
                                               paste0("<p></p><b>Duplicate flag</b> - ", databaseSpp$.duplicates,
                                                      ";   ")},
                                ###### h. collection data ####
                                # Time
                                "<p></p><b>Collection data</b> - ",
                                if("recordedBy" %in% colnames(databaseSpp)){
                                  paste0("Collector(s): ", databaseSpp$recordedBy, 
                                         ";   ")},
                                if("year" %in% colnames(databaseSpp)){
                                  paste0("Year: ", databaseSpp$year, 
                                         ";   ")},
                                if("identifiedBy" %in% colnames(databaseSpp)){
                                  paste0("Identified by: ", databaseSpp$identifiedBy, 
                                         ";   ")},
                                if("country" %in% colnames(databaseSpp)){
                                  paste0("Country: ", databaseSpp$country, 
                                         ";   ")},
                                if("references" %in% colnames(databaseSpp)){
                                  paste0("References: ", databaseSpp$references, 
                                         "   ")}
   
                                
                                            ), #you can add what do you want from columns of your data
                    
                  ###### i. colour ####
                              fillOpacity = if(walkName %in% c("TRUE", "FALSE")){0.4}else{0.7},
                              opacity = if(walkName %in% c("TRUE", "FALSE")){0.65}else{1},
                              #opacity =  if(walkName %in% c("TRUE", "FALSE")){0.25}else{1},
                              #stroke =  if(walkName %in% c("TRUE", "FALSE")){TRUE}else{FALSE},

                  # colour determined by if else
                              fillColor = 
                                if(onlySummary == FALSE){
                                  colPal(databaseSpp$mapLevels)
                                }else{colPal(databaseSpp$.summary)},
                              # Stroke color
                              color = if(walkName %in% c("TRUE", "FALSE")){
                                colPal(databaseSpp$.summary)}else{
                                  # colour TRUE border
                                if(all(databaseSpp$.summary) == TRUE){
                                  pointColours[1]
                              }else{pointColours[2]}},
                                # Internal size
                             radius = if(walkName %in% c("TRUE", "FALSE")){
                               5}else{6},
                                # border size
                             weight = if(walkName %in% c("TRUE", "FALSE")){
                               1.5}else{2.5}) #to change the size of points  
    } # END for loop

    
  ###### j. controller ####
    # Add the layers control
    mdatabaseSpp <- mdatabaseSpp %>%
      leaflet::addLegend(color = pointColours[1:length(names(databaseLoop))],
                labels = names(databaseLoop), 
                group = names(databaseLoop)) %>%
    leaflet::addLayersControl(
      baseGroups = c("OSM (default)", "Toner Lite"),
      overlayGroups = names(databaseLoop),
      options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE,
                                     sortLayers = FALSE))

  ###### k. save ####
  #then, it is to save in html format
  htmlwidgets::saveWidget(plotly::as_widget(mdatabaseSpp), 
                          file.path(outPath, #directory to save files
                                    paste0(speciesList[[x]],".html")),
                          selfcontained = TRUE,
                          title = paste0(speciesList[[x]]))
} # END for

# reset to original directory
setwd(olddir)

} # END function


#NOTE: to can view spp by spp in the viewer of R, you can run only from line 13 to line 31 changing 
  # numbers of spp in the line 13