# This function was written by James B Dorey and Angela Nava Bolańos from the 12 th of October 2022
# Its purpose is to create series of output figures or csv files for specified species
# Please contact jbdorey[at]me.com for help
#' Creates interactive html maps for species
#' 
#' Uses the occurrence data (preferably uncleaned) and outputs interactive .html maps that can be opened
#' in your browser to a specific directory. The maps can highlight if an occurrence has passed all filtering
#' (.summary == TRUE) or failed at least one filter (.summary == FALSE). This can be modified by first running
#' [BeeDC::summaryFun()] to set the columns that you want to be highlighted. It can also highlight occurrences
#' flagged as expert-identified or country outliers.
#' 
#' @param database A data frame or tibble. Occurrence records to use as input.
#' @param dir A directory as character. Directory where to save output maps.
#' @param longitude Character. The name of the longitude column. Default = "decimalLongitude".
#' @param latitude Character. The name of the latitude column. Default = "decimalLatitude".
#' @param speciesColumn Character. The name of the column containing species names (or another factor)
#' to build individual maps from. Default = "scientificName".
#' @param speciesList A character vector. Should contain species names as they appear in the 
#' speciesColumn to make maps of. User can also specify "ALL" in order to make maps of all 
#' species present in the database. Hence, a user may first filter their database and then use "ALL".
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
#' @importFrom utils installed.packages install.packages
#' @importFrom dplyr across where
#'
#' @examples
#' OutPath_Figures <- tempdir()
#' 
#' interactiveMapR(
#' # occurrence data - start with entire dataset, filter down to these species
#' database = BeeDC::bees3sp, # %>%
#'   # Select only those species in the 100 randomly chosen
#'   # dplyr::filter(scientificName %in% beeData_interactive$scientificName),
#'   # Select only one species to map
#'   # dplyr::filter(scientificName %in% "Agapostemon sericeus (Forster, 1771)"),
#' # Directory where to save files
#' dir = paste0(OutPath_Figures, "/interactiveMaps_TEST"),
#' # lat long columns
#' longitude = "decimalLongitude",
#' latitude = "decimalLatitude",
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
    database = NULL,
      # Directory where to save files
    dir = NULL,
      # lat long columns
    longitude = "decimalLongitude",
    latitude = "decimalLatitude",
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
  requireNamespace("DT")
  requireNamespace("dplyr")
  
#### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(database)){
    stop(paste0(" - No database was given. Please specify the data that you want to map ",
                "for your data-cleaning adventures. I'll do the rest."))
  }
  if(is.null(dir)){
    stop(paste0(" - No dir was given. Please specify the directory to save the maps to."))
  }
  
  ##### 0.2 Packages ####
  # Save the original directory 
  olddir <- getwd()
  
# Package names
packages <- c("leaflet", "htmlwidgets", "plotly", "dplyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


  ##### 0.3 Directories ####
  # Create directory if it does not exist
if (!dir.exists(dir)) {
  dir.create(dir, recursive = TRUE)}
  # Set directory
setwd(dir) #directory of work

  # database$IDall <- paste0(1:nrow(database)) #to add an ID by row

  #### 1.0 Data prep ####
    ##### 1.1 Remove na+ ####
database <- database %>%
  tidyr::drop_na(tidyselect::any_of(c(longitude, latitude))) 

# If there is no .expertOutlier then add one as all NA
if(!".expertOutlier" %in% colnames(database)){
  message("The column .expertOutlier was not found. One will be created with all values = TRUE.")
  database <- database %>% 
    dplyr::mutate(.expertOutlier = TRUE)
}

##### 1.2 Country list ####
# Select only the countries user provides
if(!is.null(countryList)){
  database <- database %>%
    dplyr::filter(country %in% countryList)
}

    ##### 1.3 Species list ####
if(any(stringr::str_detect(speciesList, "ALL")) == FALSE){
# Prepare the data for the loop
  database <- database %>% 
  # Select ONLY the species requested
  dplyr::filter(.data[[speciesColumn]] %in% speciesList)
}else{
  speciesList <- unique(database[[speciesColumn]])
} # END if else statement


##### 1.4 excludeApis_mellifera ####
if(excludeApis_mellifera == TRUE){
  database <- database %>%
    dplyr::filter(!scientificName == "Apis mellifera Linnaeus, 1758")
  speciesList <- setdiff(speciesList, "Apis mellifera Linnaeus, 1758")
}


##### 1.5 Overwrite ####
if(overWrite == TRUE){
    # Find completed species
  existingFiles <- list.files(path = dir) %>%
    stringr::str_remove("\\.html")
    # remove them from the to-do list
  speciesList <- setdiff(speciesList, existingFiles)
    # Re-filter the database to use only wanted species
  database <- database %>% 
    # Select ONLY the species requested
    dplyr::filter(.data[[speciesColumn]] %in% speciesList)
}

    ##### 1.6 Jitter ####
  # If the user specifies a jitter value, add that calue
if(!is.null(jitterValue)){
  database <- database %>%
    dplyr::mutate(
      decimalLongitude = base::jitter(database[[longitude]], amount = jitterValue),
      decimalLatitude = base::jitter(database[[latitude]], amount = jitterValue)
    )
}else{
    # If no jitter, ensure that the lat lon columns are the same
  database <- database %>%
    dplyr::mutate(
      decimalLongitude = database[[longitude]],
      decimalLatitude = database[[latitude]])
} # END Jitter


  # Make a new column to colour by if onlySummary == FALSE
if(onlySummary == FALSE){
  database <- database %>% 
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
options(encoding = "UTF-8")
database <- database %>% mutate(across(where(is.character), 
                              function(x){iconv(x, 
                                                to = "UTF-8",
                                                sub = "")}))

#### 2.0 produce maps ####
#function for leaflet maps
for (x in 1:length(speciesList)){
    # Filter to the xth species
  databaseLoop <- database %>% 
    dplyr::filter(.data[[speciesColumn]] == speciesList[[x]] %>% iconv(x,
                                                                       from = "UTF-8",
                                                                       to = "UTF-8",
                                                                       sub = ""))
    # Split database into classes
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
    leaflet::addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    # For the names in the list, apply the points function
  names(databaseLoop) %>%
    purrr::walk(function(walkName) {
      databaseSpp <<- databaseLoop[[walkName]]
      mdatabaseSpp <<- mdatabaseSpp %>%
    leaflet::addCircleMarkers(data = databaseSpp,
                              lng = ~decimalLongitude, lat = ~decimalLatitude, ###then you can specify what do you want in the popup window from your database
                              group = walkName,
                              if(TrueAlwaysTop == TRUE){
                              options = leaflet::leafletOptions(
                                pane = if(walkName == TRUE){"maplabels_TRUE"}else{
                                  "maplabels_FALSE"})},
                              popup = stringr::str_c(
                                sep = "",
                                ###### a. basic data ####
                                "<b>Basic data </b> - ",
                                "ID: ", databaseSpp$database_id, " ", #databaseSpp is the name of database and ID the name of the column
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
   
                                
                                            ), #you can add what do you want from columns of your database
                    
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
    })# END purrr::walk
    
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
                          file.path(dir, #directory to save files
                                    paste0(speciesList[[x]],".html")),
                          selfcontained = TRUE,
                          title = paste0(speciesList[[x]]))
} # END for

# reset to original directory
setwd(olddir)

} # END function


#NOTE: to can view spp by spp in the viewer of R, you can run only from line 13 to line 31 changing 
  # numbers of spp in the line 13