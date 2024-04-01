# This R script was written by James Dorey, starting on the 2nd of May 2022. The script intends
# to clean occurrence data from several sources using a combination of custom functions and 
# functions from the "bdc" package.
# For queries, please feel free to contact James Dorey at jbdorey@me.com

# WHAT TO ADD? You can list things here if you see the need.


#### 0.0 Script preparation ####
##### 0.1 Working directory ####
# Choose the path to the root folder in which all other folders can be found (or made by dirMaker)
RootPath <- "/Users/jamesdorey/Desktop/Uni/My_papers/Bee_SDM_paper"
# Read in this initial function
source(paste(RootPath, "/BDC_repo/NewFunctions/dirMaker.R", sep = ""))
library(magrittr)
# Create file paths and prepare for what's to come
dirMaker(
  RootPath = RootPath,
  # Input the location of the workflow script RELATIVE to the RootPath
  RDoc = "BDC_repo/BeeCleaning_SciData.R") %>%
  # Add paths created by this function to the environment()
  list2env(envir = parent.env(environment())) 
# Set the working directory
setwd(DataPath)
# Install reenv, IF NEEDED
#install.packages("renv")
renv::init() 


##### 0.2 Install packages (if needed) #####
# Install only those packages that are not already present in your system
# Choose packages that need to be installed 
# You may need to install gdal on your computer. This can be done on mac by using
# Homebrew in the terminal and the command "brew install gdal"
list.of.packages <- c("R.utils",           # To use gunzip
                      "magrittr",          # to use pipes
                      "dplyr",             #  Part of the tidyverse
                      "forcats",           # tidyverse for working with factors
                      "tidyr",             #  Part of the tidyverse
                      "rlist",             # Package to save lists
                      "galah",             #  To download ALA data
                      "praise",            #  To whispers sweet nothings 
                      "EML",               #  To work with .eml files
                      "emld",              #  To work with .eml files
                      "rlang",             #  Part of the tidyverse - core functions
                      "xml2",              #  Part of the tidyverse - reads .xml files
                      "stringr",           #  Part of the tidyverse - works with text strings
                      "lubridate",         #  Part of the tidyverse - works with dates
                      "tidyselect",        #  Part of the tidyverse
                      "mgsub",             #  To perform multiple text substitutions
                      "bdc",               # data cleaning package
                      "rvest",             # Package for interfacing with and downloading from the internet
                      "rnaturalearth",     #  Global vector map data 
                      "rnaturalearthdata", #  To access the above global map data
                      "countrycode",       # Package to deal with country codes
                      "rworldmap",
                      "readxl",
                      "cowplot",           # ggplot2 helper package
                      "ggspatial")         #  Makes ggplot2 create north arrows or scale bars
# Install sf and terra seperately
# renv::install(c("sf","terra"), type = "binary")
renv::install(c("sf","terra"), type = "binary")
# List the new (not installed) packages and then if there are any, install them.
renv::install(packages = c(list.of.packages), 
              rebuild = FALSE) # try changing to TRUE if you're having package troubles

##### 0.3 Load packages ####
# Load all packages from the list specified above, 
lapply(c(list.of.packages, "sf","terra"), 
       library, character.only = TRUE)
### Load in R scripts and character strings in our package
sapply(list.files(file.path(ScriptPath), pattern = ".R$", full.names = TRUE), source)  # loads in all functions
# Save a snapshot of the environment
renv::snapshot()


#### 1.0 Data read ####
  ##### 1.1 Read data ####
# Read in the uncleaned, but flagged, dataset
beeData <- readr::read_csv("/Users/jamesdorey/Desktop/Uni/My_papers/Bee_SDM_paper/Data_acquisition_workflow/Output/Intermediate/05_unCleaned_database.csv",
                           col_types = ColTypeR())

  ##### 1.2 Bee taxonomy ####
# Read in the custom taxonomy file
  # This can be used to filter to a particular taxon
BeeTaxonomy <- BeeBDC::beesTaxonomy()

#### 2.0 Taxon example ####
  # Users could filter to a particular taxonomic group by editing the below
  # Select only unique selected bee genera
selectedGenera <- BeeTaxonomy %>%
    # Select only tribe anthophorini (for example)
  dplyr::filter(tolower(tribe) == tolower("anthophorini")) %>%
  distinct(genus)
  
  # Filter the data
taxonData <- beeData %>%
  dplyr::filter(genus %in% selectedGenera$genus)



#### 3.0 Country example ####
# Users could filter to a particular group of countries by editing the below
studyArea <- c("Canada", "United states", "Mexico", "Guatemala")
# Filter the data 
countryData <- beeData %>%
  dplyr::filter(country %in% studyArea)



#### 4.0 Filtering example ####
  ##### 4.1 Simple filter ####
  # Users can filter the flagged data by whichever columns they wish using the summaryFun...
source(paste(ScriptPath, "summaryFun.R", sep = "/"))
filteredData <- 
    # The input dataset
  beeData %>%
    # Run the summary function
  summaryFun(
  # Use the above input dataset to filter
  data = .,
  # Choose the columns to NOT filter (or NULL to filter all columns)
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                      ".uncertaintyThreshold"),
  # In the output, do you want to REMOVE all filtering columns (TRUE), or keep them (FALSE)
  removeFilterColumns = TRUE,
  # In the output, do you want to only keep clean data according to your filtering (TRUE),
    # Or keep all data and simply update the .summary column (FALSE)
  filterClean = TRUE) # END summary function

##### 4.2 Uncertainty threshold ####
# Users may also want to filter by .uncertaintyThreshold, but want to specify their own 
  # Uncertainty threshold...
# Users can filter the flagged data by whichever columns they wish using the summaryFun...
source(paste(ScriptPath, "summaryFun.R", sep = "/"))
filteredData <- 
    # The input dataset
  beeData %>%
  # Remove any exiting .uncertaintyThreshold column
  dplyr::select(!tidyselect::any_of(".uncertaintyThreshold")) %>%
    # Chose the coordinate uncertainty to filter to...
  coordUncerFlagR(data = .,
                  uncerColumn = "coordinateUncertaintyInMeters",
                    # 10 km here
                  threshold = 10000) %>%
    # Now re-do the .summary column and filter the data using this new value
  summaryFun(
  # Select the input dataset to filter
  data = .,
  # Choose the columns to NOT filter (or NULL to filter all columns)
    # NOTE: the .uncertaintyThreshold is now removed and WILL be filtered
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms"),
  # In the output, do you want to REMOVE all filtering columns (TRUE), or keep them (FALSE)
  removeFilterColumns = TRUE,
  # In the output, do you want to only keep clean data according to your filtering (TRUE),
  # Or keep all data and simply update the .summary column (FALSE)
  filterClean = TRUE)


##### 4.2 Date filter ####
  ###### a. bdc_year_outOfRange ####
# Users can filter the flagged data by whichever columns they wish using the summaryFun...
source(paste(ScriptPath, "summaryFun.R", sep = "/"))
filteredData <- 
  # The input dataset
  beeData %>%
    # Remove any exisitng .year_outOfRange column
  dplyr::select(!".year_outOfRange") %>%
  # Chose the minimum year to filter to...
  bdc::bdc_year_outOfRange(data = .,
                           eventDate = "year",
                           year_threshold = 1970) %>%
  # Now re-do the .summary column and filter the data using this new value
  summaryFun(
    # Select the input dataset to filter
    data = .,
    # Choose the columns to NOT filter (or NULL to filter all columns)
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                        ".uncertaintyThreshold"),
    # In the output, do you want to REMOVE all filtering columns (TRUE), or keep them (FALSE)
    removeFilterColumns = TRUE,
    # In the output, do you want to only keep clean data according to your filtering (TRUE),
    # Or keep all data and simply update the .summary column (FALSE)
    filterClean = TRUE)

###### b. year range ####
# Users can filter the flagged data by whichever columns they wish using the summaryFun...
source(paste(ScriptPath, "summaryFun.R", sep = "/"))
filteredData <- 
  # The input dataset
  beeData %>%
  # Chose the year range...
  dplyr::filter(year > 1950 & year < 1970) %>%
  # Now re-do the .summary column and filter the data using this new value
  summaryFun(
    # Select the input dataset to filter
    data = .,
    # Choose the columns to NOT filter (or NULL to filter all columns)
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                        ".uncertaintyThreshold"),
    # In the output, do you want to REMOVE all filtering columns (TRUE), or keep them (FALSE)
    removeFilterColumns = TRUE,
    # In the output, do you want to only keep clean data according to your filtering (TRUE),
    # Or keep all data and simply update the .summary column (FALSE)
    filterClean = TRUE)


    # Users may choose any number of filtering steps form the main workflow to include above 
    # summaryFun(), just use pipes '%>%' between the function and use '.' as the data input
    # because this wil lfeed in the data aoutput from the above function into the proceeding one.



#### 5. Summary figures ####
##### 5.1 Duplicate chordDiagrams ####
install.packages("circlize")
if(!require("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")}
BiocManager::install("ComplexHeatmap", force = TRUE)
install.packages("paletteer")
library(paletteer)# Find palettes here
renv::snapshot()

# Read in the most-RECENT file
duplicates <- fileFinder(path = "PATH TO A FOLDER CONTAINING THE duplicateRun_ - could be supp. materials folder",
                          fileName = "duplicateRun_") %>%
  readr::read_csv() %>%
  # Select only the stingless bee data
  dplyr::filter(database_id %in% stinglessData$database_id |
                  database_id_match %in% stinglessData$database_id)

# Choose the global figure parameters
par(mar = c(2, 2, 2, 2)/2, mfrow = c(1,1))

# Create the chorDiagram. You can leave many of the below values out but we show here
# the defaults
source(paste(ScriptPath, "chordDiagramR.R", sep = "/"))
chordDiagramR(
  # The duplicate data from the dupeSummary function output  
  dupeData = duplicates,
  outPath = OutPath_Figures,
  fileName = "ChordDiagram.pdf",
  # These can be modified to help fit the final pdf that's exported.
  width = 9,
  height = 7.5,
  bg = "white",
  # How few distinct dataSources should a group have to be listed as "other"
  smallGrpThreshold = 3,
  title = "Duplicated record sources",
  # The default list of colour palettes to choose from usign the paleteer package
  palettes = c("cartography::blue.pal", "cartography::green.pal", 
               "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
               "cartography::purple.pal", "cartography::brown.pal"),
  canvas.ylim = c(-1.0,1.0), 
  canvas.xlim = c(-0.6, 0.25),
  text.col = "black",
  legendX = grid::unit(6, "mm"),
  legendY = grid::unit(18, "mm"),
  legendJustify = c("left", "bottom"),
  niceFacing = TRUE)
# Save 7*6


##### 5.2 Duplicate histogram ####
# Find the existing flag file and read it in
flagColumns <- fileFinder(path = "PATH TO A FOLDER CONTAINING THE flagsRecorded_ - could be supp. materials folder",
                           fileName = "flagsRecorded_") %>%
  readr::read_csv() %>%
    # WARNING: alternate path if wanting to produce figures for the selected taxonData (2.0 above)
  # Select only the taxonData data
  dplyr::filter(database_id %in% taxonData$database_id)

# Read in the function
source(paste(ScriptPath, "dupePlotR.R", sep = "/"))
# Create a figure shoring the total number of duplicates, kept duplicates, and unique
# records for each datasource (simplified to the text before the first underscore) and
# the proportion of the above for each data source
dupePlotR(
  flagColumns = flagColumns,
  # The outPath to save the plot as
  outPath = paste0(DataPath, "/Output", "/Figures"),
  fileName = "duplicatePlot.pdf",
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP"
)


##### 5.3 Flags by source ####
# Read in the function
source(paste(ScriptPath, "plotFlagSummary.R", sep = "/"))
  ###### a. All taxa in dataset ####
# Visualise all flags for each dataSource (simplified to the text before the first underscore)
plotFlagSummary(
    # WARNING: alternate path if wanting to produce figures for the selected taxonData (2.0 above)
      # Select only the taxonData data
  plotData = taxonData,
  # Colours in order of pass (TRUE), fail (FALSE), and NA
  flagColours = c("#127852", "#A7002D", "#BDBABB"),
  fileName = paste0("FlagsPlot_", Sys.Date(),".pdf"),
  outPath = paste0(DataPath, "/Output/Figures"),
  width = 15, height = 9,
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minkley' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL"
)

  ###### b. Single sp. summary ####
# Visualise all flags for each dataSource (simplified to the text before the first underscore)
  # A clever user might also realise the potential to summarise and produce outputs in other columns
plotFlagSummary(
  # WARNING: alternate path if wanting to produce figures for the selected taxonData (2.0 above)
  # Select only the taxonData data
  plotData = beeData,
  # Colours in order of pass (TRUE), fail (FALSE), and NA
  flagColours = c("#127852", "#A7002D", "#BDBABB"),
  fileName = paste0("FlagsPlot_Lfijiense", Sys.Date(),".pdf"),
  outPath = paste0(DataPath, "/Output/Figures"),
  width = 15, height = 9,
  # OPTIONAL:
         #  # Filter to species
           speciesName = "Lasioglossum fijiense",
             # column to look in
           nameColumn = "species",
           # Save the filtered data
           saveFiltered = TRUE,
     # Filter column to display on map
           filterColumn = ".summary",
           plotMap = TRUE,
       # amount to jitter points if desired, e.g. 0.25 or NULL
     jitterValue = NULL,
       # Map opacity value for points between 0 and 1
     mapAlpha = 1,
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minkley' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL"
)



##### 5.4 Maps ####
# Import CLEANED dataset (you can change this option)
  # WARNING: alternate path if wanting to produce figures for the selected taxonData (2.0 above)
    # Select only the taxonData data
mapData <- taxonData %>% 
  dplyr::filter(.summary == TRUE)


# Read in the function
# TO ADD: Change legend title and text to be nicer
# Add A and B
source(paste(ScriptPath, "summaryMaps.R", sep = "/"))
summaryMaps(
  mapData = mapData,
  width = 10, height = 10,
  class_n = 15,
  class_Style = "jenks",
  fileName = paste0(DataPath, "/Output/Figures/", "CountryMaps_jenks.pdf")
)



#### 6.0 Save data ####
mapData %>%
  readr::write_excel_csv(paste0(DataPath, "/Output/Intermediate/", "cleanTaxon_",
                          Sys.Date(), ".csv"))



