# This R script was written by James Dorey, starting on the 2nd of May 2022. The script serves
# as a workflow for the BeeDC package to clean and flag bee, and other, occurrence data.
# It also uses functions from several sources and particularly from the "bdc" package.
# For queries, please feel free to contact James Dorey at jbdorey@me.com


#### 0.0 Script preparation ####
##### 0.1 Working directory ####
# Choose the path to the root folder in which all other folders can be found (or made by dirMaker)
RootPath <- "/Users/jamesdorey/Desktop/Uni/My_papers/Bee_SDM_paper"

# Set the working directory
setwd(paste0(RootPath,"/Data_acquisition_workflow"))


##### 0.2 Install packages (if needed) #####
# Choose packages that need to be installed/loaded
# You may need to install gdal on your computer. This can be done on mac by using
  # Homebrew in the terminal and the command "brew install gdal"
list.of.packages <- c("R.utils",           # To use gunzip
                      "bdc",               # data cleaning package
                      "tidyr",             #  Part of the tidyverse
                      "magrittr",          # to use pipes
                      "ggplot2",           #  Creates many easthetic plots
                      #"ggVennDiagram",     # Extends ggplot2 to make venn diagrams
                      "dplyr",             #  Part of the tidyverse
                      "tibble",            # To use tibbles
                      "forcats",           # tidyverse for working with factors
                      "rlist",             # Package to save lists
                      "EML",               #  To work with .eml files
                      "emld",              #  To work with .eml files
                      "rlang",             #  Part of the tidyverse — core functions
                      "xml2",              #  Part of the tidyverse — reads .xml files
                      "stringr",           #  Part of the tidyverse — works with text strings
                      "lubridate",         #  Part of the tidyverse — works with dates
                      "tidyselect",        #  Part of the tidyverse
                      "mgsub",             #  To perform multiple text substitutions
                      "rvest",             # Package for interfacing with and downloading from the internet
                      "rnaturalearth",     #  Global vector map data 
                      "rnaturalearthdata", #  To access the above global map data
                      "countrycode",       # Package to deal with country codes
                      "rangeBuilder",
                      "renv",
                      "rworldmap",
                      "janitor",
                      "hexbin",
                      "circlize", 
                      "BiocManager",
                      "paletteer",
                      "readxl",
                      "readr",             #  Part of the tidyverse — reads files (e.g., .csv)
                      "cowplot",           # ggplot2 helper package
                      "igraph",
                      "ggspatial")        #  Makes ggplot2 create north arrows or scale bars
# Install sf, terra, galah, and ComplexHeatMap seperately
renv::install(c("sf","terra"), type = "binary")
remotes::install_github("AtlasOfLivingAustralia/galah")
BiocManager::install("ComplexHeatmap")
# List the new (not installed) packages and then if there are any, install them.
renv::install(packages = c(list.of.packages, "ropensci/rnaturalearthhires", "mattflor/chorddiag"), 
              rebuild = FALSE) # try changing to TRUE if you're having package troubles

# Initialise renv the project
renv::init() 

##### 0.3 Load packages ####
# Load all packages from the list specified above, with the addition of "rnaturalearthhires"
lapply(c(list.of.packages, "rnaturalearthhires", "chorddiag", "sf","terra", "galah", "ComplexHeatmap",
         "BeeDC"), 
       library, character.only = TRUE)
# Save a snapshot of the environment
renv::snapshot()

  ##### 0.4 BeeDC ####
# Install BeeDC 
remotes::install_github("https://github.com/jbdorey/BeeDC.git", user="jbdorey", ref = "main", 
                        force = TRUE,
                        auth_token = "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")

# Create file paths and prepare for what's to come
BeeDC::dirMaker(
  RootPath = RootPath,
  # Input the location of the workflow script RELATIVE to the RootPath
  RDoc = "Packages/BeeDC/BeeDC_fullWorkflow.R") %>%
  # Add paths created by this function to the .GlobalEnv
  list2env(envir = .GlobalEnv)  

#### 1.0 Data merge ####
##### 1.1 Download ALA data ####
# Downloads ALA data and creates a new file in the HomePath to put those data
BeeDC::atlas_downloader(path = DataPath,
               userEmail = "jbdorey@me.com",
               ALA_taxon = "Apiformes")

##### 1.2 Import and merge ALA, SCAN, iDigBio and GBIF data ####
# Supply the path to where the data are
# save_type is either "csv_files" or "R_file"
DataImp <- BeeDC::repo_merge(path = DataPath, 
                        # Find data — Many problems can be solved by running data_finder(path = DataPath)
                        # And looking for problems
                      occ_paths = BeeDC::data_finder(path = DataPath),
                      save_type = "R_file")

# Load in the most-recent version of these data if needed 
# This will return a list with 
# 1. the occurrence dataset with attributes and 
# 2. the appended eml file
DataImp <- BeeDC::importOccurrences(path = DataPath,
                             fileName = "BeeData_")

##### 1.3 Import USGS Data ####
# The USGS_formatter will find, import, format, and create metadata for the USGS dataset
# pubDate must be in day-month-year format and must be supplied by the user here.
USGS_data <- BeeDC::USGS_formatter(path = DataPath, pubDate = "19-11-2022")

##### 1.4 Formatted Source Importer ####
# Formatted source importer. Use this importer to find files that have been formatted and need to 
# be added to the larger data file (e.g., made by repo_merge and USGS_formatter)
# Combine the USGS data and the existing big dataset
Complete_data <- BeeDC::formatted_combiner(path = DataPath, 
                                    strings = c("USGS_[a-zA-Z_]+[0-9]{4}-[0-9]{2}-[0-9]{2}"), 
                                    # This should be the list-format with eml attached
                                    existingOccurrences = DataImp$Data_WebDL,
                                    existingEMLs = DataImp$eml_files) 
# In the catalogNumber, remove ".*specimennumber:" as what comes after should be the USGS
# number to match for duplicates
Complete_data$Data_WebDL <- Complete_data$Data_WebDL %>%
  dplyr::mutate(catalogNumber = stringr::str_replace(catalogNumber,
                                                     pattern = ".*\\| specimennumber:",
                                                     replacement = ""))

##### 1.5 Save data ####
# Choose the type of data format you want
BeeDC::data_saver(path = DataPath,# The main path to look for data in
           save_type = "CSV_file", # "R_file" OR "CSV_file"
           occurrences = Complete_data$Data_WebDL, # The existing datasheet
           eml_files = Complete_data$eml_files, # The existing EML files
           file_prefix = "Fin_") # The prefix for the filenames
rm(Complete_data, DataImp)


#### 2.0 Data preparation ####
##### 2.1 Standardise datasets ####
  # You may either use 
    # (a) the bdc import method (works well with general datasets) or 
    # (b) the BeeDC import method (works well with above data merge)
  # The bdc import is NOT truly supported here, but provided as an example. Please go to section
    # 2.1b below.
###### a. bdc import ####
warning(paste0("The bdc method here is not truly implemented and supported. If you use it you must do so alone.",
               " This is just a place-holder for people using the bdc package more heavily.",
               "\nPreferably, go directly to 2.1b — BeeDC import."))
# Read in the bdc metadata
bdc_metadata <- readr::read_csv(paste(DataPath, "Output", "bdc_integration.csv", sep = "/"))
# Standardise the dataset to bdc
db_standardized <- bdc::bdc_standardize_datasets(
  metadata = bdc_metadata,
  format = "csv",
  overwrite = TRUE,
  save_database = TRUE)
# read in configuration description file of the column header info
config_description <- readr::read_csv(paste(DataPath, "Output", "bdc_configDesc.csv",
                                            sep = "/"), 
                                      show_col_types = FALSE, trim_ws = TRUE)

###### b. BeeDC import ####
# You can also just read the data in using the below script. This will 
# likely be quicker and more-reliable. Find the path
occPath <- BeeDC::file_finder(path = DataPath, fileName = "Fin_BeeData_combined_")
# read in the file
db_standardized <- readr::read_csv(occPath, 
                                   # Use the basic ColTypeR function to determine types
                                   col_types = BeeDC::ColTypeR(), trim_ws = TRUE) %>%
  # add the database_id columns
  dplyr::mutate(database_id = paste("Dorey_data_", 1:nrow(db_standardized), sep = ""),
                .before = family)

###### c. match database_id ####
# IF you have prior runs from which you'd like to match database_ids with from the current run,
  # you may use the below script
# Try to match database IDs with prior runs.
  # read in a prior run of choice
  priorRun <- BeeDC::file_finder(path = DataPath,
                          file = "01_prefilter_database_9Aug22.csv") %>%
    readr::read_csv(file = ., col_types = BeeDC::ColTypeR())
  
  # This function will attempt to find the database_ids from prior runs
  db_standardized <- BeeDC::idMatchR(
    currentData = db_standardized,
    priorData = priorRun,
    # First matches will be given preference over later ones
    matchBy = tibble::lst(c("gbifID"),
                          c("catalogNumber", "institutionCode", "dataSource"),
                          c("occurrenceID", "dataSource"),
                          c("recordId", "dataSource"),
                          c("id"),
                          # Because INHS was entered as it's own dataset but is now included in the GBIF download...
                          c("catalogNumber", "institutionCode")),
    # You can exclude datasets from prior by matching their prefixs — before first underscore:
    # Which datasets are static and should be excluded from matching?
    excludeDataset = c("ASP", "BMin", "BMont", "CAES", "EaCO", "Ecd", "EcoS",
                       "Gai", "KP", "EPEL", "CAES", "EaCO", "FSCA", "SMC", "Lic", "Arm"))
  
  # Remove redundant files
  rm(priorRun, excludeDataset)

###### d. optional thin ####
    # You can thin the dataset for TESTING ONLY!
    # check_pf <- check_pf %>%
    #   # take every 100th record
    #   filter(row_number() %% 100 == 1)


##### 2.2 Paige dataset ####
# Integrate Paige Chesshire's cleaned dataset.
# Import Paige's cleaned N. American data
# IF you haven't figured out by now, dont worry about the column name warning — not all columns occur here.
PaigeNAm <- readr::read_csv(paste(DataPath, "Paige_data", "NorAmer_highQual_only_ALLfamilies.csv",
                                  sep = "/"), col_types = BeeDC::ColTypeR()) %>%
  # Change the column name from Source to dataSource to match the rest of the data.
  dplyr::rename(dataSource = Source) %>%
  # add a NEW database_id column
  dplyr::mutate(
    database_id = paste0("Paige_data_", 1:nrow(.)),
    .before = scientificName)

# Merge Paige's data with downloaded data
db_standardized <- BeeDC::PaigeIntegrater(
  db_standardized = db_standardized,
  PaigeNAm = PaigeNAm,
  # This is a list of columns by which to match Paige's data to the most-recent download with. 
  # Each vector will be matched individually
  columnStrings = list(
    c("decimalLatitude", "decimalLongitude", 
      "recordNumber", "recordedBy", "individualCount", "samplingProtocol",
      "associatedTaxa", "sex", "catalogNumber", "institutionCode", "otherCatalogNumbers",
      "recordId", "occurrenceID", "collectionID"),         # Iteration 1
    c("catalogNumber", "institutionCode", "otherCatalogNumbers",
      "recordId", "occurrenceID", "collectionID"), # Iteration 2
    c("decimalLatitude", "decimalLongitude", 
      "recordedBy", "genus", "specificEpithet"),# Iteration 3
    c("id", "decimalLatitude", "decimalLongitude"),# Iteration 4
    c("recordedBy", "genus", "specificEpithet", "locality"), # Iteration 5
    c("recordedBy", "institutionCode", "genus", 
      "specificEpithet","locality"),# Iteration 6
    c("occurrenceID","decimalLatitude", "decimalLongitude"),# Iteration 7
    c("catalogNumber","decimalLatitude", "decimalLongitude"),# Iteration 8
    c("catalogNumber", "locality") # Iteration 9
  ) )

# Remove spent data
rm(PaigeNAm)

##### 2.3 USGS ####
# The USGS dataset also partially occurs on GBIF from BISON, however, the occurrence codes are in
# a silly place... We will correct these here to help identify duplicates later
db_standardized <- db_standardized %>%
    # Remove the discoverlife html if it is from USGS
  dplyr::mutate(occurrenceID = dplyr::if_else(
    stringr::str_detect(occurrenceID, "USGS_DRO"),
    str_remove(occurrenceID, "http://www\\.discoverlife\\.org/mp/20l\\?id="),
    occurrenceID)) %>%
    # Use otherCatalogNumbers when occurrenceID is empty AND when USGS_DRO is detected there
  dplyr::mutate(
    occurrenceID = dplyr::if_else(
      stringr::str_detect(otherCatalogNumbers, "USGS_DRO") & is.na(occurrenceID),
      otherCatalogNumbers, occurrenceID)) %>%
    # Make sure that no eventIDs have snuck into the occurrenceID columns 
      # For USGS_DRO, codes with <6 digits are event ids
  dplyr::mutate(
    occurrenceID = dplyr::if_else(stringr::str_detect(occurrenceID, "USGS_DRO", negate = TRUE),
        # Keep occurrenceID if it's NOT USGS_DRO
       occurrenceID, 
        # If it IS USGS_DRO and it has => 6 numbers, keep it, else, NA
      dplyr::if_else(stringr::str_detect(occurrenceID, "USGS_DRO[0-9]{6,10}"),
                     occurrenceID, NA_character_)),
    catalogNumber = dplyr::if_else(stringr::str_detect(catalogNumber, "USGS_DRO", negate = TRUE),
        # Keep catalogNumber if it's NOT USGS_DRO
      catalogNumber, 
        # If it IS USGS_DRO and it has => 6 numbers, keep it, else, NA
      dplyr::if_else(stringr::str_detect(catalogNumber, "USGS_DRO[0-9]{6,10}"),
                     catalogNumber, NA_character_)))


##### 2.4 Additional datasets ####
  # Import additional and potentially private datasets.
    # Private dataset functions are provided but the data not integrated here until those datasets 
    # become freely available post-publication.
  # There will be some warnings were a few rows may not be formatted correctly or where dates fail
    # to parse. This is normal.
source(paste(ScriptPath, "additionalData_readRs.R", sep = "/"))
###### a. EPEL ####
EPEL_Data <- BeeDC::readr_EPEL(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/bee_data_canada.csv",
                      outFile = "jbd_EPEL_data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
###### b. Allan Smith-Pardo ####
ASP_Data <- BeeDC::readr_ASP(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/Allan_Smith-Pardo_Dorey_ready2.csv",
                      outFile = "jbd_ASP_data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
###### c. Minckley ####
BMin_Data <- BeeDC::readr_BMin(path = paste0(DataPath, "/Additional_Datasets"),
                        inFile = "/InputDatasets/Bob_Minckley_6_1_22_ScanRecent-mod_Dorey.csv",
                        outFile = "jbd_BMin_data.csv",
                        dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
###### d. BMont ####
BMont_Data <- BeeDC::readr_BMont(path = paste0(DataPath, "/Additional_Datasets"),
                          inFile = "/InputDatasets/Bombus_Montana_dorey.csv",
                          outFile = "jbd_BMont_data.csv",
                          dataLicense = "https://creativecommons.org/licenses/by-sa/4.0/")
###### e. Ecd ####
Ecd_Data <- BeeDC::readr_Ecd(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/Ecdysis_occs.csv",
                      outFile = "jbd_Ecd_data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
###### f. Gai ####
Gai_Data <- BeeDC::readr_Gai(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/upload_to_scan_Gaiarsa et al_Dorey.csv",
                      outFile = "jbd_Gai_data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
###### g. CAES ####
# This is a little slower and will have a few date warnings — 215 failed to parse. 
CAES_Data <- BeeDC::readr_CAES(path = paste0(DataPath, "/Additional_Datasets"),
                        inFile = "/InputDatasets/CT_BEE_DATA_FROM_PBI.xlsx",
                        outFile = "jbd_CT_Data.csv",
                        dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")

###### h. GeoL ####
GeoL_Data <- BeeDC::readr_GeoL(path = paste0(DataPath, "/Additional_Datasets"),
                        inFile = "/InputDatasets/Geolocate and BELS_certain and accurate.xlsx",
                        outFile = "jbd_GeoL_Data.csv")


###### i. EaCO ####
EaCO_Data <- BeeDC::readr_EaCO(path = paste0(DataPath, "/Additional_Datasets"),
                        inFile = "/InputDatasets/Eastern Colorado bee 2017 sampling.xlsx",
                        outFile = "jbd_EaCo_Data.csv",
                        dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


###### j. FSCA ####
  # Florida State Collection of Arthropods
FSCA_Data <- BeeDC::readr_FSCA(path = paste0(DataPath, "/Additional_Datasets"),
                        inFile = "InputDatasets/fsca_9_15_22_occurrences.csv",
                        outFile = "jbd_FSCA_Data.csv",
                        dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")

###### k. Texas SMC ####
# # Published or unpublished data from Texas literature not in an online database, usually copied 
# into spreadsheet from document format, or otherwise copied from a very differently-formatted spreadsheet
# # Unpublished or partially published data were obtained with express permission from the lead author
SMC_Data <- BeeDC::readr_SMC(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/TXbeeLitOccs_31Oct22.csv", 
                      outFile = "jbd_SMC_Data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")

###### l. Texas Bal ####
# # Data with GPS coordinates from Ballare et al. 2019, https://doi.org/10.1002/eap.1869. 
# The version on Dryad is missing site GPS coordinates (by accident). Kim is okay with these data 
# being made public as long as her paper is referenced.
# - Elinor Lichtenberg
Bal_Data <- BeeDC::readr_Bal(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/Beedata_ballare.xlsx", 
                      outFile = "jbd_Bal_Data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")

###### m. Palouse Lic ####
# # *Attached: My canola data. I tried to get this in DarwinCore format. 
# These data go with a manuscript currently under review. Here’s the bioRxiv version: 
# https://www.biorxiv.org/content/10.1101/2021.12.06.471474v2. 
# These are the data I will be putting on SCAN. 
# - Elinor Lichtenberg
Lic_Data <- BeeDC::readr_Lic(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/Lichtenberg_canola_records.csv", 
                      outFile = "jbd_Lic_Data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")

###### n. Arm ####
Arm_Data <- BeeDC::readr_Arm(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/Bee database Armando_Final.xlsx",
                      outFile = "jbd_Arm_Data.csv",
                      sheet = "Sheet1",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")

###### o. Dor #####
Dor_Data <- BeeDC::readr_Dor(path = paste0(DataPath, "/Additional_Datasets"),
                      inFile = "/InputDatasets/DoreyData.csv",
                      outFile = "jbd_Dor_Data.csv",
                      dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


##### 2.5 Merge all ####
# Remove these spent datasets
rm(EPEL_Data, ASP_Data, BMin_Data, BMont_Data, Ecd_Data, Gai_Data, CAES_Data, 
   # INHS_Data, MABC_Data, EcoS_Data, KP_Data,
   GeoL_Data, EaCO_Data, FSCA_Data, SMC_Data, Bal_Data, Lic_Data, Arm_data, Dor_Data)
# Read in and merge all
db_standardized <- db_standardized %>%
  dplyr::bind_rows(
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_ASP_data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_EPEL_data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_BMin_data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_BMont_data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_Ecd_data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_Gai_data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_CT_Data.csv"), col_types = BeeDC::ColTypeR()),
     readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_GeoL_Data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_EaCo_Data.csv"), col_types = BeeDC::ColTypeR()), 
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_SMC_Data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_Bal_Data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_Lic_Data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_Arm_Data.csv"), col_types = BeeDC::ColTypeR()),
    readr::read_csv(paste0(DataPath, "/Additional_Datasets", 
                           "/jbd_Dor_Data.csv"), col_types = BeeDC::ColTypeR())) %>% 
  # END bind_rows
  suppressWarnings(classes = "warning") # End suppressWarnings — due to col_types

#   # Save the dataset
db_standardized %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "00_prefilter_database.csv",
                         sep = "/"))

#### 3.0 Initial flags ####
  # Read this back in if needed.
if(!exists("db_standardized")){
  db_standardized <- readr::read_csv(paste(OutPath_Intermediate, "00_prefilter_database.csv",
                                    sep = "/"), col_types = BeeDC::ColTypeR())}

# See here for bdc prefilter tutorial — https://brunobrr.github.io/bdc/articles/prefilter.html
##### 3.1 SciName ####
  # Flag occurrences without scientificName provided
check_pf <- bdc::bdc_scientificName_empty(
  data = db_standardized,
  sci_name = "scientificName")

# now that this is saved, remove it to save space in memory
rm(db_standardized) 


##### 3.2 MissCoords ####
# Flag occurrences with missing lat and lon
check_pf <- bdc::bdc_coordinates_empty(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

##### 3.3 OutOfRange ####
# Flag occurrences that are not on earth (outside of -180 to 180 or -90 to 90 degrees)
check_pf <- bdc::bdc_coordinates_outOfRange(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

##### 3.4 ?Source ####
# Flag occurrences that don't match the basisOfRecord types below
check_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = check_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = c(
    # Keep all plus some at the bottom.
    "Event",
    "HUMAN_OBSERVATION",
    "HumanObservation",
    "LIVING_SPECIMEN",
    "LivingSpecimen",
    "MACHINE_OBSERVATION",
    "MachineObservation",
    "MATERIAL_SAMPLE",
    "O",
    "Occurrence",
    "MaterialSample",
    "OBSERVATION",
    "Preserved Specimen",
    "PRESERVED_SPECIMEN",
    "preservedspecimen Specimen",
    "Preservedspecimen",
    "PreservedSpecimen",
    "preservedspecimen",
    "S",
    "Specimen",
    "Taxon",
    "UNKNOWN",
    "",
    NA,
    "NA",
    "LITERATURE", 
    "None", "Pinned Specimen", "Voucher reared", "Emerged specimen"
  ))


##### 3.5 CountryName ####
  # Try to harmonise country names
###### a. prepare dataset ####
  # Fix up country names based on common problems above and extract ISO2 codes for occurrences
check_pf_noNa <- BeeDC::countryNameCleanR(
  data = check_pf,
  # Create a Tibble of common issues in country names and their replacements
  commonProblems = tibble::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
                                              'United States','U.S.A','MX','CA','Bras.','Braz.',
                                              'Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
                                  fix = c('United States of America','United States of America',
                                          'United States of America','United States of America',
                                          'United States of America','United States of America',
                                          'United States of America','Mexico','Canada','Brazil',
                                          'Brazil','Brazil','Northern Mariana Islands','PUERTO.RICO'))
  )


###### b. run function ####
  # Get country name from coordinates using a wrapper around the bdc_country_from_coordinates function
  # Because our dataset is much larger than those used to design bdc, we have made it so that you
  # can analyse data in smaller pieces.
suppressWarnings(
  countryOutput <- BeeDC::jbd_CfC_chunker(data = check_pf_noNa,
                                   lat = "decimalLatitude",
                                   lon = "decimalLongitude",
                                   country = "country",
                                   # How many rows to process at a time
                                   stepSize = 1000000,
                                   # Start row
                                   chunkStart = 1,
                                   path = HomePath,
                                   append = FALSE),
  classes = "warning")

###### c. re-merge ####
# Left join these datasets
check_pf <- dplyr::left_join(check_pf, 
                             countryOutput, 
                             by = "database_id",
                             suffix = c("", "CO"))  %>% 
  # Take the new country name if the original is NA
  dplyr::mutate(country = dplyr::if_else(is.na(country),
                                         countryCO,
                                         country)) %>%
  # Remove duplicates if they arose from left_join!
  dplyr::distinct()

# Save the dataset
check_pf %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "01_prefilter_database.csv",
                         sep = "/"))
# Save the countryOutput dataset
countryOutput %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "countryOutput.csv",
                         sep = "/"))
# Read in IF needed
if(!exists("check_pf")){
check_pf <- readr::read_csv(paste(DataPath, 
             "Output", "Intermediate", "01_prefilter_database.csv", sep = "/"),
             col_types = BeeDC::ColTypeR())}
# remove the interim datasets
rm(check_pf_noNa, countryOutput)

##### 3.6 StandardCoNames ####
# Run the function
  # Standardise country names and add ISO2 codes if needed
check_pf <- bdc::bdc_country_standardized(
  # Remove the countryCode and country_suggested columns to avoid an error with 
    # where two "countryCode" and "country_suggested" columns exist (i.e. if the dataset has been  
    # run before)
  data = check_pf %>% dplyr::select(!tidyselect::any_of(c("countryCode", "country_suggested"))),
  country = "country"
) 

##### 3.7 TranspCoords ####
  # Flag and correct records when lat and long appear to be transposed. We have chunked 
    # this because it is too RAM-heavy to run on our large dataset
check_pf <- BeeDC::jbd_Ctrans_chunker(
  # bdc_coordinates_transposed inputs
  data = check_pf,
  id = "database_id",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  countryCode = "countryCode",
  border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
  save_outputs = TRUE,
  sci_names = "scientificName",
  # chunker inputs
  stepSize = 1000000,  # How many rows to process at a time
  chunkStart = 1,  # Start row
  append = FALSE  # If FALSE it may overwrite existing dataset
) 

# Get a summary of the number of transposed records
table(check_pf$coordinates_transposed, useNA = "always")

# Save the dataset
check_pf %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "01_prefilter_database.csv",
                         sep = "/"))
gc()

##### 3.8 Coord-country ####
# Read data in again if needed
if(!exists("check_pf")){
  check_pf <- readr::read_csv(paste(OutPath_Intermediate, "01_prefilter_database.csv",
                                    sep = "/"))}
# Collect all country names in the country column
# rebuilt a bdc function to flag occurrences where the coordinates are inconsistent with the provided
  # country name
check_pf <- BeeDC::jbd_coordCountryInconsistent(
  data = check_pf,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  mapResolution = 50,
  pointBuffer = 0.01)

# Save the dataset
check_pf %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "01_prefilter_database.csv",
                         sep = "/"))

##### 3.9 GeoRefIssue ####
# This function Identifies records whose coordinates can potentially be extracted from locality 
  # information must be manually checked later
xyFromLocality <- bdc::bdc_coordinates_from_locality(
  data = check_pf,
  locality = "locality",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  save_outputs = TRUE
) %>%
# Save data if needed.
  readr::write_excel_csv(paste(OutPath_Check, "01_coordinates_from_locality.csv",
                         sep = "/"))
# Remove spent data
rm(xyFromLocality)

##### 3.10 flag Absent ####
# Flag the records marked as "absent"
check_pf <- BeeDC::flagAbsent(data = check_pf,
                   PresAbs = "occurrenceStatus")

##### 3.11 flag License ####
# Flag the records that may not be used according to their license information
check_pf <- BeeDC::flagLicense(data = check_pf,
                    strings_to_restrict = "all",
                    # DON'T flag if in the following dataSource(s)
                    excludeDataSource = NULL)

##### 3.12 GBIF issue ####
# Flag select issues that are flagged by GBIF
check_pf <- BeeDC::GBIFissues(data = check_pf, 
                   issueColumn = "issue", 
                   GBIFflags = c("COORDINATE_INVALID", "ZERO_COORDINATE")) 


##### 3.13 Flag Reports ####
###### a. save flags ####
# SAVE the flags so far
# This function will make sure that you keep a copy of everything that has been flagged up until now.
# This will be updated throughout the script and accessed at the end, so be wary of moving files around manually.
flagFile <- BeeDC::flagRecorder(
  data = check_pf,
  outpath = paste(OutPath_Report, sep =""),
  filename = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
    # These are the columns that will be kept along with the flags
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
    # TRUE if you want to find a file from a previous part of the script to append to
  append = FALSE)

# produce the .summary column in main dataset — will be FALSE if ANY .filtering column is FALSE
check_pf <- BeeDC::summaryFun(
  data = check_pf,
    # Don't filter these columns (or NULL)
  dontFilterThese = NULL,
    # Remove the filtering columns?
  removeFilterColumns = FALSE,
    # Filter to ONLY cleaned data?
  filterClean = FALSE)

###### b. reporting ####
(report <- bdc::bdc_create_report(data = check_pf,
                                  database_id = "database_id",
                                  workflow_step = "prefilter",
                                  save_report = TRUE)
)

###### c. figures ####
figures <-
  bdc::bdc_create_figures(data = check_pf,
                          database_id = "database_id",
                          workflow_step = "prefilter",
                          save_figures = TRUE)
# You can check figures using
figures$.coordinates_country_inconsistent

##### 3.14 save ####
# Save the intermediate dataset
check_pf %>%
  readr::write_excel_csv(., paste(OutPath_Intermediate, "01_prefilter_output.csv",
                            sep = "/"))



#### 4.0 Taxonomy ####
# See bdc tutorial here — https://brunobrr.github.io/bdc/articles/taxonomy.html
if(!exists("check_pf")){
# Read in the filtered dataset
database <-
  readr::read_csv( paste(OutPath_Intermediate, "01_prefilter_output.csv",
                         sep = "/"), col_types = BeeDC::ColTypeR())
}else{
    # OR rename and remove
  database <- check_pf
  # Remove spent dataset
  rm(check_pf)}

# Remove names_clean if it already exists (i.e. you have run this before on this dataset)
database <- database %>%
  dplyr::select(!tidyselect::any_of("names_clean"))

##### 4.1 Prep data names ####
# This next step cleans the database's scientificName column. The cleaning tool will:
# Flag and remove family names pre-pended to species names;
# Flag and remove qualifiers denoting uncertain or provisional status of taxonomic identification (e.g., confer, species, affinis, and among others);
# Flag and remove infraspecific terms (e.g., variety [var.], subspecies [subsp], forma [f.], and their spelling variations);
# Standardize names, i.e., capitalize only the first letter of the genus name and remove extra whitespaces);
# Parse names, i.e., separate author, date, annotations from taxon name.
# ! MAC: You need to install gnparser through terminal — brew
  # brew tap gnames/gn
  # brew install gnparser
parse_names <-
  bdc::bdc_clean_names(sci_names = database$scientificName, save_outputs = TRUE)

# Keep only the .uncer_terms and names_clean columns
parse_names <-
  parse_names %>%
  dplyr::select(.uncer_terms, names_clean)
# Merge names with the complete dataset
database <- dplyr::bind_cols(database, parse_names)
rm(parse_names)


##### 4.2 Taxo harmonization ####
# Read in the custom taxonomy file
data(beesTaxonomy, package = "BeeDC")

# Harmonise the names in the occurrence tibble
#   # This flags the occurrences without a matched name and matches names to their correct name 
  # according to DiscoverLife
database <- BeeDC::HarmoniseR(path = DataPath, #The path to a folder that the output can be saved
                       SynList = beesTaxonomy, # The formatted taxonomy file
                       occurrences = database)

# You don't need this file anymore...
rm(beesTaxonomy)

# Save the harmonised file.
database %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "02_taxonomy_database.csv",
                         sep = "/"))

##### 4.3 Save flags ####
# SAVE the flags so far
# This will find the most-recent flag file and append your new data to it.
# You can double-check the data and number of columns if you'd like to be thorough and sure that 
# all data are intact. <3 
flagFile <- BeeDC::flagRecorder(
  data = database,
  outpath = paste(OutPath_Report, sep =""),
  filename = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  append = TRUE,
  printSummary = TRUE)


#### 5.0 Space #### 
# the final frontier or whatever.
# Read in the last database
if(!exists("database")){
database <-
  readr::read_csv(paste(OutPath_Intermediate, "02_taxonomy_database.csv", sep = "/"),
                  col_types = BeeDC::ColTypeR())}

##### 5.1 Coord precision ####
# This function identifies records with a coordinate precision below a specified number of decimal 
# places. For example, the precision of a coordinate with 1 decimal place is 11.132 km at the 
# equator, i.e., the scale of a large city.
# "Coordinates with one, two, or three decimal places present a precision of
# ~11.1 km, ~1.1 km, and ~111 m at the equator, respectively."
# This function differs from the bdc function by ONLY flagging occurrences where BOTH lat and lon
    # are rounded (having only one or the other rounded could be due to rounding in excel).
check_space <-
  BeeDC::jbd_coordinates_precision(
    data = database,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = 2 # number of decimals to be tested
  )
# Remove the spent dataset
rm(database)

# Save the harmonised file.
check_space %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "03_space_inter_database.csv",
                         sep = "/"))

##### 5.2 Common spatial issues ####
# Only run for occurrences through clean_coordinates that are spatially 'valid'.
# Not doing this might crash R.
tempSpace <- check_space %>% 
  dplyr::filter(!.coordinates_empty == FALSE) %>%
  dplyr::filter(!.coordinates_outOfRange == FALSE) %>%
# Next, we will flag common spatial issues using functions of the package CoordinateCleaner.
# Addresses some common issues in biodiversity datasets
  CoordinateCleaner::clean_coordinates(
    x =  .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = NULL, # Tests if coords are from x country. This is not needed.
    tests = c(
      "capitals",     # records within 0.5 km of capitals centroids
      "centroids",    # records within 1 km around country and province centroids
      "equal",      # records with equal coordinates
      "gbif",         # records within 1 km of GBIF headquarters. (says 1 degree in package, but code says 1000 m)
      "institutions", # records within 100m of zoo and herbaria
      "zeros"       # records with coordinates 0,0
      # "seas"        # Not flagged as this should be flagged by coordinate country inconsistent
    ),
    capitals_rad = 1000,
    centroids_rad = 500,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 100, # remove zoo and herbaria within 100m
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_scale = 50,
    value = "spatialvalid" # result of tests are appended in separate columns
  )
# re-merge the datasets
check_space <- tempSpace %>%
  # Re-bind with the records that were excluded earlier
  dplyr::bind_rows(check_space %>% 
                     dplyr::filter(.coordinates_empty == FALSE | 
                                     .coordinates_outOfRange == FALSE) )
# Remove the temporary dataset
rm(tempSpace)

  # Save the intermediate dataset
check_space %>%
  readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_inter_database.csv",
                         sep = "/"))


##### 5.3 diagonal + grid ####
# Finds sequential numbers that could be fill-down errors in lat and long. 
# groups by eventDate, recordedBy
# This will ONLY find those where ALL records in a group are filled-down. missing occurrences
# will break the chain
check_space <- BeeDC::diagonAlley(
  data = check_space,
  # The minimum number of repeats needed to find a sequence in for flagging
  minRepeats = 4)

# SPATIAL gridding from rasterisation:
# Select only the records with more than X occurrences
griddingDF <- check_space %>%
  # Exclude NA lat and lon values
  tidyr::drop_na(c("decimalLatitude", "decimalLongitude")) %>%
  # Group by the dataset name
  dplyr::group_by(datasetName) %>%
  # Remove rows that aren't unique for lat and long
  dplyr::distinct(decimalLongitude, decimalLatitude,
                  .keep_all = TRUE) %>%
  # Find the groups with 4 or more occurrence records 
  dplyr::filter(dplyr::n() >= 4) %>%
  dplyr::ungroup()

# Run the gridding analysis to find datasets that might be gridded
gridded_datasets <- CoordinateCleaner::cd_round(
  x = griddingDF,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ds = "datasetName",
  T1 = 7,
  min_unique_ds_size = 4,
  test = "both",
  value = "dataset",
  graphs = FALSE,
  verbose = TRUE,
  reg_out_thresh = 2,
  reg_dist_min = 0.1,
  reg_dist_max = 2
) %>% 
  tibble::tibble()
# The griddingDF is no longer needed. remove it.
rm(griddingDF)

# Integrate these results with the main dataset
check_space <- check_space %>%
  # Join the datasets
  dplyr::left_join(
    # Select the columns of interest
    dplyr::select(gridded_datasets, dataset, lon.flag, lat.flag, summary),
    by = c("datasetName" = "dataset")) %>%
  # Make new columns with more-consistent naming and change the NA vlaues to = TRUE (not flagged)
  dplyr::mutate(.lonFlag = tidyr::replace_na(lon.flag, TRUE),
                .latFlag = tidyr::replace_na(lat.flag, TRUE),
                .gridSummary = tidyr::replace_na(summary, TRUE)) %>%
  # Remove old columns
  dplyr::select(!c(lon.flag, lat.flag, summary))

# Save the gridded_datasets file for later examination
gridded_datasets %>%
  readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_griddedDatasets.csv",
                         sep = "/"))
# Now remove this file
rm(gridded_datasets)

##### 5.4 Uncertainty ####
# Flag records that exceed a coordinateUncertaintyInMeters threshold
check_space <- BeeDC::coordUncerFlagR(data = check_space,
                               uncerColumn = "coordinateUncertaintyInMeters",
                               threshold = 1000)

##### 5.5 Country Checklist ####
# Read in the country-level checklist
data("beesChecklist", package = "BeeDC")

check_space <- BeeDC::countryOutlieRs(checklist = beesChecklist,
                        occData = check_space,
                        keepAdjacentCountry = TRUE,
                        pointBuffer = 0.05,
                          # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                          # Smaller numbers will result in much longer calculation times. 
                          # We have not attempted a scale of 10.
                        rnearthScale = 50)
  # A list of failed species-country combinations and their numbers can be output here
check_space %>%
  dplyr::filter(.countryOutlier == FALSE) %>%
  dplyr::select(database_id, scientificName, country) %>%
  dplyr::group_by(scientificName) %>% 
  dplyr::mutate(count_scientificName = n()) %>%
  distinct(scientificName, country, .keep_all = TRUE) %>% 
  readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_failedCountryChecklist.csv",
                         sep = "/"))


##### 5.6 Map spatial errors ####
#rebuild the .summary column
check_space <- BeeDC::summaryFun(
  data = check_space,
  dontFilterThese = NULL,
  removeFilterColumns = FALSE,
  filterClean = FALSE)
# Map ONE spatial flag at a time or map the .SUMMARY of all
# Make this selection in the col_to_map = section
check_space %>%
  dplyr::filter(.summary == FALSE) %>% # map only records flagged as FALSE
  bdc::bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".summary",
    size = 0.9
  )

##### 5.7 Space report ####
# Create the report
(report <-
   bdc::bdc_create_report(
     data = tibble::tibble(check_space %>% dplyr::select(!.uncer_terms)),
     database_id = "database_id",
     workflow_step = "space",
     save_report = TRUE)
)

##### 5.8 Space figures ####
# Sadly not a figure of outer space :( 
# Create figures of spacial data filtering
(figures <-
    BeeDC::jbd_create_figures(
      data = tibble::tibble(check_space %>% dplyr::select(!.uncer_terms)),
      path = DataPath,
      database_id = "database_id",
      workflow_step = "space",
      save_figures = TRUE)
)

# Check figures using
# options are:
# .cap == Records around country capital centroid	
# .cen == Records around country or province centroids
# .dbl == Duplicated coordinates per species
# .equ == Identical coordinates
# .otl == Geographical outliers
# .gbf == Records around the GBIF headquarters
# .inst == Records around biodiversity institutions
# .rou == Rounded (probably imprecise) coordinates
# .urb == Records within urban areas — Not relevant for bees, I think.
# Example:
figures$.rou

# Save interim dataset
check_space %>%
  readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_inter_database.csv",
                         sep = "/"))

##### 5.9 Save flags ####
# SAVE the flags so far
BeeDC::flagRecorder(
  data = check_space,
  outpath = paste(OutPath_Report, sep =""),
  filename = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  append = TRUE,
  printSummary = TRUE)

##### 5.10 Save ####
  # Save the intermediate dataset
check_space %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "03_space_database.csv",
                         sep = "/"))



#### 6.0 Time ####
# Read in the last database
if(!exists("check_space")){
  check_time <-
    readr::read_csv(paste(OutPath_Intermediate, "03_space_database.csv", sep = "/"),
                    col_types = BeeDC::ColTypeR())
  }else{
  check_time <- check_space
      # Remove the spent file
  rm(check_space)}

  # You can plot a histogram of dates here, pre-cleaning to examine potential issues
hist(lubridate::ymd_hms(check_time$eventDate, truncated = 5), breaks = 20)
# Filter some silly dates that don't make sense...
check_time$year <- ifelse(check_time$year > lubridate::year(Sys.Date()) | check_time$year < 1600,
                        NA, check_time$year)
check_time$month <- ifelse(check_time$month > 12 | check_time$month < 1,
                         NA, check_time$month)
check_time$day <- ifelse(check_time$day > 31 | check_time$day < 1,
                       NA, check_time$day)
##### 6.1 Recover dates ####
# RESCUE some records with poor date data if possible — e.g., from other columns
check_time <- BeeDC::dateFindR(data = check_time,
                        # Years above this are removed (from the recovered dates only)
                        maxYear = lubridate::year(Sys.Date()),
                        # Years below this are removed (from the recovered dates only)
                        minYear = 1700)

##### 6.2 No eventDate ####
# Flag records that simply lack collection date :(
check_time <-
  bdc::bdc_eventDate_empty(data = check_time, eventDate = "eventDate")

##### 6.3 Old records ####
# This will flag records prior to the date selected. 1950 is frequently chosen for SDM work. You may
  # not need to filter old records at all. Please just think critically about your use
check_time <-
  bdc::bdc_year_outOfRange(data = check_time,
                           eventDate = "year",
                           year_threshold = 1950)

##### 6.4 Time report ####
# Not all of it, just the time pertaining to our precise occurrence records. Obviously...
# Create a .summary column with all of the time flags where TRUE == records that passed 
# all filtering.
check_time <- BeeDC::summaryFun(
  data = check_time,
  # Don't filter these columns (or NULL)
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms"),
  # Remove the filtering columns?
  removeFilterColumns = FALSE,
  # Filter to ONLY cleaned data?
  filterClean = FALSE)

( report <-
    bdc::bdc_create_report(data = check_time,
                           database_id = "database_id",
                           workflow_step = "time",
                           save_report = FALSE)
)  

##### 6.5 Time figures ####
# Create figures
figures <-
  BeeDC::jbd_create_figures(data = check_time,
                     path = DataPath,
                     database_id = "database_id",
                     workflow_step = "time",
                     save_figures = TRUE)

# Check figures using
figures$year

# Save the ~raw time dataset into the intermediate folder
check_time %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "04_time_database.csv",
                         sep = "/"))

##### 6.6 Save flags ####
# SAVE the flags so far
BeeDC::flagRecorder(
  data = check_time,
  outpath = paste(OutPath_Report, sep =""),
  filename = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  append = TRUE,
  printSummary = TRUE)

#### 7.0 De-duplication ####
# Raw dataset can be re-read here if it does not already exist
if(!exists("check_time")){
  check_time <-
    readr::read_csv(paste(OutPath_Intermediate, "04_time_database.csv",
                          sep = "/"),
                    col_types = BeeDC::ColTypeR())}

##### 7.1 deDuplicate ####
# We will FLAG duplicates here. 
# These input columns can be hacked to de-duplicate as you wish.
check_time <- BeeDC::dupeSummary(
  data = check_time,
  path = paste0(DataPath, "/Output/Report/"),
  # options are "ID","collectionInfo", or "both"
  duplicatedBy = "collectionInfo", 
  # The columns to generate completeness info from (and to sort by completness)
  completeness_cols = c("decimalLatitude",  "decimalLongitude",
                        "scientificName", "eventDate"),
  # idColumns = c("gbifID", "occurrenceID", "recordId","id"),
  # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
  collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                     "recordedBy"),
  # The columns to combine, one-by-one with the collectionCols
  collectInfoColumns = c("catalogNumber", "otherCatalogNumbers"),
    # Custom comparisons — as a list of columns to compare
     # RAW custom comparisons do not use the character and number thresholds
  CustomComparisonsRAW = tibble::lst(c("catalogNumber", "institutionCode", "scientificName")),
     # Other custom comparisons use the character and number thresholds
  CustomComparisons = tibble::lst(c("gbifID", "scientificName"),
                                  c("occurrenceID", "scientificName"),
                                  c("recordId", "scientificName"),
                                  c("id", "scientificName")),
  # The order in which you want to KEEP duplicated based on data source
  # try unique(check_time$dataSource)
  sourceOrder = c("CAES", "Gai", "Ecd","BMont", "BMin", "EPEL", "ASP", "KP", "EcoS", "EaCO",
                  "FSCA", "Bal", "SMC", "Lic", "Arm",
                  "USGS", "ALA", "GBIF","SCAN","iDigBio"),
    # Paige ordering is done using the database_id prefix, not the dataSource prefix.
  prefixOrder = c("Paige", "Dorey"),
    # Set the complexity threshold for id letter and number length
     # minimum number of characters when WITH the numberThreshold
  characterThreshold = 2,
     # minimum number of numbers when WITH the characterThreshold
  numberThreshold = 3,
     # Minimum number of numbers WITHOUT any characters
  numberOnlyThreshold = 5
) %>% # END dupeSummary
  tibble::as_tibble(col_types = BeeDC::ColTypeR())

# Save the dataset into the intermediate folder
check_time %>%
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "04_2_dup_database.csv",
                         sep = "/"))

##### 7.2 Save flags ####
# SAVE the flags so far
BeeDC::flagRecorder(
  data = check_time,
  outpath = paste(OutPath_Report, sep =""),
  filename = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  append = TRUE,
  printSummary = TRUE)


#### 8.0 Data filtering ####
# Raw dataset can be re-read here if it does not already exist
if(!exists("check_time")){
 check_time <-
   readr::read_csv(paste(OutPath_Intermediate, "04_2_dup_database.csv",
                          sep = "/"))}
##### 8.1 rm Outliers ####
# Read in the most-recent duplicates file as well
if(!exists("duplicates")){
  duplicates <- file_finder(path = DataPath,
                            fileName = "duplicateRun_") %>%
    readr::read_csv()}
# identify the outliers and get a list of their database_ids
check_time <- manualOutlierFindeR(
  occData = check_time,
  DataPath = DataPath,
  PaigeOutliersName = "removedBecauseDeterminedOutlier.csv",
  newOutliersName = "^All_outliers_ANB_14March.xlsx",
  ColombiaOutliers_all = "All_Colombian_OutlierIDs.csv",
  # A .csv with manual outlier records that are too close to otherwise TRUE records
  NearTRUE = "nearTRUE.csv",
  duplicates = duplicates)

##### 8.2 Save uncleaned ####
  # Make sure that the .summary column is updated
check_time <- summaryFun(
  data = check_time,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                      ".uncertaintyThreshold"),
  removeFilterColumns = FALSE,
  filterClean = FALSE)
# Save the uncleaned dataset
check_time %>% readr::write_excel_csv(.,
                                paste(OutPath_Intermediate, "05_unCleaned_database.csv",
                                      sep = "/"))

##### 8.3 Save cleaned ####
# Now clean the dataset of extra columns and failed rows and save it...
BeeDC::summaryFun(
  data = check_time,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                      ".uncertaintyThreshold"),
  # Remove the filtering columns?
  removeFilterColumns = TRUE,
  # Filter to ONLY cleaned data?
  filterClean = TRUE) %>% 
# Save this CLEANED dataset
  readr::write_excel_csv(.,
                   paste(OutPath_Intermediate, "05_cleaned_database.csv",
                         sep = "/"))
# Dataset can be re-read here
    #  cleanData <-
    #    readr::read_csv(paste(OutPath_Intermediate, "05_cleaned_database.csv",
    #                           sep = "/"))


#### 9.0 Summary figures and tables ####
##### 9.1 Duplicate chordDiagrams ####
# install ComplexHeatmap if needed
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

# Read in the most-RECENT file
if(!exists("duplicates")){
  duplicates <- file_finder(path = DataPath,
                            fileName = "duplicateRun_") %>%
    readr::read_csv()}

# Choose the global figure parameters
par(mar = c(2, 2, 2, 2)/2, mfrow = c(1,1))
# Create the chorDiagram. You can leave many of the below values out but we show here
# the defaults
BeeDC::chordDiagramR(
  # The duplicate data from the dupeSummary function output  
  dupeData = duplicates,
  savePath = paste0(OutPath_Figures, "ChordDiagram.pdf", sep = "/"),
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


##### 9.2 Duplicate histogram ####
# Use the uncleaned dataset
if(!exists("check_time")){
beeData <- readr::read_csv(paste(OutPath_Intermediate, "05_unCleaned_database.csv",
                                 sep = "/"),
                           col_types = BeeDC::ColTypeR())
}else{
  beeData <- check_time
  rm(check_time)
}
# Create a figure shoring the total number of duplicates, kept duplicates, and unique
# records for each data source (simplified to the text before the first underscore) and
# the proportion of the above for each data source
BeeDC::dupePlotR(
  Data = beeData,
  # The outpath to save the plot as
  outpath = paste0(OutPath_Figures, "duplicatePlot.pdf", sep = "/"),
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'BMont' = "BMont", 'BMin' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL"
)


##### 9.3 Flags by source ####
# Visualise all flags for each dataSource (simplified to the text before the first underscore)
BeeDC::plotFlagSummary(
  plotData = beeData,
  # Colours in order of pass (TRUE), fail (FALSE), and NA
  flagColours = c("#127852", "#A7002D", "#BDBABB"),
  filename = paste0("FlagsPlot_", Sys.Date(),".pdf"),
  outpath = paste0(OutPath_Figures),
  width = 15, height = 9,
    # OPTIONAL:
      #   # Filter to species
      #       speciesName = "Holcopasites heliopsis",
      #         # column to look in
      #       nameColumn = "species",
      #        # Save the filtered data
      #       saveFiltered = TRUE,
      #   # Filter column to display on map
      #       filterColumn = ".summary",
      #       plotMap = TRUE,
      #   # amount to jitter points if desired, e.g. 0.25 or NULL
      #       jitterValue = NULL,
      #        # Map opacity value for points between 0 and 1
      #   mapAlpha = 1,
      #        # If a user wants to output the table used to make the figure, change this to TRUE
      #   saveTable = FALSE,
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'BMont' = "BMont", 'BMin' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL"
)


##### 9.4 Maps ####
# Import CLEANED dataset (you can change this option)
mapData <- readr::read_csv(paste(OutPath_Intermediate, "05_cleaned_database.csv",
                                 sep = "/"),
                           col_types = BeeDC::ColTypeR())

  ###### a. Summary maps ####
  # Draw a global summary map for occurrence and species number by country
BeeDC::summaryMaps(
  mapData = mapData,
  width = 10, height = 10,
  class_n = 15,
  class_Style = "fisher",
  filename = paste0(OutPath_Figures, "CountryMaps_fisher.pdf", sep = "/")
)

  ###### b. Interactive maps ####
  # Generate a list of random species names to map and check
beeData_interactive <- beeData %>%
    # Select only valid species
  dplyr::filter(.invalidName == TRUE) %>%
    # Get a distinct list of valid species names
  dplyr::distinct(scientificName, .keep_all = FALSE) %>% 
    # Select a random subset of species to map
  slice_sample(n = 100)

# Make the interactive maps
BeeDC::interactiveMapR(
  # occurrence data
  database = beeData %>%
      # Select only those species
    dplyr::filter(scientificName %in% beeData_interactive),
  # Directory where to save files
  dir = paste0(OutPath_Figures, "interactiveMaps", sep = "/"),
  # lat long columns
  longitude = "decimalLongitude",
  latitude = "decimalLatitude",
  # Occurrence dataset column with species names
  speciesColumn = "scientificName",
  # Which species to map — a character vector of names or "ALL"
  # Note: "ALL" is defined AFTER filtering for country
  speciesList = "ALL",
  countryList = NULL, # studyArea
  # Point jitter to see stacked points — jitters an amount in decimal degrees
  jitterValue = 0.01
)

  # EXCLUDE THIS IN DOCUMENTATION, ROBBIE...
  ##### 9.5 Data providers ####
# Read in the clean data if it's not already in the environment
if(!exists("mapData")){
  mapData <- readr::read_csv(paste(OutPath_Intermediate, "05_cleaned_database.csv",
                                sep = "/"),
                          col_types = BeeDC::ColTypeR(),
  locale = readr::locale(encoding = "UTF-8"))}
institutionList_DL <- readxl::read_excel(paste(DiscLifePath, "Apoidea Bee Collections Master List jan 2023.xlsx",
                                               sep = "/"))
source(paste(ScriptPath, "dataProvTables.R", sep = "/"))
TEST <- BeeDC::dataProvTables(
    occData = mapData,
      # Some manual extractions of instutionCode for bee data
    runBeeDataChecks = TRUE,
    institutionList = institutionList_DL)

BISON <- mapData %>%
  dplyr::filter(institutionCode == "BISON")

TEST <- mapData %>%
  dplyr::group_by(institutionCode) %>%
  dplyr::add_tally() %>%
  dplyr::select(c(institutionCode, n)) %>%
  dplyr::distinct(institutionCode, .keep_all = TRUE)

TEST2 <- occData %>%
  dplyr::filter(is.na(institutionCode)) %>%
 # dplyr::filter(stringr::str_detect(dataSource, "GBIF|iDigBio")) %>%
  dplyr::select(c(
    database_id, dataSource, scientificName, family, genus, previousIdentifications, identifiedBy, 
    decimalLatitude, decimalLongitude, stateProvince, country, eventDate, eventTime, year,
    recordNumber, recordedBy, eventID, samplingProtocol, catalogNumber, gbifID, datasetID,
    institutionCode, datasetName, otherCatalogNumbers, occurrenceID, coreid, recordId, rightsHolder,
    bibliographicCitation, references, id, verbatim_scientificName
  ))
  
table(TEST2$dataSource)

mapData$bibliographicCitation %>% unique()






 