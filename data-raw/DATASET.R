## code to prepare `DATASET` dataset goes here
# Set up datasets
library(readr)
require(usethis)
require(magrittr)
library(rnaturalearth)
setwd("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC/data-raw")
# Set data path
dataPath <- "/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_development"

#### 1.0 Example occurrence data ####
beesFlagged <- readr::read_csv(paste0(dataPath, "/beesFlagged.csv"))
usethis::use_data(beesFlagged, overwrite = TRUE, compress = "xz")

beesRaw <- readr::read_csv(paste0(dataPath, "/beesRaw.csv"))
usethis::use_data(beesRaw, overwrite = TRUE, compress = "xz")

bees3sp <- readr::read_csv(paste0(dataPath, "/bees3sp.csv"))
usethis::use_data(bees3sp, overwrite = TRUE, compress = "xz")

#### 2.0 Discover Life data ####
  ##### 2.1 Full datasets ####

    ###### a. checklist ####
# Read in the rnaturalearth dataset in order to add continent data 
countryMap <- rnaturalearth::ne_countries(returnclass = "sf", country = NULL,
                                          type = "map_units", scale = 50)  %>%
  # Select only a subset of the naturalearthdata columns to extract
  dplyr::select(name_long, name, continent, geometry) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(name_long = as.character(name_long)) %>%
  dplyr::mutate(name_long = dplyr::if_else(name_long == "Scotland",
                                           "United Kingdom", name_long))

  # CHECKLIST
beesChecklist <- readr::read_csv(paste0(dataPath, "/CheckL_combined2024-06-17.csv"),
                                 guess_max = 40000) %>%
     # Modify some country names to match rnaturalearth
   dplyr::mutate(rNaturalEarth_name = dplyr::if_else(rNaturalEarth_name == "Cape Verde",
                                                  "Republic of Cabo Verde", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(rNaturalEarth_name == "Republic of Congo",
                                                     "Republic of the Congo", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(rNaturalEarth_name == "French Southern Territories",
                              "French Southern and Antarctic Lands", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Macedonia"),
                                                     "North Macedonia", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Reunion"),
                                                     "Réunion", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Swaziland"),
                                                     "Kingdom of eSwatini", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "South Georgia and South Sandwich Islands"),
                                                     "South Georgia and the Islands", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Aland Islands"),
                                                     "Åland Islands", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Bouvet"),
                                                     "Norway", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Cocos"),
                                                     "Cocos Islands", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Falkland Islands"),
                                                     "Falkland Islands / Malvinas", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Darussalam"),
                                                     "Brunei Darussalam", rNaturalEarth_name),
                 rNaturalEarth_name = dplyr::if_else(stringr::str_detect(rNaturalEarth_name, "Scotland"),
                                                     "United Kingdom", rNaturalEarth_name)
                                  ) %>%
    # Modify some country names to match rnaturalearth
  dplyr::left_join(countryMap, by = c("rNaturalEarth_name" = "name_long")) %>%
  # Add in continent for Gibraltar
  dplyr::mutate(continent = dplyr::if_else(rNaturalEarth_name == "Gibraltar",
                                           "Europe", continent)) %>%
  # Remove some columns to save space
  dplyr::select(!c(infraspecificEpithet)) %>%
    # Move the continent column
  dplyr::relocate(continent, .after = shortName) %>%
    # Rename Alpha-3 to iso_a3_eh to match the new rnaturalearth column
  dplyr::rename(iso_a3_eh = "Alpha-3") %>%
  dplyr::select(!geometry)

base::save(beesChecklist, 
              file = paste0("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_development/",
                            "beesChecklist_", Sys.Date(), ".Rda"),
              compress = "xz")

    ###### b. taxonomy ####
  # TAXONOMY
beesTaxonomy <- readr::read_csv(paste0(dataPath, 
        "/restrictedScripts/TaxonomyFiles/TaxonomyComplete_2024-06-11.csv")) 
base::saveRDS(beesTaxonomy, 
              file = paste0("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_development/",
                            "beesTaxonomy_", Sys.Date(), ".Rda"),
              compress = "xz")



  ##### 2.2 Test datasets ####
  # Build smaller test DL datasets for those species that are in the example occurrence data (1.0)
# Get a list of all species present 
testSpecies <- unique(c(beesFlagged$scientificName, beesRaw$scientificName, 
                        bees3sp$scientificName))

  # Select only those rows for the checklist
testChecklist <- beesChecklist %>% 
  dplyr::filter(validName %in% testSpecies)
  # Save data 
usethis::use_data(testChecklist, overwrite = TRUE, compress = "xz")

# Select only those rows for the taxonomy
testTaxonomy <- beesTaxonomy %>% 
  dplyr::filter(validName %in% testSpecies)
  # Save data
usethis::use_data(testTaxonomy, overwrite = TRUE, compress = "xz")


  

