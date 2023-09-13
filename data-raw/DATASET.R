## code to prepare `DATASET` dataset goes here
# Set up datasets
library(readr)
require(usethis)
require(magrittr)
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
beesChecklist <- readr::read_csv(paste0(dataPath, "/CheckL_combined2023-08-21.csv"),
                                 guess_max = 40000) %>%
  # Remove some columns to save space
  dplyr::select(!c(infraspecificEpithet))
base::saveRDS(beesChecklist, 
              file = paste0("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_development/",
                            "beesChecklist.Rda"),
              compress = "xz")

beesTaxonomy <- readr::read_csv(paste0(dataPath, "/TaxonomyComplete_2023-08-20.csv")) 
base::saveRDS(beesTaxonomy, 
              file = paste0("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_development/",
                            "beesTaxonomy.Rda"),
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



