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
beesChecklist <- readr::read_csv(paste0(dataPath, "/CheckL_combined2023-08-21.csv"),
                                 guess_max = 40000) %>%
    # Remove some columns to save space
  dplyr::select(!c(infraspecificEpithet, official, Notes, taxon_rank, matchCertainty))
usethis::use_data(beesChecklist, overwrite = TRUE, compress = "xz")

beesTaxonomy <- readr::read_csv(paste0(dataPath, "/TaxonomyComplete_2023-08-20.csv")) %>%
    # Remove some columns to save space
  dplyr::select(!c(valid))
usethis::use_data(beesTaxonomy, overwrite = TRUE, compress = "xz")



