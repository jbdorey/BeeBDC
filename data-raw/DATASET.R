## code to prepare `DATASET` dataset goes here
# Set up datasets
library(readr)
require(usethis)
setwd("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC/data-raw")
# Set data path
dataPath <- "/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_datafiles"

#### 1.0 Example occurrence data ####
beesFlagged <- readr::read_csv(paste0(dataPath, "/beesFlagged.csv"))
usethis::use_data(beesFlagged)

beesRaw <- readr::read_csv(paste0(dataPath, "/beesRaw.csv"))
usethis::use_data(beesRaw)

bees3sp <- readr::read_csv(paste0(dataPath, "/bees3sp.csv"))
usethis::use_data(bees3sp)

#### 2.0 Discover Life data ####
beesChecklist <- readr::read_csv(paste0(dataPath, "/CheckL_combined2023-06-27.csv"),
                                 guess_max = 40000)
usethis::use_data(beesChecklist, overwrite = TRUE)

beesTaxonomy <- readr::read_csv(paste0(dataPath, "/TaxonomyComplete_2023-07-12.csv"))
usethis::use_data(beesTaxonomy, overwrite = TRUE)



