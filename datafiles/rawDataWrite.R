# Set up datasets
library(readr)
require(usethis)
setwd("/Users/jamesdorey/Desktop/Uni/Packages/BeeDC/datafiles")
  #### 1.0 Example occurrence data ####
beesFlagged <- readr::read_csv("beesFlagged.csv")
usethis::use_data(beesFlagged)

beesRaw <- readr::read_csv("beesRaw.csv")
usethis::use_data(beesRaw)

bees3sp <- readr::read_csv("bees3sp.csv")
usethis::use_data(bees3sp)

  #### 2.0 Discover Life data ####
beesChecklist <- readr::read_csv("CheckL_combined2023-06-27.csv",
                                 guess_max = 40000)
usethis::use_data(beesChecklist, overwrite = TRUE)

beesTaxonomy <- readr::read_csv("TaxonomyComplete_2023-06-27.csv")
usethis::use_data(beesTaxonomy, overwrite = TRUE)


