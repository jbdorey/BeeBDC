# Set up datasets
library(readr)
setwd("/Users/jamesdorey/Desktop/Uni/My_papers/Bee_SDM_paper/BDC_repo/BeeDC/data_raw")
  #### 1.0 Example occurrence data ####
beesFlagged <- readr::read_csv("beesFlagged.csv")
usethis::use_data(beesFlagged)

beesRaw <- readr::read_csv("beesRaw.csv")
usethis::use_data(beesRaw)

bees3sp <- readr::read_csv("bees3sp.csv")
usethis::use_data(bees3sp)

  #### 2.0 Discover Life data ####
beesChecklist <- readr::read_csv("CheckL_combined2023-02-20.csv")
usethis::use_data(beesChecklist)

beesTaxonomy <- readr::read_csv("Paige_DLdf_2022-12-07.csv")
usethis::use_data(beesTaxonomy)


