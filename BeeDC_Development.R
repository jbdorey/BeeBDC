# Development

#### 0.0 Packages ####
install.packages("devtools")
install.packages("usethis")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
remotes::install_github("AtlasOfLivingAustralia/galah@dev")

library(devtools)
library(usethis)

#### 1.0 one-time set up ####
# Most of this is not necessary
# Create a Github Personal Access Token to interact wit hmy GitHub
usethis::create_github_token()
usethis::git_vaccinate()

# Once per package 
  # initialise a Git
?use_git()
  # connect a local repo with Git
?use_github()
  # Sets up continuous integration (CI) for an R package that is developed on GitHub using GitHub Actions. CI can be used to trigger various operations for each push or pull request, such as:
use_github_actions()

#### 2.0 Create package ####
  ##### 2.1 Descriptions ####
# Write DESCRIPTION file
packageDir <- "/Users/jamesdorey/Desktop/Uni/Packages/BeeDC"
setwd(packageDir)
usethis::create_package(path = packageDir,
                        roxygen = TRUE,
                        rstudio = FALSE,
                        check_name = TRUE,
                        open = FALSE,
                        fields = list(
                          Title = "Cleans global bee, or other, occurrence data",
                          Version = "0.0.1",
                          `Authors@R` = c(person(given = "James B.",
                                               family = "Dorey",
                                               role = c("aut", "cre"),
                                               email = "jbdorey@me.com",
                                               comment = c(ORCID = "0000-0003-2721-3842"))),
                          Description = "Cleans occurrence data that are in Darwin Core format",
                          Encoding = "UTF-8",
                          roxygen = TRUE,
                          RoxygenNote = "7.2.1",
                          License = usethis::use_ccby_license()
                        ))
  # Add required packages
requiredPackages <- sort(c("R.utils","bdc","tidyr","magrittr","ggplot2","dplyr","tibble","forcats","galah", "EML","emld", "stringr","lubridate","tidyselect","mgsub","rnaturalearth","rnaturalearthdata", "circlize","BiocManager","paletteer","readxl","readr","cowplot","igraph","ggspatial", "janitor", "rlist"))
lapply(requiredPackages,  usethis::use_package, type = "Imports")


  # Add suggested packages
suggestedPackages <- sort(c("praise", "rlang", "xml2",  "rvest", "countrycode", "rangeBuilder","rworldmap","hexbin"))
lapply(suggestedPackages,  usethis::use_package, type = "Suggests")

# Order and format
usethis::use_tidy_description()



setwd(packageDir)

  ##### 2.2 Load functions ####
  # Load all of the functions
devtools::load_all()

  ##### 2.3 Generate documentation ####
#roxygen2::roxygenise()
devtools::document()


  ##### 2.4 Test package ####
# Set up tests
usethis::use_testthat(3)
devtools::test(pkg = packageDir) 


  ##### 2.5 Check package ####
devtools::check()







  
  
