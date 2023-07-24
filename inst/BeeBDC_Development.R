# Development
# Notes to fix:
  # Checklist - include data sources in dataset
  # Taxonomy check for no-species occurrences and for subgenus in species... have a general peak!
  # Harmoniser, consider adding protection to avoid matching genus or subgenus-level taxa to valid names
  # This will help show tidyverse warnings more than once every 8 hours...
options(lifecycle_verbosity = "warning")

#### 0.0 Packages ####
install.packages("devtools")
install.packages("usethis")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
remotes::install_github("AtlasOfLivingAustralia/galah@dev")
install.packages("datapasta")
install.packages("xlsx")
install.packages("testthat")
library(devtools)
library(usethis)
library(datapasta)
library(xlsx)
library(testthat)

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
# Write DESCRIPTION file inst
packageDir <- "/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC"
packageVersion <- "0.2.0"
setwd(packageDir)
usethis::create_package(path = packageDir,
                        roxygen = TRUE,
                        rstudio = FALSE,
                        check_name = TRUE,
                        open = FALSE,
                        fields = list(
                          Title = "BeeBDC: an Occurrence Data Cleaning Package",
                          Version = packageVersion,
                          `Authors@R` = c(person(given = "James B.",
                                               family = "Dorey",
                                               role = c("aut", "cre", "cph"),
                                               email = "jbdorey@me.com",
                                               comment = c(ORCID = "0000-0003-2721-3842")),
                                          person(given = "Robert",
                                                 family = "O'Reilly",
                                                 role = c("aut"),
                                                 email = "robert.oreilly@flinders.edu.au",
                                                 comment = c(ORCID = "0000-0001-5291-7396")),
                                          person(given = "Silas",
                                                 family = "Bossert",
                                                 role = c("aut"),
                                                 email = "silas.bossert@wsu.edu",
                                                 comment = c(ORCID = "0000-0002-3620-5468")),
                                          person(given = "Fischer",
                                                 family = "Fischer",
                                                 role = c("aut"),
                                                 email = "fischeer@mail.gvsu.edu" #,
                                                 #comment = c(ORCID = "0000-0003-2721-3842")
                                                 )),
                          Description = 
                          "Flags and checks occurrence data that are in Darwin Core format.
                          The package includes generic functions and data as well as some that are
                          specific to bees. This package is meant to build upon and be complimentary
                          to other excellent occurrence cleaning packages, including 'bdc' and
                          'CoordinateCleaner'.
                          This package uses datasets from several sources and particularly from the
                          Discove Life Website, created by John Ascher and John Pickering:
                          ASCHER, J. S. & PICKERING, J. 2020. Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species.
                          For further information, please see the original publication of this
                          package:
                          DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N., O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M., TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A., RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A., ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO, T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. & COBB, N. S. In review. BeeBDC: An R package and globally synthesised and flagged bee occurrence dataset. Scientific Data.",
                          language = "en-gb",
                          Encoding = "UTF-8",
                          "Config/testthat/edition" = 3,
                          BugReports = "https://github.com/jbdorey/BeeBDC/issues",
                          roxygen = TRUE,
                          LazyData = TRUE,
                          LazyDataCompression = "xz",
                          RoxygenNote = "7.2.1",
                          Depends = "R (>= 2.10)",
                          License = "GPL-3 + file LICENSE"
                        ))
  # Add required packages
requiredPackages <- sort(c(
    # File packages
  "here", "openxlsx",'readr',
    # Tidyverse packages
  'stringr', 'tidyselect', 
  "dplyr","forcats","lubridate",
    # Other
  "mgsub", "igraph",
    # GIS packages
  'CoordinateCleaner', 'rnaturalearth', 'sf',
    # Graphical packages
  "ggspatial","ComplexHeatmap", "paletteer", 'ggplot2',  "circlize", "cowplot"
                           ))
  # "R.utils","rlist",
lapply(requiredPackages,  usethis::use_package, type = "Imports")

  # Add suggested packages
suggestedPackages <- sort(c("rlang", "xml2",  "rvest", "countrycode", "rangeBuilder",
                            #"rworldmap", 
                            "hexbin", "janitor", "rnaturalearthdata", "bdc",
                            "terra", "R.utils", 
                            "testthat", "emld", "purrr", "tidyr",
                            "classInt", "htmlwidgets", "httr", "leaflet", "plotly", 
                            "BiocManager",
                            "DT"))
lapply(suggestedPackages,  usethis::use_package, type = "Suggests")

  # Add non-Cran packages
usethis::use_dev_package("galah", type = "Imports", remote = "AtlasOfLivingAustralia/galah")


# In order to initialise a package citation file: (Don't re-run!)
#usethis::use_citation()

# Order and format
usethis::use_tidy_description()

  ##### 2.2 Load functions ####
  # Load all of the functions
devtools::load_all()

  ##### 2.3 Generate documentation ####
  ###### a. documentation ####
#roxygen2::roxygenise()
devtools::document()


citation("BeeBDC")
print(citation("BeeBDC"), bibtex=TRUE)


  ##### 2.4 Test package ####
# Set up tests
usethis::use_testthat(3)
devtools::test(pkg = packageDir) 
usethis::use_test("readr_BeeBDC")


  ##### 2.5 Check package ####
devtools::check()


  ##### 2.6 News and comments ####
  # Generate news file
usethis::use_news_md()
  # generate CRAN comments file
usethis::use_cran_comments()



usethis::use_release_issue()

usethis::use_github_action()

  #### 3.0 Further CRAN tests ####
  ##### 3.1 Coverage ####
devtools::test_coverage_active_file()

  ##### 3.2 Package weights ####
pak::pkg_deps_tree("BeeBDC")


  
  
