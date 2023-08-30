
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ***BeeBDC*** <a href='https://github.com/brunobrr/bdc'><img src="https://raw.githubusercontent.com/brunobrr/bdc/master/man/figures/logo.png" align="right" width="155"/></a>

## **BeeBDC: an occurrence data cleaning package**

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bdc)](https://CRAN.R-project.org/package=bdc)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/bdc)](https://cranlogs.r-pkg.org:443/badges/grand-total/bdc)
<!-- [![rstudio mirror -->
<!-- downloads](https://cranlogs.r-pkg.org/badges/bdc)](https://cranlogs.r-pkg.org:443/badges/bdc) -->
[![R-CMD-check](https://github.com/brunobrr/bdc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brunobrr/bdc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/brunobrr/bdc/branch/master/graph/badge.svg?token=9AUF86G9LJ)](https://app.codecov.io/gh/brunobrr/bdc)
[![Pre-print
DOI](https://doi.org/10.1101/2023.06.30.547152)](https://doi.org/10.1101/2023.06.30.547152)
[![License](https://img.shields.io/badge/license-GPL%20(%3E=%203)-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

<!-- badges: end -->

#### **Overview**

The consistent implementation of biodiversity data continues to be a
challenge for ecological researchers. We present the BeeBDC package
which provides novel and updated functions for flagging, cleaning, and
visualising occurrence datasets. Our functions are mostly general in
nature but we also provide some functions and data that are specific for
use with bee occurrence data. We build upon functions and conventions in
other fantastic R packages, especially
[bdc](https://brunobrr.github.io/bdc/) and
[CoordinateCleaner](https://ropensci.github.io/CoordinateCleaner/articles/Cleaning_GBIF_data_with_CoordinateCleaner.html).
Hence, our package name is Bee **B**iodiversity **D**ata **C**leaning
(BeeBDC).

We provide a full workflow that uses BeeBDC, bdc, and CoordinateCleaner
to clean occurrence data in our Articles page and encourage users to
read and also cite our primary
[publication](https://doi.org/10.1101/2023.06.30.547152).

#### **Structure of *BeeBDC***

The *BeeBDC* toolkit is organized using the conventions set by bdc, in
modules related to different biodiversity dimensions.

------------------------------------------------------------------------

> :warning: The modules illustrated, and **functions** within, **were
> linked to form** a proposed reproducible **workflow** (see
> [**vignettes**](https://brunobrr.github.io/bdc/)). However, all
> functions **can also be executed independently**.

------------------------------------------------------------------------

#### ![](https://github.com/jbdorey/BeeBDC/tree/main/inst/extdata/icon_vignettes/Figure1_14Jan2023.pdf)

<br/>

#### 1. [**Data merge**](https://brunobrr.github.io/bdc/articles/integrate_datasets.html)

Integrate and merge different datasets from major the data repositories
— GBIF, SCAN, iDigBio, the USGS, and ALA.

- `atlasDownloader()` Downloads ALA data and creates a new file in the
  path to put those data. This function can also request downloads from
  other atlases (see:
  <http://galah.ala.org.au/articles/choosing_an_atlas.html>). However,
  it will only send the download to your email and you must do the rest
  yourself at this point.
- `repoMerge()` Locates data from GBIF, ALA, iDigBio, and SCAN within a
  directory and reads it in along with its eml metadata.
- `repoFinder()` Find GBIF, ALA, iDigBio, and SCAN files in a directory.
- `importOccurrences()` Looks for and imports the most-recent version of
  the occurrence data created by the repoMerge() function.
- `USGS_formatter()` The function finds, imports, formats, and creates
  metadata for the USGS dataset.
- `formattedCombiner()` Merges the Darwin Core version of the USGS
  dataset that was created using USGS_formatter() with the main dataset.
- `dataSaver()` Used at the end of 1.x in the example workflow in order
  to save the occurrence dataset and its associated eml metadata.

#### 2. [**Data preperation**](https://brunobrr.github.io/bdc/articles/prefilter.html)

The reading in and formatting of the major and minor \[bee\] occurrence
repositories as well as some data modifications. This section is mostly,
but not entirely, related to bee occurrence data.

- `fileFinder()` A function which can be used to find files within a
  user-defined directory based on a user-provided character string.
- `PaigeIntegrater()` Replaces publicly available data with data that
  has been manually cleaned and error-corrected for use in the paper
  Chesshire, P. R., Fischer, E. E., Dowdy, N. J., Griswold, T.,
  Hughes, A. C., Orr, M. J., . . . McCabe, L. M. (2023).
  Completeness analysis for over 3000 United States bee species
  identifies persistent data gaps. Ecography. https://doi.org/10.1111/ecog.06584.
- `readr_BeeBDC()` Read in a variety of data files that are specific to
  certain smaller data providers. There is an internal readr function
  for each dataset and each one of these functions is called by
  readr_BeeBDC. While these functions are internal, they are displayed
  in the documentation of readr_BeeBDC for clarity.
- `idMatchR()` This function attempts to match database_ids from a prior
  bdc or BeeBDC run in order to keep this column somewhat consistent
  between iterations. However, not all records contain sufficient
  information for this to work flawlessly.

#### 3. [**Initial flags**](https://brunobrr.github.io/bdc/articles/prefilter.html)

Flagging and carpentry of several, mostly general, data issues. See
bdc’s
[pre-filter](https://brunobrr.github.io/bdc/articles/prefilter.html) for
more related functions.

- `countryNameCleanR()` This is a basic function for a user to manually
  fix some country name inconsistencies.
- `jbd_CfC_chunker()` Because the jbd_country_from_coordinates()
  function is very RAM-intensive, this wrapper allows a user to specify
  chunk-sizes and only analyse a small portion of the occurrence data at
  a time. The prefix jbd\_ is used to highlight the difference between
  this function and the original bdc::bdc_country_from_coordinates().
- `jbd_Ctrans_chunker()` Because the jbd_coordinates_transposed()
  function is very RAM-intensive, this wrapper allows a user to specify
  chunk-sizes and only analyse a small portion of the occurrence data at
  a time. The prefix jbd\_ is used to highlight the difference between
  this function and the original bdc::bdc_coordinates_transposed().
- `jbd_coordCountryInconsistent()` Compares stated country name in an
  occurrence record with record’s coordinates using rnaturalearth data.
  The prefix, jbd\_ is meant to distinguish this function from the
  original bdc::bdc_coordinates_country_inconsistent().
- `flagAbsent()` Flags occurrences that are “ABSENT” for the
  occurrenceStatus (or some other user-specified) column.
- `GBIFissues()` This function will flag records which are subject to a
  user-specified vector of GBIF issues.
- `flagRecorder()` This function is used to save the flag data for your
  occurrence data as you run the BeeBDC script. It will read and append
  existing files, if asked to. Your flags should also be saved in the
  occurrence file itself automatically.

#### 4. [**Taxonomy**](https://brunobrr.github.io/bdc/articles/taxonomy.html)

Harmonisation of scientific names against a custom taxonomy or the
provided Discover Life website’s taxonomic reference.

- `harmoniseR()` Uses the Discover Life taxonomy to harmonise bee
  occurrences and flag those that do not match the checklist. This
  function could be hijacked to service other taxa if a user matched the
  format of the beesTaxonomy file.

#### 5. [**Space**](https://brunobrr.github.io/bdc/articles/space.html)

Flagging of erroneous, suspicious, and low-precision geographic
coordinates.

- `jbd_coordinates_precision()` This function flags occurrences where
  BOTH latitude and longitude values are rounded. This contrasts with
  the original function, bdc::bdc_coordinates_precision() that will flag
  occurrences where only one of latitude OR longitude are rounded. The
  BeeBDC approach saves occurrences that may have had terminal zeros
  rounded in one coordinate column.
- `diagonAlley()` A simple function that looks for potential latitude
  and longitude fill-down errors by identifying consecutive occurrences
  with coordinates at regular intervals. This is accomplished by using a
  sliding window with the length determined by minRepeats.
- `coordUncerFlagR()` To use this function, the user must choose a
  column, probably “coordinateUncertaintyInMeters” and a threshold above
  which occurrences will be flagged for geographic uncertainty.
- `countryOutlieRs()` This function flags country-level outliers using
  the checklist provided with this package. For additional context and
  column names, see ?beesChecklist().
- `jbd_create_figures()` Creates figures (i.e., bar plots, maps, and
  histograms) reporting the results of data quality tests implemented
  the bdc and BeeBDC packages. Works like bdc::bdc_create_figures(), but
  it allows the user to specify a save path.

#### 6. [**Time**](https://brunobrr.github.io/bdc/articles/time.html)

Flagging and, whenever possible, correction of inconsistent collection
date.

- `dateFindR()` A function made to search other columns for dates and
  add them to the eventDate column. The function searches the columns
  locality, fieldNotes, locationRemarks, and verbatimEventDate for the
  relevant information.

#### 7. [**De-duplication**](https://brunobrr.github.io/bdc/articles/time.html)

- `dupeSummary()` This function uses user-specified inputs and columns
  to identify duplicate occurrence records. Duplicates are identified
  iteratively and will be tallied up, duplicate pairs clustered, and
  sorted at the end of the function. The function is designed to work
  with Darwin Core data with a database_id column, but it is also
  modifiable to work with other columns.

#### 8. [**Filtering**](https://brunobrr.github.io/bdc/articles/time.html)

- `manualOutlierFindeR()` Uses expert-identified outliers with source
  spreadsheets that may be edited by users. The function will also use
  the duplicates file made using dupeSummary() to identify duplicates of
  the expert-identified outliers and flag those as well. The function
  will add a flagging column called .expertOutlier where records that
  are FALSE are the expert outliers.
- `summaryFun()` Using all flag columns (column names starting with
  “.”), this function either creates or updates the .summary flag column
  which is FALSE when ANY of the flag columns are FALSE. Columns can be
  excluded and removed after creating the .summary column. Additionally,
  the occurrence dataset can be filtered to only those where .summary =
  TRUE at the end of the function.

#### 9. [**Figures and tables**](https://brunobrr.github.io/bdc/articles/time.html)

- `chordDiagramR()` This function outputs a figure which shows the
  relative size and direction of occurrence points duplicated between
  data providers, such as, SCAN, GBIF, ALA, etc.
- `dupePlotR()` Creates a plot with two bar graphs. One shows the
  absolute number of duplicate records for each data source while the
  other shows the proportion of records that are duplicated within each
  data source.
- `plotFlagSummary()` Creates a compound bar plot that shows the
  proportion of records that pass or fail each flag (rows) and for each
  data source (columns). The function can also optionally return a point
  map for a user-specified species when plotMap = TRUE.
- `summaryMaps()` Builds an output figure that shows the number of
  species and the number of occurrences per country. Breaks the data
  into classes for visualisation. Users may filter data to their taxa of
  interest to produce figures of interest.
- `interactiveMapR()` Uses the occurrence data (preferably uncleaned)
  and outputs interactive .html maps that can be opened in your browser
  to a specific directory. The maps can highlight if an occurrence has
  passed all filtering (.summary == TRUE) or failed at least one filter
  (.summary == FALSE). This can be modified by first running
  summaryFun() to set the columns that you want to be highlighted. It
  can also highlight occurrences flagged as expert-identified or country
  outliers.
- `dataProvTables()` This function will attempt to find and build a
  table of data providers that have contributed to the input data,
  especially using the ‘institutionCode’ column. It will also look for a
  variety of other columns to find data providers using a an internally
  set sequence of if-else statements. Hence, this function is quite
  specific for bee data, but should work for other taxa in similar
  institutions.
- `flagSummaryTable()` Takes a flagged dataset and returns the total
  number of fails (FALSE) per flag (columns starting with “.”) and per
  species. Users may define the column to group the summary by. While it
  is intended to work with the scientificName column, users may select
  any grouping column (e.g., country).

#### **Installation**

You can install *BeeBDC* from GitHub

``` r
  # Some dependencies
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("ropensci/rnaturalearthhires")
BiocManager::install("ComplexHeatmap")

remotes::install_github("https://github.com/jbdorey/BeeBDC.git", ref = "main", 
                        auth_token = "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")
```

Load the package with:

``` r
library(BeeBDC)
```

#### **Package website**

See *BeeBDC* package website
(<https://jbdorey.github.io/BeeBDC/reference/index.html>) for detailed
explanation on each module.

#### **Getting help**

> If you encounter a clear bug, please file an issue
> [**here**](https://github.com/jbdorey/BeeBDC/issues). For questions or
> suggestion, please send us a email (jbdorey@icloud.com).

#### **Citation**

Paper, dataset, and package citation: Dorey, J. B., Chesshire, P. R.,
Bolaños, A. N., O’reilly, R. L., Bossert, S., Collins, S. M.,
Lichtenberg, E. M., Tucker, E., Smith-Pardo, A., Falcon-Brindis, A.,
Guevara, D. A., Ribeiro, B. R., De Pedro, D., Fischer, E., Hung, J.
K.-L., Parys, K. A., Rogan, M. S., Minckley, R. L., Velzco, S. J. E.,
Griswold, T., Zarrillo, T. A., Sica, Y., Orr, M. C., Guzman, L. M.,
Ascher, J., Hughes, A. C. & Cobb, N. S. In review. BeeBDC: A new R
package and globally synthesised and flagged bee occurrence dataset.
Scientific Data.

Package citation: Dorey, J. B., O’Reilly, R. L., Bossert, S., Fischer,
E. (2023). BeeBDC: an occurrence data cleaning package. R package
version 0.2.8. url: <https://github.com/jbdorey/BeeBDC>
