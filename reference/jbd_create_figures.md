# Create figures reporting the results of the bdc/BeeBDC packages

Creates figures (i.e., bar plots, maps, and histograms) reporting the
results of data quality tests implemented the bdc and BeeBDC packages.
Works like
[`bdc::bdc_create_figures()`](https://brunobrr.github.io/bdc/reference/bdc_create_figures.html),
but it allows the user to specify a save path.

## Usage

``` r
jbd_create_figures(
  data,
  path = OutPath_Figures,
  database_id = "database_id",
  workflow_step = NULL,
  save_figures = FALSE
)
```

## Arguments

- data:

  A data frame or tibble. Needs to contain the results of data quality
  tests; that is, columns starting with ".".

- path:

  A character directory. The path to a directory in which to save the
  figures. Default = OutPath_Figures.

- database_id:

  A character string. The column name with a unique record identifier.
  Default = "database_id".

- workflow_step:

  A character string. Name of the workflow step. Options available are
  "prefilter", "space", and "time".

- save_figures:

  Logical. Indicates if the figures should be saved for further
  inspection or use. Default = FALSE.

## Value

List containing figures showing the results of data quality tests
implemented in one module of bdc/BeeBDC. When save_figures = TRUE,
figures are also saved locally in a .png format.

## Details

This function creates figures based on the results of data quality
tests. A pre-defined list of test names is used for creating figures
depending on the name of the workflow step informed. Figures are saved
in "Output/Figures" if save_figures = TRUE.

## Examples

``` r
# \donttest{
database_id <- c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05")
lat <- c(-19.93580, -13.01667, -22.34161, -6.75000, -15.15806)
lon <- c(-40.60030, -39.60000, -49.61017, -35.63330, -39.52861)
.scientificName_emptys <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
.coordinates_empty <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
.invalid_basis_of_records <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
.summary <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

x <- data.frame(
  database_id,
  lat,
  lon,
  .scientificName_emptys,
  .coordinates_empty,
  .invalid_basis_of_records,
  .summary
)

figures <- 
jbd_create_figures(
  data = x, 
  database_id = "database_id",
  workflow_step = "prefilter",
  save_figures = FALSE
)
#> Error in jbd_create_figures(data = x, database_id = "database_id", workflow_step = "prefilter",     save_figures = FALSE): could not find function "jbd_create_figures"
# }
```
