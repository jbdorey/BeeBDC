# Creates interactive html maps for species

Uses the occurrence data (preferably uncleaned) and outputs interactive
.html maps that can be opened in your browser to a specific directory.
The maps can highlight if an occurrence has passed all filtering
(.summary == TRUE) or failed at least one filter (.summary == FALSE).
This can be modified by first running
[`summaryFun()`](https://jbdorey.github.io/BeeBDC/reference/summaryFun.md)
to set the columns that you want to be highlighted. It can also
highlight occurrences flagged as expert-identified or country outliers.

## Usage

``` r
interactiveMapR(
  data = NULL,
  outPath = NULL,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  speciesColumn = "scientificName",
  speciesList = "ALL",
  countryList = NULL,
  jitterValue = NULL,
  onlySummary = TRUE,
  overWrite = TRUE,
  customColumn1 = NULL,
  customColumn2 = NULL,
  TrueAlwaysTop = FALSE,
  excludeSpecies = c("Apis mellifera Linnaeus, 1758"),
  pointColours = c("blue", "darkred", "#ff7f00", "black"),
  returnPlot = FALSE
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records to use as input.

- outPath:

  A directory as character. Directory where to save output maps.

- lon:

  Character. The name of the longitude column. Default =
  "decimalLongitude".

- lat:

  Character. The name of the latitude column. Default =
  "decimalLatitude".

- speciesColumn:

  Character. The name of the column containing species names (or another
  factor) to build individual maps from. Default = "scientificName".

- speciesList:

  A character vector. Should contain species names as they appear in the
  speciesColumn to make maps of. User can also specify "ALL" in order to
  make maps of all species present in the data. Hence, a user may first
  filter their data and then use "ALL". Default = "ALL".

- countryList:

  A character vector. Country names to map, or NULL for to map ALL
  countries.

- jitterValue:

  Numeric. The amount, in decimal degrees, to jitter the map points by -
  this is important for separating stacked points with the same
  coordinates.

- onlySummary:

  Logical. If TRUE, the function will not look to plot country or
  expert-identified outliers in different colours.

- overWrite:

  Logical. If TRUE, the function will overwrite existing files in the
  provided directory that have the same name. Default = TRUE.

- customColumn1:

  Character. Allows the user to report on a column of their choosing in
  the output.

- customColumn2:

  Character. Allows the user to report on a column of their choosing in
  the output.

- TrueAlwaysTop:

  If TRUE, the quality (TRUE) points will always be displayed on top of
  other points. If FALSE, then whichever layer was turned on
  most-recently will be displayed on top.

- excludeSpecies:

  Character. A character vector of species names to exclude, especially
  if they are so large as to become a problem when making maps. For bee
  data, for example, "Apis mellifera Linnaeus, 1758" has too many data
  points and is default excluded.

- pointColours:

  A character vector of colours. In order provide colour for TRUE,
  FALSE, countryOutlier, and customOutlier. Default = c("blue",
  "darkred","#ff7f00", "black").

- returnPlot:

  Logical. If TRUE, return the plot to the environment. Default = FALSE.

## Value

Exports .html interactive maps of bee occurrences to the specified
directory.

## Examples

``` r
OutPath_Figures <- tempdir()

interactiveMapR(
# occurrence data - start with entire dataset, filter down to these species
data = BeeBDC::bees3sp, # %>%
  # Select only those species in the 100 randomly chosen
  # dplyr::filter(scientificName %in% beeData_interactive$scientificName),
  # Select only one species to map
  # dplyr::filter(scientificName %in% "Agapostemon sericeus (Forster, 1771)"),
# Directory where to save files
outPath = paste0(OutPath_Figures, "/interactiveMaps_TEST"),
# lat long columns
lon = "decimalLongitude",
lat = "decimalLatitude",
# Occurrence dataset column with species names
speciesColumn = "scientificName",
# Which species to map - a character vector of names or "ALL"
# Note: "ALL" is defined AFTER filtering for country
speciesList = "ALL",
# studyArea
countryList = NULL, 
# Point jitter to see stacked points - jitters an amount in decimal degrees
jitterValue = 0.01,
# If TRUE, it will only map the .summary column. Otherwise, it will map .summary
# which will be over-written by countryOutliers and manualOutliers
onlySummary = TRUE,
excludeSpecies = c("Apis mellifera Linnaeus, 1758"),
overWrite = TRUE,
  # Colours for points which are flagged as TRUE, FALSE, countryOutlier, and customOutlier
pointColours = c("blue", "darkred","#ff7f00", "black")
)
#> Error in interactiveMapR(data = BeeBDC::bees3sp, outPath = paste0(OutPath_Figures,     "/interactiveMaps_TEST"), lon = "decimalLongitude", lat = "decimalLatitude",     speciesColumn = "scientificName", speciesList = "ALL", countryList = NULL,     jitterValue = 0.01, onlySummary = TRUE, excludeSpecies = c("Apis mellifera Linnaeus, 1758"),     overWrite = TRUE, pointColours = c("blue", "darkred", "#ff7f00",         "black")): could not find function "interactiveMapR"
```
