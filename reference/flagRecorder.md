# Loads, appends, and saves occurrence flag data

This function is used to save the flag data for your occurrence data as
you run the BeeBDC script. It will read and append existing files, if
asked to. Your flags should also be saved in the occurrence file itself
automatically.

## Usage

``` r
flagRecorder(
  data = NULL,
  outPath = NULL,
  fileName = NULL,
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  append = NULL,
  printSummary = FALSE
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- outPath:

  A character path. Where the file should be saved.

- fileName:

  Character. The name of the file to be saved

- idColumns:

  A character vector. The names of the columns that are to be kept along
  with the flag columns. These columns should be useful for identifying
  unique records with flags. Default = c("database_id", "id",
  "catalogNumber", "occurrenceID", "dataSource").

- append:

  Logical. If TRUE, this will find and append an existing file generated
  by this function.

- printSummary:

  Logical. If TRUE, print a
  [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  of all filter columns - i.e. those which tidyselect::starts_with(".")

## Value

Saves a file with id and flag columns and returns this as an object.

## Examples

``` r
# Load the example data
data("beesFlagged")

  # Run the function
  OutPath_Report <- tempdir()
flagFile <- flagRecorder(
  data = beesFlagged,
  outPath = paste(OutPath_Report, sep =""),
  fileName = paste0("flagsRecorded_", Sys.Date(), ".csv"),
  # These are the columns that will be kept along with the flags
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  # TRUE if you want to find a file from a previous part of the script to append to
  append = FALSE)
#>  - .summary column detected. This will be over-written.
#>  - Data saved to /tmp/RtmpoBXGU2/flagsRecorded_2026-01-14.csv
#>  - Selected 34 columns. These include:
#> database_id, id, catalogNumber, occurrenceID, dataSource, .scientificName_empty, .coordinates_empty, .coordinates_outOfRange, .basisOfRecords_notStandard, .coordinates_country_inconsistent, .occurrenceAbsent, .unLicensed, .GBIFflags, .uncer_terms, .invalidName, .rou, .val, .equ, .zer, .cap, .cen, .gbf, .inst, .sequential, .lonFlag, .latFlag, .gridSummary, .uncertaintyThreshold, .countryOutlier, .sea, .summary, .eventDate_empty, .year_outOfRange, and .duplicates
```
