# Build a per-species summary for each and all flags

Takes a flagged dataset and returns the total number of fails (FALSE)
per flag (columns starting with ".") and per species. It will ignore the
.scientificName_empty and .invalidName columns as species are not
assigned. Users may define the column to group the summary by. While it
is intended to work with the scientificName column, users may select any
grouping column (e.g., country).

## Usage

``` r
flagSummaryTable(
  data = NULL,
  column = "scientificName",
  outPath = OutPath_Report,
  fileName = "flagTable.csv",
  percentImpacted = TRUE,
  percentThreshold = 0
)
```

## Arguments

- data:

  A data frame or tibble. The flagged dataset.

- column:

  Character. The name of the column to group by and summarise the failed
  occurrences. Default = "scientificName".

- outPath:

  A character path. The path to the directory in which the figure will
  be saved. Default = OutPath_Report. If is NULL then no file will be
  saved to the disk.

- fileName:

  Character. The name of the file to be saved, ending in ".csv". Default
  = "flagTable.csv".

- percentImpacted:

  Logical. If TRUE (the default), the program will write the percentage
  of species impacted and over the percentThreshold for each flagging
  column.

- percentThreshold:

  Numeric. A number between 0 and 100 to indicate the percent of
  individuals (\>; within each species) that is impacted by a flag, and
  to be included in the percentImpacted. Default = 0.

## Value

A tibble with a column for each flag column (starting with ".") showing
the number of failed (FALSE) occurrences per group. Also shows the (i)
total number of records, (ii) total number of failed records, and (iii)
the percentage of failed records.

## Examples

``` r
# Load the toy flagged bee data
data("beesFlagged")

  # Run the function and build the flag table
flagTibble <- flagSummaryTable(data = beesFlagged,
                              column = "scientificName",
                              outPath = paste0(tempdir()),
                              fileName = "flagTable.csv")
#>  - We will flag all columns starting with '.'
#>  - summaryFun:
#> Flagged 77 
#>   The .summary column was added to the database.
#> The percentages of species impacted by each flag in your analysis are as follows: 
#>   .coordinates_empty = 23.46%
#>   .coordinates_outOfRange = 0%
#>   .basisOfRecords_notStandard = 1.23%
#>   .coordinates_country_inconsistent = 1.23%
#>   .occurrenceAbsent = 8.64%
#>   .unLicensed = 0%
#>   .GBIFflags = 0%
#>   .uncer_terms = 0%
#>   .rou = 29.63%
#>   .val = 0%
#>   .equ = 0%
#>   .zer = 0%
#>   .cap = 0%
#>   .cen = 0%
#>   .gbf = 0%
#>   .inst = 0%
#>   .sequential = 0%
#>   .lonFlag = 0%
#>   .latFlag = 2.47%
#>   .gridSummary = 0%
#>   .uncertaintyThreshold = 12.35%
#>   .countryOutlier = 0%
#>   .sea = 1.23%
#>   .eventDate_empty = 13.58%
#>   .year_outOfRange = 13.58%
#>   .duplicates = 56.79%
                              
```
