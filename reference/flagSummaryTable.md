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
#> Error in flagSummaryTable(data = beesFlagged, column = "scientificName",     outPath = paste0(tempdir()), fileName = "flagTable.csv"): could not find function "flagSummaryTable"
                              
```
