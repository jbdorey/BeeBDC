# Build a table of data providers for bee occurrence records

This function will attempt to find and build a table of data providers
that have contributed to the input data, especially using the
'institutionCode' column. It will also look for a variety of other
columns to find data providers using an internally set sequence of
if-else statements. Hence, this function is quite specific for bee data,
but should work for other taxa in similar institutions.

## Usage

``` r
dataProvTables(
  data = NULL,
  runBeeDataChecks = FALSE,
  outPath = OutPath_Report,
  fileName = NULL
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- runBeeDataChecks:

  Logical. If TRUE, will search in other columns for specific clues to
  determine the institution.

- outPath:

  A character path. The path to the directory in which the figure will
  be saved. Default = OutPath_Report.

- fileName:

  Character. The name of the file to be saved, ending in ".csv".

## Value

Returns a table with the data providers, an specimen count, and a
species count.

## Examples

``` r
data(beesFlagged)

testOut <- dataProvTables(
data = beesFlagged,
runBeeDataChecks = TRUE,
outPath = tempdir(),
fileName = "testFile.csv")
#> Error in dataProvTables(data = beesFlagged, runBeeDataChecks = TRUE, outPath = tempdir(),     fileName = "testFile.csv"): could not find function "dataProvTables"
```
