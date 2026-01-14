# Find fill-down errors

A simple function that looks for potential latitude and longitude
fill-down errors by identifying consecutive occurrences with coordinates
at regular intervals. This is accomplished by using a sliding window
with the length determined by minRepeats.

## Usage

``` r
diagonAlley(
  data = NULL,
  minRepeats = NULL,
  groupingColumns = c("eventDate", "recordedBy", "datasetName"),
  ndec = 3,
  stepSize = 1e+06,
  mc.cores = 1
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- minRepeats:

  Numeric. The minimum number of lat or lon repeats needed to flag a
  record

- groupingColumns:

  Character. The column(s) to group the analysis by and search for
  fill-down errors within. Default = c("eventDate", "recordedBy",
  "datasetName").

- ndec:

  Numeric. The number of decimal places below which records will not be
  considered in the diagonAlley function. This is fed into
  [`jbd_coordinates_precision()`](https://jbdorey.github.io/BeeBDC/reference/jbd_coordinates_precision.md).
  Default = 3.

- stepSize:

  Numeric. The number of occurrences to process in each chunk. Default =
  1000000.

- mc.cores:

  Numeric. If \> 1, the function will run in parallel using mclapply
  using the number of cores specified. If = 1 then it will be run using
  a serial loop. NOTE: Windows machines must use a value of 1 (see
  ?parallel::mclapply). Additionally, be aware that each thread can use
  large chunks of memory. Default = 1.

## Value

The function returns the input data with a new column, .sequential,
where FALSE = records that have consecutive latitudes or longitudes
greater than or equal to the user-defined threshold.

## Details

The sliding window (and hence fill-down errors) will only be examined
within the user-defined groupingColumns; if any of those columns are
empty, that record will be excluded.

## Examples

``` r
# Read in the example data
  data(beesRaw)
 # Run the function
  beesRaw_out <- diagonAlley(
    data = beesRaw,
    # The minimum number of repeats needed to find a sequence in for flagging
    minRepeats = 4,
    groupingColumns = c("eventDate", "recordedBy", "datasetName"),
    ndec = 3,
    stepSize = 1000000,
    mc.cores = 1)
#> Removing rounded coordinates with BeeBDC::jbd_coordinates_precision...
#> jbd_coordinates_precision:
#> Removed 32 records.
#> Warning: object 'runningData_LonGrp' not found
#>  - Merging results and adding the .sequential column...
#> 
#> diagonAlley:
#> Flagged 0 records
#> The .sequential column was added to the database.
#>  - Completed in 0.04 secs
  
```
