# Flags coordinates for imprecision

This function flags occurrences where BOTH latitude and longitude values
are rounded. This contrasts with the original function,
bdc::bdc_coordinates_precision() that will flag occurrences where only
one of latitude OR longitude are rounded. The BeeBDC approach saves
occurrences that may have had terminal zeros rounded in one coordinate
column.

## Usage

``` r
jbd_coordinates_precision(
  data,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  ndec = NULL,
  quieter = FALSE
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- lat:

  Character. The name of the column to use as latitude. Default =
  "decimalLatitude".

- lon:

  Character. The name of the column to use as longitude. Default =
  "decimalLongitude".

- ndec:

  Numeric. The number of decimal places to flag in decimal degrees. For
  example, argument value of 2 would flag occurrences with nothing in
  the hundredths place (0.0x).

- quieter:

  Logical. If TRUE, the functino will run a little quieter. Default =
  FALSE.

## Value

Returns the input data frame with a new column, .rou, where FALSE
indicates occurrences that failed the test.

## Examples

``` r
beesRaw_out <- jbd_coordinates_precision(
  data = BeeBDC::beesRaw,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
    # number of decimals to be tested
  ndec = 2
)
#> jbd_coordinates_precision:
#> Flagged 30 records
#> The '.rou' column was added to the database.
table(beesRaw_out$.rou, useNA = "always")
#> 
#> FALSE  TRUE  <NA> 
#>    30    70     0 
```
