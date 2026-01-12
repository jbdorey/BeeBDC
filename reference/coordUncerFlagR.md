# Flag occurrences with an uncertainty threshold

To use this function, the user must choose a column, probably
"coordinateUncertaintyInMeters" and a threshold above which occurrences
will be flagged for geographic uncertainty.

## Usage

``` r
coordUncerFlagR(
  data = NULL,
  uncerColumn = "coordinateUncertaintyInMeters",
  threshold = NULL
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- uncerColumn:

  Character. The column to flag uncertainty in.

- threshold:

  Numeric. The uncertainty threshold. Values equal to, or greater than,
  this threshold will be flagged.

## Value

The input data with a new column, .uncertaintyThreshold.

## Examples

``` r
# Run the function
beesRaw_out <- coordUncerFlagR(data = beesRaw,
                               uncerColumn = "coordinateUncertaintyInMeters",
                               threshold = 1000)
#> \coordUncerFlagR:
#>  Flagged 15 geographically uncertain records:
#>  The column '.uncertaintyThreshold' was added to the database.
# View the output
table(beesRaw_out$.uncertaintyThreshold, useNA = "always")
#> 
#> FALSE  TRUE  <NA> 
#>    15    23    62 
```
