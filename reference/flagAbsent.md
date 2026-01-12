# Flags occurrences that are marked as absent

Flags occurrences that are "ABSENT" for the occurrenceStatus (or some
other user-specified) column.

## Usage

``` r
flagAbsent(data = NULL, PresAbs = "occurrenceStatus")
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- PresAbs:

  Character. The column in which the function will find "ABSENT" or
  "PRESENT" records. Default = "occurrenceStatus"

## Value

The input data with a new column called ".occurrenceAbsent" where FALSE
== "ABSENT" records.

## Examples

``` r
  # Bring in the data
data(beesRaw)
  # Run the function
beesRaw_out <- flagAbsent(data = beesRaw,
PresAbs = "occurrenceStatus")
#> Error in flagAbsent(data = beesRaw, PresAbs = "occurrenceStatus"): could not find function "flagAbsent"
  # See the result
table(beesRaw_out$.occurrenceAbsent, useNA = "always")
#> Error: object 'beesRaw_out' not found
```
