# Flags records with GBIF issues

This function will flag records which are subject to a user-specified
vector of GBIF issues.

## Usage

``` r
GBIFissues(data = NULL, issueColumn = "issue", GBIFflags = NULL)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- issueColumn:

  Character. The column in which to look for GBIF issues. Default =
  "issue".

- GBIFflags:

  Character vector. The GBIF issues to flag. Users may choose their own
  vector of issues to flag or use a pre-set vector or vectors, including
  c("allDates", "allMetadata", "allObservations", "allSpatial",
  "allTaxo", or "all").

  Default = c("COORDINATE_INVALID", "PRESUMED_NEGATED_LONGITUDE",
  "PRESUMED_NEGATED_LATITUDE", "COUNTRY_COORDINATE_MISMATCH",
  "ZERO_COORDINATE")

## Value

Returns the data with a new column, ".GBIFflags", where FALSE = records
with any of the provided GBIFflags.

## Examples

``` r
# Import the example data
data(beesRaw)
# Run the function
beesRaw_Out <- GBIFissues(data = beesRaw, 
   issueColumn = "issue", 
   GBIFflags = c("COORDINATE_INVALID", "ZERO_COORDINATE")) 
#>  - jbd_GBIFissues:
#> Flagged 0 
#>   The .GBIFflags column was added to the database. 

```
