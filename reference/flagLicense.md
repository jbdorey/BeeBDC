# Flag license protected records

This function will search for strings that indicate a record is
restricted in its use and will flag the restricted records.

## Usage

``` r
flagLicense(data = NULL, strings_to_restrict = "all", excludeDataSource = NULL)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- strings_to_restrict:

  A character vector. Should contain the strings used to detect
  protected records. Default = c("All Rights Reserved", "All rights
  reserved", "All rights reserved.", "ND", "Not for public")

- excludeDataSource:

  Optional. A character vector. A vector of the data sources
  (dataSource) that will not be flagged as protected, even if they are.
  This is useful if you have a private dataset that should be listed as
  "All rights reserved" which you want to be ignored by this flag.

## Value

Returns the data with a new column, .unLicensed, where FALSE = records
that are protected by a license.

## Examples

``` r
  # Read in the example data
data("beesRaw")
  # Run the function
beesRaw_out <- flagLicense(data = beesRaw,
                        strings_to_restrict = "all",
                        # DON'T flag if in the following data# source(s)
                        excludeDataSource = NULL)
#> \.unLicensed:
#>  Flagged 0 records that may NOT be used.
#>  One column was added to the database.
```
