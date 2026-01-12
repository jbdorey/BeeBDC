# Attempt to match database_ids from a prior run

This function attempts to match database_ids from a prior bdc or BeeBDC
run in order to keep this column somewhat consistent between iterations.
However, not all records contain sufficient information for this to work
flawlessly.

## Usage

``` r
idMatchR(
  currentData = NULL,
  priorData = NULL,
  matchBy = NULL,
  completeness_cols = NULL,
  excludeDataset = NULL
)
```

## Arguments

- currentData:

  A data frame or tibble. The NEW occurrence records as input.

- priorData:

  A data frame or tibble. The PRIOR occurrence records as input.

- matchBy:

  A list of character vectors Should contain the columns to iteratively
  compare.

- completeness_cols:

  A character vector. The columns to check for completeness, arrange,
  and assign the relevant prior database_id.

- excludeDataset:

  A character vector. The dataSources that are to be excluded from data
  matching. These should be static dataSources from minor providers.

## Value

The input data frame returned with an updated database_id column that
shows the database_ids as in priorData where they could be matched.
Additionally, a columnd called idContinuity is returned where TRUE
indicates a match to a prior database_id and FALSE indicates that a new
database_id was assigned.

## Examples

``` r
# Get the example data
data("beesRaw", package = "BeeBDC")
# Which datasets are static and should be excluded from matching?
excludeDataset <- c("BMin", "BMont", "CAES", "EaCO", "Ecd", "EcoS",
                    "Gai", "KP", "EPEL", "USGS", "FSCA", "SMC", "Bal", "Lic", "Arm", "BBD", 
                    "MEPB")
  # Match the data to itself just as an example of running the code.
beesRaw_out <- idMatchR(
  currentData = beesRaw,
  priorData = beesRaw,
  # First matches will be given preference over later ones
  matchBy = dplyr::lst(c("gbifID"),
                        c("catalogNumber", "institutionCode", "dataSource"),
                        c("occurrenceID", "dataSource"),
                        c("recordId", "dataSource"),
                        c("id"),
                        c("catalogNumber", "institutionCode")),
  # You can exclude datasets from prior by matching their prefixs - before first underscore:
  excludeDataset = excludeDataset)
#> Warning message: 
#>  - No completeness_cols provided. Using default of: c('decimalLatitude',  'decimalLongitude', 'scientificName', and 'eventDate')
#>  - Generating a basic completeness summary from the decimalLatitude, decimalLongitude, scientificName, eventDate columns.
#> This summary is simply the sum of complete.cases in each column. It ranges from zero to the N of columns. This will be used to sort duplicate rows and select the most-complete rows.
#>  - Starting core loop...
#>  - we matched 47 records using gbifID.
#> This leaves 51 unmatched data in the priorData file
#>  - we matched 45 records using catalogNumber, institutionCode, dataSource.
#> This leaves 6 unmatched data in the priorData file
#>  - we matched 4 records using occurrenceID, dataSource.
#> This leaves 2 unmatched data in the priorData file
#>  - we matched 0 records using recordId, dataSource.
#> This leaves 2 unmatched data in the priorData file
#>  - we matched 1 records using id.
#> This leaves 1 unmatched data in the priorData file
#>  - we matched 0 records using catalogNumber, institutionCode.
#> This leaves 1 unmatched data in the priorData file
#>  - Combining ids and assigning new ones where needed...
#>  - We matched a total of 97 database_id numbers. We then assigned new database_id numbers to 1 unmatched occurrences.
```
