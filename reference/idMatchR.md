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
#> Error in idMatchR(currentData = beesRaw, priorData = beesRaw, matchBy = dplyr::lst(c("gbifID"),     c("catalogNumber", "institutionCode", "dataSource"), c("occurrenceID",         "dataSource"), c("recordId", "dataSource"), c("id"),     c("catalogNumber", "institutionCode")), excludeDataset = excludeDataset): could not find function "idMatchR"
```
