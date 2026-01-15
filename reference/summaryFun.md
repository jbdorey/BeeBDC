# Create or update the .summary flag column

Using all flag columns (column names starting with "."), this function
either creates or updates the .summary flag column which is FALSE when
ANY of the flag columns are FALSE. Columns can be excluded and removed
after creating the .summary column. Additionally, the occurrence dataset
can be filtered to only those where .summary = TRUE at the end of the
function.

## Usage

``` r
summaryFun(
  data = NULL,
  dontFilterThese = NULL,
  onlyFilterThese = NULL,
  removeFilterColumns = FALSE,
  filterClean = FALSE
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records to use as input.

- dontFilterThese:

  A character vector of flag columns to be ignored in the creation or
  updating of the .summary column. Cannot be specified with
  onlyFilterThese.

- onlyFilterThese:

  A character vector. The inverse of dontFilterThese, where columns
  identified here will be filtered and no others. Cannot be specified
  with dontFilterThese.

- removeFilterColumns:

  Logical. If TRUE all columns starting with "." will be removed in the
  output data. This only makes sense to use when filterClean = TRUE.
  Default = FALSE.

- filterClean:

  Logical. If TRUE, the data will be filtered to only those occurrence
  where .summary = TRUE (i.e., completely clean according to the used
  flag columns). Default = FALSE.

## Value

Returns a data frame or tibble of the input data but modified based on
the above parameters.

## Examples

``` r
# Read in example data
data(beesFlagged)

# To only update the .summary column
beesFlagged_out <- summaryFun(
    data = beesFlagged,
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms", ".unLicensed"),
    removeFilterColumns = FALSE,
    filterClean = FALSE)
#>  - We will NOT flag the following columns. However, they will remain in the data file.
#> .gridSummary, .lonFlag, .latFlag, .uncer_terms, .unLicensed
#>  - summaryFun:
#> Flagged 81 
#>   The .summary column was added to the database.
  # View output
table(beesFlagged_out$.summary, useNA = "always")
#> 
#> FALSE  TRUE  <NA> 
#>    81    19     0 

# Now filter to only the clean data and remove the flag columns
beesFlagged_out <- summaryFun(
  data = BeeBDC::beesFlagged,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms", ".unLicensed"),
  removeFilterColumns = TRUE,
  filterClean = TRUE)
#>  - We will NOT flag the following columns. However, they will remain in the data file.
#> .gridSummary, .lonFlag, .latFlag, .uncer_terms, .unLicensed
#>  - summaryFun:
#> Flagged 81 
#>   The .summary column was added to the database.
#>  - REMOVED all occurrences that were FALSE for the 'summary' column.
# View output
table(beesFlagged_out$.summary, useNA = "always")
#> Warning: Unknown or uninitialised column: `.summary`.
#> 
#> <NA> 
#>    0 


```
