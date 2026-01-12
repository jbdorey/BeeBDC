# Sets up column names and types

This function uses
[`readr::cols_only()`](https://readr.tidyverse.org/reference/cols.html)
to assign a column name and the type of data (e.g.,
[`readr::col_character()`](https://readr.tidyverse.org/reference/parse_atomic.html),
and
[`readr::col_integer()`](https://readr.tidyverse.org/reference/parse_atomic.html)).
To see the default columns simply run `ColTypeR()`. This is intended for
use with
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).
Columns that are not present will NOT be included in the resulting
tibble unless they are specified using
[...](https://rdrr.io/r/base/dots.html).

## Usage

``` r
ColTypeR(..., standardFormat = NULL)
```

## Arguments

- ...:

  Additional arguments. These can be specified in addition to the ones
  default to the function. For example:

  - newCharacterColumn =
    [`readr::col_character()`](https://readr.tidyverse.org/reference/parse_atomic.html),

  - newNumericColumn =
    [`readr::col_integer()`](https://readr.tidyverse.org/reference/parse_atomic.html),

  - newLogicalColumn =
    [`readr::col_logical()`](https://readr.tidyverse.org/reference/parse_atomic.html)

- standardFormat:

  Character. Some taxa may have a standard format for data. Presently,
  Only bees have had this encoded here as "bee". Default = NULL.

## Value

Returns an object of class col_spec. See
[`readr::as.col_spec()`](https://readr.tidyverse.org/reference/as.col_spec.html)
for additional context and explication.

## References

For using the bee standard — Clos, B. D., Seltmann, K. C., Turley, N.
E., Maffei, C., Tucker, E. M., Lane, I. G., Levenson, H. K., & Woodard,
H. S. (2025). Improving the standardization of wild bee occurrence data:
towards a formal wild bee data standard. Authorea. doi:
https://doi.org/10.22541/au.173862402.22787949/v2

## Examples

``` r
  # You can simply return the below for default values
  library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
BeeBDC::ColTypeR() 
#> Error: 'ColTypeR' is not an exported object from 'namespace:BeeBDC'

  # To add new columns you can write
ColTypeR(newCharacterColumn = readr::col_character(), 
         newNumericColumn = readr::col_integer(), 
         newLogicalColumn = readr::col_logical()) 
#> Error in ColTypeR(newCharacterColumn = readr::col_character(), newNumericColumn = readr::col_integer(),     newLogicalColumn = readr::col_logical()): could not find function "ColTypeR"

# Try reading in one of the test datasets as an example:
beesFlagged %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR())
#> # A tibble: 100 × 124
#>    database_id scientificName family subfamily genus subgenus subspecies species
#>    <chr>       <chr>          <chr>  <chr>     <chr> <chr>    <lgl>      <chr>  
#>  1 Dorey_data… Pseudoanthidi… Megac… Megachil… Pseu… NA       NA         Pseudo…
#>  2 Dorey_data… Macrotera arc… Andre… Panurgin… Macr… NA       NA         Macrot…
#>  3 Dorey_data… Xanthesma fur… Colle… Euryglos… Xant… NA       NA         Xanthe…
#>  4 Dorey_data… Exomalopsis s… Apidae Apinae    Exom… NA       NA         Exomal…
#>  5 Dorey_data… Osmia bicolor… Megac… Megachil… Osmia NA       NA         Osmia …
#>  6 Paige_data… Augochlorella… Halic… Halictin… Augo… NA       NA         Augoch…
#>  7 Dorey_data… Megachile api… Megac… Megachil… Mega… NA       NA         Megach…
#>  8 Dorey_data… Trigona dalla… Apidae Apinae    Trig… NA       NA         Trigon…
#>  9 Dorey_data… Habropoda mis… Apidae Apinae    Habr… NA       NA         Habrop…
#> 10 Dorey_data… Lasioglossum … Halic… Halictin… Lasi… NA       NA         Lasiog…
#> # ℹ 90 more rows
#> # ℹ 116 more variables: specificEpithet <chr>, infraspecificEpithet <chr>,
#> #   acceptedNameUsage <lgl>, taxonRank <chr>, scientificNameAuthorship <chr>,
#> #   identificationQualifier <lgl>, higherClassification <chr>,
#> #   identificationReferences <lgl>, typeStatus <chr>,
#> #   previousIdentifications <chr>, verbatimIdentification <chr>,
#> #   identifiedBy <chr>, dateIdentified <chr>, decimalLatitude <dbl>, …
  # OR
beesRaw %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR())
#> # A tibble: 100 × 90
#>    database_id scientificName family subfamily genus subgenus subspecies species
#>    <chr>       <chr>          <chr>  <chr>     <chr> <chr>    <lgl>      <chr>  
#>  1 Dorey_data… Pseudoanthidi… Megac… Megachil… Pseu… NA       NA         Pseudo…
#>  2 Dorey_data… Macrotera arc… Andre… Panurgin… Macr… NA       NA         Macrot…
#>  3 Dorey_data… Xanthesma fur… Colle… Euryglos… Xant… NA       NA         Xanthe…
#>  4 Dorey_data… Exomalopsis s… Apidae Apinae    Exom… NA       NA         Exomal…
#>  5 Dorey_data… Osmia bicolor… Megac… Megachil… Osmia NA       NA         Osmia …
#>  6 Paige_data… Augochlorella… Halic… Halictin… Augo… NA       NA         Augoch…
#>  7 Dorey_data… Megachile api… Megac… Megachil… Mega… NA       NA         Megach…
#>  8 Dorey_data… Trigona dalla… Apidae Apinae    Trig… NA       NA         Trigon…
#>  9 Dorey_data… Habropoda mis… Apidae Apinae    Habr… NA       NA         Habrop…
#> 10 Dorey_data… Lasioglossum … Halic… Halictin… Lasi… NA       NA         Lasiog…
#> # ℹ 90 more rows
#> # ℹ 82 more variables: specificEpithet <chr>, infraspecificEpithet <chr>,
#> #   acceptedNameUsage <lgl>, taxonRank <chr>, scientificNameAuthorship <chr>,
#> #   identificationQualifier <lgl>, higherClassification <chr>,
#> #   identificationReferences <lgl>, typeStatus <chr>,
#> #   previousIdentifications <chr>, verbatimIdentification <chr>,
#> #   identifiedBy <chr>, dateIdentified <chr>, decimalLatitude <dbl>, …

  # OR, using the bee standard format from:
  
beesRaw %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR(standardFormat = "bee"))
#> # A tibble: 100 × 90
#>    database_id scientificName family subfamily genus subgenus subspecies species
#>    <chr>       <chr>          <chr>  <chr>     <chr> <chr>    <lgl>      <chr>  
#>  1 Dorey_data… Pseudoanthidi… Megac… Megachil… Pseu… NA       NA         Pseudo…
#>  2 Dorey_data… Macrotera arc… Andre… Panurgin… Macr… NA       NA         Macrot…
#>  3 Dorey_data… Xanthesma fur… Colle… Euryglos… Xant… NA       NA         Xanthe…
#>  4 Dorey_data… Exomalopsis s… Apidae Apinae    Exom… NA       NA         Exomal…
#>  5 Dorey_data… Osmia bicolor… Megac… Megachil… Osmia NA       NA         Osmia …
#>  6 Paige_data… Augochlorella… Halic… Halictin… Augo… NA       NA         Augoch…
#>  7 Dorey_data… Megachile api… Megac… Megachil… Mega… NA       NA         Megach…
#>  8 Dorey_data… Trigona dalla… Apidae Apinae    Trig… NA       NA         Trigon…
#>  9 Dorey_data… Habropoda mis… Apidae Apinae    Habr… NA       NA         Habrop…
#> 10 Dorey_data… Lasioglossum … Halic… Halictin… Lasi… NA       NA         Lasiog…
#> # ℹ 90 more rows
#> # ℹ 82 more variables: specificEpithet <chr>, infraspecificEpithet <chr>,
#> #   acceptedNameUsage <lgl>, taxonRank <chr>, scientificNameAuthorship <chr>,
#> #   identificationQualifier <lgl>, higherClassification <chr>,
#> #   identificationReferences <lgl>, typeStatus <chr>,
#> #   previousIdentifications <chr>, verbatimIdentification <chr>,
#> #   identifiedBy <chr>, dateIdentified <chr>, decimalLatitude <dbl>, …

```
