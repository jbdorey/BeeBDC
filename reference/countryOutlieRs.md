# Flag country-level outliers with a provided checklist.

This function flags country-level outliers using the checklist provided
with this package. For additional context and column names, see
[`beesChecklist()`](https://jbdorey.github.io/BeeBDC/reference/beesChecklist.md).

## Usage

``` r
countryOutlieRs(
  checklist = NULL,
  data = NULL,
  keepAdjacentCountry = TRUE,
  pointBuffer = NULL,
  scale = 50,
  stepSize = 1e+06,
  mc.cores = 1
)
```

## Arguments

- checklist:

  A data frame or tibble. The formatted checklist which was built based
  on the Discover Life website.

- data:

  A data frame or tibble. The a Darwin Core occurrence dataset.

- keepAdjacentCountry:

  Logical. If TRUE, occurrences in countries that are adjacent to
  checklist countries will be kept. If FALSE, they will be flagged.

- pointBuffer:

  Numeric. A buffer around points to help them align with a country or
  coastline. This provides a good way to retain points that occur right
  along the coast or borders of the maps in rnaturalearth

- scale:

  Numeric. The value fed into the map scale parameter for
  [`rnaturalearth::ne_countries()`](https://docs.ropensci.org/rnaturalearth/reference/ne_countries.html)'s
  scale parameter: Scale of map to return, one of 110, 50, 10 or
  'small', 'medium', 'large', where smaller numbers are higher
  resolution. WARNING: This function is tested on 110 and 50.

- stepSize:

  Numeric. The number of occurrences to process in each chunk. Default =
  1000000.

- mc.cores:

  Numeric. If \> 1, the function will run in parallel using mclapply
  using the number of cores specified. If = 1 then it will be run using
  a serial loop. NOTE: Windows machines must use a value of 1 (see
  ?parallel::mclapply). Additionally, be aware that each thread can use
  large chunks of memory. If the cores throw issues, consider setting
  mc.cores to 1. Default = 1.

## Value

The input data with two new columns, .countryOutlier or .sea. There are
three possible values for the new column: TRUE == passed, FALSE ==
failed (not in country or in the ocean), NA == did not overlap with
rnaturalearth map.

## Examples

``` r
library(magrittr)
  # Load in the test dataset
beesRaw <- BeeBDC::beesRaw
  # For the sake of this example, use the testChecklist
system.file("extdata", "testChecklist.rda", package="BeeBDC") |> load()
  # For real examples, you might download the beesChecklist from FigShare using 
  #  [BeeBDC::beesChecklist()]

beesRaw_out <- countryOutlieRs(checklist = testChecklist,
                               data = beesRaw %>%
                               dplyr::filter(dplyr::row_number() %in% 1:50),
                               keepAdjacentCountry = TRUE,
                               pointBuffer = 1,
                               scale = 50,
                               stepSize = 1000000,
                               mc.cores = 1)
#> Error in countryOutlieRs(checklist = testChecklist, data = beesRaw %>%     dplyr::filter(dplyr::row_number() %in% 1:50), keepAdjacentCountry = TRUE,     pointBuffer = 1, scale = 50, stepSize = 1e+06, mc.cores = 1): could not find function "countryOutlieRs"
table(beesRaw_out$.countryOutlier, useNA = "always")
#> Error: object 'beesRaw_out' not found
```
