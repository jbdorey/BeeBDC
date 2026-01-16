# Flag continent-level outliers with a provided checklist.

This function flags continent-level outliers using the checklist
provided with this package. For additional context and column names, see
[`beesChecklist()`](https://jbdorey.github.io/BeeBDC/reference/beesChecklist.md).

## Usage

``` r
continentOutlieRs(
  checklist = NULL,
  data = NULL,
  keepAdjacentContinent = FALSE,
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

- keepAdjacentContinent:

  Logical. If TRUE, occurrences in continents that are adjacent to
  checklist continents will be kept. If FALSE, they will be flagged.
  Defualt = FALSE.

- pointBuffer:

  Numeric. A buffer around points to help them align with a continent or
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

The input data with two new columns, .continentOutlier or .sea. There
are three possible values for the new column: TRUE == passed, FALSE ==
failed (not in continent or in the ocean), NA == did not overlap with
rnaturalearth map.

## See also

[`countryOutlieRs()`](https://jbdorey.github.io/BeeBDC/reference/countryOutlieRs.md)
for implementation at the country level. Country-level implementation
will be more data-hungry and, where data do not yet exist, difficult to
implement. Additionally, see
[`beesChecklist()`](https://jbdorey.github.io/BeeBDC/reference/beesChecklist.md)
for input data. Note, not all columns are necessary if you are building
your own dataset. At a minimum you will need *validName* and
*continent*.

## Examples

``` r
if(requireNamespace("rnaturalearthdata")){
library(magrittr)
  # Load in the test dataset
beesRaw <- BeeBDC::beesRaw
  # For the sake of this example, use the testChecklist
system.file("extdata", "testChecklist.rda", package="BeeBDC") |> load()
  # For real examples, you might download the beesChecklist from FigShare using 
  #  [BeeBDC::beesChecklist()]

beesRaw_out <- continentOutlieRs(checklist = testChecklist,
                               data = beesRaw %>%
                               dplyr::filter(dplyr::row_number() %in% 1:50),
                               keepAdjacentContinent = FALSE,
                               pointBuffer = 1,
                               scale = 50,
                               stepSize = 1000000,
                               mc.cores = 1)
table(beesRaw_out$.continentOutlier, useNA = "always")
} # END if require
#> Loading required namespace: rnaturalearthdata
#> Spherical geometry (s2) switched off
#>  - Extracting continent data from points...
#>  - Buffering failed points by pointBuffer...
#>  - Prepare the neighbouring continent dataset...
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
#>  - Compare points with the checklist...
#>  - Combining data...
#>  - Sorting and removing potentially duplicated buffered points...
#>  - Finished. 
#> We have matched 26 records to their exact continent and 0 to an adjacent continent
#> We failed to match 1 occurrences to any 'exact' or 'neighbouring' continent
#> There are 23 'NA' occurrences for the .continentOutlier column.
#> 
#> continentOutlieRs:
#> Flagged 1 for continent outlier and flagged 0 for in the .sea records.
#> Three columns were added to the database:
#> 1. The '.continentOutlier' column was added which is a filtering column. 
#> 2. The 'continentMatch' columns indicates exact, neighbour, or noMatch. 
#> 3. The '.sea' column was added as a filtering column for points in the ocean. The '.sea' column includes the user input buffer in its calculation.
#>  - Completed in 2.33 secs
#> 
#> FALSE  TRUE  <NA> 
#>     1    26    23 
```
