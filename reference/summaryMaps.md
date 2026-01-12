# Create country-level summary maps of species and occurrence numbers

Builds an output figure that shows the number of species and the number
of occurrences per country. Breaks the data into classes for
visualisation. Users may filter data to their taxa of interest to
produce figures of interest.

## Usage

``` r
summaryMaps(
  data = NULL,
  class_n = 15,
  class_Style = "fisher",
  outPath = NULL,
  fileName = NULL,
  width = 10,
  height = 5,
  dpi = 300,
  returnPlot = FALSE,
  scale = 110,
  pointBuffer = 0.01
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- class_n:

  Numeric. The number of categories to break the data into.

- class_Style:

  Character. The class style passed to
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html).
  Options are chosen style: one of "fixed", "sd", "equal", "pretty",
  "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih",
  "headtails", or "maximum". Default = "fisher"

- outPath:

  A character vector the path to the save location for the output
  figure.

- fileName:

  A character vector with file name for the output figure, ending with
  '.pdf'.

- width:

  Numeric. The width, in inches, of the resulting figure. Default = 10.

- height:

  Numeric. The height, in inches, of the resulting figure. Default = 5.

- dpi:

  Numeric. The resolution of the resulting plot. Default = 300.

- returnPlot:

  Logical. If TRUE, return the plot to the environment. Default = FALSE.

- scale:

  Numeric or character. Passed to rnaturalearth's ne_countries(). Scale
  of map to return, one of 110, 50, 10 or 'small', 'medium', 'large'.
  Default = 110.

- pointBuffer:

  Numeric. Amount to buffer points, in decimal degrees. If the point is
  outside of a country, but within this point buffer, it will count
  towards that country. It's a good idea to keep this value consistent
  with the prior flags applied. Default = 0.01.

## Value

Saves a figure to the user-specified outpath and name with a global map
of bee occurrence species and count data from the input dataset.

## Examples

``` r
# Read in data
data(beesFlagged)
OutPath_Figures <- tempdir()
# This simple example using the test data has very few classes due to the small amount of input 
# data.
summaryMaps(
data = beesFlagged,
width = 10, height = 10,
class_n = 4,
class_Style = "fisher",
outPath = OutPath_Figures,
fileName = paste0("CountryMaps_fisher_TEST.pdf"),
)
#> Spherical geometry (s2) switched off
#>  - Extracting country data from points...
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
#> Extraction complete.
#>  - Buffering naturalearth map by pointBuffer...
#> dist is assumed to be in decimal degrees (arc_degrees).
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar

```
