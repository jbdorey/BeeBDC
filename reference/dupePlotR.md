# Create a compound bar graph of duplicate data sources

Creates a plot with two bar graphs. One shows the absolute number of
duplicate records for each data source while the other shows the
proportion of records that are duplicated within each data source. This
function requires a dataset that has been run through
[`dupeSummary()`](https://jbdorey.github.io/BeeBDC/reference/dupeSummary.md).

## Usage

``` r
dupePlotR(
  data = NULL,
  outPath = NULL,
  fileName = NULL,
  legend.position.inside = c(0.85, 0.8),
  base_height = 7,
  base_width = 7,
  ...,
  dupeColours = c("#F2D2A2", "#B9D6BC", "#349B90"),
  returnPlot = FALSE
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- outPath:

  Character. The path to a directory (folder) in which the output should
  be saved.

- fileName:

  Character. The name of the output file, ending in '.pdf'.

- legend.position.inside:

  The position of the legend as coordinates. Default = c(0.85, 0.8).

- base_height:

  Numeric. The height of the plot in inches. Default = 7.

- base_width:

  Numeric. The width of the plot in inches. Default = 7.

- ...:

  Other arguments to be used to change factor levels of data sources.

- dupeColours:

  A vector of colours for the levels duplicate, kept duplicate, and
  unique. Default = c("#F2D2A2","#B9D6BC", "#349B90").

- returnPlot:

  Logical. If TRUE, return the plot to the environment. Default = FALSE.

## Value

Outputs a .pdf figure.

## Examples

``` r
# This example will show a warning for the factor levels taht are not present in the specific 
# test dataset
dupePlotR(
  data = beesFlagged,
  # The outPath to save the plot as
    # Should be something like: #paste0(OutPath_Figures, "/duplicatePlot_TEST.pdf"),
  outPath = tempdir(), 
  fileName = "duplicatePlot_TEST.pdf",
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position.inside = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minckley' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL", Lic = "Lic", Bal = "Bal", Arm = "Arm"
  )
#> Error in dupePlotR(data = beesFlagged, outPath = tempdir(), fileName = "duplicatePlot_TEST.pdf",     dupeColours = c("#F2D2A2", "#B9D6BC", "#349B90"), base_height = 7,     base_width = 7, legend.position.inside = c(0.85, 0.8), GBIF = "GBIF",     SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA",     ASP = "ASP", CAES = "CAES", `B. Mont.` = "BMont", `B. Minckley` = "BMin",     Ecd = "Ecd", Gaiarsa = "Gai", EPEL = "EPEL", Lic = "Lic",     Bal = "Bal", Arm = "Arm"): could not find function "dupePlotR"
```
