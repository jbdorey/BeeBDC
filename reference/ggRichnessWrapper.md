# ggplot2 extension for a Chao- and iNEXT-wrapper outputs

`BeeBDC::ggRichnessWrapper()` takes data output from
[`BeeBDC::iNEXTwrapper()`](https://jbdorey.github.io/BeeBDC/reference/iNEXTwrapper.md)
and
[`BeeBDC::ChaoWrapper()`](https://jbdorey.github.io/BeeBDC/reference/ChaoWrapper.md)
to produce plots for multiple groups in one go. The plots can be
multiple per page and across multiple pages.

## Usage

``` r
ggRichnessWrapper(
  iNEXT_in = country_iNEXT,
  iChao_in = NULL,
  filterOut = NULL,
  type = 1,
  se = TRUE,
  facet.var = "None",
  color.var = "Order.q",
  grey = FALSE,
  legendPerPlot = FALSE,
  show_iNEXT = TRUE,
  showPercent = TRUE,
  ChaoColour = "#55AD9B",
  iNEXTcolour = "#FD9B63",
  Chao_estimate = "iChao1 (Chiu et al. 2014)",
  nrow = 3,
  ncol = 4,
  labels = NULL,
  fileName = "richnessPlots.pdf",
  outPath = tempdir(),
  base_width = 8.3,
  base_height = 11.7,
  dpi = 300,
  ...
)
```

## Arguments

- iNEXT_in:

  a list of `iNEXT` objects computed by
  [`BeeBDC::iNEXTwrapper()`](https://jbdorey.github.io/BeeBDC/reference/iNEXTwrapper.md).

- iChao_in:

  a list of R data created
  [`BeeBDC::ChaoWrapper()`](https://jbdorey.github.io/BeeBDC/reference/ChaoWrapper.md).

- filterOut:

  Character. A list of sites/countries to exclude from plotting; for
  example, because sample size was inadequate. Default = NULL.

- type:

  three types of plots: sample-size-based rarefaction/extrapolation
  curve (`type = 1`); sample completeness curve (`type = 2`);
  coverage-based rarefaction/extrapolation curve (`type = 3`). From
  [`iNEXT::ggiNEXT()`](https://rdrr.io/pkg/iNEXT/man/ggiNEXT.html)

- se:

  a logical variable to display confidence interval around the estimated
  sampling curve. From
  [`iNEXT::ggiNEXT()`](https://rdrr.io/pkg/iNEXT/man/ggiNEXT.html)

- facet.var:

  create a separate plot for each value of a specified variable: no
  separation  
  (`facet.var="None"`); a separate plot for each diversity order
  (`facet.var="Order.q"`); a separate plot for each assemblage
  (`facet.var="Assemblage"`); a separate plot for each combination of
  order x assemblage (`facet.var="Both"`). From
  [`iNEXT::ggiNEXT()`](https://rdrr.io/pkg/iNEXT/man/ggiNEXT.html)

- color.var:

  create curves in different colors for values of a specified variable:
  all curves are in the same color (`color.var="None"`); use different
  colors for diversity orders (`color.var="Order.q"`); use different
  colors for sites (`color.var="Assemblage"`); use different colors for
  combinations of order x assemblage (`color.var="Both"`). From
  [`iNEXT::ggiNEXT()`](https://rdrr.io/pkg/iNEXT/man/ggiNEXT.html)

- grey:

  a logical variable to display grey and white ggplot2 theme. From
  [`iNEXT::ggiNEXT()`](https://rdrr.io/pkg/iNEXT/man/ggiNEXT.html)

- legendPerPlot:

  Logical. If TRUE, remove the legend from each plot. Default = FALSE

- show_iNEXT:

  Logical. If TRUE, show the estimate and 95% CIs if specified for
  iNEXT. Default = TRUE.

- showPercent:

  Logical. If TRUE, show the prrcentage increases. Default = TRUE.

- ChaoColour:

  Character. The to be used to graph Chao estimates (95% confidence
  intervals are shown with reduced opacity). Default = "#55AD9B".

- iNEXTcolour:

  Character. The to be used to graph iNEXT estimates (95% confidence
  intervals are shown with reduced opacity). Default = "#FD9B63".

- Chao_estimate:

  Character. The name of the Chao estimate to use from those calculated
  in
  [`SpadeR::ChaoSpecies()`](https://rdrr.io/pkg/SpadeR/man/ChaoSpecies.html).
  The options are "Homogeneous Model","Homogeneous (MLE)", "Chao1 (Chao,
  1984)","Chao1-bc","iChao1 (Chiu et al. 2014)","ACE (Chao & Lee,
  1992)", "ACE-1 (Chao & Lee, 1992)","1st order jackknife","2nd order
  jackknife". Default = "iChao1 (Chiu et al. 2014)".

- nrow:

  Numeric. The number of rows per figure. Figures (that don't fit in the
  nrow\*ncol grid) will be saved into additional files. Default = 3.

- ncol:

  Numeric. The number of columns per figure. Figures (that don't fit in
  the nrow\*ncol grid) will be saved into additional files. Default = 4.

- labels:

  Character. The labels for each sub-plot (a, b, c, ...). The default is
  NULL, which will provide labels a-z as required.

- fileName:

  Character. Prefix to the output files. Default = "richnessPlots.pdf".
  Changing the suffix is a convenient way to alter the file format.

- outPath:

  Character. The fodler in which to save the plots. Default =
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)

- base_width:

  Numeric. The width, in inches, to save the plot. Default = 8.3.

- base_height:

  Numeric. The height, in inches, to save the plot. Default = 11.7.

- dpi:

  Numeric. Plot resolution. Also accepts a string input: "retina" (320),
  "print" (300), or "screen" (72). Applies only to raster output types.
  Default = 300.

- ...:

  other arguments passed on to methods. Not currently used. From
  [`iNEXT::ggiNEXT()`](https://rdrr.io/pkg/iNEXT/man/ggiNEXT.html)

## Value

Saves pdf objects and returns a summary table for all levels

## Examples

``` r
if (FALSE) { # \dontrun{

data(beesCountrySubset)

   # Transform data for iNEXT
 data_nextWrapper <- beesCountrySubset %>%
   dplyr::group_by(scientificName, country_suggested) %>%
   dplyr::count() 
   
 # Calculate iNEXT with the wrapper function
 output_iNEXTwrapper <- BeeBDC::iNEXTwrapper(data = data_nextWrapper,
                                             variableColumn = "country_suggested",
                                             valueColumn = "n",
                                             mc.cores = 1)

 # Transform data for iChao
data_iChao <- beesCountrySubset %>%
  dplyr::group_by(scientificName, country_suggested) %>%
  dplyr::count() %>%
  dplyr::select(scientificName, country_suggested, n) %>%
  tidyr::pivot_wider(names_from = country_suggested,
                     values_from = n,
                     values_fill = 0) %>%
  ## Create the rownames
  tibble::column_to_rownames("scientificName") %>%
  dplyr::tibble()
  
  # Run the wrapper function
output_iChaowrapper <- BeeBDC::ChaoWrapper(data = data_iChao,
                                             datatype = "abundance",
                                             k = 10,
                                             conf = 0.95,
                                             mc.cores = 1)
                                             
   # Make the plots! 
plot_summary <- BeeBDC::ggRichnessWrapper(
iNEXT_in = output_iNEXTwrapper,
iChao_in = output_iChaowrapper,
nrow = 2,
ncol = 2,
labels = NULL,
fileName = "speciesRichnessPlots.pdf",
outPath = tempdir(),
base_width = 8.3,
base_height = 11.7, 
dpi = 300)
      
} # }                                       
```
