# Species richness estimation

## [![BeeBDC logo of a cuckoo bee sweeping up occurrence records in South America](https://photos.smugmug.com/photos/i-MpLFKTT/0/741daa6d/X4/i-MpLFKTT-X4.png)](https://github.com/jbdorey/BeeBDC)

This is a basic workflow for feeding species occurrence data, a group’s
taxonomy, and a group’s country checklist into BeeBDC in order to
produce species richness estimates. Some of these functions are also
generic wrappers around `SpadeR` and `iNEXT` functions that can be used
with any abundance data. These functions have grown from an [original
publication](https://jbdorey.github.io/BeeBDC/articles/DOI).

## Script preparation

### Working directory

Choose the path to the root folder in which all other folders can be
found.

``` r
RootPath <- paste0("/your/path/here")
```

``` r
# Create the working directory in the RootPath if it doesn't exist already
if (!dir.exists(paste0(RootPath, "/Data_acquisition_workflow"))) {
    dir.create(paste0(RootPath, "/Data_acquisition_workflow"), recursive = TRUE)
}
# Set the working directory
setwd(paste0(RootPath, "/Data_acquisition_workflow"))
```

### Install packages (if needed)

You may need to install some packages for using this workflow. In
particular, `SpadeR` and `iNEXT` are required. They should prompt you to
download them the first time that you run the functions, however, let’s
install them here and now.

``` r
install.packages("SpadeR")
install.packages("iNEXT")
```

## Parallel estimations

We can start by looking at the relatively simple `iNEXT` and `SpadeR`
wrapper functions that can take the usual inputs for those functions in
their host packages. (See
[`iNEXT::iNEXT()`](https://rdrr.io/pkg/iNEXT/man/iNEXT.html) and
[`SpadeR::ChaoSpecies()`](https://rdrr.io/pkg/SpadeR/man/ChaoSpecies.html)
for more info.) These functions can take your data and run multiple
sites/countries/whatever level you want to throw at them and run them in
parallel. This greatly simplifies the code needed to run them and also
makes its implementation MUCH faster!

### iNEXTwrapper

[`BeeBDC::iNEXTwrapper()`](https://jbdorey.github.io/BeeBDC/reference/iNEXTwrapper.md)
parallelizes
[`iNEXT::iNEXT()`](https://rdrr.io/pkg/iNEXT/man/iNEXT.html), which
estimates species richness patterns by extrapolating and interpolating
Hill numbers. By and large this kinda equates to estimating richness by
rarefaction. We can start by reading in an example dataset with 1,488
*scientificName-country_suggested* combinations across four countries;
Fiji, Uganda, Vietnam, and Zambia.

``` r
beesCountrySubset <- BeeBDC::beesCountrySubset
```

Next, let’s look at how we would usually use iNEXT… By first
transforming our *(very simple example)* occurrence dataset into the
right format and then running the function.

``` r
# Transform the data
transformedAbundance <- beesCountrySubset %>%
    dplyr::group_by(scientificName, country_suggested) %>%
    dplyr::count() %>%
    dplyr::select(scientificName, country_suggested, n) %>%
    tidyr::pivot_wider(names_from = country_suggested, values_from = n, values_fill = 0) %>%
    # Create the rownames
tibble::column_to_rownames("scientificName") %>%
    dplyr::tibble() %>%
    as.data.frame()
# Run iNEXT
output_iNEXT <- iNEXT::iNEXT(transformedAbundance, datatype = "abundance")
```

We can also view the output of this function by running
`output_iNEXT$AsyEst`

| Assemblage | Diversity         |   Observed |  Estimator |       s.e. |        LCL |        UCL |
|:-----------|:------------------|-----------:|-----------:|-----------:|-----------:|-----------:|
| Fiji       | Species richness  |  17.000000 |  17.499483 |  1.4850098 |  17.000000 |  20.410049 |
| Fiji       | Shannon diversity |   4.712056 |   4.754101 |  0.2164397 |   4.329887 |   5.178315 |
| Fiji       | Simpson diversity |   2.653013 |   2.657561 |  0.1237356 |   2.415044 |   2.900078 |
| Uganda     | Species richness  |  44.000000 |  70.243359 | 14.1752423 |  44.000000 |  98.026324 |
| Uganda     | Shannon diversity |  19.651483 |  26.633143 |  3.4844763 |  19.803695 |  33.462591 |
| Uganda     | Simpson diversity |   7.699248 |   8.128000 |  1.5689856 |   5.052845 |  11.203155 |
| Vietnam    | Species richness  |   6.000000 |  13.794872 |  5.8924098 |   6.000000 |  25.343783 |
| Vietnam    | Shannon diversity |   1.953128 |   2.307552 |  0.5923478 |   1.146572 |   3.468532 |
| Vietnam    | Simpson diversity |   1.386509 |   1.400756 |  0.1986146 |   1.011479 |   1.790034 |
| Zambia     | Species richness  | 129.000000 | 186.853965 | 17.7824798 | 152.000945 | 221.706985 |
| Zambia     | Shannon diversity |  77.832700 | 104.647375 |  7.6001195 |  89.751415 | 119.543336 |
| Zambia     | Simpson diversity |  32.753790 |  35.991359 |  6.3494595 |  23.546647 |  48.436071 |

The implementation of
[`BeeBDC::iNEXTwrapper()`](https://jbdorey.github.io/BeeBDC/reference/iNEXTwrapper.md)
is also relatively simple; including the implementation of running in
parallel (**Note: Windows machines can’t use R’s parallel functions**).
We can again modify the input data to work with the function as below
and we could leave most of the inputs to iNEXTwrapper to the defaults or
change them as we saw fit. The key variable is to change `mc.cores` to
however many threads you’d liek to use on your computer!

``` r
# Transform data
data_nextWrapper <- beesCountrySubset %>%
    dplyr::group_by(scientificName, country_suggested) %>%
    dplyr::count()

# Calculate iNEXT with the wrapper function
output_iNEXTwrapper <- BeeBDC::iNEXTwrapper(data = data_nextWrapper, variableColumn = "country_suggested",
    valueColumn = "n", q = 0, datatype = "abundance", conf = 0.95, se = TRUE, nboot = 50,
    size = NULL, endpoint = NULL, knots = 40, mc.cores = 1)
```

    ## Error:
    ## ! 'iNEXTwrapper' is not an exported object from 'namespace:BeeBDC'

    ## Error:
    ## ! object 'output_iNEXTwrapper' not found

### ChaoWrapper

[`BeeBDC::ChaoWrapper()`](https://jbdorey.github.io/BeeBDC/reference/ChaoWrapper.md)
parallelizes
[`SpadeR::ChaoSpecies()`](https://rdrr.io/pkg/SpadeR/man/ChaoSpecies.html),
which estimates species richness using various non-parametric
estimators. The primary one that I tend to use is iChao. Let’s use the
same example dataset to first run the **SpadeR** function.

``` r
# Transform data
data_iChao <- beesCountrySubset %>%
    dplyr::group_by(scientificName, country_suggested) %>%
    dplyr::count() %>%
    dplyr::select(scientificName, country_suggested, n) %>%
    tidyr::pivot_wider(names_from = country_suggested, values_from = n, values_fill = 0) %>%
    # Create the rownames
tibble::column_to_rownames("scientificName") %>%
    dplyr::tibble()

# Run ChaoSpecies for the country Fiji
output_iChao <- SpadeR::ChaoSpecies(data_iChao$Fiji, datatype = "abundance")
```

Let’s view the output of this function by running
`output_iChao$Species_table`. Once again, you’ll be able to see that
there are actually quite a few species richness estimators available
here.

|                           | Estimate |  s.e. | 95%Lower | 95%Upper |
|:--------------------------|---------:|------:|---------:|---------:|
| Homogeneous Model         |   17.182 | 0.474 |   17.011 |   20.015 |
| Homogeneous (MLE)         |   17.000 | 0.633 |   17.000 |   18.858 |
| Chao1 (Chao, 1984)        |   17.499 | 1.322 |   17.030 |   25.434 |
| Chao1-bc                  |   17.000 | 0.633 |   17.000 |   18.858 |
| iChao1 (Chiu et al. 2014) |   17.624 | 1.322 |   17.048 |   25.048 |
| ACE (Chao & Lee, 1992)    |   17.342 | 0.775 |   17.024 |   21.788 |
| ACE-1 (Chao & Lee, 1992)  |   17.366 | 0.835 |   17.026 |   22.168 |
| 1st order jackknife       |   17.999 | 1.413 |   17.128 |   24.796 |
| 2nd order jackknife       |   18.000 | 2.446 |   17.065 |   32.367 |

The implementation of the parallel wrapper is also quite simple for
[`BeeBDC::ChaoWrapper()`](https://jbdorey.github.io/BeeBDC/reference/ChaoWrapper.md).
And the number of cores can also be changed using the `mc.cores`
argument. Note, that in this case, we can run all sites (or countries)
at once!

``` r
# Run the wrapper function
output_iChaowrapper <- BeeBDC::ChaoWrapper(data = data_iChao, datatype = "abundance",
    k = 10, conf = 0.95, mc.cores = 1)
```

    ## Error:
    ## ! 'ChaoWrapper' is not an exported object from 'namespace:BeeBDC'

View the output with `output_iChaowrapper$diversityTable`.

    ## Error:
    ## ! object 'output_iChaowrapper' not found

### Visualising

These are both easy and simple to use wrapper functions. However, the
complexity of the output data types can actually be a little bit
confusing to work with. Especially if you want to visualise those data.
So, we also provide the function,
[`BeeBDC::ggRichnessWrapper()`](https://jbdorey.github.io/BeeBDC/reference/ggRichnessWrapper.md)
to display the results of both wrapper functions by site. Not only does
this functino save a plot that visualises the data (below), it also
outputs a summary of the estimates.

``` r
plot_summary <- BeeBDC::ggRichnessWrapper(iNEXT_in = output_iNEXTwrapper, iChao_in = output_iChaowrapper,
    nrow = 2, ncol = 2, labels = NULL, fileName = "speciesRichnessPlots", outPath = tempdir(),
    base_width = 8.3, base_height = 11.7, dpi = 300)
```

    ## Error:
    ## ! 'ggRichnessWrapper' is not an exported object from 'namespace:BeeBDC'

Note that Fiji’s diversity seems to be reaching asymptote, but the
remaining country’s species richnesses are still climbing. Vietnam in
particular has a huge amount of uncertainty with the iChao estimates
(green) reaching over 60 species! We knew that this was a really small
dataset (in fact we chose it as a test dataset because it is small), but
it goes to show that if your data are inadequate you may not get a great
answer. On the plus side, the 95% confidence intervals all overlap.

    ## Error:
    ## ! object 'plot_summary' not found

You can see, that these functions are moving towards simplifying quick
and mass-estimation of species richness across sites or countries! The
is the idea behind these improvements. But, we take this further below
by also attempting to sample known taxa that are otherwise missing from
our datasets!

## Estimating richness from the known unknowns

Seems like a bit of an odd title, no? Well, many taxa have country-level
checklists. Even insect taxa can be well represented in global
country-level checklists; for example,
[ants](https://www.antwiki.org/wiki/Welcome_to_AntWiki),
[butterflies](https://doi.org/10.1111/geb.13475),
[dragonflies](https://doi.org/10.1111/jbi.14457), and
[bees](https://www.discoverlife.org/mp/20q?guide=Apoidea_species&flags=HAS:).
While for vertebrates the situation is even better. Country-level global
checklists often incorporate knowledge that isn’t contianed in species
occurrence datasets but that, perhaps, we can take advantage of.

### Preparing the inputs

In our [bee species richness estimates
paper](https://jbdorey.github.io/BeeBDC/articles/DOI), we had 4,819
species (from a total of ~21,000 species) that had no occurrence records
globally. We could have run the above species richness estimates using
just the occurrences that we had, but we wanted to incorporate that
knowledge in our estimates. To do so, we found the samples sizes from
most-recent literature for 497 species and generated a curve (we also
generated this curve using the global- and country-level occurrence
datasets and found the former to match the literature curve well).

![Curves generated using 497 literature species (yellow), the global
occurrence records (light blue), and occurrences at the country-level
(dark
blue).](https://photos.smugmug.com/photos/i-sXCctD2/0/NCPsSsmb4PSncqxVGkkC9SSSdv99NNQFN2rx6q6rn/O/i-sXCctD2.png)

*Curves generated using 497 literature species (yellow), the global
occurrence records (light blue), and occurrences at the country-level
(dark blue).*

We can then extract the formula from this curve and use it to randomly
generate data for those missing species. These curves can be built using
your own data; see section 1.6 of the R code for the [bee species
richness estimates
paper](https://jbdorey.github.io/BeeBDC/articles/DOI), which uses
**ggplot2** and **mosaic**’s `fitModel` function to generate the curve
function that goes into `BeeBDC::diversityPrepR()` below.

Let’s dig in… To start with, we will *make an R object* using our
occurrence data (*beesCountrySubset*), taxonomy (from
[`BeeBDC::beesTaxonomy()`](https://jbdorey.github.io/BeeBDC/reference/beesTaxonomy.md)
or
[`BeeBDC::taxadbToBeeBDC()`](https://jbdorey.github.io/BeeBDC/reference/taxadbToBeeBDC.md)),
and country
checklist([`BeeBDC::beesChecklist()`](https://jbdorey.github.io/BeeBDC/reference/beesChecklist.md)
or a manually-made checklist with the countries in the
*rNaturalEarth_name* column and species in the *validName* column; the
latter must match the remaining data — see `BeeBDC::HarmoniseR()`). Feel
free to explore the input files or the output list of files for
modifying your own and pay attention to match the column names.

Note as well that the `curveFunction` is the one generated from the
Literature curve above.

``` r
  # Generate the R file
estimateData <- BeeBDC::diversityPrepR(
  data = beesCountrySubset,
    # Download the bee taxonomy. Download other taxonomies using BeeBDC::taxadbToBeeBDC()
  taxonomyFile = BeeBDC::beesTaxonomy(),
    # Download the bee country checklist. See notes above about making a checklist for other taxa
  checklistFile = BeeBDC::beesChecklist(),
  curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
  sampleSize = 10000,
  countryColumn = "country_suggested"
)
```

    ## Error:
    ## ! 'diversityPrepR' is not an exported object from 'namespace:BeeBDC'

### Making estimates

The output file from the above code is then used to feed into the
`BeeBDC::richnessEstimator()` function which will repeatedly sample the
provided curve and estimate species richness (a user-defined number of
times) at the country (*or site*), continental, or global level. This
function can also be run in parallel and run at whatever scale the user
asks for. If `globalSamples`, `continentSamples`, or `countrySamples`
are set to zero, they will not be run. If they are set to \>0, they will
be estimated that many times. Let’s run our estimates at the country
(*or site*) level ten times below, drawing sample sizes from the
non-occurrence species each time (*capped at the maximum sample size for
each country*).

``` r
estimates <- BeeBDC::richnessEstimateR(
  data = estimateData,
  sampleSize = 10000,
  globalSamples = 0,
  continentSamples = 0,
  countrySamples = 10,
    # Increase the number of cores to use R's parallel package and speed estimates up.
  mc.cores = 1,
    # Directory where to save files
  outPath = tempdir(),
  fileName = "Sampled.pdf"
)
```

    ## Error:
    ## ! 'richnessEstimateR' is not an exported object from 'namespace:BeeBDC'

We can look at the median outputs of our analysis with
`estimates$Summary`.

    ## Error:
    ## ! object 'estimates' not found

Or, we can look at the outputs from each iteration together with
`estimates$SiteOutput`.

    ## Error:
    ## ! object 'estimates' not found

### Visualising estimates

Of course, these values are super helpful for papers on these topics.
But similarly, we can easily make some further useful visualisations!

``` r
# To build the manual legend, make a small fake dataset
legendData <- dplyr::tibble(name = c("yes","yes"),
                                              statistic = c("iChao", "iNEXT") %>%
                                                factor(levels = c("iChao", "iNEXT")),
                                              est = c(1,1))
# Build a legend manually
violinLegend <- ggplot2::ggplot(legendData, ggplot2::aes(x = name, y = est)) + 
  ggplot2::geom_point(data = legendData, ggplot2::aes(y=est, x = name, colour = "red")) + 
   scale_color_manual(labels = c('Observed'), values = c('grey30'))  +
   ggplot2::geom_bar(aes(fill = statistic, y = est), position = position_dodge(0.90),  
                     stat = "identity") +
   ggplot2::scale_fill_manual(name = "Statistic",
                              labels = c("iChao", "iNEXT"),
                              values = c("iChao" = "#55AD9B", "iNEXT" = "#FD9B63")) +
   ggplot2::theme_classic() +
   theme(legend.title = element_blank(), legend.position = 'right', 
         legend.margin = margin(0, 0, 0, 0), legend.spacing.y = unit(0, "pt")) +
   ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, byrow = TRUE, reverse = TRUE, order = 1))

# plot the countries 
violinPlot <- ggplot2::ggplot(estimates$SiteOutput, ggplot2::aes(x = name, y = est)) + 
   ggplot2::geom_violin(position="dodge", alpha=0.5, ggplot2::aes(fill=Name, 
                                                                  y=`95%Lower`, x=variable), 
                        colour =  NA) +
   ggplot2::geom_violin(position="dodge", alpha=0.5, ggplot2::aes(fill=Name, y=`95%Upper`,
                                                                  x=variable), colour =  NA) +
   ggplot2::geom_violin(position="dodge", alpha=1, ggplot2::aes(fill=Name, y=Estimate, 
                                                                x=variable), colour =  "black") +
   ggplot2::scale_fill_manual(values=c("#55AD9B", "#FD9B63")) +
    ggplot2::geom_point(data = estimates$SiteOutput %>%
                          dplyr::distinct(variable, Observed) %>% 
                          tidyr::drop_na(), 
                        ggplot2::aes(x = variable, y = Observed), col = "grey40") +
  ggplot2::theme_classic() + 
  ggplot2::xlab("Continent") + 
  ggplot2::ylab("Species estimate") +
  guides(fill=guide_legend(title="Statistic")) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none", 
                 axis.text.x = ggplot2::element_text(angle = 60, vjust = 1, hjust=1)) +
  ggplot2::xlab(c( "")) + ggplot2::ylab(c("Species"))  +
  ggplot2::annotation_custom(cowplot::ggdraw(cowplot::get_legend(violinLegend)) %>%
                               ggplot2::ggplotGrob(), xmin = 1, xmax = 1, 
                               ymin = 350, ymax = 500)
```

    ## Error:
    ## ! object 'estimates' not found

``` r
  ## Tip: You can wrap the entire ggplot2 chunk of code in brackets () to print at the same time

# Save the plot
cowplot::save_plot(filename = paste0(tempdir(), "/violinPlot.pdf"),
                   plot = violinPlot,
                   base_width = 10,
                   base_height = 7)
```

    ## Error:
    ## ! object 'violinPlot' not found

There are many ways to plot or analyse these kinds of data. The rest I
will leave up to you! But, for more ideas, do feel free to check out the
[original publication](https://doi.org/10.21203/rs.3.rs-6372769/v1) for
this workflow as well as the [original R
workflow](https://jbdorey.github.io/BeeBDC/articles/LINK?) itself.

## Read more

You can read more about the implementation of this work in the original
citation: *Dorey J. B., Gilpin, A.-M., Johnson, N., Esquerre, D.,
Hughes, A. C., Ascher, J. S., & Orr, M. C. (Under review). How many bee
species are there? A quantitative global estimate. Nature
Communications. <https://doi.org/10.21203/rs.3.rs-6372769/v1>*
