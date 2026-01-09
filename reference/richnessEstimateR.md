# Estimate country, continental, and global species richnesses

Takes an output dataset from
[`richnessPrepR()`](https://jbdorey.github.io/BeeBDC/reference/richnessPrepR.md)
to estimate species richness using iChao and iNEXT (hill numbers) for
countries, continents, and/or the entire globe.

## Usage

``` r
richnessEstimateR(
  data = NULL,
  sampleSize = 10000,
  countrySamples = 1,
  continentSamples = 1,
  globalSamples = 1,
  countriesToExclude = NULL,
  mc.cores = 1,
  k = 10,
  filterToRecordedCountries = TRUE,
  outPath = tempdir(),
  fileName = "continentSampled.pdf"
)
```

## Arguments

- data:

  an RData file created using the
  [`richnessPrepR()`](https://jbdorey.github.io/BeeBDC/reference/richnessPrepR.md)
  function.

- sampleSize:

  Numeric. The size of the sample randomly drawn from the provided
  curve. See `curveFunction`. Default = 10000.

- countrySamples:

  Numeric. The number of times to sample the country species richness
  for both iChao and iNEXT. If equal to zero (0), then this will not be
  analysed. Default = 5.

- continentSamples:

  Numeric. The number of times to sample the continent species richness
  for both iChao and iNEXT. If equal to zero (0), then this will not be
  analysed. Default = 5.

- globalSamples:

  Numeric. The number of times to sample the global species richness for
  both iChao and iNEXT. If equal to zero (0), then this will not be
  analysed. Default = 5.

- countriesToExclude:

  Character vector. You may decide to excluse some countries if they are
  being problematic or their sample sizes are too small. Default = NULL.

- mc.cores:

  Numeric. If \> 1, the function will run in parallel using mclapply
  using the number of cores specified. If = 1 then it will be run using
  a serial loop. NOTE: Windows machines must use a value of 1 (see
  ?parallel::mclapply). Additionally, be aware that each thread can use
  large chunks of memory. Default = 1.

- k:

  Numeric. For iChao; the cut-off point (default = 10), which separates
  species into "abundant" and "rare" groups for abundance data for the
  estimator ACE; it separates species into "frequent" and "infrequent"
  groups for incidence data for the estimator ICE. Default = 10.

- filterToRecordedCountries:

  Logical. If TRUE, the checklist will be filtered to the countries
  Where occurrence records were found. Default = TRUE. Change at your
  own peril.

- outPath:

  A directory as character. Directory where to save output figure.
  Default = tempdir().

- fileName:

  A character vector with file name for the output figure, ending with
  '.pdf'. Default = "continentSampled.pdf".

## Value

Outputs an R file with four tables ("Summary", "SiteOutput",
"ContinentOutput", and "GlobalOutput"; depending on the number
required). The summary table shows the Median overall estimates, while
the remaining three shows the outputs from each iteration (useful for
plotting, see relevant vignette). Some figures may also be saved to the
selected outPath.

## See also

[`countryHarmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/countryHarmoniseR.md)
to harmonise country names based on a short list;
[`richnessPrepR()`](https://jbdorey.github.io/BeeBDC/reference/richnessPrepR.md)
to produce the input data and for the required column names; as well as
[`ChaoWrapper()`](https://jbdorey.github.io/BeeBDC/reference/ChaoWrapper.md)
and
[`iNEXTwrapper()`](https://jbdorey.github.io/BeeBDC/reference/iNEXTwrapper.md)
for the parallelised implementation of `SpadeR` and `iNEXT` functions.

## Examples

``` r
if (FALSE) { # \dontrun{

  # Use the example data 
data(beesCountrySubset)

  # First, 
estimateDataExample <- BeeBDC::richnessPrepR(
  data = beesCountrySubset,
  # Download the taxonomy
  taxonomyFile = BeeBDC::beesTaxonomy(),
  # Download the checklist
  checklistFile = BeeBDC::beesChecklist(),
  curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
  sampleSize = 10000,
  countryColumn = "country_suggested",
  limitGlobal = NULL,
  outPath = tempdir()
)

 exampleEstimate <- richnessEstimateR(
   data = estimateDataExample,
   sampleSize = 10000,
   countrySamples = 1,
   continentSamples = 1,
   globalSamples = 1,
   filterToRecordedCountries = TRUE,
   mc.cores = 1,
   # Directory where to save files
   outPath = tempdir(),
   fileName = "Sampled.pdf"
 )
 
} # } # END dontrun
```
