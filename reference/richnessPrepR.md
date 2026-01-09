# Prepare occurrence, taxonomy, and checklist data for richness estimation

Takes your occurrence dataset along with a taxonomy and checklist in
order to produce a file that's ready to be passed into the
[`richnessEstimateR()`](https://jbdorey.github.io/BeeBDC/reference/richnessEstimateR.md)
function in order to estimate species richness using iChao and iNEXT
(hill numbers) for countries, continents, or the entire globe.

## Usage

``` r
richnessPrepR(
  data = NULL,
  taxonomyFile = BeeBDC::beesTaxonomy(),
  checklistFile = BeeBDC::beesChecklist(),
  curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
  sampleSize = 10000,
  countryColumn = "country_suggested",
  limitGlobal = NULL,
  outPath = tempdir()
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input. Needs to include
  the `scientificName` column and a "country" column (see
  `countryColumn`)

- taxonomyFile:

  A data frame or tibble. The taxonomy file to use. Default =
  [`beesTaxonomy()`](https://jbdorey.github.io/BeeBDC/reference/beesTaxonomy.md)
  but see
  [`taxadbToBeeBDC()`](https://jbdorey.github.io/BeeBDC/reference/taxadbToBeeBDC.md)
  for other taxa.

- checklistFile:

  A data frame or tibble. The taxonomy to use. Default =
  [`beesChecklist()`](https://jbdorey.github.io/BeeBDC/reference/beesChecklist.md);
  use this as a template to convert other taxa checklists.

- curveFunction:

  A function. The mathematical function that describes the curve used to
  randomly sample from in order to fill empty sample sizes for species
  present in the checklist but not present in the occurrence dataset.
  Default is `function(x) (228.7531 * x * x^-log(12.1593))`, which is
  taken from the paper "How many bee species are there? A quantitative
  global estimate" by Dorey et al. Models can be fit using
  [`mosaic::fitModel()`](https://www.mosaic-web.org/mosaic/reference/fitModel.html).

- sampleSize:

  Numeric. The size of the sample randomly drawn from the provided
  curve. See `curveFunction`. Default = 10000.

- countryColumn:

  Character. The column from which country names should be sought.

- limitGlobal:

  Character vector. A character vector of the countries to filter the
  data to in order limit the extent of the global-level analysis.
  Defualt = NULL.

- outPath:

  Character. The output path where the curve plot (curvePlot.pdf) and
  the output Rdata (richnessInputs.Rda) file will be saved. Default =
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

Saves an Rdata file with the data needed to feed into the
[`richnessEstimateR()`](https://jbdorey.github.io/BeeBDC/reference/richnessEstimateR.md)
function. The
[`richnessEstimateR()`](https://jbdorey.github.io/BeeBDC/reference/richnessEstimateR.md)
function will then use iChao and/or iNEXT to estimate species richness
for countries, continents, and the globe. Also returns the RData file to
the environment.

## See also

[`countryHarmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/countryHarmoniseR.md)
to harmonise country names based on a short list.

## Examples

``` r
if (FALSE) { # \dontrun{
data(beesCountrySubset)

estimateDataExample <- richnessPrepR(
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
} # }
```
