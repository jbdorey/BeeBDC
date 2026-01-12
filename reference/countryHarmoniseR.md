# Match and harmonise country names

A small function that is useful for harmonising country names so that
they mathc between datasets. This relies on the country names matching
one of the exceptions pre-coded in and is so far used internally.

## Usage

``` r
countryHarmoniseR(
  data = NULL,
  countryColumn = NULL,
  shorterNames = TRUE,
  continentAnalysis = FALSE
)
```

## Arguments

- data:

  A data frame or tibble.

- countryColumn:

  Character. The column that contains country names to be harmonised.

- shorterNames:

  Logical. If TRUE, some very long country names will be shortened. This
  can be helpful for writing country names in plots where long names are
  annoying.

- continentAnalysis:

  Logical. Set to TRUE in order to match small entities to continents;
  limited use for the moment.

## Value

Returns the original data frame or tibble but with harmonised country
names

## Examples

``` r
# load in the test dataset
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |> load()
harmonisedCountries <- countryHarmoniseR(
  data = testTaxonomy,
  countryColumn = "country_suggested"
)
```
