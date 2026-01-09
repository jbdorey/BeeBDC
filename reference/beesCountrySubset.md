# A simple scientificName and country_suggested test dataset

A small bee occurrence dataset with 1,488
scientificName-country_suggested combinations across four countries;
Fiji, Uganda, Vietnam, and Zambia.

## Usage

``` r
data("beesCountrySubset", package = "BeeBDC")
```

## Format

An object of class `"tibble"`

- scientificName:

  Full scientificName as shown on DiscoverLife

- country_suggested:

  A country name suggested by the
  [`bdc::bdc_country_standardized()`](https://brunobrr.github.io/bdc/reference/bdc_country_standardized.html)
  function.

## References

This data set was created by during the writing of the paper: Dorey,
J.B., (upcoming) How many bee species are there? A quantitative global
estimate. Journal

## Examples

``` r
beesCountrySubset <- BeeBDC::beesCountrySubset
head(beesCountrySubset)
#> # A tibble: 6 × 2
#>   scientificName                     country_suggested
#>   <chr>                              <chr>            
#> 1 Xylocopa flavorufa (De Geer, 1778) Zambia           
#> 2 Xylocopa flavorufa (De Geer, 1778) Zambia           
#> 3 Xylocopa flavorufa (De Geer, 1778) Zambia           
#> 4 Xylocopa flavorufa (De Geer, 1778) Uganda           
#> 5 Apis mellifera Linnaeus, 1758      Zambia           
#> 6 Apis mellifera Linnaeus, 1758      Uganda           
```
