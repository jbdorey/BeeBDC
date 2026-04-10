# Get country names from coordinates

Because the
[`bdc::bdc_country_from_coordinates()`](https://brunobrr.github.io/bdc/reference/bdc_country_from_coordinates.html)
function is very RAM-intensive, this wrapper allows a user to specify
chunk-sizes and only analyse a small portion of the occurrence data at a
time. The prefix jbd\_ is used to highlight the difference between this
function and the original
[`bdc::bdc_country_from_coordinates()`](https://brunobrr.github.io/bdc/reference/bdc_country_from_coordinates.html).

## Usage

``` r
jbd_CfC_chunker(
  data = NULL,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  stepSize = 1e+06,
  chunkStart = 1,
  scale = "medium",
  path = tempdir(),
  mc.cores = 1
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records to use as input.

- lat:

  Character. The name of the column to use as latitude. Default =
  "decimalLatitude".

- lon:

  Character. The name of the column to use as longitude. Default =
  "decimalLongitude".

- country:

  Character. The name of the column containing country names. Default =
  "country.

- stepSize:

  Numeric. The number of occurrences to process in each chunk. Default =
  1000000.

- chunkStart:

  Numeric. The chunk number to start from. This can be \> 1 when you
  need to restart the function from a certain chunk. For example, can be
  used if R failed unexpectedly.

- scale:

  Passed to rnaturalearth's ne_countries(). Scale of map to return, one
  of 110, 50, 10 or 'small', 'medium', 'large'. Default = "large".

- path:

  Character. The directory path to a folder in which to save the running
  countrylist csv file.

- mc.cores:

  Numeric. If \> 1, the function will run in parallel using mclapply
  using the number of cores specified. If = 1 then it will be run using
  a serial loop. NOTE: Windows machines must use a value of 1 (see
  ?parallel::mclapply). Additionally, be aware that each thread can use
  large chunks of memory. Default = 1.

## Value

A data frame containing database_ids and a country column that needs to
be re-merged with the data input.

## Examples

``` r
if(requireNamespace("rnaturalearthdata")){
library("dplyr")
data(beesFlagged)
HomePath = tempdir()
# Tibble of common issues in country names and their replacements
commonProblems <- dplyr::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
'United States','U.S.A','MX','CA','Bras.','Braz.','Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
                                 fix = c('United States of America','United States of America',
                                 'United States of America','United States of America',
                                 'United States of America','United States of America',
                                 'United States of America','Mexico','Canada','Brazil','Brazil',
                                 'Brazil','Northern Mariana Islands','Puerto Rico'))
                                 
beesFlagged <- beesFlagged %>%
      # Replace a name to test
   dplyr::mutate(country = stringr::str_replace_all(country, "Brazil", "Brasil"))

beesFlagged_out <- countryNameCleanR(
  data = beesFlagged,
  commonProblems = commonProblems)

suppressWarnings(
  countryOutput <- jbd_CfC_chunker(data = beesFlagged_out,
                                   lat = "decimalLatitude",
                                   lon = "decimalLongitude",
                                   country = "country",
                                   # How many rows to process at a time
                                   stepSize = 1000000,
                                   # Start row
                                   chunkStart = 1,
                                   path = HomePath,
                                   scale = "medium"),
  classes = "warning")


# Left join these datasets
beesFlagged_out <- left_join(beesFlagged_out, countryOutput, by = "database_id")  %>% 
  # merge the two country name columns into the "country" column
  dplyr::mutate(country = dplyr::coalesce(country.x, country.y)) %>%
  # remove the now redundant country columns 
  dplyr::select(!c(country.x, country.y)) %>%
  # put the column back 
  dplyr::relocate(country) %>% 
  # Remove duplicates if they arose!
  dplyr::distinct()

# Remove illegal characters
beesFlagged_out$country <- beesFlagged_out$country %>%
  stringr::str_replace(., pattern = paste("\\[", "\\]", "\\?",
                                          sep=  "|"), replacement = "")
} # END if require
#>  - Using default country names and codes from https:en.wikipedia.org/wiki/ISO_3166-1_alpha-2 - static version from July 2022.
#>  - Running chunker with:
#> stepSize = 1,000,000
#> chunkStart = 1
#> chunkEnd = 1,000,000
#>  - Starting parallel operation. Unlike the serial operation (mc.cores = 1) , a parallel operation will not provide running feedback. Please be patient  as this function may take some time to complete. Each chunk will be run on  a seperate thread so also be aware of RAM usage.
#> Loading required package: rnaturalearth
#>  - Completed in 0.3 secs
#>  - We have updated the country names of 5 occurrences that previously had no country name assigned.
```
