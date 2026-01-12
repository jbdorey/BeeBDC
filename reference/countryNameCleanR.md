# Fix country name issues using a user-input list

This function is basic for a user to manually fix some country name
inconsistencies.

## Usage

``` r
countryNameCleanR(data = NULL, ISO2_table = NULL, commonProblems = NULL)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- ISO2_table:

  A data frame or tibble with the columns ISO2 and long names for
  country names. Default is a static version from Wikipedia.

- commonProblems:

  A data frame or tibble. It must have two columns: one containing the
  user-identified problem and one with a user-defined fix

## Value

Returns the input data, but with countries occurring in the
user-supplied problem column ("commonProblems") replaced with those in
the user-supplied fix column

## Examples

``` r
beesFlagged_out <- countryNameCleanR(
data = BeeBDC::beesFlagged,
commonProblems = dplyr::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
                        'United States','U.S.A','MX','CA','Bras.','Braz.',
                        'Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
                        fix = c('United States of America','United States of America',
                                'United States of America','United States of America',
                                'United States of America','United States of America',
                                'United States of America','Mexico','Canada','Brazil',
                                'Brazil','Brazil','Northern Mariana Islands','PUERTO.RICO')))
#> Error in countryNameCleanR(data = BeeBDC::beesFlagged, commonProblems = dplyr::tibble(problem = c("U.S.A.",     "US", "USA", "usa", "UNITED STATES", "United States", "U.S.A",     "MX", "CA", "Bras.", "Braz.", "Brasil", "CNMI", "USA TERRITORY: PUERTO RICO"),     fix = c("United States of America", "United States of America",         "United States of America", "United States of America",         "United States of America", "United States of America",         "United States of America", "Mexico", "Canada", "Brazil",         "Brazil", "Brazil", "Northern Mariana Islands", "PUERTO.RICO"))): could not find function "countryNameCleanR"
```
