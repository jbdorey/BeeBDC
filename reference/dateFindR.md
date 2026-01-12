# Find dates in other columns

A function made to search other columns for dates and add them to the
eventDate column. The function searches the columns locality,
fieldNotes, locationRemarks, and verbatimEventDate for the relevant
information. Additionally, for date ranges in the eventDate column,
these will Be translated into startDayOfYear and endDayOfYear and the
eventDate will be kept as the last date, but formatted in the correct
format. Ambiguous dates that can't be parsed reliably as day-month-year
or month-day-year will be rounded to the nearest certainty; if month or
day can be identified with a spelled-out month or a day \>12, these wil
be correctly formatted. Year-month-day... is the most-reliable format

## Usage

``` r
dateFindR(data = NULL, maxYear = lubridate::year(Sys.Date()), minYear = 1700)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- maxYear:

  Numeric. The maximum year considered reasonable to find. Default =
  lubridate::year(Sys.Date()).

- minYear:

  Numeric. The minimum year considered reasonable to find. Default =
  1700.

## Value

The function results in the input occurrence data with but with updated
eventDate, year, month, and day columns for occurrences where these data
were a) missing and b) located in one of the searched columns.

## Examples

``` r
# Using the example dataset, you may not find any missing eventDates are rescued (dependent on 
# which version of the example dataset the user inputs.
beesRaw_out <- dateFindR(data = beesRaw,
                         # Years above this are removed (from the recovered dates only)
                         maxYear = lubridate::year(Sys.Date()),
                         # Years below this are removed (from the recovered dates only)
                         minYear = 1700)
#> Error in dateFindR(data = beesRaw, maxYear = lubridate::year(Sys.Date()),     minYear = 1700): could not find function "dateFindR"
```
