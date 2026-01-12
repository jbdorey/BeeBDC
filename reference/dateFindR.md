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
#>  - Preparing data...
#>  - Extracting dates from year, month, day columns...
#>  - Extracting dates from fieldNotes, locationRemarks, and verbatimEventDate columns in unambiguous ymd, dmy, mdy, and my formats...
#>  - Extracting year from fieldNotes, locationRemarks, and verbatimEventDate columns in ambiguous formats...
#>  - Formating and combining the new data..
#>  - Merging all data, nearly there...
#>  - Finished. 
#> We now have 1 more full eventDate cells than in the input data.
#> We modified dates in 
#> 89 occurrences.
#>  - As it stands, there are 89 complete eventDates and 11 missing dates.
#>  - There are also 89 complete year occurrences to filter from. This is up from an initial count of 88 At this rate, you will stand to lose 11 occurrences on the basis of missing year - Operation time: 0.472053050994873 secs
```
