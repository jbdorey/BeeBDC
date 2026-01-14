# Finds files within a directory

A function which can be used to find files within a user-defined
directory based on a user-provided character string.

## Usage

``` r
fileFinder(path, fileName)
```

## Arguments

- path:

  A directory as character. The directory to recursively search.

- fileName:

  A character/regex string. The file name to find.

## Value

Returns a directory to the most-recent file that matches the provide
file. Using regex can greatly improve specificity. The function will
also write into the console the file that it has found - it is
worthwhile to check that this is the correct file to avoid complications
down the line

## Examples

``` r
# \donttest{
# load dplyr
library(dplyr)

 # Make the RootPath to the tempdir for this example
  RootPath <- tempdir()
  
 # Load the example data
 data("beesRaw", package = "BeeBDC")

# Save and example dataset to the temp dir
  readr::write_csv(beesRaw, file = paste0(RootPath, "/beesRaw.csv"))

 # Now go find it!
fileFinder(path = RootPath, fileName = "beesRaw")
#>  - No dates in file name(s). Finding most-recent from file save time...
#>  - Found the following file(s): 
#>  /tmp/RtmpQuppym/beesRaw.csv
#> [1] "/tmp/RtmpQuppym/beesRaw.csv"
# more specifically the .csv version
fileFinder(path = RootPath, fileName = "beesRaw.csv")
#>  - No dates in file name(s). Finding most-recent from file save time...
#>  - Found the following file(s): 
#>  /tmp/RtmpQuppym/beesRaw.csv
#> [1] "/tmp/RtmpQuppym/beesRaw.csv"
# }
```
