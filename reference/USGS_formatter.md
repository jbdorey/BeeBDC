# Find, import, and format USGS data to Darwin Core

The function finds, imports, formats, and creates metadata for the USGS
dataset.

## Usage

``` r
USGS_formatter(path, pubDate)
```

## Arguments

- path:

  A character path to a directory that contains the USGS data, which
  will be found using
  [`fileFinder()`](https://jbdorey.github.io/BeeBDC/reference/fileFinder.md).
  The function will look for "USGS_DRO_flat".

- pubDate:

  Character. The publication date of the dataset to update the metadata
  and citation.

## Value

Returns a list with the occurrence data, "USGS_data", and the EML data,
"EML_attributes".

## Examples

``` r
if (FALSE) { # \dontrun{
USGS_data <- USGS_formatter(path = DataPath, pubDate = "19-11-2022")
} # }
```
