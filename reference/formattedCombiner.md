# Combine the formatted USGS data with the main dataset

Merges the Darwin Core version of the USGS dataset that was created
using
[`USGS_formatter()`](https://jbdorey.github.io/BeeBDC/reference/USGS_formatter.md)
with the main dataset.

## Usage

``` r
formattedCombiner(path, strings, existingOccurrences, existingEMLs)
```

## Arguments

- path:

  A directory as character. The directory to look in for the formatted
  USGS data.

- strings:

  A regex string. The string to find the most-recent formatted USGS
  dataset.

- existingOccurrences:

  A data frame. The existing occurrence dataset.

- existingEMLs:

  An EML file. The existing EML data file to be appended.

## Value

A list with the combined occurrence dataset and the updated EML file.

## Examples

``` r
if (FALSE) { # \dontrun{
DataPath <- tempdir()
strings = c("USGS_DRO_flat_27-Apr-2022")
    # Combine the USGS data and the existing big dataset
Complete_data <- formattedCombiner(path = DataPath, 
                                    strings = strings, 
                                    # This should be the list-format with eml attached
                                    existingOccurrences = DataImp$Data_WebDL,
                                    existingEMLs = DataImp$eml_files) 
                                    } # }
```
