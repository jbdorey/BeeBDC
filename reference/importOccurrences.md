# Imports the most-recent repoMerge data

Looks for and imports the most-recent version of the occurrence data
created by the
[`repoMerge()`](https://jbdorey.github.io/BeeBDC/reference/repoMerge.md)
function.

## Usage

``` r
importOccurrences(path = path, fileName = "^BeeData_")
```

## Arguments

- path:

  A directory as a character. The directory to recursively look in for
  the above data.

- fileName:

  Character. A String of text to look for the most-recent dataset.
  Default = "^BeeData\_". Find faults by modifying
  [`fileFinder()`](https://jbdorey.github.io/BeeBDC/reference/fileFinder.md)
  and logic-checking the file that's found.

## Value

A list with a data frame of merged occurrence records, "Data_WebDL", and
a list of EML files contained in "eml_files".

## Examples

``` r
if (FALSE) { # \dontrun{
DataImp <- importOccurrences(path = DataPath)
} # }
```
