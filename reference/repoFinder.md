# Find GBIF, ALA, iDigBio, and SCAN files in a directory

Find GBIF, ALA, iDigBio, and SCAN files in a directory

## Usage

``` r
repoFinder(path)
```

## Arguments

- path:

  A directory as character. The path within which to recursively look
  for GBIF, ALA, iDigBio, and SCAN files.

## Value

Returns a list of directories to each of the above data downloads

## Examples

``` r
if (FALSE) { # \dontrun{
# Where DataPath is made by [BeeBDC::dirMaker()]
BeeBDC::repoFinder(path = DataPath)
} # }
```
