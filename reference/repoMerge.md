# Import occurrences from GBIF, ALA, iDigBio, and SCAN downloads

Locates data from GBIF, ALA, iDigBio, and SCAN within a directory and
reads it in along with its eml metadata. Please keep the original
download folder names and architecture unchanged. NOTE: This function
uses family-level data to identify taxon downloads. If this, or
something new, becomes an issue, please contact James Dorey (the
developer) as there are likely to be exceptions to how files are
downloaded. current as of versions 1.0.4.

## Usage

``` r
repoMerge(path, save_type, occ_paths)
```

## Arguments

- path:

  A directory as a character. The directory to recursively look in for
  the above data.

- save_type:

  Character. The data type to save the resulting file as. Options are:
  csv_files" or "R_file".

- occ_paths:

  A list of directories. Preferably produced using
  [`repoFinder()`](https://jbdorey.github.io/BeeBDC/reference/repoFinder.md)
  the function asks for a list of paths to the relevant input datasets.
  You can fault-find errors in this function by checking the output of
  [`repoFinder()`](https://jbdorey.github.io/BeeBDC/reference/repoFinder.md).

## Value

A list with a data frame of merged occurrence records, "Data_WebDL", and
a list of eml files contained in "eml_files". Also saves these files in
the requested format.

## Examples

``` r
if (FALSE) { # \dontrun{
DataImp <- repoMerge(path = DataPath, 
# Find data - Many problems can be solved by running [BeeBDC::repoFinder(path = DataPath)]
# And looking for problems
occ_paths = BeeBDC::repoFinder(path = DataPath),
save_type = "R_file")
} # }
```
