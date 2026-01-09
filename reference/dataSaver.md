# Simple function to save occurrence AND EML data as a list

Used at the end of 1.x in the example workflow in order to save the
occurrence dataset and its associated eml metadata.

## Usage

``` r
dataSaver(
  path = NULL,
  save_type = NULL,
  occurrences = NULL,
  eml_files = NULL,
  file_prefix = NULL
)
```

## Arguments

- path:

  Character. The main file path to look for data in.

- save_type:

  Character. The file format in which to save occurrence and EML data.
  Either "R_file" or "CSV_file"

- occurrences:

  The occurrences to save as a data frame or tibble.

- eml_files:

  A list of the EML files.

- file_prefix:

  Character. A prefix for the resulting output file.

## Value

This function saves both occurrence and EML data as a list when
save_type = "R_File" or as individual csv files when save_type =
"CSV_file".

## Examples

``` r
if (FALSE) { # \dontrun{
dataSaver(path = tempdir(),# The main path to look for data in
save_type = "CSV_file", # "R_file" OR "CSV_file"
occurrences = Complete_data$Data_WebDL, # The existing datasheet
eml_files = Complete_data$eml_files, # The existing EML files
file_prefix = "Fin_") # The prefix for the file name
} # }
```
