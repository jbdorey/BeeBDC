# Identifies duplicate occurrence records

This function uses user-specified inputs and columns to identify
duplicate occurrence records. Duplicates are identified iteratively and
will be tallied up, duplicate pairs clustered, and sorted at the end of
the function. The function is designed to work with Darwin Core data
with a database_id column, but it is also modifiable to work with other
columns.

## Usage

``` r
dupeSummary(
  data = NULL,
  path = NULL,
  duplicatedBy = NULL,
  completeness_cols = NULL,
  idColumns = NULL,
  collectionCols = NULL,
  collectInfoColumns = NULL,
  CustomComparisonsRAW = NULL,
  CustomComparisons = NULL,
  sourceOrder = NULL,
  prefixOrder = NULL,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
    ".uncertaintyThreshold", ".unLicensed"),
  characterThreshold = 2,
  numberThreshold = 3,
  numberOnlyThreshold = 5,
  catalogSwitch = TRUE
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- path:

  A character path to the location where the duplicateRun\_ file will be
  saved.

- duplicatedBy:

  A character vector. Options are c("ID", "collectionInfo", "both").
  "ID" columns runs through a series of ID-only columns defined by
  idColumns. "collectionInfo" runs through a series of columns defined
  by collectInfoColumns, which are checked in combination with
  collectionCols. "both" runs both of the above.

- completeness_cols:

  A character vector. A set of columns that are used to order and select
  duplicates by. For each occurrence, this function will calculate the
  sum of
  [`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html).
  Within duplicate clusters occurrences with a greater number of the
  completeness_cols filled in will be kept over those with fewer.

- idColumns:

  A character vector. The columns to be checked individually for
  internal duplicates. Intended for use with ID columns only.

- collectionCols:

  A character vector. The columns to be checked in combination with each
  of the completeness_cols.

- collectInfoColumns:

  A character vector. The columns to be checked in combinatino with all
  of the collectionCols columns.

- CustomComparisonsRAW:

  A list of character vectors. Custom comparisons - as a list of columns
  to iteratively compare for duplicates. These differ from the
  CustomComparisons in that they ignore the minimum number and character
  thresholds for IDs.

- CustomComparisons:

  A list of character vectors. Custom comparisons - as a list of columns
  to iteratively compare for duplicates. These comparisons are made
  after character and number thresholds are accounted for in ID columns.

- sourceOrder:

  A character vector. The order in which you want to KEEP duplicated
  based on the dataSource column (i.e. what order to prioritize data
  sources). NOTE: These dataSources are simplified to the string prior
  to the first "\_". Hence, "GBIF_Anthophyla" becomes "GBIF."

- prefixOrder:

  A character vector. Like sourceOrder, except based on the database_id
  prefix, rather than the dataSource. Additionally, this is only
  examined if prefixOrder != NULL. Default = NULL.

- dontFilterThese:

  A character vector. This should contain the flag columns to be ignored
  in the creation or updating of the .summary column. Passed to
  [`summaryFun()`](https://jbdorey.github.io/BeeBDC/reference/summaryFun.md).

- characterThreshold:

  Numeric. The complexity threshold for ID letter length. This is the
  minimum number of characters that need to be present in ADDITION TO
  the numberThreshold for an ID number to be tested for duplicates.
  Ignored by CustomComparisonsRAW. The columns that are checked are
  occurrenceID, recordId, id, catalogNumber, and otherCatalogNumbers.
  Default = 2.

- numberThreshold:

  Numeric. The complexity threshold for ID number length. This is the
  minimum number of numeric characters that need to be present in
  ADDITION TO the characterThreshold for an ID number to be tested for
  duplicates. Ignored by CustomComparisonsRAW. The columns that are
  checked are occurrenceID, recordId, id, catalogNumber, and
  otherCatalogNumbers. Default = 3.

- numberOnlyThreshold:

  Numeric. As numberThreshold except the characterThreshold is ignored.
  Default = 5.

- catalogSwitch:

  Logical. If TRUE, and the catalogNumber is empty the function will
  copy over the otherCatalogNumbers into catalogNumber and visa versa.
  Hence, the function will attempt to matchmore catalog numbers as both
  of these functions can be problematic. Default = TRUE.

## Value

Returns data with an additional column called .duplicates where FALSE
occurrences are duplicates and TRUE occurrences are either kept
duplicates or unique. Also exports a .csv to the user-specified location
with information about duplicate matching. This file is used by other
functions including
[`manualOutlierFindeR()`](https://jbdorey.github.io/BeeBDC/reference/manualOutlierFindeR.md)
and
[`chordDiagramR()`](https://jbdorey.github.io/BeeBDC/reference/chordDiagramR.md)

## See also

[`chordDiagramR()`](https://jbdorey.github.io/BeeBDC/reference/chordDiagramR.md)
for creating a chord diagram to visualise linkages between dataSources
and
[`dupePlotR()`](https://jbdorey.github.io/BeeBDC/reference/dupePlotR.md)
to visualise the numbers and proportions of duplicates in each
dataSource.

## Examples

``` r
beesFlagged_out <- dupeSummary(
data = BeeBDC::beesFlagged,
  # Should start with paste0(DataPath, "/Output/Report/"), instead of tempdir():
path = paste0(tempdir(), "/"),
# options are "ID","collectionInfo", or "both"
duplicatedBy = "collectionInfo", # I'm only running ID for the first lot because we might 
# recover other info later
# The columns to generate completeness info from
completeness_cols = c("decimalLatitude",  "decimalLongitude",
                      "scientificName", "eventDate"),
# idColumns = c("gbifID", "occurrenceID", "recordId","id"),
# The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                   "recordedBy"),
# The columns to combine, one-by-one with the collectionCols
collectInfoColumns = c("catalogNumber", "otherCatalogNumbers"),
# Custom comparisons - as a list of columns to compare
# RAW custom comparisons do not use the character and number thresholds
CustomComparisonsRAW = dplyr::lst(c("catalogNumber", "institutionCode", "scientificName")),
# Other custom comparisons use the character and number thresholds
CustomComparisons = dplyr::lst(c("gbifID", "scientificName"),
                                c("occurrenceID", "scientificName"),
                                c("recordId", "scientificName"),
                                c("id", "scientificName")),
# The order in which you want to KEEP duplicated based on data source
# try unique(check_time$dataSource)
sourceOrder = c("CAES", "Gai", "Ecd","BMont", "BMin", "EPEL", "ASP", "KP", "EcoS", "EaCO",
                "FSCA", "Bal", "SMC", "Lic", "Arm",
                "USGS", "ALA", "GBIF","SCAN","iDigBio"),
# !!!!!! BELS > GeoLocate
# Set the complexity threshold for id letter and number length
# minimum number of characters when WITH the numberThreshold
characterThreshold = 2,
# minimum number of numbers when WITH the characterThreshold
numberThreshold = 3,
# Minimum number of numbers WITHOUT any characters
numberOnlyThreshold = 5)
#> Loading required namespace: igraph
#>  - Generating a basic completeness summary from the decimalLatitude, decimalLongitude, scientificName, eventDate columns.
#> This summary is simply the sum of complete.cases in each column. It ranges from zero to the N of columns. This will be used to sort duplicate rows and select the most-complete rows.
#>  - Updating the .summary column to sort by...
#>  - We will NOT flag the following columns. However, they will remain in the data file.
#> .gridSummary, .lonFlag, .latFlag, .uncer_terms, .uncertaintyThreshold, .unLicensed
#>  - summaryFun:
#> Flagged 74 
#>   The .summary column was added to the database.
#>  - Working on CustomComparisonsRAW duplicates...
#> 
#> Completed iteration 1 of 1:
#>  - Identified 0 duplicate records and kept 0 unique records using the column(s): 
#> catalogNumber, institutionCode, scientificName
#>  - Working on CustomComparisons duplicates...
#> 
#> Completed iteration 1 of 4:
#>  - Identified 0 duplicate records and kept 0 unique records using the column(s): 
#> gbifID, scientificName
#> 
#> Completed iteration 2 of 4:
#>  - Identified 0 duplicate records and kept 0 unique records using the column(s): 
#> occurrenceID, scientificName
#> 
#> Completed iteration 3 of 4:
#>  - Identified 0 duplicate records and kept 0 unique records using the column(s): 
#> recordId, scientificName
#> 
#> Completed iteration 4 of 4:
#>  - Identified 0 duplicate records and kept 0 unique records using the column(s): 
#> id, scientificName
#>  - Working on collectionInfo duplicates...
#> 
#> Completed iteration 1 of 2:
#>  - Identified 0 duplicate records and kept 0 unique records using the columns: 
#> decimalLatitude, decimalLongitude, scientificName, eventDate, recordedBy, and catalogNumber
#> 
#> Completed iteration 2 of 2:
#>  - Identified 0 duplicate records and kept 0 unique records using the columns: 
#> decimalLatitude, decimalLongitude, scientificName, eventDate, recordedBy, and otherCatalogNumbers
#>  - Clustering duplicate pairs...
#> Duplicate pairs clustered. There are 0 duplicates across 0 kept duplicates.
#>  - Ordering data by 1. dataSource, 2. completeness and 3. .summary column...
#>  - Find and FIRST duplicate to keep and assign other associated duplicates to that one (i.e., across multiple tests a 'kept duplicate', could otherwise be removed)...
#>  - Duplicates have been saved in the file and location: /tmp/RtmpTtIYSh/duplicateRun_collectionInfo_2026-01-12.csv
#>  - Across the entire dataset, there are now 0 duplicates from a total of 100 occurrences.
#>  - Completed in 0.29 secs

```
