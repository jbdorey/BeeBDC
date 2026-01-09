# Finds outliers, and their duplicates, as determined by experts

Uses expert-identified outliers with source spreadsheets that may be
edited by users. The function will also use the duplicates file made
using
[`dupeSummary()`](https://jbdorey.github.io/BeeBDC/reference/dupeSummary.md)
to identify duplicates of the expert-identified outliers and flag those
as well. The function will add a flagging column called `.expertOutlier`
where records that are FALSE are the expert outliers.

## Usage

``` r
manualOutlierFindeR(
  data = NULL,
  DataPath = NULL,
  PaigeOutliersName = "removedBecauseDeterminedOutlier.csv",
  newOutliersName = "All_outliers_ANB.xlsx",
  ColombiaOutliers_all = "All_Colombian_OutlierIDs.csv",
  duplicates = NULL,
  NearTRUE = NULL,
  NearTRUE_threshold = 5
)
```

## Arguments

- data:

  A data frame or tibble. Occurrence records as input.

- DataPath:

  A character path to the directory that contains the outlier
  spreadsheets.

- PaigeOutliersName:

  A character patch. Should lead to outlier spreadsheet from Paige
  Chesshire (csv file).

- newOutliersName:

  A character path. Should lead to appropriate outlier spreadsheet (xlsx
  file).

- ColombiaOutliers_all:

  A character path. Should lead to spreadsheet of bee outliers from
  Colombia (csv file).

- duplicates:

  A data frame or tibble. The duplicate file produced by
  [`dupeSummary()`](https://jbdorey.github.io/BeeBDC/reference/dupeSummary.md).

- NearTRUE:

  Optional. A character file name to the csv file. If you want to remove
  expert outliers that are too close to TRUE points, use the name of the
  NearTRUE.csv. Note: This implementation is only basic for now unless
  there is a greater need in the future.

- NearTRUE_threshold:

  Numeric. The threshold (in km) for the distance to TRUE points to keep
  expert outliers.

## Value

Returns the data with a new column, `.expertOutlier` where records that
are FALSE are the expert outliers.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Read example data
  data(beesFlagged)
# Read in the most-recent duplicates file as well
if(!exists("duplicates")){
  duplicates <- fileFinder(path = DataPath,
                            fileName = "duplicateRun_") %>%
    readr::read_csv()}
# identify the outliers and get a list of their database_ids
beesFlagged_out <- manualOutlierFindeR(
  data = beesFlagged,
  DataPath = DataPath,
  PaigeOutliersName = "removedBecauseDeterminedOutlier.csv",
  newOutliersName = "^All_outliers_ANB_14March.xlsx",
  ColombiaOutliers_all = "All_Colombian_OutlierIDs.csv",
  duplicates = duplicates)
} # }
```
