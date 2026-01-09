# Query the bee taxonomy and country checklist

A simple function to return information about a particular species,
including name validity and country occurrences.

## Usage

``` r
BeeBDCQuery(
  beeName = NULL,
  searchChecklist = TRUE,
  printAllSynonyms = FALSE,
  beesChecklist = NULL,
  beesTaxonomy = NULL
)
```

## Arguments

- beeName:

  Character or character vector. A single or several bee species names
  to search for in the beesTaxonomy and beesChecklist tables.

- searchChecklist:

  Logical. If TRUE (default), search the country checklist for each
  species.

- printAllSynonyms:

  Logical. If TRUE, all synonyms will be printed out for each entered
  name. default = FALSE.

- beesChecklist:

  A tibble. The bee checklist file for BeeBDC. If is NULL then
  [`beesChecklist()`](https://jbdorey.github.io/BeeBDC/reference/beesChecklist.md)
  will be called internally to download the file. Default = NULL.

- beesTaxonomy:

  A tibble. The bee taxonomy file for BeeBDC. If is NULL then
  [`beesTaxonomy()`](https://jbdorey.github.io/BeeBDC/reference/beesTaxonomy.md)
  will be called internally to download the file. Default = NULL.

## Value

Returns a list with the elements 'taxonomyReport' and 'SynonymReport'.
IF searchChecklist is TRUE, then 'checklistReport' will also be
returned.

## Examples

``` r
  # For the sake of these examples, we will use the example taxonomy and checklist
  system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |> load()
  system.file("extdata", "testChecklist.rda", package="BeeBDC") |> load()

  # Single entry example
testQuery <- BeeBDCQuery(
  beeName = "Lasioglossum bicingulatum",
  searchChecklist = TRUE,
  printAllSynonyms = TRUE,
  beesTaxonomy = testTaxonomy,
  beesChecklist = testChecklist)
#> Error in BeeBDCQuery(beeName = "Lasioglossum bicingulatum", searchChecklist = TRUE,     printAllSynonyms = TRUE, beesTaxonomy = testTaxonomy, beesChecklist = testChecklist): could not find function "BeeBDCQuery"

  # Multiple entry example
testQuery <- BeeBDCQuery(
  beeName = c("Lasioglossum bicingulatum", "Nomada flavopicta",
  "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
  searchChecklist = TRUE,
  printAllSynonyms = TRUE,
  beesTaxonomy = testTaxonomy,
  beesChecklist = testChecklist)
#> Error in BeeBDCQuery(beeName = c("Lasioglossum bicingulatum", "Nomada flavopicta",     "Lasioglossum fijiense (Perkins and Cheesman, 1928)"), searchChecklist = TRUE,     printAllSynonyms = TRUE, beesTaxonomy = testTaxonomy, beesChecklist = testChecklist): could not find function "BeeBDCQuery"
  
    # Example way to examine a report from the output list
  testQuery$checklistReport
#> Error: object 'testQuery' not found


```
