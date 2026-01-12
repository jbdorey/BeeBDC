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
#> Starting taxonomy report...
#> Lasioglossum bicingulatum is an accpeted name with the taxon id number 31378.
#>  - 'Lasioglossum bicingulatum' has the synonyms: 
#> Starting checklist report...
#>  - Lasioglossum bicingulatum (Smith, 1853) is reportedly found in: 
#> Australia
#> The output will be returned as a list with the elements: 'taxonomyReport', 'SynonymReport', and 'checklistReport'. 
#> These can be accessed using 'output'$taxonomyReport, 'output'$SynonymReport, 'output'$checklistReport, or 'output'$failedReport.

  # Multiple entry example
testQuery <- BeeBDC::BeeBDCQuery(
  beeName = c("Lasioglossum bicingulatum", "Nomada flavopicta",
  "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
  searchChecklist = TRUE,
  printAllSynonyms = TRUE,
  beesTaxonomy = testTaxonomy,
  beesChecklist = testChecklist)
#> Starting taxonomy report...
#> Lasioglossum bicingulatum is an accpeted name with the taxon id number 31378.
#> Nomada flavopicta is an accpeted name with the taxon id number 17033.
#>  - 'Lasioglossum bicingulatum' has the synonyms: 
#>  - 'Nomada flavopicta' has the synonyms: 
#> Starting checklist report...
#>  - Lasioglossum bicingulatum (Smith, 1853) is reportedly found in: 
#> Australia
#>  - Nomada flavopicta (Kirby, 1802) is reportedly found in: 
#> Austria, Azerbaijan, Belarus, Brussels, Bulgaria, Croatia, Czech Republic, Denmark, Estonia, Finland, France, Georgia, Germany, Greece, Guernsey, Hungary, Iran, Italy, Jersey, Kazakhstan, Kyrgyzstan, Latvia, Liechtenstein, Lithuania, Luxembourg, Netherlands, Norway, Poland, Romania, Russian Federation, Slovakia, Slovenia, Spain, Sweden, Switzerland, Tajikistan, Turkey, Ukraine, Uzbekistan, United Kingdom
#> The output will be returned as a list with the elements: 'taxonomyReport', 'SynonymReport', and 'checklistReport'. 
#> These can be accessed using 'output'$taxonomyReport, 'output'$SynonymReport, 'output'$checklistReport, or 'output'$failedReport.
  
    # Example way to examine a report from the output list
  testQuery$checklistReport
#> # A tibble: 41 × 23
#>    validName     DiscoverLife_name rNaturalEarth_name shortName DiscoverLife_ISO
#>    <chr>         <chr>             <chr>              <chr>     <chr>           
#>  1 Lasioglossum… Australia         Australia          Australia AS              
#>  2 Nomada flavo… Austria           Austria            Austria   AU              
#>  3 Nomada flavo… Azerbaijan        Azerbaijan         Azerbaij… AJ              
#>  4 Nomada flavo… Belarus           Belarus            Belarus   BO              
#>  5 Nomada flavo… Belgium           Brussels           Belgium   BE              
#>  6 Nomada flavo… Bulgaria          Bulgaria           Bulgaria  BU              
#>  7 Nomada flavo… Croatia           Croatia            Croatia   HR              
#>  8 Nomada flavo… Czechia           Czech Republic     Czechia   EZ              
#>  9 Nomada flavo… Denmark           Denmark            Denmark   DA              
#> 10 Nomada flavo… Estonia           Estonia            Estonia   EN              
#> # ℹ 31 more rows
#> # ℹ 18 more variables: `Alpha-2` <chr>, `Alpha-3` <chr>, official <chr>,
#> #   Source <chr>, matchCertainty <chr>, canonical <chr>,
#> #   canonical_withFlags <chr>, family <chr>, subfamily <chr>, genus <chr>,
#> #   subgenus <lgl>, specificEpithet <chr>, species <chr>, infraspecies <chr>,
#> #   scientificNameAuthorship <chr>, taxon_rank <chr>,
#> #   infraspecificEpithet <chr>, Notes <chr>


```
