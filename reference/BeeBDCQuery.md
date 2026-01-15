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
#> Lasioglossum bicingulatum is an accpeted name with the taxon id number 30809.
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
#> Lasioglossum bicingulatum is an accpeted name with the taxon id number 30809.
#> Nomada flavopicta is an accpeted name with the taxon id number 16654.
#>  - 'Lasioglossum bicingulatum' has the synonyms: 
#>  - 'Nomada flavopicta' has the synonyms: 
#> Starting checklist report...
#>  - Lasioglossum bicingulatum (Smith, 1853) is reportedly found in: 
#> Australia
#>  - Nomada flavopicta (Kirby, 1802) is reportedly found in: 
#> Austria, Azerbaijan, Belarus, Brussels, Bulgaria, Croatia, Czech Republic, Denmark, Estonia, Finland, France, Georgia, Germany, Greece, Greenland, Guernsey, Hungary, Iran, Italy, Jersey, Kazakhstan, Kyrgyzstan, Latvia, Liechtenstein, Lithuania, Luxembourg, Netherlands, Norway, Poland, Romania, Russian Federation, Slovakia, Slovenia, Spain, Sweden, Switzerland, Tajikistan, Turkey, Ukraine, Uzbekistan, United Kingdom
#> The output will be returned as a list with the elements: 'taxonomyReport', 'SynonymReport', and 'checklistReport'. 
#> These can be accessed using 'output'$taxonomyReport, 'output'$SynonymReport, 'output'$checklistReport, or 'output'$failedReport.
  
    # Example way to examine a report from the output list
  testQuery$checklistReport
#> # A tibble: 42 × 24
#>    validName            DiscoverLife_name rNaturalEarth_name shortName continent
#>    <chr>                <chr>             <chr>              <chr>     <chr>    
#>  1 Lasioglossum bicing… Australia         Australia          Australia Oceania  
#>  2 Nomada flavopicta (… Austria           Austria            Austria   Europe   
#>  3 Nomada flavopicta (… Azerbaijan        Azerbaijan         Azerbaij… Asia     
#>  4 Nomada flavopicta (… Belarus           Belarus            Belarus   Europe   
#>  5 Nomada flavopicta (… Belgium           Brussels           Belgium   NA       
#>  6 Nomada flavopicta (… Bulgaria          Bulgaria           Bulgaria  Europe   
#>  7 Nomada flavopicta (… Croatia           Croatia            Croatia   Europe   
#>  8 Nomada flavopicta (… Czechia           Czech Republic     Czechia   Europe   
#>  9 Nomada flavopicta (… Denmark           Denmark            Denmark   Europe   
#> 10 Nomada flavopicta (… Estonia           Estonia            Estonia   Europe   
#> # ℹ 32 more rows
#> # ℹ 19 more variables: DiscoverLife_ISO <chr>, `Alpha-2` <chr>,
#> #   iso_a3_eh <chr>, official <chr>, Source <chr>, matchCertainty <chr>,
#> #   canonical <chr>, canonical_withFlags <chr>, family <chr>, subfamily <chr>,
#> #   genus <chr>, subgenus <lgl>, specificEpithet <chr>, species <chr>,
#> #   infraspecies <chr>, scientificNameAuthorship <chr>, taxon_rank <chr>,
#> #   Notes <chr>, name <chr>


```
