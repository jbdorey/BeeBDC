# Sets up column names and types

This function uses
[`readr::cols_only()`](https://readr.tidyverse.org/reference/cols.html)
to assign a column name and the type of data (e.g.,
[`readr::col_character()`](https://readr.tidyverse.org/reference/parse_atomic.html),
and
[`readr::col_integer()`](https://readr.tidyverse.org/reference/parse_atomic.html)).
To see the default columns simply run `ColTypeR()`. This is intended for
use with
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).
Columns that are not present will NOT be included in the resulting
tibble unless they are specified using
[...](https://rdrr.io/r/base/dots.html).

## Usage

``` r
ColTypeR(..., standardFormat = NULL)
```

## Arguments

- ...:

  Additional arguments. These can be specified in addition to the ones
  default to the function. For example:

  - newCharacterColumn =
    [`readr::col_character()`](https://readr.tidyverse.org/reference/parse_atomic.html),

  - newNumericColumn =
    [`readr::col_integer()`](https://readr.tidyverse.org/reference/parse_atomic.html),

  - newLogicalColumn =
    [`readr::col_logical()`](https://readr.tidyverse.org/reference/parse_atomic.html)

- standardFormat:

  Character. Some taxa may have a standard format for data. Presently,
  Only bees have had this encoded here as "bee". Default = NULL.

## Value

Returns an object of class col_spec. See
[`readr::as.col_spec()`](https://readr.tidyverse.org/reference/as.col_spec.html)
for additional context and explication.

## References

For using the bee standard — Clos, B. D., Seltmann, K. C., Turley, N.
E., Maffei, C., Tucker, E. M., Lane, I. G., Levenson, H. K., & Woodard,
H. S. (2025). Improving the standardization of wild bee occurrence data:
towards a formal wild bee data standard. Authorea. doi:
https://doi.org/10.22541/au.173862402.22787949/v2

## Examples

``` r
  # You can simply return the below for default values
  library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
BeeBDC::ColTypeR() 
#> cols_only(
#>   database_id = col_character(),
#>   scientificName = col_character(),
#>   family = col_character(),
#>   subfamily = col_character(),
#>   genus = col_character(),
#>   subgenus = col_character(),
#>   subspecies = col_character(),
#>   species = col_character(),
#>   specificEpithet = col_character(),
#>   infraspecificEpithet = col_character(),
#>   acceptedNameUsage = col_character(),
#>   taxonRank = col_character(),
#>   scientificNameAuthorship = col_character(),
#>   identificationQualifier = col_character(),
#>   higherClassification = col_character(),
#>   identificationReferences = col_character(),
#>   typeStatus = col_character(),
#>   previousIdentifications = col_character(),
#>   verbatimIdentification = col_character(),
#>   identifiedBy = col_character(),
#>   dateIdentified = col_character(),
#>   decimalLatitude = col_double(),
#>   decimalLongitude = col_double(),
#>   verbatimLatitude = col_character(),
#>   verbatimLongitude = col_character(),
#>   verbatimElevation = col_character(),
#>   stateProvince = col_character(),
#>   country = col_character(),
#>   continent = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   locality = col_character(),
#>   island = col_character(),
#>   county = col_character(),
#>   municipality = col_character(),
#>   countryCode = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level0Gid = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level0Name = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level1Gid = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level1Name = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   license = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   issue = col_character(),
#>   eventDate = col_character(),
#>   eventTime = col_character(),
#>   startDayOfYear = col_integer(),
#>   endDayOfYear = col_integer(),
#>   day = col_integer(),
#>   month = col_integer(),
#>   year = col_integer(),
#>   basisOfRecord = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   type = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   occurrenceStatus = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   recordNumber = col_character(),
#>   recordedBy = col_character(),
#>   eventID = col_character(),
#>   Location = col_character(),
#>   samplingProtocol = col_character(),
#>   samplingEffort = col_character(),
#>   fieldNumber = col_character(),
#>   individualCount = col_double(),
#>   organismQuantity = col_double(),
#>   coordinatePrecision = col_double(),
#>   coordinateUncertaintyInMeters = col_double(),
#>   spatiallyValid = col_logical(),
#>   catalogNumber = col_character(),
#>   gbifID = col_character(),
#>   datasetID = col_character(),
#>   institutionCode = col_character(),
#>   datasetName = col_character(),
#>   otherCatalogNumbers = col_character(),
#>   occurrenceID = col_character(),
#>   taxonKey = col_character(),
#>   coreid = col_character(),
#>   recordId = col_character(),
#>   collectionID = col_character(),
#>   associatedSequences = col_character(),
#>   institutionID = col_character(),
#>   verbatimScientificName = col_character(),
#>   verbatimEventDate = col_character(),
#>   associatedTaxa = col_character(),
#>   associatedOrganisms = col_character(),
#>   fieldNotes = col_character(),
#>   sex = col_character(),
#>   rights = col_character(),
#>   rightsHolder = col_character(),
#>   accessRights = col_character(),
#>   dctermsLicense = col_character(),
#>   dctermsType = col_character(),
#>   dctermsAccessRights = col_character(),
#>   associatedReferences = col_character(),
#>   bibliographicCitation = col_character(),
#>   dctermsBibliographicCitation = col_character(),
#>   references = col_character(),
#>   flags = col_character(),
#>   informationWithheld = col_character(),
#>   isDuplicateOf = col_character(),
#>   hasCoordinate = col_logical(),
#>   hasGeospatialIssues = col_logical(),
#>   assertions = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   occurrenceYear = col_datetime(format = ""),
#>   id = col_character(),
#>   duplicateStatus = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   associatedOccurrences = col_character(),
#>   locationRemarks = col_character(),
#>   dataSource = col_character(),
#>   dataBase_scientificName = col_character(),
#>   .rou = col_logical(),
#>   .val = col_logical(),
#>   .equ = col_logical(),
#>   .zer = col_logical(),
#>   .cap = col_logical(),
#>   .cen = col_logical(),
#>   .sea = col_logical(),
#>   .otl = col_logical(),
#>   .gbf = col_logical(),
#>   .inst = col_logical(),
#>   .dpl = col_logical(),
#>   .summary = col_logical(),
#>   names_clean = col_character(),
#>   verbatim_scientificName = col_character(),
#>   .uncer_terms = col_logical(),
#>   .eventDate_empty = col_logical(),
#>   .year_outOfRange = col_logical(),
#>   .duplicates = col_logical(),
#>   .lonFlag = col_logical(),
#>   .latFlag = col_logical(),
#>   .gridSummary = col_logical(),
#>   .basisOfRecords_notStandard = col_logical(),
#>   .scientificName_empty = col_logical(),
#>   .coordinates_empty = col_logical(),
#>   .coordinates_outOfRange = col_logical(),
#>   coordinates_transposed = col_logical(),
#>   country_suggested = col_character(),
#>   .countryOutlier = col_logical(),
#>   countryMatch = col_character(),
#>   .expertOutlier = col_logical(),
#>   .occurrenceAbsent = col_logical(),
#>   .coordinates_country_inconsistent = col_logical(),
#>   .unLicensed = col_logical(),
#>   .invalidName = col_logical(),
#>   .sequential = col_logical(),
#>   idContinuity = col_logical(),
#>   .uncertaintyThreshold = col_logical(),
#>   .GBIFflags = col_logical(),
#>   finalLatitude = col_double(),
#>   finalLongitude = col_double(),
#>   Source = col_character()
#> )

  # To add new columns you can write
ColTypeR(newCharacterColumn = readr::col_character(), 
         newNumericColumn = readr::col_integer(), 
         newLogicalColumn = readr::col_logical()) 
#> cols_only(
#>   database_id = col_character(),
#>   scientificName = col_character(),
#>   family = col_character(),
#>   subfamily = col_character(),
#>   genus = col_character(),
#>   subgenus = col_character(),
#>   subspecies = col_character(),
#>   species = col_character(),
#>   specificEpithet = col_character(),
#>   infraspecificEpithet = col_character(),
#>   acceptedNameUsage = col_character(),
#>   taxonRank = col_character(),
#>   scientificNameAuthorship = col_character(),
#>   identificationQualifier = col_character(),
#>   higherClassification = col_character(),
#>   identificationReferences = col_character(),
#>   typeStatus = col_character(),
#>   previousIdentifications = col_character(),
#>   verbatimIdentification = col_character(),
#>   identifiedBy = col_character(),
#>   dateIdentified = col_character(),
#>   decimalLatitude = col_double(),
#>   decimalLongitude = col_double(),
#>   verbatimLatitude = col_character(),
#>   verbatimLongitude = col_character(),
#>   verbatimElevation = col_character(),
#>   stateProvince = col_character(),
#>   country = col_character(),
#>   continent = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   locality = col_character(),
#>   island = col_character(),
#>   county = col_character(),
#>   municipality = col_character(),
#>   countryCode = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level0Gid = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level0Name = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level1Gid = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   level1Name = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   license = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   issue = col_character(),
#>   eventDate = col_character(),
#>   eventTime = col_character(),
#>   startDayOfYear = col_integer(),
#>   endDayOfYear = col_integer(),
#>   day = col_integer(),
#>   month = col_integer(),
#>   year = col_integer(),
#>   basisOfRecord = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   type = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   occurrenceStatus = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   recordNumber = col_character(),
#>   recordedBy = col_character(),
#>   eventID = col_character(),
#>   Location = col_character(),
#>   samplingProtocol = col_character(),
#>   samplingEffort = col_character(),
#>   fieldNumber = col_character(),
#>   individualCount = col_double(),
#>   organismQuantity = col_double(),
#>   coordinatePrecision = col_double(),
#>   coordinateUncertaintyInMeters = col_double(),
#>   spatiallyValid = col_logical(),
#>   catalogNumber = col_character(),
#>   gbifID = col_character(),
#>   datasetID = col_character(),
#>   institutionCode = col_character(),
#>   datasetName = col_character(),
#>   otherCatalogNumbers = col_character(),
#>   occurrenceID = col_character(),
#>   taxonKey = col_character(),
#>   coreid = col_character(),
#>   recordId = col_character(),
#>   collectionID = col_character(),
#>   associatedSequences = col_character(),
#>   institutionID = col_character(),
#>   verbatimScientificName = col_character(),
#>   verbatimEventDate = col_character(),
#>   associatedTaxa = col_character(),
#>   associatedOrganisms = col_character(),
#>   fieldNotes = col_character(),
#>   sex = col_character(),
#>   rights = col_character(),
#>   rightsHolder = col_character(),
#>   accessRights = col_character(),
#>   dctermsLicense = col_character(),
#>   dctermsType = col_character(),
#>   dctermsAccessRights = col_character(),
#>   associatedReferences = col_character(),
#>   bibliographicCitation = col_character(),
#>   dctermsBibliographicCitation = col_character(),
#>   references = col_character(),
#>   flags = col_character(),
#>   informationWithheld = col_character(),
#>   isDuplicateOf = col_character(),
#>   hasCoordinate = col_logical(),
#>   hasGeospatialIssues = col_logical(),
#>   assertions = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   occurrenceYear = col_datetime(format = ""),
#>   id = col_character(),
#>   duplicateStatus = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#>   associatedOccurrences = col_character(),
#>   locationRemarks = col_character(),
#>   dataSource = col_character(),
#>   dataBase_scientificName = col_character(),
#>   .rou = col_logical(),
#>   .val = col_logical(),
#>   .equ = col_logical(),
#>   .zer = col_logical(),
#>   .cap = col_logical(),
#>   .cen = col_logical(),
#>   .sea = col_logical(),
#>   .otl = col_logical(),
#>   .gbf = col_logical(),
#>   .inst = col_logical(),
#>   .dpl = col_logical(),
#>   .summary = col_logical(),
#>   names_clean = col_character(),
#>   verbatim_scientificName = col_character(),
#>   .uncer_terms = col_logical(),
#>   .eventDate_empty = col_logical(),
#>   .year_outOfRange = col_logical(),
#>   .duplicates = col_logical(),
#>   .lonFlag = col_logical(),
#>   .latFlag = col_logical(),
#>   .gridSummary = col_logical(),
#>   .basisOfRecords_notStandard = col_logical(),
#>   .scientificName_empty = col_logical(),
#>   .coordinates_empty = col_logical(),
#>   .coordinates_outOfRange = col_logical(),
#>   coordinates_transposed = col_logical(),
#>   country_suggested = col_character(),
#>   .countryOutlier = col_logical(),
#>   countryMatch = col_character(),
#>   .expertOutlier = col_logical(),
#>   .occurrenceAbsent = col_logical(),
#>   .coordinates_country_inconsistent = col_logical(),
#>   .unLicensed = col_logical(),
#>   .invalidName = col_logical(),
#>   .sequential = col_logical(),
#>   idContinuity = col_logical(),
#>   .uncertaintyThreshold = col_logical(),
#>   .GBIFflags = col_logical(),
#>   finalLatitude = col_double(),
#>   finalLongitude = col_double(),
#>   Source = col_character(),
#>   newCharacterColumn = col_character(),
#>   newNumericColumn = col_integer(),
#>   newLogicalColumn = col_logical()
#> )

# Try reading in one of the test datasets as an example:
beesFlagged %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR())
#> # A tibble: 100 × 124
#>    database_id scientificName family subfamily genus subgenus subspecies species
#>    <chr>       <chr>          <chr>  <chr>     <chr> <chr>    <lgl>      <chr>  
#>  1 Dorey_data… Pseudoanthidi… Megac… Megachil… Pseu… NA       NA         Pseudo…
#>  2 Dorey_data… Macrotera arc… Andre… Panurgin… Macr… NA       NA         Macrot…
#>  3 Dorey_data… Xanthesma fur… Colle… Euryglos… Xant… NA       NA         Xanthe…
#>  4 Dorey_data… Exomalopsis s… Apidae Apinae    Exom… NA       NA         Exomal…
#>  5 Dorey_data… Osmia bicolor… Megac… Megachil… Osmia NA       NA         Osmia …
#>  6 Paige_data… Augochlorella… Halic… Halictin… Augo… NA       NA         Augoch…
#>  7 Dorey_data… Megachile api… Megac… Megachil… Mega… NA       NA         Megach…
#>  8 Dorey_data… Trigona dalla… Apidae Apinae    Trig… NA       NA         Trigon…
#>  9 Dorey_data… Habropoda mis… Apidae Apinae    Habr… NA       NA         Habrop…
#> 10 Dorey_data… Lasioglossum … Halic… Halictin… Lasi… NA       NA         Lasiog…
#> # ℹ 90 more rows
#> # ℹ 116 more variables: specificEpithet <chr>, infraspecificEpithet <chr>,
#> #   acceptedNameUsage <lgl>, taxonRank <chr>, scientificNameAuthorship <chr>,
#> #   identificationQualifier <lgl>, higherClassification <chr>,
#> #   identificationReferences <lgl>, typeStatus <chr>,
#> #   previousIdentifications <chr>, verbatimIdentification <chr>,
#> #   identifiedBy <chr>, dateIdentified <chr>, decimalLatitude <dbl>, …
  # OR
beesRaw %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR())
#> # A tibble: 100 × 90
#>    database_id scientificName family subfamily genus subgenus subspecies species
#>    <chr>       <chr>          <chr>  <chr>     <chr> <chr>    <lgl>      <chr>  
#>  1 Dorey_data… Pseudoanthidi… Megac… Megachil… Pseu… NA       NA         Pseudo…
#>  2 Dorey_data… Macrotera arc… Andre… Panurgin… Macr… NA       NA         Macrot…
#>  3 Dorey_data… Xanthesma fur… Colle… Euryglos… Xant… NA       NA         Xanthe…
#>  4 Dorey_data… Exomalopsis s… Apidae Apinae    Exom… NA       NA         Exomal…
#>  5 Dorey_data… Osmia bicolor… Megac… Megachil… Osmia NA       NA         Osmia …
#>  6 Paige_data… Augochlorella… Halic… Halictin… Augo… NA       NA         Augoch…
#>  7 Dorey_data… Megachile api… Megac… Megachil… Mega… NA       NA         Megach…
#>  8 Dorey_data… Trigona dalla… Apidae Apinae    Trig… NA       NA         Trigon…
#>  9 Dorey_data… Habropoda mis… Apidae Apinae    Habr… NA       NA         Habrop…
#> 10 Dorey_data… Lasioglossum … Halic… Halictin… Lasi… NA       NA         Lasiog…
#> # ℹ 90 more rows
#> # ℹ 82 more variables: specificEpithet <chr>, infraspecificEpithet <chr>,
#> #   acceptedNameUsage <lgl>, taxonRank <chr>, scientificNameAuthorship <chr>,
#> #   identificationQualifier <lgl>, higherClassification <chr>,
#> #   identificationReferences <lgl>, typeStatus <chr>,
#> #   previousIdentifications <chr>, verbatimIdentification <chr>,
#> #   identifiedBy <chr>, dateIdentified <chr>, decimalLatitude <dbl>, …

  # OR, using the bee standard format from:
  
beesRaw %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR(standardFormat = "bee"))
#> # A tibble: 100 × 90
#>    database_id scientificName family subfamily genus subgenus subspecies species
#>    <chr>       <chr>          <chr>  <chr>     <chr> <chr>    <lgl>      <chr>  
#>  1 Dorey_data… Pseudoanthidi… Megac… Megachil… Pseu… NA       NA         Pseudo…
#>  2 Dorey_data… Macrotera arc… Andre… Panurgin… Macr… NA       NA         Macrot…
#>  3 Dorey_data… Xanthesma fur… Colle… Euryglos… Xant… NA       NA         Xanthe…
#>  4 Dorey_data… Exomalopsis s… Apidae Apinae    Exom… NA       NA         Exomal…
#>  5 Dorey_data… Osmia bicolor… Megac… Megachil… Osmia NA       NA         Osmia …
#>  6 Paige_data… Augochlorella… Halic… Halictin… Augo… NA       NA         Augoch…
#>  7 Dorey_data… Megachile api… Megac… Megachil… Mega… NA       NA         Megach…
#>  8 Dorey_data… Trigona dalla… Apidae Apinae    Trig… NA       NA         Trigon…
#>  9 Dorey_data… Habropoda mis… Apidae Apinae    Habr… NA       NA         Habrop…
#> 10 Dorey_data… Lasioglossum … Halic… Halictin… Lasi… NA       NA         Lasiog…
#> # ℹ 90 more rows
#> # ℹ 82 more variables: specificEpithet <chr>, infraspecificEpithet <chr>,
#> #   acceptedNameUsage <lgl>, taxonRank <chr>, scientificNameAuthorship <chr>,
#> #   identificationQualifier <lgl>, higherClassification <chr>,
#> #   identificationReferences <lgl>, typeStatus <chr>,
#> #   previousIdentifications <chr>, verbatimIdentification <chr>,
#> #   identifiedBy <chr>, dateIdentified <chr>, decimalLatitude <dbl>, …

```
