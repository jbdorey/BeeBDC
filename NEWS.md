# Changes in BeeBDC version 1.1.1
- Trying to close unused connections in formattedCombiner tests that throw errors on CRAN's arm tests (but not reproducible on an M1 or M3 mac that I could test on).
- Removed extra UTF-8 characters causing a note on CRAN's Linux tests.
- Updating citations and such.

# Changes in BeeBDC version 1.1.0 
- A new function added, `BeeBDC::taxadbToBeeBDC()`, that can use the **taxadb** package to download taxonomic data for any taxa. The function will transform the **taxadb** format into the **BeeBDC** format that can be put directly into `BeeBDC::harmoniseR()`. Users may choose their data source (e.g., "gbif" or "itis"), but some formats may be better than others. *Comments and issues are welcome in regards to how well the function works, or not, for your taxon.*
- A minor fix where the legend colours for the `BeeBDC::interactiveMapR()` function were inverted from what they should have been. Thanks to Neil Cobb for pointing this out.
- Minor fixes to `BeeBDC::dateFindR()` to identify more dates and exceptions on the advice of Elsa Youngsteadt.
- Minor update to `BeeBDC::dupeSummary()` to update `igraph::clusters ` to `igraph::components()`; a simple renaming.


# Changes in BeeBDC version 1.0.5 

- Minor alteration to plotFlagSummary to allow the removal of columns to the level where one or more  factors, "Initial","Time","Summary","Taxonomy","Space", are no longer present. Basically, a minor upgrade to make the function resilient to different input data. 
- Fixed an issue caused by a stability fix from leaflet [#884](https://github.com/rstudio/leaflet/pull/884) where the tonerLite base map did not work and so would stop points from showing on the map.


# Changes in BeeBDC version 1.0.4

- Suggestions by Elsa Youngsteadt (North Carolina State University) to allow functionality in harmoniseR to search for name matches in the verbatimScientificName column. This functionality has been optionally added with the checkVerbatim argument (default = FALSE) whereby if TRUE the function will make all checks on the normal name columns and then, *only for those rows that failed*, it will check the verbatimScientificName column for matches. 

- Exception found by Elsa Youngsteadt where repoMerge and attr_builder would fail to complete when multiple families were included in a single GBIF, iDigBio, or SCAN download. This has been updated whereby these functions can now work together in order to identify a multi-family download and label the dataSource column accordingly. This will also be reflected in the metadata and EML data to show the different sources, while maintaining the metadata for each download (doi, link, ...).

- Minor exception found by Elsa Youngsteadt where some synonyms would fail to be matched due to double brackets (e.g., "Lasioglossum (leucocomum) (Lovell)"). This has been fixed by letting harmoniseR stringr::str_replace instead of stringr::str_replace_all when finding matches without subgenus. Additionally, harmoniseR now ignores "non-ambiguous..." flags as these are more notes than actual issues.

- Update to rnaturalearthdata 1.0.0 breaks `BeeBDC::countryOutliers()`. This function now uses the column "iso_a3_eh" instead of "iso_a3". Thanks to @PMassicotte for identifying the issue and the solution.


# Changes in BeeBDC version 1.0.3

- Minor update to jbd_correct_coordinates to maintain functionality with sf version 1.0-15.


# Changes in BeeBDC version 1.0.2

- Minor update to plotFlagSummary to allow individual species maps to get updated.

- Update to atlasDownloader from mjwestgate to work with galah version 2.0.0

- Minor update to the bee taxonomy file (29th November 2023), especially in regards to a few species that were getting associated with genus-only identifications. In particular, users who have downloaded version 1 of the dataset should be careful with the following species: *Coelioxys texanus*, *Lasioglossum albipenne*, *Megachile brevis*, and *Xylocopa virginica*. Likely the verbatimScientificName column could be filtered to remove these issues with something like:
beeData %>% dplyr::filter(verbatimScientificName %in% c("Coelioxys", "Lasioglossum", "Megachile", "Xylocopa")). Thanks to Angela Nava-Bolaños for identifying this issue.


# Changes in BeeBDC version 1.0.1

- ComplexHeatmap has been moved from imports to suggests and chordDiagramR will now ask a user if they want to install BiocManager and ComplexHeatmap (only if each is not already installed) before completing the function. This will simplify the installation process greatly. 

- A new readr function has been added (readr_VicWam), which reads data from the combined Victorian and Western Australian Museums in Australia.

- A typo and potential pitfall has been fixed in countryOutliers where sometimes .sea records were not flagged and where NA record counts for .countryOutlier were not stated in the text output.

- How users access the taxonomy and checklist files were slightly modified to include better warnings to help with troubleshooting as well as specific file-path operations for Windows machines.

- jbd_coordCountryInconsistent was updated to better capture mismatched countries by using more columns to match.

- Tests and workflows were updated in accordingly.


