# Changes in version 1.3.4

- "Suggested" packages are now used conditionally
- Minor spelling mistake fixed in `BeeBDC::BeeBDCQuery()` 
- Thanks on both counts to Prof. Brian D. Ripley of CRAN!

# Changes in version 1.3.2

- Problems with both `BeeBDC::beesTaxonomy()` and `BeeBDC::beesChecklist()` are resolved by modifying the `downloader::download()` function (from version 0.4.2) function internally. A huge thanks to mitchwebb for offering to fix some of these download issues and to users (Felix Klaus and Nicolas Leclercq) for finding problems and bringing them to my attention! I have also now incorporated more error messages to hopefully better guide users when things are going wrong. Additionally, the function now uses `file.path()` in order to resolve directories and so does not rely on checking the OS (however, the OS test is still in place to help troubleshooting).
- Updates to the `BeeBDC::BeesTaxonomy()` and `BeeBDC::BeesChecklist()` datasets to a 2026 version of Discover Life — J.S. & Pickering, J. (2026) Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). <https://www.discoverlife.org/mp/20q?guide=Apoidea_species>
- For `BeeBDC::ggRichnessWrapper()`, users may change the *fileName* suffix to change the output format (e.g., "pdf" or "jpg"). 
- Changes to `BeeBDC::dateFindR()` by mitchwebb (a huge thanks for your active help to BeeBDC and the broader community). The major changes relate to finding and correctly extracting exceptions. More info can bee seen at the pull request here — https://github.com/jbdorey/BeeBDC/pull/15; mitchwebb:dateFindR-improvements

# Changes in version 1.3.1

-   Turn back on ggplot2 tests after update.
-   Fix issues with richness functions and vignette - name problems and make multi core functions run quietly.
-   Small changes to `BeeBDC::BeesTaxonomy()` and `BeeBDC::BeesChecklist()` to allow users to choose their download method.
-   Fixes for the richness estimation vignette where the download would not work within a markdown environment.

# Changes in version 1.3.0

-   Updates `BeeBDC::flagAbsent()` to also check the *individualCount* count and flag those where *individualCount == 0*.
-   updated `BeeBDC::ColTypeR()` to include a bee-specific Darwin Core data standard using the new argument *standardFormat = "bee"*.
-   Updated `BeeBDC::atlasDownloader()` to use newer `galah` syntax.
-   Updated `BeeBDC::interactiveMapR()` to allow the present plot to be returned in the R environment viewer.
-   Added new functions to estimate species richness across multiple sites and in parallel for added speed.
-   `BeeBDC::richnessPrepR()` takes input occurrence data, taxonomy, and checklists to produce an R data file for the following functions.
-   `BeeBDC::iNEXTwrapper()` is a wrapper for `iNEXT::iNEXT()` to interpolate and extrapolate Hill numbers with order q (rarify species richness).
-   `BeeBDC::ChaoWrapper()` is a wrapper for `SpadeR::ChaoSpecies()` to non-parametrically estimate species richness.
-   `BeeBDC::richnessEstimateR()` Takes an output dataset from [BeeBDC::richnessPrepR()] to estimate species richness using iChao (non-parametric species richness; `BeeBDC::ChaoWrapper()`) and iNEXT (hill numbers; `BeeBDC::iNEXTwrapper()`) for countries, continents, and/or the entire globe.
-   `BeeBDC::ggRichnessWrapper()` Takes the outputs from `BeeBDC::iNEXTwrapper()` and `BeeBDC::ChaoWrapper()` to create a summary table and output figure.
-   `BeeBDC::countryHarmoniseR` added as a helper function to harmonise some country names that are often inconsistent and otherwise problematic. This was going to be an internal function but it has been made available and exported.
-   Added a new test dataset, `beesCountrySubsets` to test the new species richness functions.
-   Added (Vignette)[<https://jbdorey.github.io/BeeBDC/articles/speciesRichness_example.html>] for implementing the above functions.
-   Fixed issue with upcoming ggplot2 version causing test failures. Temporarily will disable tests of ggplot2 type to enable successful publication to CRAN for both BeeBDC and ggplot2.
-   Depends on "R (\>= 4.1.0)" to being usage of \|\> pipe operator.
-   `BeeBDC::atlasDownloader()` now also returns the galah download file to the environment.
-   `BeeBDC::interactiverMapR()` now builds the .countryOutlier and .summary columns if they are not already present in the dataset to allow the function to work on more datasets.
-   `BeeBDC::interactiverMapR()` now allows the inclusion of up to two custom columns to report on in the map. These are input with customColumn1 and customColumn2.
-   Slight re-jig of the main vignettes by splitting out the bee-specific data preparation into its own page.

# Changes in version 1.2.1

-   `BeeBDC::taxadbToBeeBDC()` now prompts users to install taxadb when it's not already installed.
-   Minor updates to fix breaks with external updates.
-   NOTE: A more major update is planned for next year if users have requests or problems, feel free to get in touch and I'll see what's possible given available time.

# Changes in BeeBDC version 1.2.0

-   Release of a new function, `BeeBDC::continentOutlieRs()`, which is conceptually the same as `BeeBDC::countryOutlieRs()` except on a continental level. This function recognises that sometimes knowledge (including data) are insufficient for country-level analyses. A continent-level function might actually see greater use for taxa beyond bees where country-level checklists may not exist but a continent-level checklist could be created.
-   I also provide updates to `BeeBDC::beesTaxonomy()` and `BeeBDC::beesChecklist()` to allow the new function to be used properly. It will also include new tests and added sections to the vignette. I have also added a warning for use of this function with older versions of beesChecklist and added compatibility with `BeeBDC::plotFlagSummary()`.
-   I have added an option in `BeeBDC::summaryFun()` to allow users to specify "onlyFilterThese" instead of "dontFilterThese".

# Changes in BeeBDC version 1.1.1

-   Trying to close unused connections in formattedCombiner tests that throw errors on CRAN's arm tests (but not reproducible on an M1 or M3 mac that I could test on).
-   Removed extra UTF-8 characters causing a note on CRAN's Linux tests.
-   Updating citations and such.

# Changes in BeeBDC version 1.1.0

-   A new function added, `BeeBDC::taxadbToBeeBDC()`, that can use the **taxadb** package to download taxonomic data for any taxa. The function will transform the **taxadb** format into the **BeeBDC** format that can be put directly into `BeeBDC::harmoniseR()`. Users may choose their data source (e.g., "gbif" or "itis"), but some formats may be better than others. *Comments and issues are welcome in regards to how well the function works, or not, for your taxon.*
-   A minor fix where the legend colours for the `BeeBDC::interactiveMapR()` function were inverted from what they should have been. Thanks to Neil Cobb for pointing this out.
-   Minor fixes to `BeeBDC::dateFindR()` to identify more dates and exceptions on the advice of Elsa Youngsteadt.
-   Minor update to `BeeBDC::dupeSummary()` to update `igraph::clusters` to `igraph::components()`; a simple renaming.

# Changes in BeeBDC version 1.0.5

-   Minor alteration to plotFlagSummary to allow the removal of columns to the level where one or more factors, "Initial","Time","Summary","Taxonomy","Space", are no longer present. Basically, a minor upgrade to make the function resilient to different input data.
-   Fixed an issue caused by a stability fix from leaflet [#884](https://github.com/rstudio/leaflet/pull/884) where the tonerLite base map did not work and so would stop points from showing on the map.

# Changes in BeeBDC version 1.0.4

-   Suggestions by Elsa Youngsteadt (North Carolina State University) to allow functionality in harmoniseR to search for name matches in the verbatimScientificName column. This functionality has been optionally added with the checkVerbatim argument (default = FALSE) whereby if TRUE the function will make all checks on the normal name columns and then, *only for those rows that failed*, it will check the verbatimScientificName column for matches.

-   Exception found by Elsa Youngsteadt where repoMerge and attr_builder would fail to complete when multiple families were included in a single GBIF, iDigBio, or SCAN download. This has been updated whereby these functions can now work together in order to identify a multi-family download and label the dataSource column accordingly. This will also be reflected in the metadata and EML data to show the different sources, while maintaining the metadata for each download (doi, link, ...).

-   Minor exception found by Elsa Youngsteadt where some synonyms would fail to be matched due to double brackets (e.g., "Lasioglossum (leucocomum) (Lovell)"). This has been fixed by letting harmoniseR stringr::str_replace instead of stringr::str_replace_all when finding matches without subgenus. Additionally, harmoniseR now ignores "non-ambiguous..." flags as these are more notes than actual issues.

-   Update to rnaturalearthdata 1.0.0 breaks `BeeBDC::countryOutliers()`. This function now uses the column "iso_a3_eh" instead of "iso_a3". Thanks to @PMassicotte for identifying the issue and the solution.

# Changes in BeeBDC version 1.0.3

-   Minor update to jbd_correct_coordinates to maintain functionality with sf version 1.0-15.

# Changes in BeeBDC version 1.0.2

-   Minor update to plotFlagSummary to allow individual species maps to get updated.

-   Update to atlasDownloader from mjwestgate to work with galah version 2.0.0

-   Minor update to the bee taxonomy file (29th November 2023), especially in regards to a few species that were getting associated with genus-only identifications. In particular, users who have downloaded version 1 of the dataset should be careful with the following species: *Coelioxys texanus*, *Lasioglossum albipenne*, *Megachile brevis*, and *Xylocopa virginica*. Likely the verbatimScientificName column could be filtered to remove these issues with something like: beeData %\>% dplyr::filter(verbatimScientificName %in% c("Coelioxys", "Lasioglossum", "Megachile", "Xylocopa")). Thanks to Angela Nava-Bolaños for identifying this issue.

# Changes in BeeBDC version 1.0.1

-   ComplexHeatmap has been moved from imports to suggests and chordDiagramR will now ask a user if they want to install BiocManager and ComplexHeatmap (only if each is not already installed) before completing the function. This will simplify the installation process greatly.

-   A new readr function has been added (readr_VicWam), which reads data from the combined Victorian and Western Australian Museums in Australia.

-   A typo and potential pitfall has been fixed in countryOutliers where sometimes .sea records were not flagged and where NA record counts for .countryOutlier were not stated in the text output.

-   How users access the taxonomy and checklist files were slightly modified to include better warnings to help with troubleshooting as well as specific file-path operations for Windows machines.

-   jbd_coordCountryInconsistent was updated to better capture mismatched countries by using more columns to match.

-   Tests and workflows were updated in accordingly.
