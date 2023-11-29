# Changes in BeeBDC version 1.0.2

- Minor update to plotFlagSummary to allow individual species maps to get updated.

- Update to atlasDownloader from mjwestgate to work with galah version 2.0.0

- Minor update to the bee taxonomy file (29th November 2023), especially in regards to a few species that were getting associated with genus-only identifications. In particular, users who have downloaded version 1 of the dataset should be careful with the following species: *Coelioxys texanus*, *Lasioglossum albipenne*, *Megachile brevis*, and *Xylocopa virginica*. Likely the verbatimScientificName column coudl be filtered to remove these issues with something like:
beeData %>% dplyr::filter(verbatimScientificName %in% c("Coelioxys", "Lasioglossum", "Megachile", "Xylocopa")). Thanks to Angela Nava-Bolaños for identifying this issue.


# Changes in BeeBDC version 1.0.1

- ComplexHeatmap has been moved from imports to suggests and chordDiagramR will now ask a user if they want to install BiocManager and ComplexHeatmap (only if each is not already installed) before completing the function. This will simplify the installation process greatly. 

- A new readr function has been added (readr_VicWam), which reads data from the combined Victorian and Western Australian Museums in Australia.

- A typo and potential pitfall has been fixed in countryOutliers where sometimes .sea records were not flagged and where NA record counts for .countryOutlier were not stated in the text output.

- How users access the taxonomy and checklist files were slightly modified to include better warnings to help with troubleshooting as well as specific file-path operations for Windows machines.

- jbd_coordCountryInconsistent was updated to better capture mismatched countries by using more columns to match.

- Tests and workflows were updated in accordingly.


