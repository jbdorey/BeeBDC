# Data news

## Data updates

Here I will try to manage a list of data updates and the over-arching
changes in the datasets. These changes are commensurate with those in
the “Package news” section where changes of functions within **BeeBDC**
themselves are documented.

### Bees taxonomy

The bee taxonomy files have been changed as below. Most changes
represent a new pull from Discover Life and most also represent new
manual additions due to mistakes, missing names, changes on Discover
Life, or recent taxonomic revisions.

- 2026-05-04 **current**:
  <https://api.figshare.com/v2/file/download/64193731>

- 2026-01-12: <https://open.flinders.edu.au/ndownloader/files/60945820>

- 2024-06-17: <https://open.flinders.edu.au/ndownloader/files/47089969>

- 2023-11-29: <https://open.flinders.edu.au/ndownloader/files/43331472>

- 2023-10-10: <https://open.flinders.edu.au/ndownloader/files/42613126>

- 2023-09-20: <https://open.flinders.edu.au/ndownloader/files/42402264>

- Original: <https://open.flinders.edu.au/ndownloader/files/42320595>

### Bees checklist

The updated checklist files represent new pulls from Discover Life.

- 2026-01-12 **current**:
  <https://open.flinders.edu.au/ndownloader/files/60945823>

- 2024-06-17: <https://open.flinders.edu.au/ndownloader/files/47092720>

- Original: <https://open.flinders.edu.au/ndownloader/files/42320598>

### Bee occurrence dataset

The bee occurrence dataset has ended up being updated least frequently.
This is in part because it takes this longest and because I prioritise
updating the package and allowing researchers to use the datasets and
functions as they see fit. This is, however, naive as many researchers
will simply take the dateset provided and run with it. There are known
issues with the dataset version and, thanks to Zach Portman, it has been
pointed out that these should be made more obvious and documented. This
may not be perfect documentation but major issues are highlighted below.

#### Clean and unclean datasets

- Upcoming version…

  - A completely new 2026 pull is in the making…

- *Version 1.3.4 runs of early data —
  05_cleaned_database_2023v1_3_3.csv.zip* and
  *05_unCleaned_database_2023v1_3_3.csv.zip —* This new run of an early
  dataset from start to finish using BeeBDC version 1.3.4 has the
  following changes:

  - Now has 19,890,804 rows of data (uncleaned)

  - Has the below taxonomic issues fixed. However, this process has
    highlighted that some names are not formatting correctly from the
    Discover Life taxonomy (bee taxonomy version 2026-05-04) and will be
    updated in BeeBDC 1.4.0 or before

  - Better matches from
    [`BeeBDC::jbd_coordCountryInconsistent()`](https://jbdorey.github.io/BeeBDC/reference/jbd_coordCountryInconsistent.md)
    and
    [`BeeBDC::dateFindR()`](https://jbdorey.github.io/BeeBDC/reference/dateFindR.md)

  - [`BeeBDC::harmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/HarmoniseR.md)
    now matches names in the verbatimScientificName column if there is
    no match from the scientificName column

  - Addition of flags from
    [`BeeBDC::continentOutlieRs()`](https://jbdorey.github.io/BeeBDC/reference/continentOutlieRs.md)

  - [`BeeBDC::flagAbsent()`](https://jbdorey.github.io/BeeBDC/reference/flagAbsent.md)
    also now checks if *individualCount == 0*

&nbsp;

- *05_cleaned_database_2024-02-15.csv.zip* and
  *05_unCleaned_database_2024-02-15.csv.zip*

  - Both of these datasets have an issue with 25 names where names were
    “successfully” matched to genus/subgenus-level taxa. This was a
    problem that was fixed in BeeBDC’s Bee taxonomy list from version
    1.0.2. However, it was perpetuated because I re-ran the dataset
    thinking to fix it, but needed to run it from an earlier step,
    rather than just over the top of the original data versions. Hence,
    the problems persisted. The problem names are as below and are
    over-applied in the dataset:
    1.  “Apis mellifera Linnaeus, 1758”
    2.  “Leioproctus leai (Cockerell, 1913)”
    3.  “Leioproctus boroniae (Cockerell, 1921)”  
        \[4\] “Xylocopa virginica (Linnaeus, 1771)”
    4.  “Bombus hypnorum (Linnaeus, 1758)”
    5.  “Coelioxys texanus Cresson, 1872”
    6.  “Dufourea longispinis (Wu, 1987)”
    7.  “Euglossa macrorhyncha Dressler, 1982”
    8.  “Melecta diligens Lieftinck, 1983”
    9.  “Melissodes microstictus Cockerell, 1905”
    10. “Perditomorpha arnaui Moure, 1954”
    11. “Leioproctus michenerianus (E. A. B. Almeida, 2008)”
    12. “Macropis steironematis Robertson, 1891”
    13. “Dasypoda argentata Panzer, 1809”
    14. “Halictus simplex Blüthgen, 1923”
    15. “Lasioglossum albipenne (Robertson, 1890)”
    16. “Andrena columbiana Viereck, 1917”
    17. “Osmia coloradensis Cresson, 1878”
    18. “Colletes kincaidii Cockerell, 1898”
    19. “Lasioglossum Curtis, 1833”
    20. “Halictus Latreille, 1804”
    21. “Andrena Fabricius, 1775”
    22. “Colletes Latreille, 1802”
    23. “Osmia Panzer, 1806”
    24. “Halictus”
  - Download links:
    - [*05_cleaned_database_2024-02-15.csv.zip*](https://open.flinders.edu.au/ndownloader/files/44554373)

    - [*05_unCleaned_database_2024-02-15.csv.zip*](https://open.flinders.edu.au/ndownloader/files/44554382)
