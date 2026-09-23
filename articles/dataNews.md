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

- 2026-07-09 **current**:
  <https://api.figshare.com/v2/file/download/66453893>

  - This version fixes an issue where not all names on Discover Life are
    separated by the usual delimiter and so not all synonyms are
    recognised. Additionally, recent Australian revisions by Leijs and
    Walker are included in addition to the new Discover Life pull. This
    is a major improvement on the prior version.

- 2026-05-04: <https://api.figshare.com/v2/file/download/64193731>

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

  - A completely new 2026 pull is in the making… it is *possible* to get
    early access if you are willing to collaborate.

- *Version 1.3.4 runs of early data* This new run of an early dataset
  from start to finish using BeeBDC version 1.3.4 has the following
  changes:

  - Has the below taxonomic issues fixed. However, this process has
    highlighted that some names are not formatting correctly from the
    Discover Life taxonomy (bee taxonomy version 2026-05-04) and will be
    updated in BeeBDC 1.4.0 or before.

  - Better matches from
    [`BeeBDC::jbd_coordCountryInconsistent()`](https://jbdorey.github.io/BeeBDC/reference/jbd_coordCountryInconsistent.md)
    and
    [`BeeBDC::dateFindR()`](https://jbdorey.github.io/BeeBDC/reference/dateFindR.md).

  - [`BeeBDC::harmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/harmoniseR.md)
    now matches names in the verbatimScientificName column if there is
    no match from the scientificName column.

  - Addition of flags from
    [`BeeBDC::continentOutlieRs()`](https://jbdorey.github.io/BeeBDC/reference/continentOutlieRs.md).

  - [`BeeBDC::flagAbsent()`](https://jbdorey.github.io/BeeBDC/reference/flagAbsent.md)
    also now checks if *individualCount == 0*.

  - Download links:

    - [*05_cleaned_database_2024-02-15_v1_3_4.csv.zip*](https://open.flinders.edu.au/ndownloader/files/69213001)

    - [*05_unCleaned_database_2024-02-15_v1_3_4.csv.zip*](https://open.flinders.edu.au/ndownloader/files/69213007)

- *05_cleaned_database_2024-02-15.csv.zip* and
  *05_unCleaned_database_2024-02-15.csv.zip*

  - Both of these datasets have an issue with 17 scientificNames where
    names were “successfully” matched to genus/subgenus-level taxa. This
    was a problem that was fixed in BeeBDC’s Bee taxonomy list from
    version 1.0.2. However, it was perpetuated because I re-ran the
    dataset thinking to fix it, but needed to run it from an earlier
    step, rather than just over the top of the original data versions.
    Hence, the problems persisted. The problem names are as below and
    are over-applied in the dataset are in the table below. Not as well
    that five species (across 38 records) match a species name where
    they are missing an entry in the *genus* column. These appear in the
    *names_clean* column, however, they are correctly flagged under the
    *.invalidName* column.

  A .csv file with the database_id for the problematic genera records is
  available at **LINK**.

  | **scientificName** | **verbatimScientificName_old** | **n_old** | **verbatimScientificName_v1.3.4** | **n_v1.3.4** |
  |:---|:---|:---|:---|:---|
  | Lasioglossum rohweri | Lasioglossum | 23 | Lasioglossum | 23 |
  | Lasioglossum albuquerquense | Lasioglossum | 5 | Lasioglossum | 5 |
  | Megachile texana | Megachile | 3 | Megachile | 3 |
  | Augochloropsis metallica | Bombus | 2 | Bombus | 2 |
  | Megachile apicalis | Megachile | 1 | Megachile | 1 |
  | Megachile inermis | Bombus | 1 | Bombus | 1 |
  | Andrena columbiana Viereck, 1917 | Andrena | 21534 | NA | NA |
  | Apis mellifera Linnaeus, 1758 | Apis | 3226 | NA | NA |
  | Apis mellifera Linnaeus, 1758 | Apis (Apis) | 45 | NA | NA |
  | Bombus hypnorum (Linnaeus, 1758) | Bombus | 42394 | NA | NA |
  | Bombus hypnorum (Linnaeus, 1758) | Bombus (Psithyrus) | 989 | NA | NA |
  | Bombus hypnorum (Linnaeus, 1758) | Bombus (Melanopygus) | 1 | NA | NA |
  | Bombus hypnorum (Linnaeus, 1758) | Bombus (Pyrobombus) | 4 | NA | NA |
  | Bombus hypnorum (Linnaeus, 1758) | Bombus (Mendacibombus) | 2 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys | 3233 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys (Cyrtocoelioxys) | 228 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys (Glyptocoelioxys) | 112 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys (Acrocoelioxys) | 56 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys (Neocoelioxys) | 13 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys (Melanocoelioxys) | 2 | NA | NA |
  | Coelioxys texanus Cresson, 1872 | Coelioxys (Haplocoelioxys) | 2 | NA | NA |
  | Colletes kincaidii Cockerell, 1898 | Colletes | 13969 | NA | NA |
  | Dasypoda argentata Panzer, 1809 | Dasypoda | 15 | NA | NA |
  | Dufourea longispinis (Wu, 1987) | Halictoides | 4 | NA | NA |
  | Euglossa macrorhyncha Dressler, 1982 | Euglossa | 2389 | NA | NA |
  | Euglossa macrorhyncha Dressler, 1982 | Euglossa (Glossura) | 90 | NA | NA |
  | Halictus simplex Blüthgen, 1923 | Halictus | 25346 | NA | NA |
  | Lasioglossum albipenne (Robertson, 1890) | Lasioglossum | 102320 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus | 18544 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Andrenopsis) | 9 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Baeocolletes) | 6 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Cladocerapis) | 176 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Euryglossidia) | 82 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Exleycolletes) | 22 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Filiglossa) | 86 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Lamprocolletes) | 12 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Leioproctus) | 1230 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Protomorpha) | 58 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Protodiscelis) | 150 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Perditomorpha) | 118 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Tetraglossula) | 90 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Pygopasiphae) | 6 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Nomiocolletes) | 4 | NA | NA |
  | Leioproctus leai (Cockerell, 1913) | Leioproctus (Goniocolletes) | 82 | NA | NA |
  | Macropis steironematis Robertson, 1891 | Macropis | 11 | NA | NA |
  | Megachile brevis Say, 1837 | Megachile | 19739 | NA | NA |
  | Melecta diligens Lieftinck, 1983 | Melecta (Melecta) | 6 | NA | NA |
  | Melecta diligens Lieftinck, 1983 | Melecta | 287 | NA | NA |
  | Melissodes microstictus Cockerell, 1905 | Melissodes | 26646 | NA | NA |
  | Osmia coloradensis Cresson, 1878 | Osmia | 10852 | NA | NA |
  | Xylocopa virginica (Linnaeus, 1771) | Xylocopa | 9448 | NA | NA |
  | Xylocopa virginica (Linnaeus, 1771) | Xylocopa (Koptortosoma) | 1216 | NA | NA |
  | Xylocopa virginica (Linnaeus, 1771) | Xylocopa (Lestis) | 155 | NA | NA |
  | Xylocopa virginica (Linnaeus, 1771) | Xylocopa (Schonnherria) | 75 | NA | NA |
  | Xylocopa virginica (Linnaeus, 1771) | Xylocopa (Mesotrichia) | 18 | NA | NA |

  - Download links:

    - [*05_cleaned_database_2024-02-15.csv.zip*](https://open.flinders.edu.au/ndownloader/files/44554373)

    - [*05_unCleaned_database_2024-02-15.csv.zip*](https://open.flinders.edu.au/ndownloader/files/44554382)
