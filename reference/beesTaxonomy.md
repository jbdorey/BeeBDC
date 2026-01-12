# Download a nearly complete taxonomy of bees globally

Downloads the taxonomic information for the bees of the world. Source of
taxonomy is listed under "source" but are mostly derived from the
Discover Life website. The data will be sourced from the BeeBDC
article's Figshare.

Note that sometimes the download might not work without restarting R. In
this case, you could alternatively download the dataset from the URL
below and then read it in using `base::readRDS("filePath.Rda")`. Note
that as of version 1.3.2, this function internally uses the "download"
function from the `downloader` package on CRAN.

## Usage

``` r
beesTaxonomy(
  URL = "https://open.flinders.edu.au/ndownloader/files/60945820",
  ...
)
```

## Arguments

- URL:

  A character vector to the FigShare location of the dataset. The
  default will be to the most-recent version.

- ...:

  Extra variables that can be passed to `downloader::download()`.

## Value

A downloaded beesTaxonomy.Rda file in the
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and the same tibble
returned to the environment.

## Details

**Column details**

**flags** Flags or comments about the taxon name.

**taxonomic_status** Taxonomic status. Values are "accepted" or
"synonym"

**source** Source of the name.

**accid** The id of the accepted taxon name or "0" if taxonomic_status
== accepted.

**id** The id number for the taxon name.

**kingdom** The biological kingdom the taxon belongs to. For bees,
kingdom == Animalia.

**phylum** The biological phylum the taxon belongs to. For bees, phylum
== Arthropoda.

**class** The biological class the taxon belongs to. For bees, class ==
Insecta.

**order** The biological order the taxon belongs to. For bees, order ==
Hymenoptera.

**family** The family of bee which the species belongs to.

**subfamily** The subfamily of bee which the species belongs to.

**tribe** The tribe of bee which the species belongs to.

**subtribe** The subtribe of bee which the species belongs to.

**validName** The valid scientific name as it should occur in the
“scientificName” column in a Darwin Core file.

**canonical** The scientificName without the scientificNameAuthority.

**canonical_withFlags** The scientificName without the
scientificNameAuthority and with Discover Life taxonomy flags.

**genus** The genus the bee species belongs to.

**subgenus** The subgenus the bee species belongs to.

**species** The specific epithet for the bee species.

**infraspecies** The infraspecific epithet for the bee addressed.

**authorship** The author who described the bee species.

**taxon_rank** Rank for the bee taxon addressed in the entry.

**notes** Additional notes about the name/taxon.

**Previous taxonomies:**

- 2026-01-12 **current**:
  https://open.flinders.edu.au/ndownloader/files/60945820

- 2024-06-17: https://open.flinders.edu.au/ndownloader/files/47089969

- 2023-11-29: https://open.flinders.edu.au/ndownloader/files/43331472

- 2023-10-10: https://open.flinders.edu.au/ndownloader/files/42613126

- 2023-09-20: https://open.flinders.edu.au/ndownloader/files/42402264

- Original: https://open.flinders.edu.au/ndownloader/files/42320595

## References

This dataset was created using the Discover Life taxonomy. Dataset is
from the publication: DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N.,
O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M.,
TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A.,
RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A.,
ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO,
T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. &
COBB, N. S. In review. A globally synthesised and flagged bee occurrence
dataset and cleaning workflow. Scientific Data. The taxonomy data are
mostly compiled from Discover Life data, www.discoverlife.org: ASCHER,
J. S. & PICKERING, J. 2020. Discover Life bee species guide and world
checklist (Hymenoptera: Apoidea: Anthophila).
http://www.discoverlife.org/mp/20q?guide=Apoidea_species.

## See also

[`taxadbToBeeBDC()`](https://jbdorey.github.io/BeeBDC/reference/taxadbToBeeBDC.md)
to download any other taxonomy (of any taxa or of bees) and
[`harmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/HarmoniseR.md)
for the taxon-cleaning function where these taxonomies are implemented.

## Examples

``` r
if (FALSE) { # \dontrun{
beesTaxonomy <- BeeBDC::beesTaxonomy()
} # }

```
