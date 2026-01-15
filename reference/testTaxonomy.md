# An example of the beesTaxonomy file

A small test taxonomy file for package tests. This dataset was built by
filtering the taxonomy data from the three test datasets, beesFlagged,
beesRaw, bees3sp.

## Usage

``` r
data("testTaxonomy", package = "BeeBDC")
```

## Format

An object of class `"tibble"`

- taxonomic_status:

  Taxonomic status. Values are "accepted" or "synonym"

- source:

  Source of the name.

- accid:

  The id of the accepted taxon name or "0" if taxonomic_status ==
  accepted.

- id:

  The id number for the taxon name.

- kingdom:

  The biological kingdom the taxon belongs to. For bees, kingdom ==
  Animalia.

- phylum:

  The biological phylum the taxon belongs to. For bees, phylum ==
  Arthropoda.

- class:

  The biological class the taxon belongs to. For bees, class == Insecta.

- order:

  The biological order the taxon belongs to. For bees, order ==
  Hymenoptera.

- family:

  The family of bee which the species belongs to.

- subfamily:

  The subfamily of bee which the species belongs to.

- tribe:

  The tribe of bee which the species belongs to.

- subtribe:

  The subtribe of bee which the species belongs to.

- validName:

  The valid scientific name as it should occur in the 'scientificName"
  column in a Darwin Core file.

- canonical:

  The scientificName without the scientificNameAuthority.

- canonical_withFlags:

  The scientificName without the scientificNameAuthority and with
  Discover Life taxonomy flags.

- genus:

  The genus the bee species belongs to.

- subgenus:

  The subgenus the bee species belongs to.

- species:

  The specific epithet for the bee species.

- infraspecies:

  The infraspecific epithet for the bee addressed.

- authorship:

  The author who described the bee species.

- taxon_rank:

  Rank for the bee taxon addressed in the entry.

- notes:

  Additional notes about the name/taxon.

## References

This dataset is a subset of the beesTaxonomy file described in: Dorey,
J.B., Fischer, E.E., Chesshire, P.R., Nava-Bolaños, A., O’Reilly, R.L.,
Bossert, S., Collins, S.M., Lichtenberg, E.M., Tucker, E., Smith-Pardo,
A., Falcon-Brindis, A., Guevara, D.A., Ribeiro, B.R., de Pedro, D.,
Hung, J.K.-L., Parys, K.A., McCabe, L.M., Rogan, M.S., Minckley, R.L.,
Velzco, S.J.E., Griswold, T., Zarrillo, T.A., Jetz, W., Sica, Y.V., Orr,
M.C., Guzman, L.M., Ascher, J., Hughes, A.C. & Cobb, N.S. (2023) A
globally synthesised and flagged bee occurrence dataset and cleaning
workflow. Scientific Data, 10, 1–17.
https://www.doi.org/10.1038/S41597-023-02626-W

## Examples

``` r
beesRaw <- BeeBDC::testTaxonomy
head(testTaxonomy)
#> # A tibble: 6 × 24
#>   flags taxonomic_status source    accid    id kingdom phylum class order family
#>   <chr> <chr>            <chr>     <dbl> <dbl> <chr>   <chr>  <chr> <chr> <chr> 
#> 1 NA    accepted         Discover…     0  2288 Animal… Arthr… Inse… Hyme… Andre…
#> 2 NA    accepted         Discover…     0  4305 Animal… Arthr… Inse… Hyme… Andre…
#> 3 NA    accepted         Discover…     0  4765 Animal… Arthr… Inse… Hyme… Andre…
#> 4 NA    accepted         Discover…     0  4783 Animal… Arthr… Inse… Hyme… Andre…
#> 5 NA    accepted         Discover…     0  5059 Animal… Arthr… Inse… Hyme… Andre…
#> 6 NA    accepted         Discover…     0  5264 Animal… Arthr… Inse… Hyme… Andre…
#> # ℹ 14 more variables: subfamily <chr>, tribe <chr>, subtribe <chr>,
#> #   validName <chr>, canonical <chr>, canonical_withFlags <chr>, genus <chr>,
#> #   subgenus <chr>, species <chr>, infraspecies <chr>, authorship <chr>,
#> #   taxon_rank <chr>, valid <lgl>, notes <chr>
```
