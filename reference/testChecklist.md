# An example of the beesChecklist file

A small test checklist file for package tests. This dataset was built by
filtering the checklist data from the three test datasets, beesFlagged,
beesRaw, bees3sp.

## Usage

``` r
data("testChecklist", package = "BeeBDC")
```

## Format

An object of class `"tibble"`

- validName:

  The valid scientificName as it should occur in the scientificName
  column.

- DiscoverLife_name:

  The full country name as it occurs on Discover Life.

- rNaturalEarth_name:

  Country name from rnaturalearth's name_long and type = "map_units".

- shortName:

  A short version of the country name.

- continent:

  The continent where that country is found.

- DiscoverLife_ISO:

  The ISO country name as it occurs on Discover Life.

- Alpha-2:

  Alpha-2 from rnaturalearth.

- iso_a3_eh:

  iso_a3_eh from rnaturalearth.

- official:

  Official country name = "yes" or only a Discover Life name = "no".

- Source:

  A text strign denoting the source or author of the name-country pair.

- matchCertainty:

  Quality of the name's match to the Discover Life checklist.

- canonical:

  The valid species name without scientificNameAuthority.

- canonical_withFlags:

  The validName without the scientificNameAuthority but with Discover
  Life flags.

- family:

  Bee family.

- subfamily:

  Bee subfamily.

- genus:

  Bee genus.

- subgenus:

  Bee subgenus.

- infraspecies:

  Bee infraSpecificEpithet.

- species:

  Bee specificEpithet.

- scientificNameAuthorship:

  Bee scientificNameAuthorship.

- taxon_rank:

  Rank of the taxon name.

- Notes:

  Discover Life country name notes.

## References

This dataset is a subset of the beesChecklist file described in: Dorey,
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
beesRaw <- BeeBDC::testChecklist
head(testChecklist)
#> # A tibble: 6 × 24
#>   validName             DiscoverLife_name rNaturalEarth_name shortName continent
#>   <chr>                 <chr>             <chr>              <chr>     <chr>    
#> 1 Bombus lucorum (Linn… Afghanistan       Afghanistan        Afghanis… Asia     
#> 2 Bombus terrestris (L… Afghanistan       Afghanistan        Afghanis… Asia     
#> 3 Apis mellifera Linna… Albania           Albania            Albania   Europe   
#> 4 Bombus lapidarius (L… Albania           Albania            Albania   Europe   
#> 5 Bombus lucorum (Linn… Albania           Albania            Albania   Europe   
#> 6 Bombus pascuorum (Sc… Albania           Albania            Albania   Europe   
#> # ℹ 19 more variables: DiscoverLife_ISO <chr>, `Alpha-2` <chr>,
#> #   iso_a3_eh <chr>, official <chr>, Source <chr>, matchCertainty <chr>,
#> #   canonical <chr>, canonical_withFlags <chr>, family <chr>, subfamily <chr>,
#> #   genus <chr>, subgenus <lgl>, specificEpithet <chr>, species <chr>,
#> #   infraspecies <chr>, scientificNameAuthorship <chr>, taxon_rank <chr>,
#> #   Notes <chr>, name <chr>
```
