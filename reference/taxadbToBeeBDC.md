# Import and convert taxadb taxonomies to BeeBDC format

Uses the taxadb R package to download a requested taxonomy and then
transforms it into the input BeeBDC format. This means that any taxonomy
in their databases can be used with BeeBDC. You can also save the output
to your computer and to the R environment for immediate use. See details
below for a list of providers or see
[`taxadb::td_create()`](https://rdrr.io/pkg/taxadb/man/td_create.html).

## Usage

``` r
taxadbToBeeBDC(
  name = NULL,
  rank = NULL,
  provider = "gbif",
  version = "22.12",
  collect = TRUE,
  ignore_case = TRUE,
  db = NULL,
  removeEmptyNames = TRUE,
  outPath = getwd(),
  fileName = NULL,
  ...
)
```

## Arguments

- name:

  Character. Taxonomic scientific name (e.g. "Aves"). As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- rank:

  Character. Taxonomic rank name. (e.g. "class"). As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- provider:

  Character. From which provider should the hierarchy be returned?
  Default is 'gbif', which can also be configured using
  options(default_taxadb_provide = ..."). See
  [`taxadb::td_create()`](https://rdrr.io/pkg/taxadb/man/td_create.html)
  for a list of recognized providers. NOTE: gbif seems to have the
  most-complete columns, especially in terms of
  scientificNameAuthorship, which is important for matching ambiguous
  names. As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- version:

  Character. Which version of the taxadb provider database should we
  use? defaults to latest. See tl_import for details. Default = 22.12.
  As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- collect:

  Logical. Should we return an in-memory data.frame (default, usually
  the most convenient), or a reference to lazy-eval table on disk
  (useful for very large tables on which we may first perform subsequent
  filtering operations.). Default = TRUE. As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- ignore_case:

  Logical. should we ignore case (capitalization) in matching names? Can
  be significantly slower to run. Default = TRUE. As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- db:

  a connection to the taxadb database. See details of
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).
  Default = Null which should work. As defined by
  [`taxadb::filter_rank()`](https://rdrr.io/pkg/taxadb/man/filter_rank.html).

- removeEmptyNames:

  Logical. If True (default), it will remove entries without an entry
  for specificEpithet.

- outPath:

  Character. The path to a directory (folder) in which the output should
  be saved.

- fileName:

  Character. The name of the output file, ending in '.csv'.

- ...:

  Arguments passed to
  [`taxadb::td_create()`](https://rdrr.io/pkg/taxadb/man/td_create.html).

## Value

Returns a taxonomy file (to the R environment and to the disk, if a
fileName is provided) as a tibble that can be used with
[`BeeBDC::harmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/HarmoniseR.md).

## See also

[`beesTaxonomy()`](https://jbdorey.github.io/BeeBDC/reference/beesTaxonomy.md)
for the bee taxonomy and
[`harmoniseR()`](https://jbdorey.github.io/BeeBDC/reference/HarmoniseR.md)
for the taxon-cleaning function where these taxonomies are implemented.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Run the function using the bee genus Apis as an example...
ApisTaxonomy <- BeeBDC::taxadbToBeeBDC(
  name = "Apis",
  rank = "Genus",
  provider = "gbif",
  version = "22.12",
  removeEmptyNames = TRUE,
  outPath = getwd(),
  fileName = NULL,
  ...
  )
  } # }
```
