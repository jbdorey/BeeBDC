# Description of the BeeBDC test taxonomic dataset
# 17th June 2024

#' An example of the beesTaxonomy file
#'
#'
#' A small test taxonomy file for package tests. This dataset was built by filtering the 
#' taxonomy data from the three test datasets, beesFlagged, beesRaw, bees3sp.
#'
#' @docType data
#'
#' @usage data("testTaxonomy", package = "BeeBDC")
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#'  \item{taxonomic_status}{Taxonomic status. Values are "accepted" or "synonym"}
#'  \item{source}{Source of the name.}
#'  \item{accid}{The id of the accepted taxon name or "0" if taxonomic_status == accepted.}
#'  \item{id}{The id number for the taxon name.}
#'  \item{kingdom}{The biological kingdom the taxon belongs to. For bees, kingdom == Animalia.}
#'  \item{phylum}{The biological phylum the taxon belongs to. For bees, phylum == Arthropoda.}
#'  \item{class}{The biological class the taxon belongs to. For bees, class == Insecta.}
#'  \item{order}{The biological order the taxon belongs to. For bees, order == Hymenoptera.}
#'  \item{family}{The family of bee which the species belongs to.}
#'  \item{subfamily}{The subfamily of bee which the species belongs to.}
#'  \item{tribe}{The tribe of bee which the species belongs to.}
#'  \item{subtribe}{The subtribe of bee which the species belongs to.}
#'  \item{validName}{The valid scientific name as it should occur in the 'scientificName" column in a Darwin Core file.}
#'  \item{canonical}{The scientificName without the scientificNameAuthority.}
#'  \item{canonical_withFlags}{The scientificName without the scientificNameAuthority and with Discover Life taxonomy flags.}
#'  \item{genus}{The genus the bee species belongs to.}
#'  \item{subgenus}{The subgenus the bee species belongs to.}
#'  \item{species}{The specific epithet for the bee species.}
#'  \item{infraspecies}{The infraspecific epithet for the bee addressed.}
#'  \item{authorship}{The author who described the bee species.}
#'  \item{taxon_rank}{Rank for the bee taxon addressed in the entry.}
#'  \item{notes}{Additional notes about the name/taxon.}
#' }
#' @references This dataset is a subset of the beesTaxonomy file described in:
#' Dorey, J.B., Fischer, E.E., Chesshire, P.R., Nava-Bolaños, A., O’Reilly, R.L., Bossert, S., Collins, S.M., Lichtenberg, E.M., Tucker, E., Smith-Pardo, A., Falcon-Brindis, A., Guevara, D.A., Ribeiro, B.R., de Pedro, D., Hung, J.K.-L., Parys, K.A., McCabe, L.M., Rogan, M.S., Minckley, R.L., Velzco, S.J.E., Griswold, T., Zarrillo, T.A., Jetz, W., Sica, Y.V., Orr, M.C., Guzman, L.M., Ascher, J., Hughes, A.C. & Cobb, N.S. (2023) A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data, 10, 1–17. https://www.doi.org/10.1038/S41597-023-02626-W
#' @keywords datasets
#' @examples
#' 
#' beesRaw <- BeeBDC::testTaxonomy
#' head(testTaxonomy)
#' 
"testTaxonomy"





