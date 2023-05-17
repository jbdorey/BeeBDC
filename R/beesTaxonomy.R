# Description of the BeeDC taxonomic dataset
# 16th of March 2023

#' A nearly complete taxonomy of bees globally
#'
#' Contains taxonomic information for the bees of the world. Source of taxonomy is listed under 
#' "source" but are mostly derived from the Discover Life website.
#'
#' @docType data
#'
#' @usage data(beesTaxonomy)
#'
#' @format An object of class \code{"tibble"} ## `beesTaxonomy`
#' \describe{
#'  \item{flags}{Flags or comments about the taxon name.}
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
#'  \item{validName}{The valid scientific name as it should occur in the “scientificName” column in a Darwin Core file.}
#'  \item{canonical}{The scientificName without the scientificNameAuthority.}
#'  \item{canonical_withFlags}{The scientificName without the scientificNameAuthority and with Discover Life taxonomy flags.}
#'  \item{genus}{The genus the bee species belongs to.}
#'  \item{subgenus}{The subgenus the bee species belongs to.}
#'  \item{species}{The specific epithet for the bee species.}
#'  \item{infraspecies}{The infraspecific epithet for the bee addressed.}
#'  \item{authorship}{The author who described the bee species.}
#' \item{taxon_rank}{Rank for the bee taxon addressed in the entry.}
#' \item{valid}{Logical for if the name is valid. TRUE == a valid name, FALSE == invalid scientific name.}
#' \item{notes}{Additional notes about the name/taxon.}
#'  
#'  
#' }
#' @references This dataset was created using the Discover Life taxonomy. See rawDataWrite.R
#' for further information on download date in file name.
#' Dataset is from the publication: 
#' DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N., O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M., TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A., RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A., ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO, T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. & COBB, N. S. In review. A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data.
#' @keywords datasets
#' @examples
#'
#' data(beesTaxonomy)
#' head(beesTaxonomy)
#'
"beesTaxonomy"





