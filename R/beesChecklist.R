# Description of the BeeBDC country checklist dataset
# 16th of March 2023

#' A country-level checklist of bees from Discover Life
#'
#'  
#'  The table contains taxonomic and country information for the bees of the world based on 
#'  data collated on Discover Life. See [BeeBDC::beesTaxonomy()] for further context. 
#'
#' @docType data
#' 
#' @usage data("beesChecklist", package = "BeeBDC")
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#'  \item{validName}{The valid scientificName as it should occur in the scientificName column.}
#'  \item{DiscoverLife_name}{The full country name as it occurs on Discover Life.}
#'  \item{rNaturalEarth_name}{Country name from rnaturalearth's name_long.}
#'  \item{shortName}{A short version of the country name.}
#'  \item{DiscoverLife_ISO}{The ISO country name as it occurs on Discover Life.}
#'  \item{Alpha-2}{Alpha-2 from rnaturalearth.}
#'  \item{Alpha-3}{Alpha-3 from rnaturalearth.}
#'  \item{Source}{A text strign denoting the source or author of the name-country pair.}
#'  \item{canonical}{The valid species name without scientificNameAuthority.}
#'  \item{canonical_withFlags}{The validName without the scientificNameAuthority but with Discover Life flags.}
#'  \item{family}{Bee family.}
#'  \item{subfamily}{Bee subfamily.}
#'  \item{genus}{Bee genus.}
#'  \item{subgenus}{Bee subgenus.}
#'  \item{infraspecies}{Bee infraSpecificEpithet.}
#'  \item{species}{Bee specificEpithet.}
#'  \item{scientificNameAuthorship}{Bee scientificNameAuthorship.}
#'  
#' }
#' @references This dataset was created using the Discover Life checklist and taxonomy. 
#' for further information on download date in file name.
#' Dataset is from the publication: 
#' DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N., O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M., TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A., RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A., ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO, T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. & COBB, N. S. In review. A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data.
#' The checklist data are mostly compiled from Discover Life data, www.discoverlife.org:
#' ASCHER, J. S. & PICKERING, J. 2020. Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species.
#' 
#' @keywords datasets
#' @examples
#'
#'## Not run:
#' data("beesChecklist", package = "BeeBDC")
#' head(beesChecklist)
#'## End(Not run)
"beesChecklist"





