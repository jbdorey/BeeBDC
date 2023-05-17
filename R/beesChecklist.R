# Description of the BeeDC country checklist dataset
# 16th of March 2023

#' A country-level checklist of bees from Discover Life
#'
#'  
#'  The table contains taxonomic and country information for the bees of the world based on 
#'  data collated on Discover Life. See [BeeDC::beesTaxonomy()] for further context. 
#'
#' @docType data
#'
#' @usage data(beesChecklist)
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#'  \item{validName}{The valid scientificName as it should occur in the scientificName column.}
#'  \item{ISO}{The ISO country name as it occurs on Discover Life.}
#'  \item{Long_country_name}{The full country name as it occurs on Discover Life.}
#'  \item{Country_name}{A short version of the country name.}
#'  \item{Alpha-2}{Alpha-2 from rnaturalearth.}
#'  \item{Alpha-3}{Alpha-3 from rnaturalearth.}
#'  \item{ISO.y}{A longer-format ISO code.}
#'  \item{database_id}{Another format of Alpha-3.}
#'  \item{official}{Official country name = "yes" or only a Discover Life name = "no".}
#'  \item{rNaturalEarth_name}{Country name from rnaturalearth's name_long.}
#'  \item{Country_or_island}{Country or island name from Discover Life.}
#'  \item{Notes}{Discover Life country name notes.}
#'  \item{Freq}{The number of unique species names documented per country.}
#'  \item{matchName}{A simplified validName/scientificName for matching to records and other datasets.}
#'  \item{accid}{The accepted taxon id according to beesTaxonomy. For accepted taxa, accid == 0.}
#'  \item{id}{The id number given to the species name in [BeeDC::beesTaxonomy()].}
#'  \item{family}{Bee family.}
#'  \item{subfamily}{Bee subfamily.}
#'  \item{validName_taxonomy}{The valid species name (validName) from [BeeDC::beesTaxonomy()}
#'  \item{canonical}{The valid species name without scientificNameAuthority.}
#'  \item{canonical_withFlags}{The validName without the scientificNameAuthority but with Discover Life flags.}
#'  \item{genus}{Bee genus.}
#'  \item{subgenus}{Bee subgenus.}
#'  \item{species}{Bee specificEpithet.}
#'  \item{infraspecies}{Bee infraSpecificEpithet.}
#'  \item{authorship}{Bee scientificNameAuthorship.}
#'  \item{taxon_rank}{Rank of the taxon name.}
#'  \item{matchCertainty}{Quality of the name's match to the Discover Life checklist.}
#'  \item{sciName_country}{A simplified merger of validName and country for matching between tables or datasets.}
#'  
#' }
#' @references This dataset was created using the Discover Life checklist and taxonomy. 
#' for further information on download date in file name.
#' Dataset is from the publication: 
#' DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N., O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M., TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A., RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A., ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO, T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. & COBB, N. S. In review. A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data.
#' @keywords datasets
#' @examples
#'
#'## Not run:
#' data(beesChecklist)
#' head(beesChecklist)
#'## End(Not run)
"beesChecklist"





