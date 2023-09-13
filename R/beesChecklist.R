# Description of the BeeBDC country checklist dataset
# 16th of March 2023

#' Download a country-level checklist of bees from Discover Life
#'
#'  
#'  Download the table contains taxonomic and country information for the bees of the world based 
#'  on data collated on Discover Life. The data will be sourced from the BeeBDC article's 
#'  Figshare.
#'  
#'  See [BeeBDC::beesTaxonomy()] for further context. 
#'
#' @param URL A character vector to the FigShare location of the dataset. The default will be to
#' the most-recent version.
#' @param ... Extra variables that can be passed to [utils::download.file()]
#' 
#' @return A downloaded beesChecklist.Rda file in the outPath and the same tibble returned to
#' the environment.
#' 
#'
#'  **Column details **
#'  
#'  **validName** The valid scientificName as it should occur in the scientificName column.
#'  
#'  **DiscoverLife_name** The full country name as it occurs on Discover Life.
#'  
#'  **rNaturalEarth_name** Country name from rnaturalearth's name_long.
#'  
#'  **shortName** A short version of the country name.
#'  
#'  **DiscoverLife_ISO** The ISO country name as it occurs on Discover Life.
#'  
#'  **Alpha-2** Alpha-2 from rnaturalearth.
#'  
#'  **Alpha-3** Alpha-3 from rnaturalearth.
#'  
#'  **official** Official country name = "yes" or only a Discover Life name = "no".
#'  
#'  **Source** A text strign denoting the source or author of the name-country pair.
#'  
#'  **matchCertainty** Quality of the name's match to the Discover Life checklist.
#'  
#'  **canonical** The valid species name without scientificNameAuthority.
#'  
#'  **canonical_withFlags** The validName without the scientificNameAuthority but with Discover Life flags.
#'  
#'  **family** Bee family.
#'  
#'  **subfamily** Bee subfamily.
#'  
#'  **genus** Bee genus.
#'  
#'  **subgenus** Bee subgenus.
#'  
#'  **infraspecies** Bee infraSpecificEpithet.
#'  
#'  **species** Bee specificEpithet.
#'  
#'  **scientificNameAuthorship** Bee scientificNameAuthorship.
#'  
#'  **taxon_rank** Rank of the taxon name.
#'  
#'  **Notes** Discover Life country name notes.
#'  
#' 
#' @references This dataset was created using the Discover Life checklist and taxonomy. 
#' Dataset is from the publication: 
#' DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N., O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M., TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A., RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A., ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO, T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. & COBB, N. S. In review. A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data.
#' The checklist data are mostly compiled from Discover Life data, www.discoverlife.org:
#' ASCHER, J. S. & PICKERING, J. 2020. Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species.
#' 
#' @export
#' 
#' @examples
#'\dontrun{
#' beesChecklist <- BeeBDC::beesChecklist()
#'}


beesChecklist <- function(URL = "https://figshare.com/ndownloader/files/42320598?private_link=bce1f92848c2ced313ee",
                          ...){
  destfile <- checklist <- attempt <- NULL
  
  # Run a code to download the data and deal with potential internet issues
  checklist <- NULL                                 
  attempt <- 0 
  suppressWarnings(
    while( is.null(checklist) && attempt <= 3) {    
      # Download the file
      try(utils::download.file(URL, destfile = paste0(tempdir(), "/beesChecklist.Rda")),
          silent = TRUE)
      # Load the file 
      try(
        checklist <- base::readRDS(paste0(tempdir(), "/beesChecklist.Rda")),
        silent = TRUE)
      
      # Count the next attempt
      attempt <- attempt + 1       
      # Wait one second before the next request 
      if(attempt > 1){Sys.sleep(1)            
        print( paste("Attempt: ", attempt, " of 4"))}    # Inform user of number of attempts
    } 
  )
  
  if(is.null(checklist)){
    message(" - Checklist download failed. Please check your internet connection.")
  }
  
    # Return the data to the user
  return(checklist)
} # END function





