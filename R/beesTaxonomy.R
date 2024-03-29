# Description of the BeeBDC taxonomic dataset
# 16th of March 2023

#' Download a nearly complete taxonomy of bees globally
#'
#'
#' Downloads the taxonomic information for the bees of the world. 
#' Source of taxonomy is listed under "source" but are mostly derived from the Discover Life 
#' website. The data will be sourced from the BeeBDC article's Figshare.
#'  
#' Note that sometimes the download might not work without restarting R. In this case, you could
#' alternatively download the dataset from the URL below and then read it in using 
#' `base::readRDS("filePath.Rda")`.
#' 
#' 
#' 
#'  **Column details** 
#'  
#'  **flags** Flags or comments about the taxon name.
#'  
#'  **taxonomic_status** Taxonomic status. Values are "accepted" or "synonym"
#'  
#'  **source** Source of the name.
#'  
#'  **accid** The id of the accepted taxon name or "0" if taxonomic_status == accepted.
#'  
#'  **id** The id number for the taxon name.
#'  
#'  **kingdom** The biological kingdom the taxon belongs to. For bees, kingdom == Animalia.
#'  
#'  **phylum** The biological phylum the taxon belongs to. For bees, phylum == Arthropoda.
#'  
#'  **class** The biological class the taxon belongs to. For bees, class == Insecta.
#'  
#'  **order** The biological order the taxon belongs to. For bees, order == Hymenoptera.
#'
#'  
#'  **family** The family of bee which the species belongs to.
#'  
#'  **subfamily** The subfamily of bee which the species belongs to.
#'  
#'  **tribe** The tribe of bee which the species belongs to.
#'  
#'  **subtribe** The subtribe of bee which the species belongs to.
#'  
#'  **validName** The valid scientific name as it should occur in the “scientificName” column in a Darwin Core file.
#'  
#'  **canonical** The scientificName without the scientificNameAuthority.
#'  
#'  **canonical_withFlags** The scientificName without the scientificNameAuthority and with Discover Life taxonomy flags.
#'  
#'  **genus** The genus the bee species belongs to.
#'  
#'  **subgenus** The subgenus the bee species belongs to.
#'  
#'  **species** The specific epithet for the bee species.
#'  
#'  **infraspecies** The infraspecific epithet for the bee addressed.
#'  
#'  **authorship** The author who described the bee species.
#'  
#'  **taxon_rank** Rank for the bee taxon addressed in the entry.
#'  
#'  **notes** Additional notes about the name/taxon.
#' 
#'
#' @param URL A character vector to the FigShare location of the dataset. The default will be to
#' the most-recent version.
#' @param ... Extra variables that can be passed to [utils::download.file()]
#'
#'
#'  
#' @return A downloaded beesTaxonomy.Rda file in the [tempdir()] and the same tibble returned to
#' the environment.
#'  
#' 
#' @references This dataset was created using the Discover Life taxonomy.
#' Dataset is from the publication: 
#' DOREY, J. B., CHESSHIRE, P. R., BOLAÑOS, A. N., O’REILLY, R. L., BOSSERT, S., COLLINS, S. M., LICHTENBERG, E. M., TUCKER, E., SMITH-PARDO, A., FALCON-BRINDIS, A., GUEVARA, D. A., RIBEIRO, B. R., DE PEDRO, D., FISCHER, E., HUNG, J. K.-L., PARYS, K. A., ROGAN, M. S., MINCKLEY, R. L., VELZCO, S. J. E., GRISWOLD, T., ZARRILLO, T. A., SICA, Y., ORR, M. C., GUZMAN, L. M., ASCHER, J., HUGHES, A. C. & COBB, N. S. In review. A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data.
#' The taxonomy data are mostly compiled from Discover Life data, www.discoverlife.org:
#' ASCHER, J. S. & PICKERING, J. 2020. Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species.
#'
#' @seealso [BeeBDC::taxadbToBeeBDC()] to download any other taxonomy (of any taxa or of bees)
#' and [BeeBDC::harmoniseR()] for the 
#' taxon-cleaning function where these taxonomies are implemented.
#' 
#' @export
#' 
#' @examples
#'\dontrun{
#' beesTaxonomy <- BeeBDC::beesTaxonomy()
#'}
#' 
#'
beesTaxonomy <- function(URL = "https://open.flinders.edu.au/ndownloader/files/43331472",
                         ...){
  destfile <- taxonomy <- attempt <- nAttempts <- error_funcFile <- error_func <-  NULL

  # Set the number of attempts
  nAttempts = 5
  
  # Set up the error message function
  error_func <- function(e){
    message(paste("Download attempt failed..."))
  }
  error_funcFile <- function(e){
    message(paste("Could not read download..."))
  }
  # Check operating system
  OS <- dplyr::if_else(.Platform$OS.type == "unix",
                       "MacLinux",
                       "Windows")
  
  
    # Run a code to download the data and deal with potential internet issues
  taxonomy <- NULL                                 
  attempt <- 1
  suppressWarnings(
  while( is.null(taxonomy) && attempt <= nAttempts) {   
    # Don't attempt for the last attempt
    if(attempt < nAttempts){
      
# WINDOWS
      if(OS != "MacLinux"){
    # Download the file to the outPath 
    tryCatch(utils::download.file(URL, destfile = normalizePath(paste0(tempdir(), 
                                                                       "/beesTaxonomy.Rda"))),
        error = error_func, warning = error_func)
    # Load the file from the outPath
      tryCatch(
    taxonomy <- base::readRDS(normalizePath(paste0(tempdir(), "/beesTaxonomy.Rda"))),
    error = error_funcFile, warning = error_funcFile)
      }else{
        # Download the file to the outPath 
        tryCatch(utils::download.file(URL, destfile = paste0(tempdir(), "/beesTaxonomy.Rda")),
                 error = error_func, warning = error_func)
        # Load the file from the outPath
        tryCatch(
          taxonomy <- base::readRDS(paste0(tempdir(), "/beesTaxonomy.Rda")),
          error = error_funcFile, warning = error_funcFile)
      }
        
        
    } # END if
    
    if(attempt < nAttempts){
      # Wait one second before the next request 
    if(attempt > 1){Sys.sleep(1)            
      print( paste("Attempt: ", attempt, " of ", nAttempts-1))}    # Inform user of number of attempts
    } # END IF #2
    # Count the next attempt
    attempt <- attempt + 1   
  } # END while
  )
  
  if(is.null(taxonomy)){
    message(" - Taxonomy download failed. Please check your internet connection.")
  }

  
  # Return the data to the user
  return(taxonomy)
} # END function



