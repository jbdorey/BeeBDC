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
#'  **validName** The valid scientific name as it should occur in the 'scientificName" column in a Darwin Core file.
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
#' @param destfile a character string (or vector, see the url argument) with the file path where 
#' the downloaded file is to be saved. Tilde-expansion is performed. If NULL (default), then destfile
#' is set as `paste0(tempdir(), "/beesTaxonomy.Rda")` (normalizePath for Windows).
#' @param method The method to be used for downloading files. Current download methods are 
#' "internal", "libcurl", "wget", "curl" and "wininet" (Windows only), and there is a value 
#' "auto": see ‘Details’ and ‘Note’. The method can also be set through the option 
#' "download.file.method": see options(). description
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
#' Dorey, J.B., Fischer, E.E., Chesshire, P.R., Nava-Bolaños, A., O’Reilly, R.L., Bossert, S., Collins, S.M., Lichtenberg, E.M., Tucker, E., Smith-Pardo, A., Falcon-Brindis, A., Guevara, D.A., Ribeiro, B.R., de Pedro, D., Hung, J.K.-L., Parys, K.A., McCabe, L.M., Rogan, M.S., Minckley, R.L., Velzco, S.J.E., Griswold, T., Zarrillo, T.A., Jetz, W., Sica, Y.V., Orr, M.C., Guzman, L.M., Ascher, J., Hughes, A.C. & Cobb, N.S. (2023) A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data, 10, 1–17. https://www.doi.org/10.1038/S41597-023-02626-W
#' The taxonomy data are mostly compiled from Discover Life data, www.discoverlife.org:
#' Ascher, J.S. & Pickering, J. (2020) Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species
#'
#' @seealso [BeeBDC::taxadbToBeeBDC()] to download any other taxonomy (of any taxa or of bees)
#' and [BeeBDC::harmoniseR()] for the 
#' taxon-cleaning function where these taxonomies are implemented. It may also be worth seeing 
#' [BeeBDC::beesChecklist()].
#' 
#' @export
#' 
#' @examples
#'\dontrun{
#' beesTaxonomy <- BeeBDC::beesTaxonomy()
#'}
#' 
#'
beesTaxonomy <- function(URL = "https://open.flinders.edu.au/ndownloader/files/47089969",
                         destfile = NULL, method = "auto",
                         ...){
  destfile <- taxonomy <- attempt <- nAttempts <- error_funcFile <- error_func <-  NULL

  # Set the number of attempts
  nAttempts = 5
  
  # Set up the error message function
  error_func <- function(e){
    message(paste("Taxonomy download attempt failed..."))
  }
  error_funcFile <- function(e){
    message(paste("Could not read taxonomy download..."))
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
        # Set destfile if it's not provided by user
        if(is.null(destfile)){
          destfile <- normalizePath(paste0(tempdir(),
                                           "/beesTaxonomy.Rda"))}
    # Download the file to the outPath 
    tryCatch(utils::download.file(URL, destfile = destfile,
                                  method = method,
                                  ...),
        error = error_func, warning = error_func)
    # Load the file from the outPath
      tryCatch(
    taxonomy <- base::readRDS(normalizePath(paste0(tempdir(), "/beesTaxonomy.Rda"))),
    error = error_funcFile, warning = error_funcFile)
      }else{
        
        # Set destfile if it's not provided by user
        if(is.null(destfile)){
          destfile <- paste0(tempdir(), "/beesTaxonomy.Rda")}
        # Download the file to the outPath 
        tryCatch(utils::download.file(URL, destfile = destfile,
                                      method = method,
                                      ...),
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



