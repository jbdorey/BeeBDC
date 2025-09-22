# Description of the BeeBDC country checklist dataset
# 16th of March 2023

#' Download a country-level checklist of bees from Discover Life
#'
#'  
#' Download the table contains taxonomic and country information for the bees of the world based 
#' on data collated on Discover Life. The data will be sourced from the BeeBDC article's 
#' Figshare.
#'  
#' Note that sometimes the download might not work without restarting R. In this case, you could
#' alternatively download the dataset from the URL below and then read it in using 
#' `base::readRDS("filePath.Rda")`.
#'  
#' @seealso [BeeBDC::beesTaxonomy()] for further context. 
#'
#' @param URL A character vector to the FigShare location of the dataset. The default will be to
#' the most-recent version.
#' @param destfile a character string (or vector, see the url argument) with the file path where 
#' the downloaded file is to be saved. Tilde-expansion is performed. If NULL (default), then destfile
#' is set as `paste0(tempdir(), "/beesChecklist.Rda")` (normalizePath for Windows).
#' @param method The method to be used for downloading files. Current download methods are 
#' "internal", "libcurl", "wget", "curl" and "wininet" (Windows only), and there is a value 
#' "auto": see ‘Details’ and ‘Note’. The method can also be set through the option 
#' "download.file.method": see options(). description
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
#'  **rNaturalEarth_name** Country name from rnaturalearth's name_long and type = "map_units".
#'  
#'  **shortName** A short version of the country name.
#'  
#'  **continent** The continent where that country is found.
#'  
#'  **DiscoverLife_ISO** The ISO country name as it occurs on Discover Life.
#'  
#'  **Alpha-2** Alpha-2 from rnaturalearth.
#'  
#'  **iso_a3_eh** iso_a3_eh from rnaturalearth.
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
#' Dorey, J.B., Fischer, E.E., Chesshire, P.R., Nava-Bolaños, A., O’Reilly, R.L., Bossert, S., Collins, S.M., Lichtenberg, E.M., Tucker, E., Smith-Pardo, A., Falcon-Brindis, A., Guevara, D.A., Ribeiro, B.R., de Pedro, D., Hung, J.K.-L., Parys, K.A., McCabe, L.M., Rogan, M.S., Minckley, R.L., Velzco, S.J.E., Griswold, T., Zarrillo, T.A., Jetz, W., Sica, Y.V., Orr, M.C., Guzman, L.M., Ascher, J., Hughes, A.C. & Cobb, N.S. (2023) A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data, 10, 1–17. https://www.doi.org/10.1038/S41597-023-02626-W
#' The checklist data are mostly compiled from Discover Life data, www.discoverlife.org:
#' Ascher, J.S. & Pickering, J. (2020) Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species
#' 
#' @export
#' 
#' @examples
#'\dontrun{
#' beesChecklist <- BeeBDC::beesChecklist()
#'}
beesChecklist <- function(URL = "https://figshare.com/ndownloader/files/47092720",
                          destfile = NULL, method = "auto",
                          ...){
  destfile <- checklist <- attempt <- nAttempts <- error_funcFile <- error_func <- NULL
  
  # Set the number of attempts
  nAttempts = 5
    
    # Set up the error message function
  error_func <- function(e){
    message(paste("Checklist download attempt failed..."))
  }
  error_funcFile <- function(e){
    message(paste("Could not read checklist download..."))
  }
  
    # Check operating system
  OS <- dplyr::if_else(.Platform$OS.type == "unix",
                                             "MacLinux",
                                             "Windows")
  
  # Run a code to download the data and deal with potential internet issues
  checklist <- NULL                                 
  attempt <- 1 
  suppressWarnings(
    while( is.null(checklist) && attempt <= nAttempts) {    
        # Don't attempt for the last attempt
      if(attempt < nAttempts){
        
  # WINDOWS
        if(OS != "MacLinux"){
            # Set destfile if it's not provided by user
          if(is.null(destfile)){
          destfile <- normalizePath(paste0(tempdir(),
                               "/beesChecklist.Rda"))}
      # Download the file
      tryCatch(utils::download.file(URL, 
                                    destfile = destfile,
                                    method = method,
                                    ...),
          error = error_func, warning = error_func)
      # Load the file 
        tryCatch(
        checklist <- base::readRDS(normalizePath(paste0(tempdir(), "/beesChecklist.Rda"))),
        error = error_funcFile, warning = error_funcFile)
        }else{
  # MAC OR LINUX
          
          # Set destfile if it's not provided by user
          if(is.null(destfile)){
            destfile <- paste0(tempdir(), "/beesChecklist.Rda")}
          # Download the file
          tryCatch(utils::download.file(URL, destfile = destfile,
                                        method = method,
                                        ...),
                   error = error_func, warning = error_func)
          # Load the file 
          tryCatch(
            checklist <- base::readRDS(paste0(tempdir(), "/beesChecklist.Rda")),
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
  
  if(is.null(checklist)){
    message(" - Checklist download failed. Please check your internet connection.")
  }
  
    # Return the data to the user
  return(checklist)
} # END function





