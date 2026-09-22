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
#' `base::readRDS("filePath.Rda")`. Note that as of version 1.3.2, this function internally uses 
#'  a modified version of the "download" function from the 
#' `downloader` package on CRAN. Additionally, BeeBDC will try a different 
#' download method with each failed attempt (for Windows: "auto" > "internal" > "libcurl" > "wget" > "curl"; 
#` for Mac/Unix: "libcurl" > "wget" > "curl" > "auto";
#' or "auto" if any particular method is not available).
#'  
#'  See [BeeBDC::beesTaxonomy()] for further context. 
#'
#' @param URL A character vector to the FigShare location of the dataset. The default will be to
#' the most-recent version.
#' @param mode A character passed on to `utils::download.file()`. Default = "wb" for Windows or "w" for Mac/Linux.
#' @param headers Character. Passed on to  `utils::download.file()`. Default = NULL.
#' @param token Character. Optional personal access token from FigShare account for authorisation. 
#' Default = NULL.
#' @param alternateURL Logical. If TRUE then the function will use the an alernate version of the 
#' download URL. Might be worth trying if it is failing. Default = FALSE.
#' @param ... Extra variables that can be passed to `downloader::download()`.
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
#'  **Previous checklists:**
#'  
#'   - 2026-01-12 **current**: https://open.flinders.edu.au/ndownloader/files/60945823
#'  
#'   - 2024-06-17: https://open.flinders.edu.au/ndownloader/files/47092720
#'   
#'   - Original: https://open.flinders.edu.au/ndownloader/files/42320598
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
beesChecklist <- function(URL = "https://api.figshare.com/v2/file/download/60945823",
                          mode = NULL,
                          headers = NULL,
                          token = NULL,
                          alternateURL = FALSE,
                          ...){
  destfile <- checklist <- attempt <- nAttempts <- error_funcFile <- error_func <- NULL
  downloadReturn <- . <- NULL
  
    # If user wants to try the Flinders URL, which seems to not be working as well
  if(alternateURL == TRUE){
    URL <- URL %>%
      stringr::str_remove_all(".*/") %>%
      stringr::str_c("https://open.flinders.edu.au/ndownloader/files/", .)
  }
  
  #### 0.0 Prep ####
  # Set the number of attempts
  nAttempts = 6
    
    ##### 0.1 Errors ####
      ###### a. messages ####
    # Set up the error message function
  error_func <- function(e){
    message(paste("Checklist download attempt failed..."))
  }
  error_funcFile <- function(e){
    message(paste("Could not read checklist download..."))
  }
  
      ###### b. error catcher ####
  # Function to capture error outputs from here
        # Source - https://stackoverflow.com/a
        # Posted by Martin Morgan, modified by community. See post 'Timeline' for 
        # change history
        # Retrieved 2026-01-12, License - CC BY-SA 2.5
  errorCatcher <- function(fun) {
      warn <- err <- NULL
      res <- withCallingHandlers(
        tryCatch(fun(...), error=function(e) {
          err <<- conditionMessage(e)
          NULL
        }), warning=function(w) {
          warn <<- append(warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      list(res, warn=warn, err=err)
    }
  
    ##### 0.2 Check OS ####
    # Check operating system
  OS <- dplyr::if_else(.Platform$OS.type == "unix",
                                             "MacLinux",
                                             "Windows")
  writeLines(paste0("The operating system detected is ", OS, "."))
  
    ##### 0.3 Downloader function ####
  # Please note that this function is taken directly from the "downloader" package version 0.4.1
  # This is the purpose of the package, but I have taken their excellent function to avoid
  # Another dependency for BeeBDC. MY apologies and thanks to the authors.
  download <- function(URL, destfile = NULL, methodNum = NULL, headers = headers, ...) {
    # First, check protocol. If http or https, check platform:
    if (grepl('^https?://', URL)) {
      #### Windows ####
      if (tolower(.Platform$OS.type) == "windows") {
        # Set method to NULL to then be over-written
        method <- NULL
        # Try httr first
        if(methodNum == 1){
          message(paste0("Trying first download method using httr::GET..."))
          httr::GET(
            URL,
            httr::add_headers(
              Authorization = paste("token", token)
            ),
            httr::write_disk(destfile, overwrite = TRUE)
          )} # END methodNum == 1
        
        # Try different methods if failed
        if(methodNum == 2){method <- "auto"}
        if(methodNum == 3){method <- "wininet"}
          # Check also if libcurl is an option
        if(methodNum == 4 && capabilities("libcurl")){
          method <- "libcurl"}
          # Check also if wget is an option
        if(methodNum == 5 && nzchar(Sys.which("wget")[1])){
          method <- "wget"}
          # Check also if curl is an option
        if(methodNum == 6 && nzchar(Sys.which("curl")[1])){
          method <- "curl"}
          # If one of the above fails, use "internal"
        if(is.null(method)){
          method <- "internal"
        }
        
        if(is.null(mode)){
          mode <- "wb"}
        
        message(paste0("Trying download method ", method, " and mode ", mode, "..."))
        # download.file will complain about file size with something like:
        #       Warning message:
        #         In download.file(urURLl, ...) : downloaded length 19457 != reported length 200
        # because apparently it compares the length with the status code returned (?)
        # so we supress that
       if(methodNum > 1){
        message(paste0("Trying download method ", method, " and mode ", mode, "..."))
        downloadReturn <- utils::download.file(URL, 
                                               method = method, 
                                               destfile = destfile, 
                                               mode = mode,
                                               headers = headers) %>%
          errorCatcher()}
        
      } else {
        #### Mac/Linux ####
        method <- NULL
        # Try httr first
        if(methodNum == 1){
          message(paste0("Trying first download method using httr::GET..."))
          httr::GET(
            URL,
            httr::add_headers(
              Authorization = paste("token", token)
            ),
            httr::write_disk(destfile, overwrite = TRUE)
          )} # END methodNum == 1
        # If non-Windows, check for libcurl/curl/wget/lynx, then call download.file with
        # appropriate method.
        if (capabilities("libcurl") && methodNum == 2) {
          method <- "libcurl"
        } else if (nzchar(Sys.which("wget")[1]) && methodNum == 3) {
          method <- "wget"
        } else if (nzchar(Sys.which("curl")[1]) && methodNum == 4) {
          method <- "curl"
          # curl needs to add a -L option to follow redirects.
          # Save the original options and restore when we exit.
          orig_extra_options <- getOption("download.file.extra")
          on.exit(options(download.file.extra = orig_extra_options))
          options(download.file.extra = paste("-L", orig_extra_options))
        } else if (nzchar(Sys.which("lynx")[1]) && methodNum == 5) {
          method <- "lynx"
        } else if(methodNum == 6){
          method <- "auto"
        }
        if(is.null(method)){
          method <- "auto"
        }
        if(is.null(mode)){
          mode <- "wb"  
        }
        if(methodNum > 1){
        message(paste0("Trying download method ", method, " and mode ", mode, "..."))
        downloadReturn <- utils::download.file(URL, 
                                               method = method, 
                                               destfile = destfile, 
                                               mode = mode,
                                               headers = headers,
                                               ...) %>%
          errorCatcher()}
      }
    } else {
      downloadReturn <- utils::download.file(URL, destfile = destfile, 
                                             mode = "wb", headers = headers, ...) %>%
        errorCatcher()
    }
    return(downloadReturn)
  } # END download function
  
  
  #### 1.0 Download ####
  # Run a code to download the data and deal with potential internet issues
  checklist <- NULL                                 
  attempt <- 1 
  savePath <- file.path(tempdir(), "beesChecklist.Rda") %>% 
      # Change all backlashes to forward slashes -- sometimes they mix on Windows...
    stringr::str_replace_all("\\\\","/")
  writeLines(paste0("Saving file temporarily to ", savePath))
  suppressWarnings(
    while( is.null(checklist) && attempt <= nAttempts) {    
      # Initial message
      print( paste("Attempt: ", attempt, " of ", nAttempts))
        # Don't attempt for the last attempt
      if(attempt <= nAttempts){
# Windows or Mac/Linux
      # Download the file
      tryCatch(downloadReturn <- download(URL, destfile = savePath, 
                                          # Change the method based on attempt number
                                          methodNum = attempt,
                                          headers = headers),
          error = error_func, warning = error_func)
      # Load the file 
        tryCatch(
        checklist <- base::readRDS(savePath),
        error = error_funcFile, warning = error_funcFile)
      } # END if attempt < nAttempts

      
      if(attempt <= nAttempts){
      # Wait five seconds before the next request 
      if(is.null(checklist)){Sys.sleep(5)}
      } # END IF #2
      # Count the next attempt
      attempt <- attempt + 1   
      
      if(attempt > 1 && !is.null(downloadReturn)){
        # Output errors per run
        # Check download errors
        if(!stringr::str_detect(paste0(downloadReturn, collapse = ""), 
        "could not find function")){
        # Remove NULL elements
        downloadReturn <- downloadReturn[-which(sapply(downloadReturn, is.null))]
        # Paste message
        message(paste0("\n - Possible *download* error(s) returned:\n", paste0(
        names(downloadReturn), ": ", downloadReturn, collapse = "\n")))}
        # Check file errors
        fileError <- base::readRDS(savePath) %>% 
        errorCatcher()
        if(!stringr::str_detect(paste0(fileError, collapse = ""), 
        "could not find function")){
        # Remove NULL elements
        fileError <- fileError[-which(sapply(fileError, is.null))]
        message(paste0("\n - Possible *file* error(s) returned:\n", paste0(
        names(fileError), ": ", fileError, collapse = "\n")))}
      } # END if( attempt > 1){
    } # END while 
  )
  
    #### 2.0 Error list ####
  if(is.null(checklist)){
    # Check system capacities
    message(paste0(
      "System capabilities are:\n",
      " * Has libcurl? ", capabilities("libcurl"),
      "\n * Has wget? ", nzchar(Sys.which("wget")[1]),
      "\n * Has curl? ", nzchar(Sys.which("curl")[1])
    ))
    
    message(paste0(" - Checklist download failed. Please check your internet connection.\n",
                "Alternatively, feel free to paste the download url into your browser (",
                URL, ")",
                " and download the file directly. \n",
                "This file can then be read into R using:\n",
                "beesChecklist <- readRDS('path/to/downloaded/file/beesChecklist.Rda')"))
    
    
    stop("Errors finished.")
  }
  
  #### 3.0 Return ####
    # Return the data to the user
  return(checklist)
} # END function





