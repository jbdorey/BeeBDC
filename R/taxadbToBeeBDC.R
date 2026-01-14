# This function was written by James Dorey to build taxonomy files using taxadb and transform them
# into the BeeBDC input format
# This function was written from the 19th of March 2024. For questions, please email James
# at jbdorey[at]me.com

#' Import and convert taxadb taxonomies to BeeBDC format
#' 
#' Uses the taxadb R package to download a requested taxonomy and then transforms it into the input
#' BeeBDC format. This means that any taxonomy in their databases can be used with BeeBDC. You can
#' also save the output to your computer and to the R environment for immediate use. See 
#' details below for a list of providers or see `taxadb::td_create()`.
#' 
#'
#' @param name Character. Taxonomic scientific name (e.g. "Aves"). 
#' As defined by  `taxadb::filter_rank()`.
#' @param rank Character. Taxonomic rank name. (e.g. "class"). 
#' As defined by `taxadb::filter_rank()`.
#' @param provider Character. From which provider should the hierarchy be returned?
#' Default is 'gbif', which can also be configured using options(default_taxadb_provide = ...").
#'  See `taxadb::td_create()` for a list of recognized providers. NOTE: gbif seems to have the most-complete 
#'  columns, especially in terms of scientificNameAuthorship, which is important for matching
#'  ambiguous names.
#'  As defined by `taxadb::filter_rank()`.
#' @param version Character. Which version of the taxadb provider database should we use? defaults 
#' to latest. See tl_import for details. Default = 22.12. 
#' As defined by `taxadb::filter_rank()`.
#' @param collect Logical. Should we return an in-memory data.frame 
#' (default, usually the most convenient), or a reference to lazy-eval table on disk 
#' (useful for very large tables on which we may first perform subsequent filtering operations.).
#' Default = TRUE. 
#' As defined by `taxadb::filter_rank()`.
#' @param ignore_case Logical. should we ignore case (capitalization) in matching names? 
#' Can be significantly slower to run. Default = TRUE. 
#' As defined by `taxadb::filter_rank()`.
#' @param db a connection to the taxadb database. See details of `taxadb::filter_rank()`. Default 
#' = Null which should work. 
#' As defined by `taxadb::filter_rank()`.
#' @param ... Arguments passed to `taxadb::td_create()`.
#' 
#' @param removeEmptyNames Logical. If True (default), it will remove entries without an entry
#' for specificEpithet.
#' @param outPath Character. The path to a directory (folder) in which the output should be saved.
#' @param fileName Character. The name of the output file, ending in '.csv'.
#'
#'
#' @return Returns a taxonomy file (to the R environment and to the disk, if a fileName is 
#' provided) as a tibble that can be used with `BeeBDC::harmoniseR()`.
#' 
#' @importFrom dplyr %>%
#' 
#' @seealso [BeeBDC::beesTaxonomy()] for the bee taxonomy and [BeeBDC::harmoniseR()] for the 
#' taxon-cleaning function where these taxonomies are implemented.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   # Run the function using the bee genus Apis as an example...
#' ApisTaxonomy <- BeeBDC::taxadbToBeeBDC(
#'   name = "Apis",
#'   rank = "Genus",
#'   provider = "gbif",
#'   version = "22.12",
#'   removeEmptyNames = TRUE,
#'   outPath = getwd(),
#'   fileName = NULL,
#'   ...
#'   )
#'   }
#' 

taxadbToBeeBDC <- function(
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
) {  
  # locally bind variables to the function
  . <- taxonomy_taxadb <- taxonomyOut <- canonical <- authorship <- taxonomic_status <- species <-
    taxonID <- id <- accid <- id_matched <- NULL

  
  # Load required packages 
  requireNamespace("stringr")
  requireNamespace("dplyr")
  requireNamespace("taxadb")
  
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(name)){
    stop(" - Please provide an argument for name I'm a program not a magician.")
  }
  if(is.null(rank)){
    stop(" - Please provide an argument for rank I'm a program not a magician.")
  }
  if(provider == "ncbi"){
    stop(" - Sorry, ncbi doesn't include a taxonID against which to match the acceptedNameUsageID.")
  }
  if (!provider %in% c("itis",
                 "ncbi",
                 "col",
                 "tpl",
                 "gbif",
                 "fb",
                 "slb",
                 "wd",
                 "ott",
                 "iucn")) {
    stop(provider, " provided is not a valid name")
  }
  
  
  ##### 0.2 taxadb test ####
  ###### a. test ####
  # Check if taxadb is installed
  # TRUE if taxadb is found
  suppressWarnings(
    suggestedTest <- system.file(package='taxadb') %>% 
      stringr::str_count() > 0 
  )

  ###### b. taxadb ####
  if(suggestedTest == FALSE){
    # Set up instructions for download on fail
    instructions <- paste(" Please try installing the package for yourself", 
                          "using the following command: \n",
                          "install.packages(\"taxadb\")")
    # Set up fail function for tryCatch
    error_func <- function(e){
      stop(paste("Failed to install the taxadb package.\n", 
                 instructions))
    }
    # Begin interactive input
    input <- 1
    if (interactive()){
      input <- utils::menu(c("Yes", "No"), 
                           title = paste0("Install the taxadb package? \n"))
    }
    if(input == 1){
      # Start taxadb install
      message("Installing the taxadb package.")
      tryCatch(
        utils::install.packages("taxadb"), 
        error = error_func, warning = error_func)
    } # END input == 1
    
    else{
      stop(writeLines(paste("The taxadb package is necessary for BeeBDC::taxadbToBeeBDC.\n", 
                            instructions)))
    } # END else
  } # END suggestedTest == FALSE
  

#### 1.0 Download taxonomy ####
  ##### 1.1 Download ####
  writeLines(" - Downloading taxonomy...")
  taxadb::td_create(provider = provider,
                    schema = "dwc",
                    version = version,
                    # Only provide inputs here if user-inputs are provided.
                    if(is.null(db)){db = taxadb::td_connect()
                    }else{db = db},
                    ...)
    # User output
  writeLines(paste0(" - taxadb save the taxonomy to: ",
                    taxadb::taxadb_dir()))
  
  ##### 1.2 Turn into data table ####
  # Run the filter_rank function to output the data table taxonomy
taxonomy_taxadb <- taxadb::filter_rank(name,
                                       rank,
                                       provider = provider,
                                       collect = collect,
                                       ignore_case = ignore_case,
                                        # Only provide inputs here if user-inputs are provided.
                                       if(is.null(db)){db = taxadb::td_connect()
                                       }else{db = db},
                                       version = version
) 
  
  ##### 1.3 missing columns ####
  if(!"scientificNameAuthorship" %in% colnames(taxonomy_taxadb)){
    taxonomy_taxadb <- taxonomy_taxadb %>%
      dplyr::mutate(scientificNameAuthorship = NA_character_)
    warning(paste0(" - BeeBDC: no scientificNameAuthorship in downloaded data. BeeBDC really ", 
                   "likes this column as it helps identify ambiguities."))
  }
  if(!"infraspecificEpithet" %in% colnames(taxonomy_taxadb)){
    taxonomy_taxadb <- taxonomy_taxadb %>%
      dplyr::mutate(infraspecificEpithet = NA_character_)
    warning(paste0(" - BeeBDC: no infraspecificEpithet in downloaded data. This can be a really",
                   " helpful column for some taxa."))
  }
  
#### 2.0 Transform data ####
  ##### 2.1 basic rename and mutate ####
    # Begin transforming the taxonomy to BeeBDC format
taxonomyOut <- taxonomy_taxadb %>%
      # Rename columns... sadly away from DWC... for now.
    dplyr::rename(taxonomic_status = "taxonomicStatus",
                  authorship = "scientificNameAuthorship",
                  infraspecies = "infraspecificEpithet",
                  species = "specificEpithet",
                  taxon_rank = "taxonRank",
                  canonical = "scientificName") %>%
      # Build new columns
    dplyr::mutate(validName = stringr::str_c(canonical, 
                                             dplyr::if_else(!is.na(authorship),
                                                            paste0(authorship), ""),
                                             sep = " ") %>%
                                               stringr::str_squish(),
                  canonical_withFlags = canonical,
                  valid = dplyr::if_else(taxonomic_status == "accepted",
                                         TRUE, FALSE))
  ##### 2.2 Remove empty names ####
      # Remove empty names
  if(removeEmptyNames == TRUE){
    taxonomyOut <- taxonomyOut %>%
      dplyr::filter(stats::complete.cases(species))
  }
  
  ##### 2.3 Add id and accid ####
    # Add id and accepted accid
  taxonomyOut <- taxonomyOut %>%
      # Add id as a simple count, top to bottom
    dplyr::mutate(id = 1:nrow(.)) %>%
      # Add accid for the accepted names
    dplyr::mutate(accid = dplyr::if_else(taxonomic_status == "accepted",
                                 0, NA_integer_)) 
    # Match the synonyms to their accepted names
  taxonomyOut <- taxonomyOut %>%
    dplyr::left_join(taxonomyOut %>% dplyr::select(taxonID, id),
                     by = c("acceptedNameUsageID" = "taxonID"),
                     suffix = c("", "_matched")) %>%
      # Transfer this id to the accid column
    dplyr::mutate(accid = dplyr::if_else(is.na(accid),
                                         id_matched, accid)) %>%
      # Drop the temporary column
    dplyr::select(!id_matched) %>%
      # Add in source
    dplyr::mutate(source = stringr::str_c("taxadb_",provider, sep = "")) %>%
      # Add in empty columns
    dplyr::mutate(flags = NA_character_,
                  notes = NA_character_)
  
  ##### 2.4 Clean missing columns ####
    # Clean up potentially missing columns
  if(!"subfamily" %in% colnames(taxonomyOut)){
    taxonomyOut <- taxonomyOut %>%
      dplyr::mutate(subfamily = NA_character_)
  }
  if(!"tribe" %in% colnames(taxonomyOut)){
    taxonomyOut <- taxonomyOut %>%
      dplyr::mutate(tribe = NA_character_)
  }
  if(!"subtribe" %in% colnames(taxonomyOut)){
    taxonomyOut <- taxonomyOut %>%
      dplyr::mutate(subtribe = NA_character_)
  }
  if(!"subgenus" %in% colnames(taxonomyOut)){
    taxonomyOut <- taxonomyOut %>%
      dplyr::mutate(subgenus = NA_character_)
  }

    ##### 2.5 Re-order columns ####
  taxonomyOut <- taxonomyOut %>%
    dplyr::relocate(c("flags","taxonomic_status","source","accid","id",
                  "kingdom","phylum","class","order","family",
                  "subfamily","tribe","subtribe","validName","canonical",
                  "canonical_withFlags","genus","subgenus","species","infraspecies","authorship",
                  "taxon_rank","valid","notes"))
  
  
  #### 3.0 Identify ambiguities ####
    # Test for duplicate and ambiguous names
  taxonomyOut <- taxoDuplicator(
    SynList = taxonomyOut,
    source1 = provider,
      # This is not needed in this instance
    source2 = "")
  
  #### 4.0 Save ####
    # Save the output file if a fileName is provided
  if(!is.null(fileName)){
    readr::write_excel_csv(taxonomyOut,
                           file = paste0(outPath, "/", fileName, sep = ""))
  }
  
    # Return the dataset
  return(taxonomyOut)
  
  
  
} # END taxadbToBeeBDC
  