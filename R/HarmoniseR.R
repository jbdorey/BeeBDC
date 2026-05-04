# This function was written by James Dorey to harmonise the names of bees using the Ascher-Orr-Chesshire
# bee taxonomies.
# The function first merges names based on scientificName, then merging the bdc cleaned_name and
# scientificNameAuthorship and matching those, followed by matching to canoncial with flags, then canonical. 
# In all of these cases, names that are ambiguous at that level are removed so that only confident
# matches are maintaned. 
# This function was written between the 18th and 20th of May 2022. For questions, please email James
# at jbdorey[at]me.com


#' Harmonise taxonomy of occurrence data
#' 
#' Uses BeeBDC-formatted taxonomy data to harmonise occurrence records and flag those that do not match
#' the taxonomy [BeeBDC::harmoniseR()] prefers to use the names_clean columns that is generated 
#' by [bdc::bdc_clean_names()]. While this is not required, you may find better results by running
#' that function on your dataset first.
#' It is possible to download taxonomy file for other taxa using [BeeBDC::taxadbToBeeBDC()]
#' which can download taxonomies from ITIS, GBIF, and more. You could also match the format of the
#' [BeeBDC::beesTaxonomy()] file.
#'
#' @param path A directory as character. The path to a folder that the output can be saved.
#' @param taxonomy A data frame or tibble. The taxonomy file to use. 
#' Default = [BeeBDC::beesTaxonomy()]; for other taxa see first [BeeBDC::taxadbToBeeBDC()].
#' @param data A data frame or tibble. Occurrence records as input.
#' @param speciesColumn Character. The name of the column containing species names. Default = "scientificName".
#' @param rm_names_clean Logical. If TRUE then the names_clean column will be removed at the end of
#' this function to help reduce confusion about this column later. Default = TRUE
#' @param checkVerbatim Logical. If TRUE then the verbatimScientificName will be checked as well 
#' for species matches. This matching will ONLY be done after harmoniseR has failed for the other 
#' name columns. NOTE: this column is *not* first run through `bdc::bdc_clean_names`. Default = FALSE
#' @param relaxAmbiguous Logical. If TRUE, then ambiguous names will be returned as "TRUE" for 
#' .invalidName. However, they will be listed as "ambiguouslyMatchedName" 
#' under the "scientificNameAuthorship" column and the speciesColumn will NOT include the authority
#' information. Hence, the names can be used but in should remain clear in the data that they must
#' be used with caution. Default = FALSE.
#' @param matchHigherTaxonomy Logical. If TRUE, then BeeBDC will try to match higher taxonomies
#' (e.g., family names) using genera and such. This will work better if you have run the data through
#' `bdc::bdc_clean_names` beforehand. The function will update the taxonRank column, if it is 
#' otherwise empty. Default = TRUE.
#' @param stepSize Numeric. The number of occurrences to process in each chunk. Default = 1000000.
#' @param mc.cores Numeric. If > 1, the function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory.
#'  Default = 1.
#'
#' @return The occurrences are returned with update taxonomy columns, including: scientificName, 
#' species, family, subfamily, genus, subgenus, specificEpithet, infraspecificEpithet, and 
#' scientificNameAuthorship. A new column, .invalidName, is also added and is FALSE when the occurrence's
#' name did not match the supplied taxonomy. 
#' 
#' @importFrom dplyr %>%
#' 
#' @seealso [BeeBDC::taxadbToBeeBDC()] to download any taxonomy (of any taxa or of bees) and
#' [BeeBDC::beesTaxonomy()] for the bee taxonomy download.
#' 
#' @export
#'
#' @examples
#' # load in the test dataset
#' system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |> load()
#' 
#' # See also
#' ?BeeBDC::taxadbToBeeBDC()
#' 
#' beesRaw_out <- BeeBDC::harmoniseR(
#'   #The path to a folder that the output can be saved
#' path = tempdir(),
#' # The formatted taxonomy file
#' taxonomy = testTaxonomy, 
#' data = BeeBDC::beesFlagged,
#' speciesColumn = "scientificName")
#' table(beesRaw_out$.invalidName, useNA = "always")

harmoniseR <- function(
    data = NULL,
    path = NULL, #The path to a folder that the output can be saved
    taxonomy = BeeBDC::beesTaxonomy(), # The formatted taxonomy file
    speciesColumn = "scientificName",
    rm_names_clean = TRUE,
    checkVerbatim = FALSE,
    relaxAmbiguous = FALSE,
    matchHigherTaxonomy = TRUE,
    stepSize = 1000000,
    mc.cores = 1
) {  
  # locally bind variables to the function
  . <- id <- validName<-canonical<-canonical_withFlags<-family<-subfamily<-genus<-subgenus<-
    species<-infraspecies<-authorship<-taxonomic_status<-flags<-accid<-validName_valid<-
    family_valid<-subfamily_valid<-canonical_withFlags_valid<-genus_valid<-subgenus_valid<-
    species_valid<-infraspecies_valid<-authorship_valid<-database_id<-names_clean<-
    scientificNameAuthorship<-taxonRank<-authorFound<-SciNameAuthorSimple<-
    authorSimple<-united_SciName<-verbatimScientificName <- scientificName <- BeeBDC_order <- NULL
  
  
  
  # Load required packages 
  requireNamespace("rlang")
  requireNamespace("dplyr")
  
  # Record start time
  startTime <- Sys.time()
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data. I'm a program not a magician.")
  }
  if(is.null(taxonomy)){
    stop(" - Please provide an argument for taxonomy I'm a program not a magician.")
  }
  if(is.null(path)){
    stop(" - Please provide an argument for path I'm a program not a magician.")
  }
  if(!"verbatimScientificName" %in% colnames(data) & checkVerbatim == TRUE){  
    stop(paste0(" - If 'checkVerbatim = TRUE', then the verbatimScientificName column must be 
  present in the data."))
  }
  
  
  
  #### 1.0  _match columns ####
  # Make a synonym index list
  writeLines(paste(" - Formatting taxonomy for matching..."))
  # save the original column names
  OG_colnames <- unique(c("database_id", colnames(data)))
  # Save the original number of rows
  OG_rowNum <- nrow(data)
  
  ##### 1.1 Prepare columns ####
  # To make the function more general, allow some column changing internally.
  ###### a. rename to scientificName ####
  # Temporarily rename the speciesColumn to "scientificName" within the function
  data <- data %>%
    dplyr::rename("scientificName" = tidyselect::any_of(speciesColumn))
  ###### b. temp names_clean ####
  # IF the names_clean column does not exist, temporarily add it to the dataset using the 
  # scientificName column's data.
  if(!"names_clean" %in% colnames(data)){
    data <- data %>%
      dplyr::mutate(names_clean = scientificName)
    message(paste0("The names_clean column was not found and will be temporarily copied from",
                   " scientificName"))
  }
  ###### c. database_id ####
  # If the database_id column isn't in the dataset, then add it for internal use
  if(!"database_id" %in% colnames(data)){
    data <- data %>%
      dplyr::mutate(database_id = paste0("BeeBDC_TempCode_", dplyr::row_number()), .before = 1)
    message("The database_idcolumn was not found, making this column with 'BeeBDC_TempCode_'...")
  }
  ###### d. scientificNameAuthorship ####
  # If there is no scientificNameAuthorship, make all NA
  if(!"scientificNameAuthorship" %in% colnames(data)){
    data <- data %>%
      dplyr::mutate(scientificNameAuthorship = NA_character_)
    message("The scientificNameAuthorship column was not found, making this column full of NAs.")
  }
  ###### e. taxonRank ####
  # If there is no taxonRank, make all NA
  if(!"taxonRank" %in% colnames(data)){
    data <- data %>%
      dplyr::mutate(taxonRank = NA_character_)
    message("The taxonRank column was not found, making this column full of NAs.")
  }
  ###### f. species ####
  # If there is no species, make all NA
  if(!"species" %in% colnames(data)){
    data <- data %>%
      dplyr::mutate(species = scientificName)
    message("The species column was not found, filling this column with scientificName.")
  }
  
  # Remove non-ambiguous tags 
  taxonomy <- taxonomy %>%
    dplyr::mutate(flags = flags %>%
                    stringr::str_remove_all("non-ambiguous canonical|	non-ambiguous can_wFlags"))
  
  # Add a new column which has the canonical names matched to the synonyms
  taxonomy <- taxonomy %>%
    dplyr::left_join(x = ., 
                     # left join ONLY the validName, canonical, and canonical_withFlags
                     y = dplyr::select(taxonomy, 
                                       tidyselect::any_of(
                                         c("id", "validName", "canonical", "canonical_withFlags", 
                                           "family", "subfamily", "genus", "subgenus", "species", 
                                           "infraspecies", "authorship"))), 
                     by = c("accid" = "id"), suffix = c("", "_valid"),
                     multiple = "all")
  # Now, also duplicate the accepted names into the ._matched columns  
  AccMatched <- taxonomy %>% 
    # select only the ACCEPTED NAMES
    dplyr::filter(taxonomic_status == "accepted") %>%
    # duplicate the valid columns into the matched column locations
    dplyr::mutate(validName_valid = validName,
                  canonical_valid = canonical,
                  canonical_withFlags_valid = canonical_withFlags,
                  family_valid = family,
                  subfamily_valid = subfamily,
                  genus_valid = genus, 
                  subgenus_valid = subgenus, 
                  species_valid = species, 
                  infraspecies_valid = infraspecies, 
                  authorship_valid = authorship)
  
  # Merge these datasets
  taxonomy <- taxonomy %>%
    # First filter for the reverse of above - SYNONYM NAMES
    dplyr::filter(taxonomic_status == "synonym") %>%
    # combine
    dplyr::bind_rows(AccMatched)
  
  
  rm(AccMatched)
  
  
  #### 2.0 Harmonise data ####
  writeLines(paste("\n",
                   " - Harmonise the occurrence data with unambiguous names...", sep = ""))
  # Create the parallel-able function
  unAmbiguousFunction <- function(data){
    ##### 2.1 Valid Name ####
    ###### a. prep synonyms ####
    # Filter out the AMBIGUOUS validNames prior to matching
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous validNames
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""),
        "ambiguous validName")) 
    
    ###### b. assign names ####
    # Clean up some illegal characters
    data$scientificName <- data$scientificName %>%
      stringr::str_replace(pattern = "^\"", replacement = "") %>%
      stringr::str_replace(pattern = "\"$", replacement = "")
    
    # Match names first with the validName column
    occs_21 <- data %>%
      dplyr::left_join(currenttaxonomy %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)),
                       # Match scientific name with the valid synonym name
                       by = c("scientificName" = "validName"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    #   # Add a column to express the name-match quality - "high" IF there is a match at this point
    # dplyr::mutate(nameQuality = dplyr::if_else(stats::complete.cases(validName_valid),
    #   "high", "NA")) 3,703
    
    ###### c. return Occs ####
    # Return the matched data
    occs_21 <- occs_21 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) # 1,927
    
    
    ##### 2.2 validName_comb ####
    # Now we will try and match the valid name by combining the names_clean and scientificNameAuthorship columns
    ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    ## SAME as 2.1 ##
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous validNames
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""),
        "ambiguous validName")) 
    
    ###### b. assign names ####
    # Match names first with the validName column
    occs_22 <- data %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% occs_21$database_id) %>%
      # Make a new column by combining names_clean and scientificNameAuthorship
      tidyr::unite(col = "united_SciName", names_clean, scientificNameAuthorship, sep = " ",
                   na.rm = TRUE) 
    
    # Match names first with the validName column
    occs_22 <- occs_22 %>%
      dplyr::left_join(currenttaxonomy %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)),
                       # Match scientific name with the valid synonym name
                       by = c("united_SciName" = "validName"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data
    runningOccurrences <- occs_22 %>%
      dplyr::filter(stats::complete.cases(validName_valid) & validName_valid != "NA") %>%
      # Bind the previous rows
      dplyr::bind_rows(occs_21) # 2,678
    # Remove this spent files
    rm(occs_21, occs_22)
    
    
    ##### 2.3 canonical_wFlags ####
    ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous validNames and can_wFlags
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""),
        paste("ambiguous validName", 
              "ambiguous can_wFlags",
              sep = "|"))) %>%
      # remove the rows where the canonical and canonical_withFlags match
      # ONLY matches those with added canonicals flags
      dplyr::filter(!canonical == canonical_withFlags)
    
    
    ###### b. assign names ####
    # Match names first with the validName column
    occs_23 <- data %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
      dplyr::left_join(currenttaxonomy %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)),
                       # Match scientific name with the valid synonym name
                       by = c("species" = "canonical_withFlags"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data
    runningOccurrences <- occs_23 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningOccurrences)
    # Remove this spent file 
    rm(occs_23)
    
    
    ##### 2.4 canonical ####
    ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous names
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""), 
        paste("ambiguous validName", 
              "ambiguous can_wFlags",
              "ambiguous canonical",
              sep = "|"))) 
    
    ###### b. assign names ####
    # Match names first with the validName column
    occs_24 <- data %>%
      # Keep the unmatched names
      dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
      dplyr::left_join(currenttaxonomy %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, 
                                         validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)),
                       # Match scientific name with the valid synonym name
                       by = c("names_clean" = "canonical"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data
    runningOccurrences <- occs_24 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningOccurrences) %>%
      # Make sure no duplicates have snuck in
      dplyr::distinct(database_id, .keep_all = TRUE)
    # Remove spent file
    rm(occs_24)
    
    
    ##### 2.5 sciName_comb ####
    # Now we will try and match the valid name by combining the scientificName and scientificNameAuthorship columns
    ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    ## SAME as 2.1 ##
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous validNames
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""),
        "ambiguous validName")) 
    
    ###### b. assign names ####
    # Match names first with the validName column
    occs_25 <- data %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
      # Make a new column by combining names_clean and scientificNameAuthorship
      tidyr::unite(col = "united_SciName", names_clean, scientificNameAuthorship, sep = " ",
                   na.rm = TRUE) 
    
    # Match names first with the validName column
    occs_25 <- occs_25 %>%
      dplyr::left_join(currenttaxonomy %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)),
                       # Match scientific name with the valid synonym name
                       by = c("united_SciName" = "validName"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data
    runningOccurrences <- occs_25 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningOccurrences) %>%
      # Make sure no duplicates have snuck in
      dplyr::distinct(database_id, .keep_all = TRUE)
    # Remove spent file
    rm(occs_25)
    
    
    
    ##### 2.6 No subgenus validName ####
    # Match scientificName with validName; remove subgenus from both
    ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous names
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""), 
        paste("ambiguous validName", 
              "ambiguous canonical",
              sep = "|"))) 
    
    ###### b. assign names ####
    # Match names first with the validName column
    occs_26 <- data %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
      dplyr::mutate(scientificNameMatch = scientificName %>% 
                      # Replace subgenus with nothing
                      stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                      stringr::str_squish())
    
    # Match names first with the validName column
    occs_26 <- occs_26 %>%
      dplyr::left_join(currenttaxonomy %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)) %>%
                         dplyr::mutate(validNameMatch = validName %>% 
                                         # Replace subgenus with nothing
                                         stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                                         stringr::str_squish()),
                       # Match scientific name with the valid synonym name
                       by = c("scientificNameMatch" = "validNameMatch"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data
    runningOccurrences <- occs_26 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningOccurrences) %>%
      # Make sure no duplicates have snuck in
      dplyr::distinct(database_id, .keep_all = TRUE)
    # Remove spent file
    rm(occs_26, currenttaxonomy)
    
    ##### 2.7 No subgenus canonical ####
    # Match scientificName with canonical; remove subgenus from both
    ###### a. prep synonyms ####
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    # For those that did not match, attempt to match them with the Canonical with flags column...
    # Filter out the AMBIGUOUS validNames prior to matching
    currenttaxonomy <- taxonomy %>%
      # REMOVE ambiguous names
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""), 
        paste("ambiguous validName", 
              "ambiguous canonical",
              sep = "|"))) 
    
    ###### b. assign names ####
    # Match names first with the canonical column
    occs_27 <- data %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
      dplyr::mutate(scientificNameMatch = scientificName %>% 
                      # Replace subgenus with nothing
                      stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                      stringr::str_squish())
    
    # Match names first with the canonical column
    occs_27 <- occs_27 %>%
      dplyr::left_join(currenttaxonomy %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)) %>%
                         dplyr::mutate(canonicalMatch = canonical %>% 
                                         # Replace subgenus with nothing
                                         stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                                         stringr::str_squish()),
                       # Match scientific name with the canonical synonym name
                       by = c("scientificNameMatch" = "canonicalMatch"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data
    runningOccurrences <- occs_27 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningOccurrences) %>%
      # Make sure no duplicates have snuck in
      dplyr::distinct(database_id, .keep_all = TRUE)
    # Remove spent file
    rm(occs_27, currenttaxonomy)
    return(runningOccurrences)
  } # END unAmbiguousFunction
  
  # Run the function
  runningOccurrences <- data %>%
    # Make a new column with the ordering of rows
    dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
    # Group by the row number and step size
    dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
    # Split the dataset up into a list by group
    dplyr::group_split(.keep = TRUE) %>%
    # Run the actual function
    parallel::mclapply(., unAmbiguousFunction,
                       mc.cores = mc.cores
    ) %>%
    # Combine the lists of tibbles
    dplyr::bind_rows() 
  
  #### 3.0 Ambiguous names ####
  writeLines(paste("\n",
                   " - Attempting to harmonise the occurrence data with ambiguous names...", sep = ""))
  ambiguousFunction <- function(ambData){
    ##### 3.1 Prepare datasets ####
    ###### a. prep synonyms ####
    # Synonym list of ambiguous names
    # Filter TO the AMBIGUOUS validNames prior to matching
    ambiguousSynonyms <- taxonomy %>%
      # Keep only ambiguous validNames
      dplyr::filter(stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""), 
        "ambiguous")) %>%
      # Remove non-ambiguous matches
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""),
        "non-")) %>%
      # Remove ambiguous validName matches because these are ambiguous even with authorities.
      # Perhaps in future these can be matched by geography.
      dplyr::filter(!stringr::str_detect(
        # Replace NA in flags with "" to allow matching
        tidyr::replace_na(flags, ""),
        "ambiguous validName")) %>%
      # Remove those without authorship
      dplyr::filter(!is.na(authorship)) %>% 
      # # Change the authorship to be easier to match by removing capitals and punctuation
      dplyr::mutate(authorSimple = stringr::str_remove_all(authorship, 
                                                           pattern = "[:punct:]") %>% tolower())
    
    
    ###### b. author in sciName  ####
    if(!all(is.na(ambData$scientificNameAuthorship))){
      # Create a list of scientificNameAuthorships that can be found in scientificName, where the former is empty
      ambiguousAuthorFound <- ambData %>%
        # check only the data without authorship
        dplyr::filter(is.na(scientificNameAuthorship)) %>%
        # Select only UNDER genus-level IDs
        dplyr::filter(taxonRank %in% c("Especie", "forma", "Infrasubspecies", "Race",
                                       "species", "Species", "SPECIES", "subsp.", "subspecies",
                                       "Subspecies", "SUBSPECIES", "syn", "var.", "variety",
                                       "Variety", "VARIETY", NA, "NA")) %>%
        # Make a new column with the authorship extracted from scientificName
        dplyr::mutate(authorFound = stringr::str_extract(
          # Use a simplified scientificName string
          string = stringr::str_remove_all(scientificName, 
                                           pattern = "[:punct:]") %>% tolower(),
          pattern = paste(ambiguousSynonyms$authorSimple, collapse = "|"))) %>%
        # Keep only matched names
        dplyr::filter(stats::complete.cases(authorFound)) %>%
        # Return only the database_id and authorFound for merging...
        dplyr::select(tidyselect::all_of(c("database_id", "authorFound")))
      
      # Add the author to those data that were lacking
      ambData <- ambData %>%
        # Add authorFound to original dataset
        dplyr::left_join(ambiguousAuthorFound, by = "database_id",multiple = "all") %>%
        # If scientificNameAuthorship is empty, use authorFound from ambiguousAuthorFound
        dplyr::mutate(scientificNameAuthorship = 
                        dplyr::if_else(is.na(scientificNameAuthorship),
                                       # If missing replace the na with the authorFound
                                       authorFound,
                                       # IF already complete, keep the original
                                       scientificNameAuthorship))
      
      # Remove used data
      rm(ambiguousAuthorFound)}
    
    ###### c. ambiguous data ####
    # Filter occurrence dataset to those with ambiguous names AND authorship values
    data_amb <- ambData %>%
      # Keep those with authorship recorded
      dplyr::filter(stats::complete.cases(scientificNameAuthorship)) %>%
      # Keep those that are in the ambiguous names list
      dplyr::filter(scientificName %in% ambiguousSynonyms$validName |
                      scientificName %in% ambiguousSynonyms$canonical_withFlags |
                      scientificName %in% ambiguousSynonyms$canonical) %>%
      # Simplify scientificNameAuthorship to make easier matches
      dplyr::mutate(SciNameAuthorSimple = stringr::str_remove_all(scientificNameAuthorship, 
                                                                  pattern = "[:punct:]") %>% tolower())
    
    
    ##### 3.2 Valid Name ####
    ###### a. assign names ####
    # Match names first with the validName column
    runningAmb_occs <- data_amb %>%
      # Select only rows with both scientificName and SciNameAuthorSimple
      dplyr::filter(stats::complete.cases(scientificName) & stats::complete.cases(SciNameAuthorSimple)) %>%
      # Add taxonomy information to the occurrence data.
      dplyr::left_join(ambiguousSynonyms %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid, authorSimple)),
                       # Match scientific name with the valid synonym name
                       by = c("scientificName" = "validName",
                              "SciNameAuthorSimple" = "authorSimple"),
                       suffix = c("", "_harmon"),
                       multiple = "all",relationship = "many-to-many") 
    
    ###### b. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- runningAmb_occs %>%
      dplyr::filter(stats::complete.cases(validName_valid)) # 1,927
    
    
    ##### 3.3 validName_comb ####
    # Now we will try and match the valid name by combining the names_clean and scientificNameAuthorship columns
    
    
    ###### a. assign names ####
    # Match names first with the validName column
    occs_33 <- data_amb %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
      # Make a new column by combining names_clean and scientificNameAuthorship
      tidyr::unite(col = "united_SciName", names_clean, scientificNameAuthorship, sep = " ",
                   na.rm = TRUE) 
    
    # Match names first with the validName column
    occs_33 <- occs_33 %>%
      # Select only rows with both united_SciName and SciNameAuthorSimple
      dplyr::filter(stats::complete.cases(united_SciName) & stats::complete.cases(SciNameAuthorSimple)) %>%
      # Add taxonomy information to the occurrence data.
      dplyr::left_join(ambiguousSynonyms %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid, authorSimple)),
                       # Match scientific name with the valid synonym name
                       by = c("united_SciName" = "validName",
                              "SciNameAuthorSimple" = "authorSimple"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### b. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- occs_33 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningAmb_occs) # 2,678
    # Remove this spent files
    rm(occs_33)
    
    
    ##### 3.4 canonical_wFlags ####
    ###### a. prep datasets ####
    # Synonym list of ambiguous names
    # Filter TO the AMBIGUOUS validNames prior to matching
    syns_34 <- ambiguousSynonyms %>%
      # remove the rows where the canonical and canonical_withFlags match
      dplyr::filter(!canonical == canonical_withFlags)
    
    
    ###### b. assign names ####
    # Match names first with the validName column
    occs_34 <- data_amb %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
      # Select only rows with both species and SciNameAuthorSimple
      dplyr::filter(stats::complete.cases(species) & stats::complete.cases(SciNameAuthorSimple)) %>%
      # Add taxonomy information to the occurrence data.
      dplyr::left_join(syns_34 %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid, authorSimple)),
                       # Match scientific name with the valid synonym name
                       by = c("species" = "canonical_withFlags",
                              "SciNameAuthorSimple" = "authorSimple"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- occs_34 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningAmb_occs)
    # Remove this spent file 
    rm(occs_34, syns_34)
    
    
    ##### 3.5 canonical ####
    ###### b. assign names ####
    # Match names first with the validName column
    occs_35 <- data_amb %>%
      # Keep the unmatched names
      dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
      # Select only rows with both names_clean and SciNameAuthorSimple
      dplyr::filter(stats::complete.cases(names_clean) & stats::complete.cases(SciNameAuthorSimple)) %>%
      # Add taxonomy information to the occurrence data.
      dplyr::left_join(ambiguousSynonyms %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid, authorSimple)),
                       # Match scientific name with the valid synonym name
                       by = c("names_clean" = "canonical",
                              "SciNameAuthorSimple" = "authorSimple"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### c. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- occs_35 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningAmb_occs)%>%
      # Make sure no duplicates have snuck in
      dplyr::distinct(database_id, .keep_all = TRUE)
    # Remove spent file
    rm(occs_35)
    
    
    
    ##### 3.6 validName_comb ####
    # Now we will try and match the valid name by combining the names_clean and scientificNameAuthorship columns
    ###### a. assign names ####
    # Match names first with the validName column
    occs_36 <- data_amb %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
      # Make a new column by combining names_clean and scientificNameAuthorship
      tidyr::unite(col = "united_SciName", names_clean, scientificNameAuthorship, sep = " ",
                   na.rm = TRUE) 
    
    # Match names first with the validName column
    occs_36 <- occs_36 %>%
      # Select only rows with both united_SciName and SciNameAuthorSimple
      dplyr::filter(stats::complete.cases(united_SciName) & stats::complete.cases(SciNameAuthorSimple)) %>%
      # Add taxonomy information to the occurrence data.
      dplyr::left_join(ambiguousSynonyms %>%
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, validName_valid,
                                         family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid, authorSimple)),
                       # Match scientific name with the valid synonym name
                       by = c("united_SciName" = "validName",
                              "SciNameAuthorSimple" = "authorSimple"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### b. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- occs_36 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningAmb_occs) # 2,678
    # Remove this spent files
    rm(occs_36)
    
    
    ##### 3.7 No subgenus validName ####
    # Match scientificName with validName; remove subgenus from both
    ###### a. assign names ####
    # Match names first with the validName column
    occs_37 <- data_amb %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
      dplyr::mutate(scientificNameMatch = scientificName %>% 
                      # Replace subgenus with nothing
                      stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                      stringr::str_squish())
    
    occs_37 <- occs_37 %>%
      dplyr::left_join(ambiguousSynonyms %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, 
                                         validName_valid, family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)) %>%
                         dplyr::mutate(validNameMatch = validName %>% 
                                         # Replace subgenus with nothing
                                         stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                                         stringr::str_squish()),
                       # Match scientific name with the valid synonym name
                       by = c("scientificNameMatch" = "validNameMatch"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### b. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- occs_37 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningAmb_occs)
    # Remove this spent files
    rm(occs_37)
    
    
    ##### 3.8 No subgenus canonical ####
    # Match scientificName with canonical; remove subgenus from both
    ###### a. assign names ####
    # Match names first with the canonical column
    occs_38 <- data_amb %>%
      # remove already-matched names
      dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
      dplyr::mutate(scientificNameMatch = scientificName %>% 
                      # Replace subgenus with nothing
                      stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                      stringr::str_squish())
    
    occs_38 <- occs_38 %>%
      dplyr::left_join(ambiguousSynonyms %>% 
                         dplyr::select(c(id, accid, validName, canonical_withFlags, canonical, 
                                         validName_valid, family_valid, subfamily_valid,
                                         canonical_withFlags_valid, genus_valid, subgenus_valid, 
                                         species_valid, infraspecies_valid, authorship_valid)) %>%
                         dplyr::mutate(canonicalMatch = canonical %>% 
                                         # Replace subgenus with nothing
                                         stringr::str_replace("\\([A-Za-z]+\\)", "") %>%
                                         stringr::str_squish()),
                       # Match scientific name with the valid synonym name
                       by = c("scientificNameMatch" = "canonicalMatch"),
                       suffix = c("", "_harmon"),
                       multiple = "all", relationship = "many-to-many") 
    
    ###### b. return Occs ####
    # Return the matched data_amb
    runningAmb_occs <- occs_38 %>%
      dplyr::filter(stats::complete.cases(validName_valid)) %>%
      # Bind the previous rows
      dplyr::bind_rows(runningAmb_occs)
    # Remove this spent files
    rm(occs_38)
    return(runningAmb_occs)
  } # END ambiguousFunction
  
  # Run the function
  runningAmb_occs <- data %>%
    # Make a new column with the ordering of rows
    dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
    # Group by the row number and step size
    dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
    # Split the dataset up into a list by group
    dplyr::group_split(.keep = TRUE) %>%
    # Run the actual function
    parallel::mclapply(., ambiguousFunction,
                       mc.cores = mc.cores
    ) %>%
    # Combine the lists of tibbles
    dplyr::bind_rows() 

    
  ##### 3.9 Combine 2.x & 3.x ####
  # Merge the results from 2.x and 3.x 
  runningOccurrences <- runningOccurrences %>%
    # Remove the ambiguous data from the prior-matched data
    dplyr::filter(!database_id %in% runningAmb_occs$database_id) %>%
    # Add in the ambiguous data again.
    dplyr::bind_rows(runningAmb_occs)
  
  # Remove the spent runningAmb_occs data
  rm(runningAmb_occs)
  
  
  #### 4.0 verbatimScientificName ####
  if(checkVerbatim == TRUE){
    writeLines(paste0("checkVerbatim = TRUE. Checking the verbatimScientificName column..."))
    
    ##### 4.1 failedMatches ####
    # Find the data that did not match
    failedMatches <- data %>%
      # Remove the matched names from the OG dataset
      dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
      # Move the verbatimScientificName to scientificName
      dplyr::mutate(scientificName = verbatimScientificName)
    
    #### 4.2 Run unAmbiguous names ####
    # Run the function
    runningOccurrences_verb <- failedMatches %>%
      # Make a new column with the ordering of rows
      dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
      # Group by the row number and step size
      dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
      # Split the dataset up into a list by group
      dplyr::group_split(.keep = TRUE) %>%
      # Run the actual function
      parallel::mclapply(., unAmbiguousFunction,
                         mc.cores = mc.cores
      ) %>%
      # Combine the lists of tibbles
      dplyr::bind_rows() 
    
    
    #### 4.3 Run ambiguous names ####
    # Run the function
    runningAmb_occs_verb <- failedMatches %>%
      # Make a new column with the ordering of rows
      dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
      # Group by the row number and step size
      dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
      # Split the dataset up into a list by group
      dplyr::group_split(.keep = TRUE) %>%
      # Run the actual function
      parallel::mclapply(., ambiguousFunction,
                         mc.cores = mc.cores
      ) %>%
      # Combine the lists of tibbles
      dplyr::bind_rows() 
    
    
    #### 4.4 Combine 4.2-.3 ####
    runningOccurrences_verb <- runningOccurrences_verb %>%
      # Remove the ambiguous data from the prior-matched data
      dplyr::filter(!database_id %in% runningAmb_occs_verb$database_id) %>%
      # Add in the ambiguous data again.
      dplyr::bind_rows(runningAmb_occs_verb)
    
    
    #### 4.5 Combine 3.6 & 4.4 ####
    # Merge the results from 2.x and 3.x 
    runningOccurrences <- runningOccurrences %>%
      # Remove the ambiguous data from the prior-matched data
      dplyr::filter(!database_id %in% runningOccurrences_verb$database_id) %>%
      # Add in the ambiguous data again.
      dplyr::bind_rows(runningOccurrences_verb)
    
    # Remove the spent runningAmb_occs data
    rm(runningOccurrences_verb, runningAmb_occs_verb)
    
  }
  
  
  #### 5.0 Merge ####
  writeLines(" - Formatting merged datasets...")
  # merge datasets
  runningOccurrences <- runningOccurrences %>%
    # Put the scientific name into a new column called verbatimScientificName
    dplyr::mutate(verbatimScientificName = scientificName) %>%
    # select the columns we want to keep
    dplyr::select( c(tidyselect::any_of(OG_colnames), validName_valid, 
                     verbatimScientificName,
                     family_valid, subfamily_valid,
                     canonical_withFlags_valid, genus_valid, subgenus_valid, 
                     species_valid, infraspecies_valid, authorship_valid)) %>%
    # rename validName_valid to scientificName and place it where it used to sit.
    dplyr::mutate(scientificName = validName_valid, .after = database_id) %>%
    # Add in the other taxonomic data
    dplyr::mutate(species = canonical_withFlags_valid,
                  family = family_valid, 
                  subfamily = subfamily_valid,
                  genus = genus_valid,
                  subgenus = subgenus_valid,
                  specificEpithet = species_valid,
                  infraspecificEpithet = infraspecies_valid,
                  scientificNameAuthorship = authorship_valid,
                  .after = scientificName) %>%
    # Remove extra columns
    dplyr::select(!c(canonical_withFlags_valid, family_valid, subfamily_valid, genus_valid,
                     subgenus_valid, species_valid, infraspecies_valid, authorship_valid,
                     validName_valid)) %>%
    # Add the .invalidName columns as TRUE (not flagged)
    dplyr::mutate(.invalidName = TRUE)
  
  
  ##### 5.1 User output ####
  nMatchedRows <- nrow(runningOccurrences)
  nUnmatchedRows <- nrow(data) -  nrow(runningOccurrences)
  
  ###### a. failedMatches ####
  # Find the data that did not match
  failedMatches <- data %>%
    # Remove the matched names from the OG dataset
    dplyr::filter(!database_id %in% runningOccurrences$database_id) %>%
    # Add the .invalidName columns as TRUE (not flagged)
    dplyr::mutate(.invalidName = FALSE) 
  # Remove this spent file
  rm(data)
  
  
  ###### b. Add column ####
  runningOccurrences <- runningOccurrences %>%
    # Bind the failed matches
    dplyr::bind_rows(failedMatches) %>%
    # Make sure no duplicates have snuck in
    dplyr::distinct(database_id, .keep_all = TRUE)
  
  
  #### 6.0 Less strict ####
    ##### 6.1 Loosen ambiguous names ####
  # If the user wants to loosen the requirement to exclude ambiguous names then provide that option
  if(relaxAmbiguous == TRUE){
    runningAmbiguous <- runningOccurrences %>%
      # First, select the invalid names
      dplyr::filter(.invalidName == FALSE)
    writeLines(paste0(" You have chose to relax ambiguous names. Checking ",
                      format(nrow(runningAmbiguous), big.mark = ","),
               " failed names now..."))
    
    # Run the unambiguous function, but FIRST remove all of the ambiguous warnings by clearing the 
    # "flags" column in the taxonomy file.
    taxonomy <- taxonomy %>%
      dplyr::mutate(flags = "")
    
    runningAmbiguous_matched <- runningAmbiguous %>%
      # Make a new column with the ordering of rows
      dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>%
      # Group by the row number and step size
      dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>%
      # Split the dataset up into a list by group
      dplyr::group_split(.keep = TRUE) %>%
      # Run the actual function
      parallel::mclapply(., unAmbiguousFunction,
                         mc.cores = mc.cores
      ) %>%
      # Combine the lists of tibbles
      dplyr::bind_rows() %>%
      # Put the scientific name into a new column called verbatimScientificName
      dplyr::mutate(verbatimScientificName = scientificName) %>%
      # select the columns we want to keep
      dplyr::select( c(tidyselect::any_of(OG_colnames), validName_valid, 
                       verbatimScientificName,
                       family_valid, subfamily_valid,
                       canonical_withFlags_valid, genus_valid, subgenus_valid, 
                       species_valid, infraspecies_valid, authorship_valid)) %>%
      # rename validName_valid to scientificName and place it where it used to sit.
      dplyr::mutate(scientificName = validName_valid, .after = database_id) %>%
      # Add in the other taxonomic data
      dplyr::mutate(species = canonical_withFlags_valid,
                    family = family_valid, 
                    subfamily = subfamily_valid,
                    genus = genus_valid,
                    subgenus = subgenus_valid,
                    specificEpithet = species_valid,
                    infraspecificEpithet = infraspecies_valid,
                    scientificNameAuthorship = authorship_valid,
                    .after = scientificName) %>%
      # Remove extra columns
      dplyr::select(!c(canonical_withFlags_valid, family_valid, subfamily_valid, genus_valid,
                       subgenus_valid, species_valid, infraspecies_valid, authorship_valid,
                       validName_valid)) %>%
      # Add the .invalidName columns as TRUE (not flagged)
      dplyr::mutate(.invalidName = TRUE) %>%
        # Remove the authorship from the scientificName column
      dplyr::mutate(scientificName = stringr::str_remove(scientificName, scientificNameAuthorship) %>%
                      stringr::str_remove_all("\\(\\)") %>% 
                      stringr::str_squish(),
                    scientificNameAuthorship = "ambiguouslyMatchedName")
    # Provide some user feedback
    writeLines(paste0(" BeeBDC has matched ",
                      format(nrow(runningAmbiguous_matched), big.mark = ","),
                      " rows that would otherwise be excluded as ambiguous names.\n",
                      "Remember that these names will be listed as 'ambiguouslyMatchedName' ", 
                      "under the 'scientificNameAuthorship' column and the speciesColumn will NOT ",
                      "include the authority information. Hence, the names can be used but in",
                      "should remain clear in the data that they must be used with caution. "))
    
    # Combine this file with all of the failed names
    runningAmbiguous_combined <- runningAmbiguous %>% 
        # Remove the newly matched ambiguous data
      dplyr::filter(!database_id %in% runningAmbiguous_matched$database_id) %>%
        # Re-add it in again
      dplyr::bind_rows(runningAmbiguous_matched) 
    rm(runningAmbiguous_matched, runningAmbiguous)
    
      # Combine with the overall file
    # Combine this file with all of the failed names
    runningOccurrences <- runningOccurrences %>% 
      # Remove the newly matched ambiguous data
      dplyr::filter(!database_id %in% runningAmbiguous_combined$database_id) %>%
      # Re-add it in again
      dplyr::bind_rows(runningAmbiguous_combined) 
    rm(runningAmbiguous_combined)
  }

    ##### 6.2 Match higher taxonomy ####
  # Names are only matched at the species level. Here, BeeBDC will also match higher taxonomy 
  # names so that family and such can be added in
if(matchHigherTaxonomy == TRUE){
  # Create and order of operations. Genus goes before subgenus to avoid problems with exact matches
  columnOrder <- c("kingdom"  ,"phylum"    ,"class",
                   "order"    ,"family"    ,"subfamily","tribe",       
                   "subtribe","subgenus","genus") %>% rev()
    # Get a simplified taxonomy of subgenus and higher
  simpleTaxonomy <- taxonomy %>%
    # Take only valid names
    dplyr::filter(taxonomic_status == "accepted") %>%
    dplyr::select(tidyselect::any_of(columnOrder)) %>%
    dplyr::distinct()
    
  runningHigher <- runningOccurrences %>%
    # Get the failed names
    dplyr::filter(.invalidName == FALSE) %>% 
      # Remove the columns of interest
    dplyr::select(!tidyselect::any_of(columnOrder))
  

  # Create a function to check each column for higher taxonomy from bottom up
  higherTaxon_fun <- function(
    higherData = runningHigher,
    higherTaxonomy = simpleTaxonomy,
    levelOrder = columnOrder,
    #level = "subgenus",
    level = NULL){
    # Conditionally remove subgenus from match if searching for genus
    if(level == "genus"){
      higherTaxonomy <- higherTaxonomy %>%
      dplyr::mutate(subgenus = NA_character_)
    }
    if(!level == "subgenus"){
    # For each level, remove the preceding levels' columns
    higherTaxonomy <- higherTaxonomy[,stringr::str_which(string = levelOrder,
                                               pattern = paste0("^",level,"$")):length(levelOrder)]
    }
    # Extract the level to work with 
    taxonLevel <- higherTaxonomy %>%
        # Get only complete cases for that level
      dplyr::filter(complete.cases(.data[[level]])) %>%
        # Get only distinct data
      dplyr::distinct()
    
    # look for matches
    higherData_matched <- higherData %>%
      dplyr::left_join(taxonLevel,
                       by = c("names_clean" = level),
                       keep = TRUE) %>%
      # Re-order columns by the beebdc colTypeR order
      dplyr::select(tidyselect::any_of(c(names(BeeBDC::ColTypeR()$cols),
                                          # Ensure that all names are included 
                                         c("kingdom"  ,"phylum"    ,"class",
                                           "order"    ,"family"    ,"subfamily","tribe",       
                                           "subtribe","subgenus","genus")))) %>% 
      # Keep only matches
      dplyr::filter(complete.cases(kingdom)) %>% 
        # Add in the taxonRank column info
      dplyr::mutate(taxonRank = dplyr::if_else(is.na(taxonRank),
                                               level, NA_character_))
    # Return matches
    return(higherData_matched)
  }
    # Run the function to find all of the higher taxonomies
  runningHigher_matched <- lapply(
    X = as.list(columnOrder),
    FUN = higherTaxon_fun,
    higherData = runningHigher,
    higherTaxonomy = simpleTaxonomy,
    levelOrder = columnOrder
  ) %>%
    dplyr::bind_rows()  %>%
    dplyr::distinct(database_id, .keep_all = TRUE)
  
  
  # Combine this file with all of the failed names
  runningHigher_combined <- runningHigher %>% 
    # Remove the newly matched ambiguous data
    dplyr::filter(!database_id %in% runningHigher_matched$database_id) %>%
    # Re-add it in again
    dplyr::bind_rows(runningHigher_matched) 
  rm(runningHigher_matched, runningHigher)
  
  # Combine with the overall file
  # Combine this file with all of the failed names
  runningOccurrences <- runningOccurrences %>% 
    # Remove the newly matched ambiguous data
    dplyr::filter(!database_id %in% runningHigher_combined$database_id) %>%
    # Re-add it in again
    dplyr::bind_rows(runningHigher_combined) 
  rm(runningHigher_combined)
  
}
  
  
  #### 7.0 Return and output ####
  ##### 7.1 Repair sp column ####
  # Return the speciesColumn name to it's original state
  names(runningOccurrences)[names(runningOccurrences) == "scientificName"] <- speciesColumn
  if(rm_names_clean == TRUE){
    message("Removing the names_clean column...")
    runningOccurrences <- runningOccurrences %>% 
      dplyr::select(!tidyselect::any_of("names_clean"))
  }
  
  
  # Cut down the failed list...
  # failedMatches <- failedMatches %>%
  #   dplyr::select(tidyselect::any_of("taxonRank")) %>%
  #   dplyr::filter(!taxonRank %in% c("Especie", "forma", "Infrasubspecies", "Race",
  #                                    "species", "Species", "SPECIES", "subsp.", "subspecies",
  #                                   "Subspecies", "SUBSPECIES", "syn", "var.", "variety",
  #                                   "Variety", "VARIETY")) 
  
  
  ##### 7.2 Output ####
  writeLines(paste(
    " - We matched valid names to ", 
    format(sum(runningOccurrences$.invalidName == TRUE), big.mark = ","), " of ",
    format(OG_rowNum, big.mark = ","), " occurrence records. This leaves a total of ",
    format(sum(runningOccurrences$.invalidName == FALSE), big.mark = ","), " unmatched occurrence records.",
    #  " Of the unmatched records, approximately ", 
    # format( nrow(failedMatches), big.mark = ","), 
    #  " are only identified to genus rank or higher.",
    sep = ""))
  
  writeLines(paste("\nharmoniseR:"))
  message(paste(format(sum(runningOccurrences$.invalidName == FALSE), big.mark = ","))) 
  writeLines(paste(
    "records were flagged.\nThe column, '.invalidName' was added to the database.\n"))
  
  message(paste0(
    " - We updated the following columns: ", speciesColumn,", species, family, subfamily, genus, subgenus, ",
    "specificEpithet, infraspecificEpithet, and scientificNameAuthorship. ",
    "The previous ",speciesColumn," column was converted to verbatimScientificName"
  ))
  
  # End time message
  endTime <- Sys.time()
  message(paste(
    " - Completed in ", 
    round(difftime(endTime, startTime), digits = 2 ),
    " ",
    units(round(endTime - startTime, digits = 2)),
    sep = ""))
  
  ##### 7.3 Return ####
  # Return this file
  return(runningOccurrences)
}
