


#' Query the bee taxonomy and country checklist
#'
#' A simple function to return information about a particular species, including name validity and
#' country occurrences.
#'
#' @param beeName Character or character vector. A single or several bee species names to search for
#' in the beesTaxonomy and beesChecklist tables.
#' @param searchChecklist Logical. If TRUE (default), search the country checklist for each species.
#' @param printAllSynonyms Logical. If TRUE, all synonyms will be printed out for each entered name.
#' default = FALSE.
#' @param beesChecklist A tibble. The bee checklist file for BeeBDC. If is NULL then 
#' [BeeBDC::beesChecklist()] will be called internally to download the file. Default = NULL.
#' @param beesTaxonomy A tibble. The bee taxonomy file for BeeBDC. If is NULL then 
#' [BeeBDC::beesTaxonomy()] will be called internally to download the file. Default = NULL.
#'
#' @return Returns a list with the elements 'taxonomyReport' and 'SynonymReport'. IF searchChecklist
#' is TRUE, then 'checklistReport' will also be returned.
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#'   # For the sake of these examples, we will use the example taxonomy and checklist
#'   system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |> load()
#'   system.file("extdata", "testChecklist.rda", package="BeeBDC") |> load()
#' 
#'   # Single entry example
#' testQuery <- BeeBDCQuery(
#'   beeName = "Lasioglossum bicingulatum",
#'   searchChecklist = TRUE,
#'   printAllSynonyms = TRUE,
#'   beesTaxonomy = testTaxonomy,
#'   beesChecklist = testChecklist)
#' 
#'   # Multiple entry example
#' testQuery <- BeeBDC::BeeBDCQuery(
#'   beeName = c("Lasioglossum bicingulatum", "Nomada flavopicta",
#'   "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
#'   searchChecklist = TRUE,
#'   printAllSynonyms = TRUE,
#'   beesTaxonomy = testTaxonomy,
#'   beesChecklist = testChecklist)
#'   
#'     # Example way to examine a report from the output list
#'   testQuery$checklistReport
#' 
#' 
#' 
BeeBDCQuery <- function(
    beeName = NULL,
    searchChecklist = TRUE,
    printAllSynonyms = FALSE,
    beesChecklist = NULL,
    beesTaxonomy = NULL){
  # locally bind variables to the function
  data <- validName <- accid <- inputName <- id <- 
    rowMatched <- . <- NULL
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(beeName)){
    stop(" - Please provide an argument for beeName. I'm a program not a magician.")
  }

  
  #### 1.0 Data preperation ####
  # Download the datasets
  if(searchChecklist == TRUE & is.null(beesChecklist)){
    beesChecklist <- BeeBDC::beesChecklist()}
  if(is.null(beesTaxonomy)){
    beesTaxonomy <- BeeBDC::beesTaxonomy()}
  
    # Change beeName to be matched exactly
  beeNameExact <- paste0("^",  beeName, "$")
  
  #### 2.0 Taxonomy check ####
  message("Starting taxonomy report...")
  ##### 2.1 Taxonomy report ####
  # Get a report of the queried names and their matched rows in beesTaxonomy 
  report_beesTaxonomy <- 
    # Make a tibble with the input name(s)
    dplyr::tibble(inputName = beeName) %>%
    dplyr::left_join(beesTaxonomy, relationship = "many-to-many", keep = TRUE,
                     by = c("inputName" = "validName")) %>%
    # Do the same and bind using canonical
    dplyr::bind_rows(
      dplyr::tibble(inputName = beeName) %>%
        dplyr::left_join(beesTaxonomy, relationship = "many-to-many", keep = TRUE,
                         by = c("inputName" = "canonical"))
    ) %>%
    # Do the same and bind using canonical_withFlags
    dplyr::bind_rows(
      dplyr::tibble(inputName = beeName) %>%
        dplyr::left_join(beesTaxonomy, relationship = "many-to-many", keep = TRUE,
                         by = c("inputName" = "canonical_withFlags"))) %>%
    # Drop rows that did not match
    tidyr::drop_na(validName) %>%
    # Keep only distinct rows
    dplyr::distinct() 
      
    # Extract accepted names
  acceptedNames <- report_beesTaxonomy %>%
    dplyr::filter(accid == 0) %>%
    dplyr::mutate(inputID = 0, .after = inputName)
    # Extract accepted names for synonyms provided
  synonyms <- report_beesTaxonomy %>%
    dplyr::filter(accid != 0) %>% 
  # Filter to the inputName and inputID
    dplyr::select(tidyselect::all_of(c("inputName", "accid"))) %>% 
    dplyr::rename("inputID" = "accid") %>%
      # Rejoin with the beesTaxonomy, but ONLY have valid names
    dplyr::left_join(beesTaxonomy, by = c("inputID" = "id"), keep = TRUE) %>%
    dplyr::distinct()
    # recombine into report_beesTaxonomy
  report_beesTaxonomy <- dplyr::bind_rows(acceptedNames, synonyms)
    
  
    ##### 2.2 Taxonomy output ####
  for(i in 1:nrow(report_beesTaxonomy)){
    writeLines(paste0(
      report_beesTaxonomy$inputName[[i]], " is ", 
      # IF Synonym
      if(report_beesTaxonomy$inputID[[i]] > 0){
        paste0("a synonym of ",
               report_beesTaxonomy$validName[[i]], " with the taxon id number ", 
               report_beesTaxonomy$id[[i]],
               ".")
      }else{ # IF accepted
        paste0("an accpeted name with the taxon id number ", report_beesTaxonomy$id[[i]], ".")
      } # END else
    ))
  } # End loop
  
    ##### 2.3 printAllSynonyms ####
  # Find the synonyms for the entered species
  synonymsMatched <- beesTaxonomy %>%
    dplyr::filter(accid %in% report_beesTaxonomy$id) %>%
    dplyr::left_join(report_beesTaxonomy %>% 
                       dplyr::select(tidyselect::all_of(c("inputName", "id"))),
                     by = c("accid" = "id"), relationship = "many-to-many") %>%
    dplyr::relocate(inputName, .before = "flags") %>%
    dplyr::arrange(accid, inputName)
  
    # If printAllSynonyms is TRUE then print the synonyms
  if(printAllSynonyms == TRUE){
    for(i in 1:length(report_beesTaxonomy$inputName)){
        # Get the synonyms for the ith entry
      synonymsLoop <- synonymsMatched %>%
        dplyr::filter(accid == report_beesTaxonomy$inputID[[i]])
      if(nrow(synonymsLoop) > 0){
        # Print user output
      writeLines(paste0(
        " - '", report_beesTaxonomy$inputName[[i]], "'",
        " has the synonyms: ", paste0(synonymsLoop$validName, collapse = ", ")
      ))} # END if > 0
      if(nrow(synonymsLoop) == 0){
        synonymsLoop <- synonymsMatched %>%
          dplyr::filter(accid == report_beesTaxonomy$id[[i]])
        # Print user output
        writeLines(paste0(
          " - '", report_beesTaxonomy$inputName[[i]], "'",
          " has the synonyms: ", unique(paste0(synonymsLoop$validName, collapse = ", ") %>%
                                          stringr::str_remove(report_beesTaxonomy$inputName[[i]]))
        ))} # END if > 0
      
      
    } # END loop
  } # END printAllSynonyms
  
  
  #### 3.0 Checklist ####
    ##### 3.1 Checklist user output ####
  if(searchChecklist == TRUE){
    message("Starting checklist report...")
      # Get the relevant rows of accepted names in the beesChecklist
    checklistMatched <- beesChecklist %>%
      dplyr::filter(validName %in% unique(report_beesTaxonomy$validName)) %>% 
      dplyr::arrange(validName)
 
      for(i in 1:length(unique(checklistMatched$validName))){
          # Select the ith species
        loopSpecies <- checklistMatched %>%
          dplyr::filter(validName == checklistMatched$validName[[i]])
          # User output
        writeLines(paste0(
          " - ", checklistMatched$validName[[i]],
          " is reportedly found in: \n",
          paste0(unique(loopSpecies$rNaturalEarth_name), collapse = ", ")
        ))
      }
    
  } # END searchChecklist
  
    #### 4.0 Return reports ####
    # Make a report of the species that did not match
  failedReport <- beeName %>%
    setdiff(., report_beesTaxonomy$inputName) %>%
    dplyr::tibble(unmatchedSpecies = .)
  
    # If searchChecklist is requested, then return the output as a list with it included
  if(searchChecklist == TRUE){
    output <- dplyr::lst(report_beesTaxonomy, synonymsMatched, checklistMatched, failedReport) %>%
      stats::setNames(c("taxonomyReport", "SynonymReport", "checklistReport", "failedReport"))
    writeLines(paste0(
      "The output will be returned as a list with the elements: ",
      "'taxonomyReport', 'SynonymReport', and 'checklistReport'. \n", "These can be accessed using",
      " 'output'$taxonomyReport, 'output'$SynonymReport, 'output'$checklistReport, or ",
      "'output'$failedReport."
    ))
  }else{
    output <- dplyr::lst(report_beesTaxonomy, synonymsMatched, failedReport) %>%
      stats::setNames(c("taxonomyReport", "SynonymReport", "failedReport"))
    writeLines(paste0(
      "The output will be returned as a list with the elements: ",
      "'taxonomyReport' and 'SynonymReport'. \n", "These can be accessed using",
      " 'output'$taxonomyReport, 'output'$SynonymReport, or ",
      "'output'$failedReport."
    ))
  }
 
   # Return the matched data
  return(output)
}  # END BeeBDCQuery


