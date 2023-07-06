# This function was written by James B Dorey on the 20th of February 2023 to remove too-simple
  # id codes from occurrences. It is intended to be used internal in the jbd_dupeSummary function
#' @importFrom dplyr %>%
#' 
deSimplifieR <- function(inputData = NULL,
                         characterThreshold = 2,
                         numberThreshold = 3,
                         numberOnlyThreshold = 5)
{
  
  requireNamespace("dplyr")
  
  #### 1.0 Remove simple strings ####
    # Remove simple codes, only do for each column if that column exists
  inputData <- inputData %>%
  dplyr::mutate(
    if("occurrenceID" %in% colnames(inputData)){
    occurrenceID = dplyr::if_else( stringr::str_count(occurrenceID, "[A-Za-z]") >= characterThreshold &
                                     stringr::str_count(occurrenceID, "[0-9]") >= numberThreshold |
                                     stringr::str_count(occurrenceID, "[0-9]") >= numberOnlyThreshold, 
                                   occurrenceID, NA_character_)},
    if("recordId" %in% colnames(inputData)){
    recordId = dplyr::if_else( stringr::str_count(recordId, "[A-Za-z]") >= characterThreshold &
                                 stringr::str_count(recordId, "[0-9]") >= numberThreshold |
                                 stringr::str_count(recordId, "[0-9]") >= numberOnlyThreshold, 
                               recordId, NA_character_)},
    if("id" %in% colnames(inputData)){
    id = dplyr::if_else( stringr::str_count(id, "[A-Za-z]") >= characterThreshold &
                           stringr::str_count(id, "[0-9]") >= numberThreshold |
                           stringr::str_count(id, "[0-9]") >= numberOnlyThreshold, 
                         id, NA_character_)}, 
    if("catalogNumber" %in% colnames(inputData)){
    catalogNumber = dplyr::if_else( stringr::str_count(catalogNumber, "[A-Za-z]") >= characterThreshold &
                                      stringr::str_count(catalogNumber, "[0-9]") >= numberThreshold |
                                      stringr::str_count(catalogNumber, "[0-9]") >= numberOnlyThreshold, 
                                    catalogNumber, NA_character_)}, 
    if("otherCatalogNumbers" %in% colnames(inputData)){
    otherCatalogNumbers = dplyr::if_else( stringr::str_count(otherCatalogNumbers, "[A-Za-z]") >= characterThreshold &
                                            stringr::str_count(otherCatalogNumbers, "[0-9]") >= numberThreshold |
                                            stringr::str_count(otherCatalogNumbers, "[0-9]") >= numberOnlyThreshold, 
                                          otherCatalogNumbers, NA_character_)})


return(inputData)
}