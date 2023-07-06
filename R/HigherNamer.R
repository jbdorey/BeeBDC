# This function was written by James Dorey to match higher order names to species.genus names
# For queries, please contact James Dorey at jbdorey[at]me.com
# This function was started on 15th May 2022 and last updated 17th May 2022
#' @importFrom dplyr %>%

HigherNamer <- function(HigherNameList = HigherOrders,
                        InSynList = DLdf){
  # locally bind variables to the function
  HigherOrders <- DLdf <- taxonomic_status <- validName <- family <- subfamily <- NULL
  subfamily <- tribe <- subtribe <- id <- NULL
  
  # Drop the completely NA row at the top
  InSynList <- InSynList %>%
    tidyr::drop_na(taxonomic_status) %>%
      # Drop some left-over Discover life stuff
    dplyr::filter(!validName %in% c("Kinds of Apoidea species", "Scientific name",
                                    "Other names")) %>%
    dplyr::filter(!stringr::str_detect(validName, "Updated: |Discover Life"))
  
  # Match and copy the Higher Order names across
  InSynList$family  <- HigherNameList$family[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  InSynList$subfamily  <- HigherNameList$subfamily[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  InSynList$tribe  <- HigherNameList$tribe[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  InSynList$subtribe  <- HigherNameList$subtribe[ cbind(match(InSynList$genus, HigherNameList$Genus ) )]
  
  # Make sure these are nurmeric columns...
  InSynList$accid <- as.numeric(InSynList$accid)
  InSynList$id <- as.numeric(InSynList$id)
  
  # For those synonyms that do not have a higher taxonomy, take it from the accepted name
  emptyFamily <- InSynList %>%
    dplyr::filter(is.na(family))
    # Match to their accepted names
  emptyFamily <- emptyFamily %>%
    dplyr::select(!c(family, subfamily, tribe, subtribe)) %>%
    dplyr::left_join(InSynList %>% dplyr::select(c(family, subfamily, tribe, subtribe, id)),
                     by = c("accid" = "id"))
  
    # Re-join these data
  InSynList <- InSynList %>%
    dplyr::filter(!is.na(family)) %>%
    dplyr::bind_rows(emptyFamily)
  
  # Return the list
  return(InSynList)
} # END function
