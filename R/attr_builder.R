#####  2.2 attr_builder ####
#' @importFrom lubridate as_date
#' @importFrom stringr str_to_sentence
#' @importFrom dplyr %>%

attr_builder <- function(path_i = path_i, occ_input = data_i){
  # locally bind variables to the function
  lubridate <- stringr <- family <- data_i <- NULL
  
  requireNamespace("lubridate")
  requireNamespace("dplyr")
  requireNamespace("xml2")
  # This function behaves differently depending on the data source, but returns common outputs.
  #### ALA START ####
  if(grepl("/data.csv", path_i) == "TRUE"){
    # Find and take the citations file 
    citations_i <- gsub("/data.csv", "/citation.csv", path_i) %>% 
      readr::read_csv(col_types = readr::cols(.default = readr::col_character())) 
    # Find and take the download information file 
    galahDL_i <- gsub("/data.csv", "/galah_DL_info.csv", path_i) %>% 
      readr::read_csv(n_max = 1, col_types = readr::cols(.default = readr::col_character())) 
    # Read in the closest thing ALA has to an abstract
    galahAbstract_i <- gsub("/data.csv", "/README.html", path_i) %>% 
      rvest::read_html() %>%
      rvest::html_text2()
    # Combine all of these attributes into a tibble
    Attributes_i <- dplyr::tibble(dataSource = paste("ALA_", 
                                                      galahDL_i$taxon, 
                                                      sep = ""),
                                   alternateIdentifier = if("search_url" %in% colnames(galahDL_i)){
                                     galahDL_i$search_url}else(NA_character_),
                                   title = "ALA Occurrence Download. ", 
                                   pubDate = galahDL_i$download_date %>%
                                                              lubridate::as_date(),
                                   dateStamp = galahDL_i$download_date,
                                   doi = galahDL_i$doi,
                                   downloadLink = galahDL_i$download_link,
                                   abstract = dplyr::lst(galahAbstract_i),
                                   citations = dplyr::lst(citations_i),
                                   downloadCitation = paste("ALA.org.au. (", 
                                                            lubridate::as_date(galahDL_i$download_date) %>% 
                                                              format("%d %B %Y"),
                                                            "). ALA Occurrence Download. ",
                                                            galahDL_i$doi,
                                                            sep = ""),
                                   rights = dplyr::lst("See occurrence records"),
                                  taxon = galahDL_i$taxon)
    
    # combine the input eml and the attributes tibble into a list for output from the function
    EML_attributes <- list("No_eml_from_ALA", Attributes_i)
    names(EML_attributes) <- c("source_eml","Source_tibble") 
    # output this list
    return(EML_attributes)
  } # ALA END 
  
  #### GBIF START ####
  if(grepl("occurrence.txt", path_i) == "TRUE"){
    # Find and take the metadata file
    sourceEML_i <- emld::as_emld(gsub("/occurrence.txt", "/metadata.xml", path_i), from = "xml" )
    # Find and take the citations file - convert into a list
    citations_i <- gsub("/occurrence.txt", "/citations.txt", path_i) %>% 
      readr::read_lines() %>%
      dplyr::lst()
    # Find and take the rights file - convert into a list
    rights_i <- gsub("/occurrence.txt", "/rights.txt", path_i) %>% 
      readr::read_lines() %>%
      dplyr::lst()
    # Find the download name
    fam_name <- tidyr::drop_na(
      occ_input, tidyselect::any_of("family")) %>%
      dplyr::pull(family) %>%
      unique()
    # Combine all of these attributes int o tibble
    Attributes_i <- dplyr::tibble(dataSource = paste("GBIF_", 
                                                      fam_name, 
                                                      sep = ""),
                                   alternateIdentifier =  sourceEML_i$dataset$alternateIdentifier,
                                   title = sourceEML_i$dataset$title, 
                                   pubDate = sourceEML_i$dataset$pubDate %>% 
                                     stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
                                     lubridate::as_date(),
 dateStamp = sourceEML_i$additionalMetadata$metadata$gbif$dateStamp,
 doi = paste("https://doi.org/",
             sourceEML_i$additionalMetadata$metadata$gbif$citation$identifier, sep = ""),
 downloadLink = sourceEML_i$additionalMetadata$metadata$gbif$physical$distribution$online$url$url,
 abstract = dplyr::lst(sourceEML_i$dataset$abstract),
 citations = dplyr::lst(citations_i),
 downloadCitation = paste("GBIF.org. (", 
                          lubridate::as_date(sourceEML_i$dataset$pubDate) %>% 
                            format("%d %B %Y"),
                          "). GBIF Occurrence Download. ",
                          paste("https://doi.org/",
                                sourceEML_i$additionalMetadata$metadata$gbif$citation$identifier, 
                                sep = ""),
                          sep = ""),
                                   rights = dplyr::lst(rights_i),
 taxon = fam_name)
    
    # combine the input eml and the attributes tibble into a list for output from the function
    EML_attributes <- list(sourceEML_i, Attributes_i)
    names(EML_attributes) <- c("source_eml","Source_tibble") 
    # output this list
    return(EML_attributes)
  } # GBIF END 
  
  
  #### iDigBio START ####
  if(grepl("occurrence_raw.csv", path_i) == "TRUE"){
    # Find and take the citations file - convert into a list
    citations_i <- gsub("/occurrence_raw.csv", "/records.citation.txt", path_i) %>% 
      readr::read_lines() %>%
      dplyr::lst()
    # Combine all of these attributes int o tibble
    Attributes_i <- dplyr::tibble(dataSource = paste("iDigBio_", citations_i$.[2] %>% 
                                                        stringr::str_match_all("[A-Za-z]+") %>%
                                                        unlist() %>% 
                                                        dplyr::last(), sep = ""),
                                  # Check for alt identifier and include if there
                                   alternateIdentifier =  
if(length(path_i %>% 
           stringr::str_match_all("[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+") %>%
           unlist()) > 0){
  # TRUE
               path_i %>% 
                 stringr::str_match_all("[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+") %>%
                 unlist()
      }else{
        # FALSE
        NA_character_},
                                   title = citations_i$.[1], 
                                   pubDate = citations_i$.[3] %>% 
  stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
                                     lubridate::as_date(),
                                   dateStamp = citations_i$.[3] %>% 
  stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}") %>%
                                     as.character(),
                                   doi = "Use downloadLink for iDigBio",
                                   downloadLink = paste("http://s.idigbio.org/idigbio-downloads/", 
                                                        path_i %>% 
stringr::str_match_all("[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+"),
                                                        ".zip", sep = ""),
                                   abstract = dplyr::lst(citations_i$.[1:4]),
                                   citations = dplyr::lst(citations_i$.[4:length(citations_i$.)]),
                                   downloadCitation = paste("iDigBio.org. (", 
                                                            citations_i$.[3] %>% 
stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
                                                              lubridate::as_date() %>% 
                                                              format("%d %B %Y"),
                                                            "). iDigBio Occurrence Download. ",
                                                  paste("http://s.idigbio.org/idigbio-downloads/", 
path_i %>% 
  stringr::str_match_all("[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+-[a-zA-Z0-9]+"),
                                                                  ".zip", sep = ""),
                                                            sep = ""),
                                   rights = dplyr::lst("See occurrence records"),
taxon = (citations_i$.[2] %>% 
  stringr::str_match_all("[A-Za-z]+") %>%
  unlist() %>% 
  dplyr::last())
) # END tibble()
    
    # combine the input eml and the attributes tibble into a list for output from the function
    EML_attributes <- list("No_eml_from_iDigBio", Attributes_i)
    names(EML_attributes) <- c("source_eml","Source_tibble") 
    # output this list
    return(EML_attributes)
  } # iDigBio END 
  
  #### SCAN START ####
  if(grepl("occurrences.csv", path_i) == "TRUE"){
    # Find and take the metadata file
    sourceEML_i <- xml2::read_xml(gsub("/occurrences.csv", "/eml.xml", path_i), from = "xml" ) %>% 
      emld::as_emld()
    # Combine all of these attributes int o tibble
    Attributes_i <- dplyr::tibble(dataSource = paste("SCAN_", 
                                                      unique(stringr::str_to_sentence(occ_input$family)), 
                                                      sep = ""),
alternateIdentifier =  sourceEML_i$additionalMetadata$metadata$symbiota$citation$identifier,
                                   title =  sourceEML_i$dataset$title$title, 
pubDate = sourceEML_i$additionalMetadata$metadata$symbiota$dateStamp %>% 
  stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
                                     lubridate::as_date(),
dateStamp = sourceEML_i$additionalMetadata$metadata$symbiota$dateStamp,
                                   doi = "SCAN does not provide a doi. See download link.",
downloadLink = "SCAN does not provide a download link.",
                                   abstract = dplyr::lst("SCAN does not provide a single abstract"),
                                   citations =     dplyr::lst(paste("SCAN. ", 
                          (sourceEML_i$additionalMetadata$metadata$symbiota$dateStamp %>% 
                             stringr::str_match("[0-9]{4}")),
                          ". http//:scan-bugs.org/portal/index.php. ",
                          "accessed on ", 
                          (sourceEML_i$additionalMetadata$metadata$symbiota$dateStamp %>% 
                             stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}")),
                          ". ", sourceEML_i$additionalMetadata$metadata$symbiota$citation$citation, 
                          sep = "" ) ),
                                   downloadCitation = paste("SCAN. (", 
                         sourceEML_i$additionalMetadata$metadata$symbiota$dateStamp %>% 
                           stringr::str_match("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
                           lubridate::as_date() %>% 
                           format("%d %B %Y"),
                         "). SCAN-Bugs Occurrence Download ",
                         "http//:scan-bugs.org/portal/index.php. uuid - ",
                         sourceEML_i$additionalMetadata$metadata$symbiota$citation$identifier,
                         sep = ""),
                                   rights = dplyr::lst("See .xml for rights"),
taxon = unique(stringr::str_to_sentence(occ_input$family)))
    # combine the input eml and the attributes tibble into a list for output from the function
    EML_attributes <- list(sourceEML_i, Attributes_i)
    names(EML_attributes) <- c("source_eml","Source_tibble") 
    # output this list
    return(EML_attributes)
  } # SCAN END
} # END attr_builder
