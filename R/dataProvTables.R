# This Function is designed to produce a summary table of data sources.
# It was written by James B Dorey from the 20th of January 2023.
#' @importFrom dplyr %>%
dataProvTables <- function(
    data = NULL,
    runBeeDataChecks = FALSE,
    institutionList = NULL
){
  requireNamespace("dplyr")
  
  #### 0.0 Warnings ####
  if(is.null(data)){
    stop("You must provide a checklist of countries")
  }
  # locally bind variables to the function
  dataSource<-institutionCode<-recordNumber<-institutionCodeNew<-recordedBy<-occurrenceID<-
    catalogNumber<-bibliographicCitation<-datasetName<-otherCatalogNumbers<-rightsHolder<-
    references<-eventID<-datasetID<-database_id<-id<-scientificName<-DataPath <- NULL
  
  #### 1.0 Data prep ####
  if(runBeeDataChecks == TRUE){
      ##### 1.1 Find codes ####
    # Find institutionCodes using other information
    temp <- data %>%
        # Select ALA as data source and only those missing institutionCode
      dplyr::filter(stringr::str_detect(dataSource, "ALA|GBIF|iDigBio") & 
                      is.na(institutionCode)) %>%
        # Find clues to institutionCode and add it
      dplyr::mutate(
        # WAM
        institutionCodeNew = ifelse(
          stringr::str_detect(recordNumber, "^WAM "), "WAM", NA),
        # HYM
        institutionCodeNew = dplyr::if_else(stringr::str_detect(recordNumber, "^HYM ") & 
                                              is.na(institutionCodeNew), "NMV", institutionCodeNew),
        institutionCodeNew = dplyr::if_else(stringr::str_detect(recordedBy, "Assorted Museum of Victoria") & 
                                              is.na(institutionCodeNew), "NMV", institutionCodeNew),
          # bowerbird
        institutionCodeNew = dplyr::if_else(stringr::str_detect(recordNumber, "bowerbird") & 
                                              is.na(institutionCodeNew), "bowerbird", institutionCodeNew),
        # PaDIL
        institutionCodeNew = dplyr::if_else(stringr::str_detect(tolower(recordNumber), "^bee|^rlj ") & 
                                              is.na(institutionCodeNew), "PaDIL", institutionCodeNew),
        # Flickr
        institutionCodeNew = dplyr::if_else( stringr::str_detect(tolower(occurrenceID), "flickr") & 
                                              is.na(institutionCodeNew), "Flickr", institutionCodeNew),
        # questagame
        institutionCodeNew = dplyr::if_else( stringr::str_detect(tolower(recordedBy), "questagame") & 
                                               is.na(institutionCodeNew), "QuestaGame", institutionCodeNew),
        # WINC
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "WINC") & 
                                               is.na(institutionCodeNew), "WINC", institutionCodeNew),
        # biocollectALA
        institutionCodeNew = dplyr::if_else( stringr::str_detect(recordNumber, "biocollect") & 
                                               is.na(institutionCodeNew), "Wildlife Watch NSC", institutionCodeNew),
        # Aus Museum
        institutionCodeNew = dplyr::if_else( stringr::str_detect(recordNumber, "^K   |AM  ") & 
                                               is.na(institutionCodeNew), "AM", institutionCodeNew),
        # WildNet
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "WildNet") & 
                                               is.na(institutionCodeNew), "WildNet", institutionCodeNew),
        ## OTHER
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "^RSKM_ENT") & 
                                               is.na(institutionCodeNew), "RSKM", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "^UVM") & 
                                               is.na(institutionCodeNew), "UVM", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "^BIOUG") & 
                                               is.na(institutionCodeNew), "BIOUG", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "^ZMUO") & 
                                               is.na(institutionCodeNew), "ZMUO", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "data\\.biodiversitydata\\.nl") & 
                                               is.na(institutionCodeNew), "Naturalis Biodiversity Center", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(bibliographicCitation, "FinBIF") & 
                                               is.na(institutionCodeNew), "FinBIF", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetName, "Swiss National Apoidea Databank") & 
                                               is.na(institutionCodeNew), "CSCF", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(otherCatalogNumbers, "VTST_ENT") & 
                                               is.na(institutionCodeNew), "VTST", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(otherCatalogNumbers, "ECNRUFC") & 
                                               is.na(institutionCodeNew), "ECNRUFC", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(recordedBy, "SPIPOLL") & 
                                               is.na(institutionCodeNew), "SPIPOLL", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetName, "Riiklik keskkonnaseire programm") & 
                                               is.na(institutionCodeNew), "Riiklik keskkonnaseire programm", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Suffolk Biodiversity Information Service") & 
                                               is.na(institutionCodeNew), "Suffolk Biodiversity Information Service", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Staffordshire Ecological Record") & 
                                               is.na(institutionCodeNew), "Staffordshire Ecological Record", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "NatureSpot") & 
                                               is.na(institutionCodeNew), "NatureSpot", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Biological Records Centre") & 
                                               is.na(institutionCodeNew), "Biological Records Centre", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(references, "www\\.ebi\\.ac\\.uk/ena") & 
                                               is.na(institutionCodeNew), "European Nucleotide Archive", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "observation\\.org") & 
                                               is.na(institutionCodeNew), "Observation.org", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(eventID, "plutof.ut.ee") & 
                                               is.na(institutionCodeNew), "Plotuf", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "USNM ENT") & 
                                               is.na(institutionCodeNew), "USNM", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "boldsystems\\.org") & 
                                               is.na(institutionCodeNew), "BOLD", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetName, "Chronicle of Nature") & 
                                               is.na(institutionCodeNew), "Chronicle of Nature", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Isle of Wight Local Records Centre") & 
                                               is.na(institutionCodeNew), "Isle of Wight Local Records Centre", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Gloucestershire Centre for Environmental Records") & 
                                               is.na(institutionCodeNew), "Gloucestershire Centre for Environmental Records", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetName, "Bumble bees collected in a large-scale field experiment in power line clearings, southeast Norway") & 
                                               is.na(institutionCodeNew), "Norwegian University of Life Sciences (NMBU)", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(eventID, "WestAfricaBees") & 
                                               is.na(institutionCodeNew), "Station d'Ecologie de Lamto", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(eventID, "TaborW") & 
                                               is.na(institutionCodeNew), "Vermont Center for Ecostudies", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetName, "Abeilles fichier Nico Schneider") & 
                                               is.na(institutionCodeNew), "National Museum of Natural History, Luxembourg", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "BSRU") & 
                                               is.na(institutionCodeNew), "Chulalongkorn University Natural History Museum (CUNHM)", institutionCodeNew),
        
institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "MFV:VT|USFWS-RCN|VCE:|USfWS-RCN|MNWR:bombus|^Frey21-") & 
                                               is.na(institutionCodeNew), "Vermont Center for Ecostudies", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "21BAM") & 
                                               is.na(institutionCodeNew), "Vermont Center for Ecostudies", institutionCodeNew),

        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "Par\\.V\\.|Viter\\.|Vit\\.S|^UNCG|Rufford\\.UA|Parkh\\.faun|Medobory\\.data|Ins\\.Khar") & 
                                               is.na(institutionCodeNew), "Ukrainian Nature Conservation Group (UNCG)", institutionCodeNew),

        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "Calabuig:Ringsted") & 
                                               is.na(institutionCodeNew), "Natural History Museum of Denmark", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Cambridgeshire & Peterborough Environmental Records Centre") & 
                                               is.na(institutionCodeNew), "Cambridgeshire & Peterborough Environmental Records Centre", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(references, "natureshare\\.org\\.au") & 
                                               is.na(institutionCodeNew), "NatureShare", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Manx Wildlife Trust") & 
                                               is.na(institutionCodeNew), "Manx Wildlife Trust", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Ministry of Justice") & 
                                               is.na(institutionCodeNew), "Ministry of Justice, UK", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Natural England") & 
                                               is.na(institutionCodeNew), "Natural England", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Queensland Government") & 
                                               is.na(institutionCodeNew), "Queensland Government", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Royal Horticultural Society") & 
                                               is.na(institutionCodeNew), "Royal Horticultural Society", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Sheffield and Rotherham Wildlife Trust") & 
                                               is.na(institutionCodeNew), "Sheffield and Rotherham Wildlife Trust", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(rightsHolder, "Prioksko-Terrasnyi Biosphere Reserve") & 
                                               is.na(institutionCodeNew), "Prioksko-Terrasnyi Biosphere Reserve", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "mohonk:") & 
                                               is.na(institutionCodeNew), "Mohonk Preserve", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "^LUXNATFUND|^DSS00") & 
                                               is.na(institutionCodeNew), "National Museum of Natural History, Luxembourg", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetName, "Collection hymenopteres MNHNL") & 
                                               is.na(institutionCodeNew), "National Museum of Natural History, Luxembourg", institutionCodeNew),
        
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "IIA-ENT") & 
                                               is.na(institutionCodeNew), "INSTITUTO DE INVESTIGACaO AGRONOMICA - IIA", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(occurrenceID, "indiabiodiversity\\.org") & 
                                               is.na(institutionCodeNew), "IndiaBiodiversity.org", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "^OPI") & 
                                               is.na(institutionCodeNew), "South Australia, Department for Environment and Water", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(catalogNumber, "^WFM[0-9]+") & 
                                               is.na(institutionCodeNew), "Western Australia, Department of Biodiversity, Conservation and Attractions", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(recordedBy, "LIMOUSIN,BUREAU D'ETUDE DGE|PNR PL|Agniele Touret-Alby, Quentin Rome|AGASSE-YVER Florence, MARLE Micka|AUBERT Matthieu|BESSIERE A\\., BLEOMELEN Alwin \\(PNM\\)|Bottinelli Julien|PERCHE NATURE|BRUGEROLLES Yvan|CENSE Thierry,CENSE Colette|CESARI Lily \\(Naturoptere\\)|CHOREIN Adrien \\(CEN Centre\\)|Cocquempot C\\.|DAMOISEAU Sebastien \\(CERCOPE\\)|GENOUD David|Gosselin M\\.|Grumdi|Jean-Pierre Viallard|Jean-Pierre Carreras|Jean-Loup d'Hondt|Jean-Loup d'Hondt|Jean-Louis PRATZ|Jean-Jacques Laffitte|Jean-Laurent HENTZ|Jean-Francois Campion|Jean-Sebastien Carteron|LE DIVELEC Romain|MAILLIER Sebastien|MARTHA Benoit|PLATEAUX Luc|PRATZ Jean-Louis|Sebastien LAGUET|Sebbbounet|Thierry ROBERT|\\(ECOSPHERE\\)|\\(CEN AQUITAINE\\)") & 
                                               is.na(institutionCodeNew), "OFB-CNRS-MNHN", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(datasetID, "E053-2614A8C02B7C|B2C9849D2ACF|2614A8C021C1|2614A8C0FB88|2614A8C0E6FC|2614A8C0CF63|2614A8C05B99|2614A8C00BBE|2614A8C0C021|2614A8C0FC45|2614A8C067D6|2614A8C0753D|2614A8C057EA|5014A8C02001|5014A8C04A0C|2614A8C00722|-E053-|-e053-|F0DFF9845389|8EA38E099656|26033513335E|C9074EB78761|9AC2D5DAB4DF|DF2C4D61871E|E06C2F2DB641|A83B5C8B393F|3D6B50E67F30|6DB84CF2329A|EEE933FAF77E|9776F5D05D87|A174FB52126E|26E5D9FC07CC|1CF99798EDF1|4E1ECB87BC1F|B6A9BC006CB2|6943DF45F77E|E17388AB56E2|B65CCE479F2E|C02F4B1A3FE8") & 
                                               is.na(institutionCodeNew), "OFB-CNRS-MNHN", institutionCodeNew),
        institutionCodeNew = dplyr::if_else( stringr::str_detect(bibliographicCitation, "Wild bees of Belgium") & 
                                               is.na(institutionCodeNew), "Belgian Biodiversity Platform", institutionCodeNew),
        # COMBINE with    institutionCode                                         
        institutionCode = dplyr::if_else(is.na(institutionCode),
                                         institutionCodeNew,
                                         institutionCode) 
        ) %>%
      dplyr::select(!institutionCodeNew)
    
    # Combine
    data <- data %>%
      # Remove the occs from temp and then add them in again
      dplyr::filter(!database_id %in% temp$database_id) %>%
      dplyr::bind_rows(temp) 
    
      ##### 1.2 Make edits ####
    data <- data %>%
        # Make sure USGS is included
      dplyr::mutate(institutionCode = dplyr::if_else(
        is.na(institutionCode) & stringr::str_detect(id, "USGS_DRO"),
        "USGS", institutionCode)) 
    
    data <- data %>%
        # Make some corrections for consistency
    dplyr::mutate(institutionCode = dplyr::if_else(
      stringr::str_detect(institutionCode, "C\\.A\\. Triplehorn Insect Collection, Ohio State University"),
      "C.A. Triplehorn Insect Collection, Ohio State University, Columbus, OH (OSUC)", institutionCode)) %>%
    dplyr::mutate(institutionCode = dplyr::if_else(
      stringr::str_detect(institutionCode, "Caledonian Conservation"),
      "Caledonian Conservation", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "^CardObs$"),
        "CardObs", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "Instituto Nacional de Pesquisas da Amaz"),
        "Instituto Nacional de Pesquisas da Amazonia (INPA)", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "\\(JBB\\)"),
        "Jardin Botanico de Bogota Jose Celestino Mutis (JBB)", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "^SDA$"),
        "SDA - Secretaria Distrital de Ambiente", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "Universidad del Magdalena"),
        "Universidad del Magdalena (UniMagdalena)", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "Universidad Nacional de Colombia"),
        "Universidad Nacional de Colombia (UNAL)", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "University of Guelph"),
        "University of Guelph", institutionCode)) %>%
      dplyr::mutate(institutionCode = dplyr::if_else(
        stringr::str_detect(institutionCode, "USDA[ -]ARS"),
        "USDA-ARS", institutionCode))
      }
  
  #### 2.0 Table ####
  ##### 2.1 occCount ####
    # Get a count of the number of occurrences per institutionCode
  occCount <- data %>%
      # Group by institutionCode
    dplyr::group_by(institutionCode) %>%
      # Get a tally of occurrences for each institutionCode
    dplyr::add_tally() %>%
      # Select only institutionCode and the tally and then keep one of each
    dplyr::select(c(institutionCode, n)) %>%
    dplyr::distinct(institutionCode, .keep_all = TRUE) %>%
    # Rename the count column
    dplyr::rename("occurrenceCount" = n)
  
  ##### 2.2 spCount ####
    # Get a count of the number of species per institutionCode
  spCount <- data %>%
    # Keep only distinct institutionCode, scientificName combinations 
    dplyr::distinct(institutionCode, scientificName,.keep_all = TRUE) %>%
    # Group by institutionCode
    dplyr::group_by(institutionCode) %>%
    # Get a tally of occurrences for each institutionCode
    dplyr::add_tally() %>%
    # Select only institutionCode and the tally and then keep one of each
    dplyr::select(c(institutionCode, n)) %>%
    dplyr::distinct(institutionCode, .keep_all = TRUE) %>%
      # Rename the count column
    dplyr::rename("speciesCount" = n)
  
  ##### 2.3 Merge ####
  counts <- occCount %>%
    dplyr::left_join(spCount, by = "institutionCode")
  
  
  #### 3.0 Save and return ####
  readr::write_csv(counts, 
                   paste0(DataPath, "/Output/Report/", "dataProviders.csv"))

  
  return(counts)
  
  
} # END dataProvTables