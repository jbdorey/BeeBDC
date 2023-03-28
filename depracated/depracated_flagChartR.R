# This function was written by James Dorey to create charts from the flag data.
# This function was written from the 9th of June 2022. For questions, please email James
# at jbdorey@me.com

flagChartR <- function(
    data = NULL,
    flagType = NULL,
    path = NULL,
    largeVenns = FALSE){
  require(ggplot2)
  require(ggVennDiagram)
  require(dplyr)
  require(mgsub)
  #### 0.0 Prep ####
    ##### 0.1 Packages ####
  
    ##### 0.2 Warnings ####
  if(is.null(data)){
    warning(" — Please provide a dataset. I don't know where it is, I'm a program, damnit!")
  }
  if(is.null(flagType)){
    warning(paste(" — No flagTypes were selected! We will use the following flagTypes:\n",
                  "'initial' ", "'taxonomy' ", "'space', and ", "'time'. I.e. all of them.",
                  sep = ""))
    flagType = c("initial", "taxonomy", "space", "time",
                 "all")  }
  
    ##### 0.3 Invert TRUE/FALSE ####
  flagCols <- data %>%
    dplyr::select(tidyselect::starts_with("."))
  
  data <- data %>% 
    dplyr::select(!tidyselect::starts_with(".")) %>%
    dplyr::bind_cols(!flagCols)
  

  #### 1.0 Plot ####
    ##### 1.1 Initial plots ####
  if( any(c("initial", "all") %in% flagType)){
    ###### a. technical ####
    tempData <- data %>%
    # Select the relevant columns
    dplyr::select(database_id, .duplicate, .scientificName_empty, .coordinates_empty,
                  .occurrenceAbsent) %>%
    dplyr::rename(Duplicates = .duplicate,
                  SciName = .scientificName_empty,
                  NoCoordinates = .coordinates_empty,
                  AbsentRecord = .occurrenceAbsent)
  tempData$Duplicates <- dplyr::if_else(tempData$Duplicates == TRUE,
                                        tempData$database_id,
                                                     "NA")
  tempData$SciName <- dplyr::if_else(tempData$SciName == TRUE,
                                     tempData$database_id,
                                                     "NA")
  tempData$NoCoordinates <- dplyr::if_else(tempData$NoCoordinates == TRUE,
                                           tempData$database_id,
                                                       "NA")
  tempData$AbsentRecord <- dplyr::if_else(tempData$AbsentRecord == TRUE,
                                       tempData$database_id,
                                                    "NA")
  
  if(largeVenns == TRUE){
    tempData_technical = tempData
  }
  ##### Venn a ####
  if(largeVenns == FALSE){
    ggTechnical <-  ggVennDiagram::ggVennDiagram(dplyr::select(tempData, !database_id),
                                                 label_alpha = 0.5) +
    ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
  
  ggplot2::ggsave(plot = ggTechnical,
                  filename = "Flags_ggTechincal.pdf", 
                  path = paste(path, "Output", "Figures",
                               sep = "/"),
                  device = "pdf",
                  width = 9,
                  height = 6)
  } # END Venn a

  ##### b. spatial ####
  tempData <- data %>%
    # Select the relevant columns
    dplyr::select(database_id, .coordinates_outOfRange, 
                  .basisOfRecords_notStandard, .unLicensed,
                  .coordinates_country_inconsistent) %>%
    dplyr::rename(CoordsOffMap = .coordinates_outOfRange,
                  BasisOfRecord = .basisOfRecords_notStandard,
                  NoLicense = .unLicensed,
                  CoordsNotInCountry = .coordinates_country_inconsistent)
  tempData$CoordsOffMap <- dplyr::if_else(tempData$CoordsOffMap == TRUE,
                                                        tempData$database_id,
                                                     "NA")
  tempData$BasisOfRecord <- dplyr::if_else(tempData$BasisOfRecord == TRUE,
                                          tempData$database_id,
                                                  "NA")
  tempData$NoLicense <- dplyr::if_else(tempData$NoLicense == TRUE,
                                                tempData$database_id,
                                                        "NA")
  tempData$CoordsNotInCountry <- dplyr::if_else(tempData$CoordsNotInCountry == TRUE,
                                          tempData$database_id,
                                                    "NA")
  ##### largeVenn ####
  if(largeVenns == TRUE){
    tempData <- tempData %>% 
      dplyr::bind_cols( dplyr::select(tempData_technical, !database_id))

    large_tempData <- tempData %>%
      dplyr::select(Duplicates, SciName, NoCoordinates, AbsentRecord, CoordsNotInCountry, 
                    BasisOfRecord, CoordsOffMap)
      
    # Make plot
    ggSpatial <-  ggVennDiagram::ggVennDiagram(large_tempData, label_alpha = 0.5) +
      ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
    # SAVE plot
    ggplot2::ggsave(plot = ggSpatial,
                    filename = "Flags_allInitial.pdf", 
                    path = paste(path, "Output", "Figures",
                                 sep = "/"),
                    device = "pdf",
                    width = 15,
                    height = 15)
  } # END LargeVenn
    
  ##### Venn b ####
  if(largeVenns == FALSE){
    # Make plot
  ggSpatial <-  ggVennDiagram::ggVennDiagram(dplyr::select(tempData, !database_id),
                                             label_alpha = 0.5) +
    ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
    # SAVE plot
  ggplot2::ggsave(plot = ggSpatial,
                  filename = "Flags_ggSpatial.pdf", 
                  path = paste(path, "Output", "Figures",
                               sep = "/"),
                  device = "pdf",
                  width = 15,
                  height = 15)
  }# END Venn b
  
  } # END 1.1 intitial
  
  
  ##### 1.2 Taxonomy plots ####
  if( any(c("taxonomy", "all") %in% flagType)){
    
    tempData <- data %>%
      # Select the relevant columns
      dplyr::select(database_id, .uncer_terms, .invalidName) %>%
      dplyr::rename(uncertainTerm = .uncer_terms,
                    InvalidName = .invalidName)
    tempData$uncertainTerm <- dplyr::if_else(tempData$uncertainTerm == TRUE,
                                             tempData$database_id,
                                             "NA")
    tempData$InvalidName <- dplyr::if_else(tempData$InvalidName == TRUE,
                                            tempData$database_id,
                                            "NA")

    tempData <- tempData %>%
        dplyr::select(uncertainTerm, InvalidName)
      
      # Make plot
      ggSpatial <-  ggVennDiagram::ggVennDiagram(tempData, label_alpha = 0.5) +
        ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
      # SAVE plot
      ggplot2::ggsave(plot = ggSpatial,
                      filename = "Flags_taxonomy.pdf", 
                      path = paste(path, "Output", "Figures",
                                   sep = "/"),
                      device = "pdf",
                      width = 15,
                      height = 15)
  } # END 1.2 taxonomy
    
  
  
  ##### 1.3 Space plots ####
  if( any(c("space", "all") %in% flagType)){
    tempData <- data %>%
      # Select the relevant columns
      dplyr::select(database_id, .rou, .equ, .zer, .cap, .cen, .gbf, .inst) %>%
      dplyr::rename(RoundedCoords = .rou,
                    IdenticalCoords = .equ,
                    Zeros = .zer,
                    Capitals = .cap,
                    CountryCentroids = .cen,
                    GBIF = .gbf,
                    Institutions = .inst)
      # format columns
    tempData$RoundedCoords <- dplyr::if_else(tempData$RoundedCoords == TRUE,
                                          tempData$database_id,
                                          "NA")
    tempData$IdenticalCoords <- dplyr::if_else(tempData$IdenticalCoords == TRUE,
                                       tempData$database_id,
                                       "NA")
    tempData$Zeros <- dplyr::if_else(tempData$Zeros == TRUE,
                                             tempData$database_id,
                                             "NA")
    tempData$Capitals <- dplyr::if_else(tempData$Capitals == TRUE,
                                         tempData$database_id,
                                         "NA")
    tempData$CountryCentroids <- dplyr::if_else(tempData$CountryCentroids == TRUE,
                                          tempData$database_id,
                                          "NA")
    tempData$GBIF <- dplyr::if_else(tempData$GBIF == TRUE,
                                             tempData$database_id,
                                             "NA")
    tempData$Institutions <- dplyr::if_else(tempData$Institutions == TRUE,
                                         tempData$database_id,
                                         "NA") 
    
    ###### 1.3.1 largeVenns = T ####
    if(largeVenns == TRUE){
      large_tempData <- tempData %>%
        dplyr::select(RoundedCoords, IdenticalCoords, Zeros, Capitals, CountryCentroids, 
                      GBIF, Institutions)
      # Make plot
      ggSpatial <-  ggVennDiagram::ggVennDiagram(large_tempData, label_alpha = 0.5) +
        ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
      # SAVE plot
      ggplot2::ggsave(plot = ggSpatial,
                      filename = "Flags_allSpace.pdf", 
                      path = paste(path, "Output", "Figures",
                                   sep = "/"),
                      device = "pdf",
                      width = 15,
                      height = 15)
    }
    
    ##### 1.3.2 largeVenns = F ####
    if(largeVenns == FALSE){
      ###### a ######
      # Make plot
      ggSpace1 <-  ggVennDiagram::ggVennDiagram(tempData[,2:5], label_alpha = 0.5) +
        ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
      # SAVE plot
      ggplot2::ggsave(plot = ggSpace1,
                      filename = "Flags_space11.pdf", 
                      path = paste(path, "Output", "Figures",
                                   sep = "/"),
                      device = "pdf",
                      width = 9,
                      height = 6)
      ###### b ######
      # Make plot
      ggSpace2 <-  ggVennDiagram::ggVennDiagram(tempData[,6:ncol(tempData)], label_alpha = 0.5) +
        ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
      # SAVE plot
      ggplot2::ggsave(plot = ggSpace2,
                      filename = "Flags_space2.pdf", 
                      path = paste(path, "Output", "Figures",
                                   sep = "/"),
                      device = "pdf",
                      width = 9,
                      height = 6)
      }
    } # END Space
    

  ##### 1.2 Time plots ####
  if( any(c("time", "all") %in% flagType)){
    
    tempData <- data %>%
      # Select the relevant columns
      dplyr::select(database_id, .eventDate_empty, .year_outOfRange) %>%
      dplyr::rename(EmptyDate = .eventDate_empty,
                    YearOutOfRange = .year_outOfRange)
    tempData$EmptyDate <- dplyr::if_else(tempData$EmptyDate == TRUE,
                                             tempData$database_id,
                                             "NA")
    tempData$YearOutOfRange <- dplyr::if_else(tempData$YearOutOfRange == TRUE,
                                           tempData$database_id,
                                           "NA")
    
    tempData <- tempData %>%
      dplyr::select(EmptyDate, YearOutOfRange)
    
    # Make plot
    ggTime <-  ggVennDiagram::ggVennDiagram(tempData, label_alpha = 0.5) +
      ggplot2::scale_fill_gradient(low="#ffffd9",high = "#253494")
    # SAVE plot
    ggplot2::ggsave(plot = ggTime,
                    filename = "Flags_time.pdf", 
                    path = paste(path, "Output", "Figures",
                                 sep = "/"),
                    device = "pdf",
                    width = 9,
                    height = 6)
  } # END 1.4 Time

  
} # END flagChart
