
#### importOccurrences ####
# Looks for and loads in previously-created datafiles
importOccurrences <- function(path = path,
           meta_process = TRUE, # If TRUE (default), processes additional metadata files
           fileName = "BeeData_", #occurrence file name. If not provided, R will search to match "BeeData_"
           uuid_col = "uuid", # column in data frame with UUIDs or name of uuid column to create.
           canonical_col = "scientificName",
           longitude_col = "decimalLongitude",
           latitude_col = "decimalLatitude",
           date_col = "eventDate",
           flagInfo = "flagInfo.csv", #table with flags and parameter columns
           taxonomic_scope = "bees", #rank that defines taxonomic scope of occurrence dataset (i.e rank used to define occurrences)
           geographic_scope = "global",
           custodian_name, #name of person running cleaning workflow
           custodian_email, #email of person running cleaning workflow
           citations, #list of citations from which data were sourced
           download_date = "guess", #date data were downloaded in character format)
           spatRef = "epsg: 4326"){ #spatial reference system as epsg code
  # Load required packages
    require(readr)
    require(dplyr)
    require(magrittr)
    require(lubridate)
    require(stringr)
  # if the fileName is not provided...
  if(!exists("fileName")){
    fileName = "BeeData"
  }
  
  #### Find files ####
  # Find all of the previously-produced data files
  most_recent <- file_finder(path = path, fileName = fileName)
  
  # Return information to user
  writeLines(paste(" — Great, R has detected file(s), including... ", "\n",
                   paste(most_recent, collapse = "\n") ), sep = "")
  
  #### Detect format ####
    # Find the format of the most-recent files. This could potentially be .csv or .rds
    # TRUE IF .rds are present (that are not attribute files):
  (rdata_query <- any(most_recent[stringr::str_count(most_recent, pattern = "([aA]ttribute)|\\.rds") > 0] %>%
                        stringr::str_detect(., pattern = "([aA]ttribute)", negate = TRUE) == TRUE))
    # TRUE IF .csv data are present:
  (csv_query <- any(stringr::str_detect(most_recent, "([aA]ttributes)")) == TRUE &&
      any(stringr::str_detect(most_recent, ".*\\.csv{1}")) == TRUE)

  #### Both present ####
  # IF their is a complete .rds file among the most-recent files AND a .csv version...
  if(rdata_query == TRUE && csv_query == TRUE){
    writeLines(paste("\n", 
                     " — Oh boy, it looks like there are both .csv and .rds versions of your data!", 
                     "\n", "R will preferentially use the .rds file.", "\n",
                     "NOTE: the .rds file can be very slow to load"))
    # File to read:
    fileLoc <- most_recent[intersect(grep(".*\\.rds{1}", most_recent),
                                     grep("([aA]ttributes)", most_recent, invert = TRUE))]
    # Read in the .rds file
    writeLines(paste("Reading in ", fileLoc, "...", sep = ""))
   
    # Find the index of the string that matches and select that to read in
    occurDF <- fileLoc %>%
      readRDS()
  } #END IF both
  
  #### RData present ####
  # IF their is ONLY a complete .rds file among the most-recent files...
  if(rdata_query == TRUE && csv_query == FALSE){
    writeLines(paste(" — .rds export version found. Loading this file...", "\n",
                     "NOTE: the .rds file can be very slow to load"))
    # File to read:
    fileLoc <- most_recent[intersect(grep(".*\\.rds{1}", most_recent),
                                     grep("([aA]ttributes)", most_recent, invert = TRUE))]
    # Read in the .rds file
    writeLines(paste("Reading in ", 
                     fileLoc,
               "...", sep = ""))
    # Find the index of the string that matches and select that to read in
    occurDF <- as.character(fileLoc) %>%
      readRDS()
    writeLines("Completed reading in .rds file")
  } #END IF .rds

  #### CSV present ####
  # IF their is ONLY a complete .csv file among the most-recent files...
  if(csv_query == TRUE && rdata_query == FALSE){
    writeLines(paste(" — .csv exported version found. Loading this file..."))
    ColTypes <- ColTypeR()
    # Find the most-recent .csv occurrence file
    occurDF <- most_recent[intersect(grep(".*\\.csv", most_recent),
                                      grep("([aA]ttributes)", most_recent, invert = TRUE))] %>%
      read_csv(col_types = ColTypes)
    # Find the most-recent .rds attributes file
    attr_loc <- most_recent[stringr::str_which(most_recent, "(.*[aA]ttribute)(.*\\.rds)")] 
      # Check to see if the attributes file exists or not
    if(length(attr_loc) == 0){
      writeLines("No attribute file found... Please make sure that one exists to include the EML data")
    }else{
      writeLines(
        paste("Reading attribute file named ", attr_loc, "..." ))
      attr_file <- readRDS(attr_loc)
      }
    # Add the attributes file to the occurrence data file 
    attributes(occurDF) <- attr_file
    # Read in the EML file 
      # Find the folder that the attributes file is in.
    EML_home <- stringr::str_replace(attr_loc, pattern = "\\/[a-zA-Z0-9-_]+\\.rds$", "")
      # Find the .xml file in the same location as the attribute's folder
    EML_loc <- file_finder(path = EML_home, fileName = "eml.*\\.rds")
      # Read in the EML file
    EML_file <- readRDS(EML_loc)
  } #END IF .csv
  
  # Extract and save the data and the metadata based on their class
  for(i in 1:length(occurDF)){
      # If eml
    if(base::any(class(occurDF[[i]]) %in% c( "emld") )){
      eml_files <- occurDF[i]
    }
      # If tibble 
    if(base::any(class(occurDF[[i]]) %in% c("tbl_df", "data.frame", "tbl"))){
      Data_WebDL <- occurDF[i]
    }
  }

#### Matt's Metadata query ####
  # If you want to process in Matt's metadata...
  if(meta_process == TRUE){
    #Check/Add UUID column
    if(uuid_col %in% names(Data_WebDL)){
      if(min(nchar(Data_WebDL[[{{uuid_col}}]])) < 36) stop("The UUID column is not in a valid format. Specify a new name for a uuid column and UUIDs will be generated automatically.")
      if(dplyr::n_distinct(Data_WebDL[[{{uuid_col}}]]) != nrow(Data_WebDL)){
        stop("The UUID column is not a unique identifier. Specify a new name for a uuid column and generate new UUIDs.")
      }
    } else {
      require(uuid)
      print(" — No uuid column found. One is being created...")
      Data_WebDL[[{{uuid_col}}]] <- UUIDgenerate(n = nrow(Data_WebDL))
      print(paste("A", uuid_col, "column has been created with universal unique identifiers."))
    }
    
    #print a summary
    summ <- setNames(c(nrow(Data_WebDL), n_distinct(Data_WebDL[[canonical_col]])), c("n_obs", "n_spp"))
    
    spNobs <- Data_WebDL %>% 
      group_by(.data[[canonical_col]]) %>%
      count() %>%
      pull(n)
  
      print(paste("This occurrence dataset contains", 
                format(summ["n_obs"], big.mark=",",scientific=FALSE), 
                "observations from",
                format(summ["n_spp"], big.mark=",",scientific=FALSE),
                "taxa with between",
                format(min(spNobs), big.mark=",",scientific=FALSE),
                "and",
                format(max(spNobs), big.mark=",",scientific=FALSE),
                "observations per taxon."))
    rm(summ, spNobs)
    
    ###Step 3: add flag info table
    
    if(!is.null(flagInfo)){
        # Find the flagInfo file
      flagInfo <- file_finder(path = path, fileName = flagInfo)
      writeLines(paste("Found file — ", flagInfo))
      Data_WebDL <- addFlagInfo(Data_WebDL,
                             flagInfo)
    }
    
    ###Step 4: add metadata
    #add key column names
    keyCols = c("id" = uuid_col,
                "canonical" = canonical_col,
                "lon" = longitude_col,
                "lat" = latitude_col)
    attr(Data_WebDL, 'keyCols') = keyCols
    
    #add flag column attributes
    attr(Data_WebDL, 'flagCols') <- character()
    
    #Taxonomic scope
    if(!is.null(taxonomic_scope)){
      Data_WebDL <- addTaxoScope(Data_WebDL,
                              taxonomic_scope)
    }
    
    #Geographic scope
    if(!is.null(geographic_scope)){
      Data_WebDL <- addGeoScope(Data_WebDL,
                             geographic_scope)
    }
    
    #Custodian
    if(!is.null(custodian_name)){
      Data_WebDL <- addCustodian(Data_WebDL,
                              custodian_name)
    }
    
    #Email
    if(!is.null(custodian_email)){
      Data_WebDL <- addEmail(Data_WebDL,
                          custodian_email)
    }
    
    #Spatial reference system
    if(is.null(spatRef) | !is.character(spatRef)){
      stop("A coordinate reference system must be provided in an interpretable character format (e.g. 'epsg: 4326').")
    } else{
      Data_WebDL <- addCRS(Data_WebDL,
                        spatRef)
    }
    
    #Download date
    if(is.null(download_date)){
      warning("No download date provided. Using today's date")
      attr(Data_WebDL, 'download_date') <- Sys.Date()
    }
    
    if(download_date == "guess"){
      print(paste("Guessing the download date as today,", Sys.Date()))
      attr(Data_WebDL, 'download_date') <- Sys.Date()
    }
    
    if(is.character(download_date) &
       download_date != "guess"){
      attr(Data_WebDL, 'download_date') <- download_date
    }
    
    ### Step 4: Warn about missing attributes
    attr_obs <- list(flagInfo,
                     taxonomic_scope,
                     geographic_scope,
                     custodian_name,
                     custodian_email,
                     citations,
                     download_date)
    names(attr_obs) <- c("flagInfo",
                         "taxonomic_scope",
                         "geographic_scope",
                         "custodian_name",
                         "custodian_email",
                         "citations",
                         "download_date")
    missing_attr <- names(attr_obs)[which(sapply(attr_obs, is.null))]
    
    if(length(missing_attr) >= 1){
      warning(paste0("The following metadata attributes have not been provided: ",
                     stringr::str_c(missing_attr, collapse = ", "),
                     ". We strongly urge you to diligently record your metadata."))
    }
  } # END meta_process
  
#### Return data ####
  # Re-combine the data and EML data
  Data_WebDL <- dplyr::lst(Data_WebDL, 
                          eml_files)
  
  # Return the Data_WebDL
  return(Data_WebDL)
  # Return end product and print completion note
  writeLines(paste(" — Fin.", praise::praise(), sep = "\n"))
} # END data_importer


