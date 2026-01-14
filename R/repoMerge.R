####  2. repoMerge ####
#' Import occurrences from GBIF, ALA, iDigBio, and SCAN downloads
#' 
#' Locates data from GBIF, ALA, iDigBio, and SCAN within a directory and reads it in along with its eml
#' metadata. Please keep the original download folder names and architecture unchanged.
#' NOTE: This function uses family-level data to identify taxon downloads. If this, or something new,
#' becomes an issue, please contact James Dorey (the developer) as there are likely to be exceptions
#' to how files are downloaded. current as of versions 1.0.4.
#'
#' @param path A directory as a character. The directory to recursively look in for the above data.
#' @param save_type Character. The data type to save the resulting file as. Options are: 
#' csv_files" or "R_file".
#' @param occ_paths A list of directories. Preferably produced using [BeeBDC::repoFinder()] the 
#' function asks for a list of paths to the relevant input datasets. You can fault-find errors 
#' in this function by checking the output of [BeeBDC::repoFinder()].
#'
#' @return A list with a data frame of merged occurrence records, "Data_WebDL", and a list of eml 
#' files contained in "eml_files". Also saves these files in the requested format.
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom readr col_character cols
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' DataImp <- repoMerge(path = DataPath, 
#' # Find data - Many problems can be solved by running [BeeBDC::repoFinder(path = DataPath)]
#' # And looking for problems
#' occ_paths = BeeBDC::repoFinder(path = DataPath),
#' save_type = "R_file")
#' }
repoMerge <- function(path, save_type, occ_paths){
  . <- family <- data_i <- NULL
  
    #### 0.0 Prep ####
  requireNamespace("dplyr")
    # Remove empty elements
  occ_paths <- occ_paths[lapply(occ_paths,length)>0]
  
  #### 1.0 Data read+merge ####
  ###### 1.1 Loop prep. ####
  {startTime <- Sys.time()
  # print user information
  writeLines( paste(" - Reading and joining ",length(unlist(occ_paths))," occurrence files.", "\n",
                    "Depending on file size and number, this could take some time.","\n",
                    sep = ""))
  # Make an internal copy of the template for use in the loop as the template tibble
  data_template <- matrix(ncol = length(BeeBDC::ColTypeR()[[1]] %>% names()), nrow = 0) %>% as.data.frame() %>% 
    stats::setNames(BeeBDC::ColTypeR()[[1]] %>% names()) %>% dplyr::tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  # Copy the template
  Data_WebDL <- data_template
  # Make an empty eml file for the loop
  eml_files <- dplyr::tibble()
  # Set up a counter to keep track of the number of files processed
  counter = 1
  
  ##### 1.2 Data loop ####
  for(j in 1:length(occ_paths)){ # Start path loop
    for(i in 1:length(unlist(occ_paths[j]))){
      # Firstly, select the download path for the loop
      path_i <- occ_paths[j] %>% unlist() %>% .[i]
      
      ###### a. loop data ####
      # Use the custom function dataReader to read in the occurrence data in the correct format
      data_i <- dataReader(path_i = path_i, home_path = path) #%>%
        #readr::type_convert(col_types = BeeBDC::ColTypeR())
        # If there is a date range of days, take only the first day
      data_i$day <- data_i$day %>% 
        stringr::str_replace(pattern = "-.*", replacement = "") %>% 
        stringr::str_squish() #%>%
        #as.integer()
      
      
      ###### b. attr. mangement ####
      # Use the custom function attr_builder to extract attribute information from file
      data_Attributes <- data_i %>%
          # Group by family
        dplyr::group_by(family) %>%
          # Split the groups into their own lists
        dplyr::group_split(.keep = TRUE) %>%
        lapply(., attr_builder,
               path_i = path_i) 
      
        # Extract all of the Source_tibble tibbles and bind them together
      data_Source_tibbles <- data_Attributes %>%
        lapply(., function(x) dplyr::tibble(x$Source_tibble)) %>%
        # Combine the lists of tibbles
        dplyr::bind_rows()
        # Recombine the source eml dat aand the source tibble data
      data_Attributes <- list(data_Attributes[[1]][[1]], data_Source_tibbles) %>%
        stats::setNames(c("source_eml","Source_tibble"))
      
      
      
        # Add an additional test for iDigBio to extract families
      if(any(data_Attributes$Source_tibble$dataSource %>% stringr::str_detect("iDigBio"))){
        data_Attributes$Source_tibble <- data_Attributes$Source_tibble %>% 
          dplyr::mutate(
            dataSource = stringr::str_c("iDigBio_", unique(data_i$family) %>% 
                                          stringr::str_replace_na("NA")),
            taxon = stringr::str_c(unique(data_i$family) %>% stringr::str_replace_na("NA"))
          )} # END if iDigBio

      
      # Test if the attributes are sourced from a single family, or from more. Should == TRUE if it 
      # is from a single family and == FALSE if from a multiple family download.
      singleFamilyTest <- nrow(data_Attributes$Source_tibble) == 1
      
        ###### b1. single family ####
      if(singleFamilyTest == TRUE){
        # Annotate the dataSource column with information from the attributes (based on one input 
          # family)
        data_i$dataSource <- data_Attributes$Source_tibble$dataSource
      } # END singleFamilyTest == TRUE
      ###### b2. many families ####
      if(singleFamilyTest == FALSE){
        # If the family column is NA, allow linking of the dataSource by temporarily modifying that
          # column to include the source_NA
        data_i <- data_i %>%
          dplyr::mutate(family = dplyr::if_else(is.na(family),
                                                "NA",
                                                family))
        # Annotate the dataSource column with information from the attributes (based on many input 
          # families)
        data_i <- data_i %>%
            # Remove the empty dataSource column
          dplyr::select(!tidyselect::any_of("dataSource")) %>%
            # Match the family-level dataSources to the dataSource column using left_join
          dplyr::left_join(data_Attributes$Source_tibble %>%
                             dplyr::select(c("dataSource", "taxon")),
                           by = c("family" = "taxon"))
      } # END singleFamilyTest == FALSE
      
      
      ## Source table ##  
      # Bind with either the template or running GBIF_Data_WebDL tibble
      Data_WebDL <- dplyr::bind_rows(Data_WebDL, data_i) # END rbind.fill
      # Extract existing attributes
      ExistingAttrs <- attributes(Data_WebDL)
      # Combine with new attributes
      # combine tibbles
      SrcTbl_new <- dplyr::bind_rows(ExistingAttrs$dataSource, data_Attributes$Source_tibble)
      # Add the new attribute information into the file
      attr(Data_WebDL, which = "dataSource") <- SrcTbl_new
      ## eml files ##
      # Make the eml file into a list
      new_eml_file <- list(data_Attributes$source_eml)
      # Name that list with the appropriate source and name
      names(new_eml_file) <- if(singleFamilyTest == TRUE){
                  # If singleFamilyTest == TRUE, then use that single family dataSource name as the
                  # eml name
        data_Attributes$Source_tibble$dataSource
        }else{
                  # If singleFamilyTest == FALSE, then concatenate those names into a text string
                  # separated by "; " for each family present
        stringr::str_c(data_Attributes$Source_tibble$dataSource, collapse = "; ")}
        
      # Append this to the running eml list file
      eml_files <- append(eml_files, new_eml_file) %>%
        emld::as_emld( from = "list")
      
      
      ###### c. Progress print ####
      # Print the progress and number of records (rows) to user
      writeLines( paste(" - Completed", i,"of", length(unlist(occ_paths[j])), names(occ_paths[j]),
                        "files. ","\n",
                        counter, "of", length(unlist(occ_paths)), "total files processed","\n",
                        "Cumulative number of rows =", 
                        format(nrow(Data_WebDL), big.mark=",",scientific=FALSE), sep = " ") )
      # progress the counter by 1
      counter = counter + 1
    } # End GBIF loop
  } # End path loop
  
  ##### 3. Save data ####
  # user-input includes the path and save_type, which are supplied from repoMerge()
  dataSaver(path = path, # Path to HomeDirectory
             save_type = save_type, # save_type is either "csv_files" or "R_file"
             occurrences = Data_WebDL, # Input data file
             eml_files = eml_files, # Input eml_files
             file_prefix = "BeeData_") 

  # Completion message to user with endTime
  endTime <- Sys.time()
  writeLines( paste(" - Completed in ", round(endTime - startTime, digits = 2), " ",
                    units(round(endTime - startTime, digits = 2)), sep = ""))
  } # END Data Loop
  return( dplyr::lst(Data_WebDL, eml_files))
} # END repoMerge function

