
##### 1. ALA_downloader ####
ALA_downloader <- function(path, userEmail = NULL, ALA_taxon, DL_reason = 4){
  #### Intro checks ####
  writeLines(paste("1.","\n",
                   " — Note: galah has a 50 million record download limit.", "\n",
                   "You may call atlas_counts() to check.", "\n",
                   " — Additionally, you must register your email with ALA otherwise you will get an ",
                   "error message.", "\n",
                   "See here — https://www.ala.org.au","\n",
                   " — Valid donwload reasons include can be found by running show_all_reasons()",
                   sep = ""))
  # Check for a userEmail input present and halt if FALSE
  if(exists("userEmail") == FALSE){
    stop("You must provide a userEmail for the ALA download.")
  }
  # Check for a userEmail format and halt if FALSE
  if(grepl( ".[^@]+@{1}.+\\..+", userEmail) == FALSE){
    stop("The email you entered might be incorrect, please double-check the format.")
  }
  require(galah)
  require(rvest)
  require(httr)
  # Define ColsToKeep
  ColsToKeep <- ColTypeR()[[1]] %>% names()
  # Create a new working directory for ALA data in the path provided
  dir.create(paste(path,"ALA_galah_path", sep = "/"), showWarnings = FALSE)
  ALA_galah_path <- paste(path,"ALA_galah_path", sep = "/")
  # Set up the ALA download configuration
  writeLines(" — Setting galah configuration.")
  galah::galah_config(cache_directory= ALA_galah_path, download_reason_id = DL_reason, verbose=TRUE, 
                      email = userEmail, send_email = TRUE, atlas = "ALA") 

  #### ALA download ####
  # Choose ALA columns to download
  # Thankfully, ALA has a fantastic r package, galah, that allows easy download of occurrence data.
  # Thank you, ALA <3
  # DOWNLOAD ALA data here
  # Apiformes is an informal name that is helpful to select the bee families out of the superfamily Apoidea.
  writeLines(paste("2.","\n",
                   " — Beginning ALA download via galah.", "\n",
                   "A progress bar of your download should appear shortly. You will also receive an email ",
                   "when your download is complete.", sep = ""))
  ALA_Occurence_download <- galah::galah_call() %>%
    galah::galah_identify(ALA_taxon) %>%
    galah::galah_select(tidyselect::any_of(ColsToKeep)) %>% 
    galah::atlas_occurrences(mint_doi = TRUE)
  # get download attributes from file and make it into a dataframe
  attrs_ALA_Occurence_download <- attributes(ALA_Occurence_download) 
  
  writeLines(paste("3.","\n"," — ALA download is complete.", "\n",
                   "The script will now download and unzip all of the data and metadata to ",
                   ALA_galah_path, ". This may take a short while.",
                   sep = ""))
  # Find the download link using the doi provided
  doi_link <- rvest::read_html(url(attrs_ALA_Occurence_download$doi, "rb")) %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    grep("/download$", ., value = TRUE)
  # Download the file
  httr::GET(url = paste("https://doi.ala.org.au", doi_link[1], sep = ""),
            httr::write_disk(
              file.path(paste(ALA_galah_path,
                              "/galah_download_", Sys.Date(), ".zip", 
                              sep = "")), 
              overwrite = TRUE))  
  # unzip the file
  unzip(
    # File to unzip
    zipfile = paste(ALA_galah_path,
                    "/galah_download_", Sys.Date(), ".zip", 
                    sep = ""), 
    # Where to put the extracted file
    exdir = paste(ALA_galah_path,
                  "/galah_download_folder", 
                  sep = ""),
    overwrite = TRUE) 
  
  #### Save data ####
  # Save some download information
  tibble(
    downloaders_email = userEmail,
    taxon = ALA_taxon,
    doi = attrs_ALA_Occurence_download$doi,
    search_url = attrs_ALA_Occurence_download$search_url,
    download_link = doi_link,
    data_type = attrs_ALA_Occurence_download$data_type,
    data_request = paste(dplyr::lst(attrs_ALA_Occurence_download$data_request)),
    ALA_download_reason = DL_reason,
    download_date = Sys.Date()) %>%
    write_csv(file = paste(ALA_galah_path,
                           "/galah_download_folder/", 
                           "galah_DL_info.csv", 
                           sep = ""))
  # Write user instructions
  writeLines(paste("4.","\n"," — Fin.",
                   sep = ""))
}
##### Current end ALA ####
