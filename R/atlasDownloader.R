
##### 1. atlasDownloader ####
#' Download occurrence data from the Atlas of Living Australia (ALA)
#'
#'
#' Downloads ALA data and creates a new file in the path to put those data. This function can also
#' request downloads from other atlases (see: http://galah.ala.org.au/articles/choosing_an_atlas.html).
#' However, it will only send the download to your email and you must do the rest yourself at this point.
#' 
#' @param path A character directory. The path to a folder where the download will be stored.
#' @param userEmail A character string. The email used associated with the user’s ALA account; 
#' user must make an ALA account to download data. 
#' @param ALA_taxon A character string. The taxon to download from ALA. Uses [galah::galah_identify()]
#' @param DL_reason Numeric. The reason for data download according to [galah::galah_config()]
#' @param atlas Character. The atlas to download occurrence data from - see here https://galah.ala.org.au/R/articles/choosing_an_atlas.html for details.
#' Note: the default is "ALA" and is probably the only atlas which will work seamlessly with the rest
#' of the workflow. However, different atlases can still be downloaded and a doi will be sent to 
#' your email.
#'
#' @return Completes an ALA data download and saves those data to the path provided.
#' 
#' @importFrom dplyr %>%
#' @importFrom utils unzip
#' @export
#'
#' @examples
#' \dontrun{
#' atlasDownloader(path = DataPath,
#'                userEmail = "InsertYourEmail",
#'                ALA_taxon = "Apiformes",
#'                DL_reason = 4)
#'                }
atlasDownloader <- function(path, userEmail = NULL, ALA_taxon, DL_reason = 4, atlas = "ALA"){
  # locally bind variabls to the function
  . <- NULL
  
  #### Intro checks ####
  writeLines(paste("1.","\n",
                   " - Note: galah has a 50 million record download limit.", "\n",
                   "You may call atlas_counts() to check.", "\n",
                   " - Additionally, you must register your email with your ", atlas, " otherwise you will get an ",
                   "error message.", "\n",
                   "See here - https://www.ala.org.au - or your relevant atlas","\n",
                   " - Valid donwload reasons include can be found by running show_all_reasons()",
                   sep = ""))
  # Check for a userEmail input present and halt if FALSE
  if(exists("userEmail") == FALSE){
    stop("You must provide a userEmail for the ",atlas," download.")
  }
  # Check for a userEmail format and halt if FALSE
  if(grepl( ".[^@]+@{1}.+\\..+", userEmail) == FALSE){
    stop("The email you entered might be incorrect, please double-check the format.")
  }
  requireNamespace("galah")
  requireNamespace("rvest")
  requireNamespace("httr")
  # Define ColsToKeep
  ColsToKeep <- BeeBDC::ColTypeR()[[1]] %>% names()
  # Create a new working directory for ALA data in the path provided
  dir.create(paste0(path, "/", atlas, "_galah_path", sep = ""), showWarnings = FALSE)
  atlas_galah_path <- paste(path, "/", atlas, "_galah_path", sep = "")
  # Set up the ALA download configuration
  writeLines(" - Setting galah configuration.")
  galah::galah_config(cache_directory= atlas_galah_path, download_reason_id = DL_reason, verbose=TRUE, 
                      email = userEmail, send_email = TRUE, atlas = atlas) 

  #### ALA download ####
  # Choose ALA columns to download
  # Thankfully, ALA has a fantastic r package, galah, that allows easy download of occurrence data.
  # Thank you, ALA <3
  # DOWNLOAD ALA data here
  # Apiformes is an informal name that is helpful to select the bee families out of the superfamily Apoidea.
  writeLines(paste("2.","\n",
                   " - Beginning atlas download via galah.", "\n",
                   "A progress bar of your download should appear shortly. You will also receive an email ",
                   "when your download is complete.", sep = ""))
  ALA_Occurence_download <- galah::galah_call() %>%
    galah::galah_identify(ALA_taxon) %>%
    galah::galah_select(tidyselect::any_of(ColsToKeep)) %>% 
    galah::atlas_occurrences(mint_doi = TRUE)
  # get download attributes from file and make it into a dataframe
  attrs_ALA_Occurence_download <- attributes(ALA_Occurence_download) 
  
  writeLines(paste("3.","\n"," - atlas download is complete.", "\n",
                   "The script will now download and unzip all of the data and metadata to ",
                   atlas_galah_path, ". This may take a short while.",
                   sep = ""))
  # Find the download link using the doi provided
  doi_link <- rvest::read_html(url(attrs_ALA_Occurence_download$doi, "rb")) %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    grep("/download$", ., value = TRUE)
  
  prefixHTML <- if(atlas == "ALA"){"https://doi.ala.org.au"}
  # Download the file
  httr::GET(url = paste(prefixHTML, doi_link[1], sep = ""),
            httr::write_disk(
              file.path(paste(atlas_galah_path,
                              "/galah_download_", Sys.Date(), ".zip", 
                              sep = "")), 
              overwrite = TRUE))  
  # unzip the file
  unzip(
    # File to unzip
    zipfile = paste(atlas_galah_path,
                    "/galah_download_", Sys.Date(), ".zip", 
                    sep = ""), 
    # Where to put the extracted file
    exdir = paste(atlas_galah_path,
                  "/galah_download_folder", 
                  sep = ""),
    overwrite = TRUE) 
  
  #### Save data ####
  # Save some download information
  dplyr::tibble(
    downloaders_email = userEmail,
    taxon = ALA_taxon,
    doi = attrs_ALA_Occurence_download$doi,
    search_url = attrs_ALA_Occurence_download$search_url,
    download_link = doi_link,
    data_type = attrs_ALA_Occurence_download$data_type,
    data_request = paste(dplyr::lst(attrs_ALA_Occurence_download$data_request)),
    ALA_download_reason = DL_reason,
    download_date = Sys.Date()) %>%
    write_excel_csv(file = paste(atlas_galah_path,
                           "/galah_download_folder/", 
                           "galah_DL_info.csv", 
                           sep = ""))
  # Write user instructions
  writeLines(paste("4.","\n"," - Fin.",
                   sep = ""))
}
##### Current end ALA ####
