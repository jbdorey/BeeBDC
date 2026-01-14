# Additional inputs were made by mjwestgate to allow continued support with galah V2


##### 1. atlasDownloader ####
#' Download occurrence data from the Atlas of Living Australia (ALA)
#'
#'
#' Downloads ALA data and creates a new file in the path to put those data. This function can also
#' request downloads from other atlases (see: http://galah.ala.org.au/articles/choosing_an_atlas.html).
#' However, it will only send the download to your email and you must do the rest yourself at this point.
#' 
#' @param path A character directory. The path to a folder where the download will be stored.
#' @param userEmail A character string. The email used associated with the user's ALA account; 
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
  # locally bind variables to the function
  . <- file_name <- NULL
  
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
  # Define ColsToKeep
  ColsToKeep <- BeeBDC::ColTypeR()[[1]] %>% names()
  # Create a new working directory for ALA data in the path provided
  dir.create(paste0(path, "/", atlas, "_galah_path", sep = ""), showWarnings = FALSE)
  atlas_galah_path <- paste0(path, "/", atlas, "_galah_path")
  # Set up the ALA download configuration
  writeLines(" - Setting galah configuration.")
  galah::galah_config(directory = atlas_galah_path, 
                      download_reason_id = DL_reason,
                      verbose=TRUE, 
                      email = userEmail, 
                      send_email = TRUE, 
                      atlas = atlas) 

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
  # Use of `Sys.Date()` comes with the risk that consecutive downloads on the same day will 
    # overwrite each other, even if they are for different queries
  # Note: `file_name` given above is chosen for consistency with previous version of BeeBDC
  file_name <- paste0("galah_download_", Sys.Date(), ".zip")
  ALA_Occurence_download <- galah::galah_call() %>%
    galah::galah_identify(ALA_taxon) %>%
    galah::galah_select(tidyselect::any_of(ColsToKeep)) %>% 
    galah::atlas_occurrences(mint_doi = FALSE, file = file_name)
  # get download attributes from file and make it into a dataframe
  attrs_ALA_Occurence_download <- attributes(ALA_Occurence_download) 
  
  writeLines(paste("3.","\n"," - atlas download is complete.", "\n",
                   "The script will now unzip all of the data and metadata to ",
                   atlas_galah_path, ". This may take a short while.",
                   sep = ""))
  
  # unzip the file
  utils::unzip(
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
    doi = attr(ALA_Occurence_download, "doi"),
    search_url = attr(ALA_Occurence_download, "search_url"),
    # data_type = attrs_ALA_Occurence_download$data_type, # not supported post galah v.2
    # data_request = paste(dplyr::lst(attrs_ALA_Occurence_download$data_request)), # not supported post galah v.2
    ALA_download_reason = DL_reason,
    download_date = Sys.Date()) %>%
    write_excel_csv(file = paste(atlas_galah_path,
                           "/galah_download_folder/", 
                           "galah_DL_info.csv", 
                           sep = ""))
  # Write user instructions
  writeLines(paste("4.","\n"," - Fin.",
                   sep = ""))
  
  return(ALA_Occurence_download)
}
##### Current end ALA ####
