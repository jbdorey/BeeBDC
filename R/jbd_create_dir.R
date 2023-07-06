#' Internal function. Create directories for saving the outputs of the bdc package
#'
#' Creates directories for saving the outputs of bdc package, namely
#' checks, figures, reports, and databases.
#'
#' @param path Character. A path as a character vector for where to create the directories. If 
#' no path is provided (the default), the directories will be created using [here::here()].
#'
#'
#' @importFrom fs dir_create
#' @importFrom here here
#'
#' @return None
#'
#' @details:
#' Function used to created four folder for saving results of some functions.
#' @noRd

#' @examples
#' \dontrun{
#' jbd_create_dir()
#' }
jbd_create_dir <- function(path = NULL) {
    # Option 1 if no path is provided, create directories using here::here
  if(is.null(path)){
  fs::dir_create(here::here("Output/Check"))
  fs::dir_create(here::here("Output/Intermediate"))
  fs::dir_create(here::here("Output/Report"))
  fs::dir_create(here::here("Output/Figures"))
  } # END option 1
  
  # Option 2 if a path is provided, create directories at that path
  if(!is.null(path)){
    fs::dir_create(paste0(path, "/Output/Check"))
    fs::dir_create(paste0(path, "/Output/Intermediate"))
    fs::dir_create(paste0(path, "/Output/Report"))
    fs::dir_create(paste0(path, "/Output/Figures"))
  } # END option 2
  
} # END function
