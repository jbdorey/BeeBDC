#' Internal function. Create directories for saving the outputs of the bdc package
#'
#' Creates directories for saving the outputs of bdc package, namely
#' checks, figures, reports, and databases.
#'
#' @param path Character. A path as a character vector for where to create the directories. If 
#' no path is provided (the default), the directories will be created using [here::here()].
#'
#'
#' @importFrom here here
#'
#' @return None
#'
#' @details:
#' Function used to created four folder for saving results of some functions.
#' @noRd

#' @examples
#' \donttest{
#' jbd_create_dir(path = tempdir())
#' }
jbd_create_dir <- function(path = NULL) {
    # Option 1 if no path is provided, create directories using here::here
  if(is.null(path)){
    dir.create(here::here("Output/Check"), recursive = TRUE)
    dir.create(here::here("Output/Intermediate"), recursive = TRUE)
    dir.create(here::here("Output/Report"), recursive = TRUE)
    dir.create(here::here("Output/Figures"), recursive = TRUE)
  } # END option 1
  
  # Option 2 if a path is provided, create directories at that path
  if(!is.null(path) & !file.exists(path)){
    dir.create(paste0(path, "/Output/Check"), recursive = TRUE)
    dir.create(paste0(path, "/Output/Intermediate"), recursive = TRUE)
    dir.create(paste0(path, "/Output/Report"), recursive = TRUE)
    dir.create(paste0(path, "/Output/Figures"), recursive = TRUE)
  } # END option 2
  
} # END function
