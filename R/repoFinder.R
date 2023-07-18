##### 2.1 repoFinder ####
#' Find GBIF, ALA, iDigBio, and SCAN files in a directory
#'
#' @param path A directory as character. The path within which to recursively look for GBIF, ALA, 
#' iDigBio, and SCAN files.
#'
#' @return Returns a list of directories to each of the above data downloads
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' # Where DataPath is made by [BeeBDC::dirMaker()]
#' BeeBDC::repoFinder(path = DataPath)
#' }
repoFinder <- function(path){
  . <- NULL
  requireNamespace("dplyr")
  # Find ALL occurrence file downloads contained within the HomePath and return their location and 
  # their information
  AllOccLocs <- file.info(list.files(path, full.names = T, pattern = "occurrence(s)?(_raw)?\\.|^data.csv",
                                     recursive = TRUE))
  # Split out each data source's occurrence files and file paths based on their slight differences
  occ_paths <- list(
    # ALA paths
    rownames(AllOccLocs) %>% grep("/data.csv", ., value = TRUE),
    # GBIF paths
    rownames(AllOccLocs) %>% grep("/occurrence.txt", ., value = TRUE),
    # iDigBio paths
    rownames(AllOccLocs) %>% grep("/occurrence_raw.csv", ., value = TRUE),
    # SCAN paths
    rownames(AllOccLocs) %>% grep("/occurrences.csv", ., value = TRUE))
  # Return those paths
  names(occ_paths) <- c("ALA_data", "GBIF_data", "iDigBio_data", "SCAN_data")
  return(occ_paths)
}

