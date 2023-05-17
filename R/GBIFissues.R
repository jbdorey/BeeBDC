# This function was written by James B Dorey to chose and flag some issues as listed by GBIF.
  # This function was written on the 30th of May 2022. For questions, please contact James at
  # jbdorey[at]me.com

# Possible flags:
  # allDates: RECORDED_DATE_INVALID, RECORDED_DATE_UNLIKELY
  # allMetadata: AMBIGUOUS_COLLECTION, COLLECTION_MATCH_NONE, COUNTRY_DERIVED_FROM_COORDINATES,
    # INSTITUTION_MATCH_NONE, DIFFERENT_OWNER_INSTITUTION, COUNTRY_INVALID, AMBIGUOUS_INSTITUTION,
    # COLLECTION_MATCH_FUZZY, INSTITUTION_COLLECTION_MISMATCH, INSTITUTION_MATCH_FUZZY
  # allObservations: OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT, BASIS_OF_RECORD_INVALID,
    # INDIVIDUAL_COUNT_INVALID
  # allSpatial: GEODETIC_DATUM_ASSUMED_WGS84, COORDINATE_ROUNDED, COORDINATE_PRECISION_INVALID
    # FOOTPRINT_WKT_INVALID, PRESUMED_NEGATED_LONGITUDE, CONTINENT_INVALID
    # PRESUMED_NEGATED_LATITUDE, COORDINATE_INVALID, COUNTRY_COORDINATE_MISMATCH
    # COORDINATE_UNCERTAINTY_METERS_INVALID, ZERO_COORDINATE, GEODETIC_DATUM_INVALID
  # allTaxo: TAXON_MATCH_HIGHERRANK, TYPE_STATUS_INVALID, TAXON_MATCH_FUZZY


#' Flags records with GBIF issues
#' 
#' This function will flag records which are subject to a user-specified vector of GBIF issues.
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param issueColumn Character. The column in which to look for GBIF issues. Default = "issue".
#' @param GBIFflags Character vector. The GBIF issues to flag. Users may choose their own vector of issues to flag or
#' use a pre-set vector or vectors, including c("allDates", "allMetadata", "allObservations", 
#' "allSpatial", "allTaxo", or "all").
#' 
#' Default = c("COORDINATE_INVALID", "PRESUMED_NEGATED_LONGITUDE", "PRESUMED_NEGATED_LATITUDE", "COUNTRY_COORDINATE_MISMATCH", "ZERO_COORDINATE")
#'
#' @return Returns the data with a new column, ".GBIFflags", where FALSE = records with any of the provided 
#' GBIFflags.
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' # Import the example data
#' data(beesRaw)
#' # Run the function
#' beesRaw_Out <- GBIFissues(data = beesRaw, 
#'    issueColumn = "issue", 
#'    GBIFflags = c("COORDINATE_INVALID", "ZERO_COORDINATE")) 
#' 
#' 
GBIFissues <- function (data = NULL, 
                            issueColumn = "issue", 
                            GBIFflags = NULL) 
{
  .data <- .GBIFflags <- NULL
  #### 0.0 Warnings ####
  if(is.null(data)){
    stop("\n - Please provide an argument for data. I'm a program, not a magician.")
  }
  if(is.null(GBIFflags)){
    warning("\n - GBIFflags not provided. Please provide an argument. I'm a program, not a magician.")
    writeLines(paste(
      " - Possible options are:\n",
      paste("TAXON_MATCH_HIGHERRANK", "TYPE_STATUS_INVALID", "TAXON_MATCH_FUZZY",
      "GEODETIC_DATUM_ASSUMED_WGS84", "COORDINATE_ROUNDED", "COORDINATE_PRECISION_INVALID",
      "FOOTPRINT_WKT_INVALID", "PRESUMED_NEGATED_LONGITUDE", "CONTINENT_INVALID",
      "PRESUMED_NEGATED_LATITUDE", "COORDINATE_INVALID", "COUNTRY_COORDINATE_MISMATCH",
      "COORDINATE_UNCERTAINTY_METERS_INVALID", "ZERO_COORDINATE", "GEODETIC_DATUM_INVALID",
      "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT", "BASIS_OF_RECORD_INVALID",
      "INDIVIDUAL_COUNT_INVALID", 
      "AMBIGUOUS_COLLECTION", "COLLECTION_MATCH_NONE", 
      "COUNTRY_DERIVED_FROM_COORDINATES",
      "INSTITUTION_MATCH_NONE", "DIFFERENT_OWNER_INSTITUTION", "COUNTRY_INVALID", 
      "AMBIGUOUS_INSTITUTION", "COLLECTION_MATCH_FUZZY", 
      "INSTITUTION_COLLECTION_MISMATCH", "INSTITUTION_MATCH_FUZZY",
      "RECORDED_DATE_INVALID", "RECORDED_DATE_UNLIKELY", collapse = "", sep = ", "), "\n",
    " - Or:\n",
    "allDates, allMetadata, allObservations, allTaxo, allSpatial, or all. ",
    "We recommend thinking about what is required.", sep = ""
    ))
    message(paste(
      "Using default of:\n",
      paste("COORDINATE_INVALID", "PRESUMED_NEGATED_LONGITUDE", "PRESUMED_NEGATED_LATITUDE", 
             "COUNTRY_COORDINATE_MISMATCH", "ZERO_COORDINATE", sep = ", "),
    sep = ""))
    GBIFflags <- c("COORDINATE_INVALID", "PRESUMED_NEGATED_LONGITUDE", "PRESUMED_NEGATED_LATITUDE", 
      "COUNTRY_COORDINATE_MISMATCH", "ZERO_COORDINATE")
  }
  
  #### 1.0 Flag options ####
    # Flag allDates
  if (any(GBIFflags == "allDates")) {
    GBIFflags <- c("RECORDED_DATE_INVALID", "RECORDED_DATE_UNLIKELY")} 
  # Flag allMetadata
  if (any(GBIFflags == "allMetadata")) {
    GBIFflags <- c("AMBIGUOUS_COLLECTION", "COLLECTION_MATCH_NONE", 
                       "COUNTRY_DERIVED_FROM_COORDINATES",
                       "INSTITUTION_MATCH_NONE", "DIFFERENT_OWNER_INSTITUTION", "COUNTRY_INVALID", 
                       "AMBIGUOUS_INSTITUTION", "COLLECTION_MATCH_FUZZY", 
                       "INSTITUTION_COLLECTION_MISMATCH", "INSTITUTION_MATCH_FUZZY")} 
  # Flag allObservations
  if (any(GBIFflags == "allObservations")) {
    GBIFflags <- c("OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT", "BASIS_OF_RECORD_INVALID",
                       "INDIVIDUAL_COUNT_INVALID")} 
  # Flag allSpatial
  if (any(GBIFflags == "allSpatial")) {
    GBIFflags <- c("GEODETIC_DATUM_ASSUMED_WGS84", "COORDINATE_ROUNDED", "COORDINATE_PRECISION_INVALID",
                       "FOOTPRINT_WKT_INVALID", "PRESUMED_NEGATED_LONGITUDE", "CONTINENT_INVALID",
                       "PRESUMED_NEGATED_LATITUDE", "COORDINATE_INVALID", "COUNTRY_COORDINATE_MISMATCH",
                       "COORDINATE_UNCERTAINTY_METERS_INVALID", "ZERO_COORDINATE", "GEODETIC_DATUM_INVALID")} 
  # Flag allTaxo
  if (any(GBIFflags == "allTaxo")) {
    GBIFflags <- c("TAXON_MATCH_HIGHERRANK", "TYPE_STATUS_INVALID", "TAXON_MATCH_FUZZY")} 
  # Flag all
  if (any(GBIFflags == "all")) {
    GBIFflags <- c("TAXON_MATCH_HIGHERRANK", "TYPE_STATUS_INVALID", "TAXON_MATCH_FUZZY",
                   "GEODETIC_DATUM_ASSUMED_WGS84", "COORDINATE_ROUNDED", "COORDINATE_PRECISION_INVALID",
                   "FOOTPRINT_WKT_INVALID", "PRESUMED_NEGATED_LONGITUDE", "CONTINENT_INVALID",
                   "PRESUMED_NEGATED_LATITUDE", "COORDINATE_INVALID", "COUNTRY_COORDINATE_MISMATCH",
                   "COORDINATE_UNCERTAINTY_METERS_INVALID", "ZERO_COORDINATE", "GEODETIC_DATUM_INVALID",
                   "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT", "BASIS_OF_RECORD_INVALID",
                   "INDIVIDUAL_COUNT_INVALID", 
                   "AMBIGUOUS_COLLECTION", "COLLECTION_MATCH_NONE", 
                   "COUNTRY_DERIVED_FROM_COORDINATES",
                   "INSTITUTION_MATCH_NONE", "DIFFERENT_OWNER_INSTITUTION", "COUNTRY_INVALID", 
                   "AMBIGUOUS_INSTITUTION", "COLLECTION_MATCH_FUZZY", 
                   "INSTITUTION_COLLECTION_MISMATCH", "INSTITUTION_MATCH_FUZZY",
                   "RECORDED_DATE_INVALID", "RECORDED_DATE_UNLIKELY")} 
  
  
    # Add flag column
  data <- data %>% dplyr::mutate(.GBIFflags = !tolower(.data[[issueColumn]]) %in% 
                                   tolower(GBIFflags))
    # User output
  message(paste(" - jbd_GBIFissues:\nFlagged", 
                format(sum(data$.GBIFflags == FALSE, na.rm = TRUE), big.mark = ","),
                       "\n ",
                "The .GBIFflags column was added to the database.", "\n",
                sep = " "))
    # Return the dataset
  return(data)
}