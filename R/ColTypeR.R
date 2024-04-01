  # This function was written by James Dorey on the 20th of May 2022 in ored to define the columns
    # used during occurrence record cleaning
  # For help you may email James at jbdorey[at]me.com

#' Sets up column names and types
#' 
#' This function uses [readr::cols_only()] to assign a column name and the type of data 
#' (e.g., [readr::col_character()], 
#' and [readr::col_integer()]). To see the default columns simply run [BeeBDC::ColTypeR()]. 
#' This is intended for use with [readr::read_csv()]. Columns that are not present will NOT be included 
#' in the resulting tibble unless they are specified using [...].
#'
#' @param ... Additional arguments. These can be specified in addition to the ones default to the 
#' function.  For example:
#' * newCharacterColumn = [readr::col_character()],
#' * newNumericColumn = [readr::col_integer()],
#' * newLogicalColumn = [readr::col_logical()]
#' 
#' @importFrom readr col_character col_double col_factor col_integer col_logical col_datetime
#' @importFrom dplyr %>%
#'
#' @return Returns an object of class col_spec. 
#' See [readr::as.col_spec()] for additional context and explication.
#' @export
#'
#' @examples 
#'   # You can simply return the below for default values
#'   library(dplyr)
#' BeeBDC::ColTypeR() 
#' 
#'   # To add new columns you can write
#' ColTypeR(newCharacterColumn = readr::col_character(), 
#'          newNumericColumn = readr::col_integer(), 
#'          newLogicalColumn = readr::col_logical()) 
#' 
#' # Try reading in one of the test datasets as an example:
#' beesFlagged %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR())
#'   # OR
#' beesRaw %>% dplyr::as_tibble(col_types = BeeBDC::ColTypeR())
#' 
#' 
ColTypeR <- function(...){
  ColTypes <- readr::cols_only(
    # Character Strings
    # CHR - taxonomy
    database_id  = readr::col_character(), scientificName = readr::col_character(), 
    family = readr::col_character(), subfamily = readr::col_character(), genus = readr::col_character(), 
    subgenus = readr::col_character(), subspecies = readr::col_character(), species = readr::col_character(), 
    specificEpithet = readr::col_character(), infraspecificEpithet = readr::col_character(), 
    acceptedNameUsage = readr::col_character(), taxonRank = readr::col_character(),
    scientificNameAuthorship = readr::col_character(), 
    identificationQualifier = readr::col_character(), higherClassification = readr::col_character(), 
    identificationReferences = readr::col_character(), typeStatus = readr::col_character(), 
    previousIdentifications = readr::col_character(), verbatimIdentification = readr::col_character(), 
    identifiedBy = readr::col_character(), dateIdentified = readr::col_character(),
    # DBL - Locality info
    decimalLatitude = readr::col_double(), decimalLongitude = readr::col_double(),
    verbatimLatitude = readr::col_character(), verbatimLongitude = readr::col_character(),
    verbatimElevation = readr::col_character(),
    # CHR/Factor - Locality info
    stateProvince = readr::col_character(), country = readr::col_character(), continent = readr::col_factor(), 
    locality = readr::col_character(), island = readr::col_character(),
    county = readr::col_character(), municipality = readr::col_character(),
    # CHR/Factor - Country codes
    countryCode = readr::col_factor(), level0Gid = readr::col_factor(), level0Name = readr::col_factor(), 
    level1Gid = readr::col_factor(), level1Name = readr::col_factor(), license = readr::col_factor(), 
    issue = readr::col_character(), 
    # Date/Time - Collection time
    eventDate = readr::col_character(), 
    eventTime = readr::col_character(), 
    startDayOfYear = readr::col_integer(),
    endDayOfYear = readr::col_integer(),
    # Int - Collection time
    day = readr::col_integer(), month = readr::col_integer(), year = readr::col_integer(),
    # Factor - Collection info
    basisOfRecord = readr::col_factor(), type = readr::col_factor(), occurrenceStatus = readr::col_factor(), 
    # CHR - Collection info
    recordNumber = readr::col_character(), recordedBy = readr::col_character(), eventID = readr::col_character(), 
    Location = readr::col_character(), samplingProtocol = readr::col_character(), 
    samplingEffort = readr::col_character(),
    # Int - Collection info
    individualCount = readr::col_double(), organismQuantity = readr::col_double(), 
    # mixed - Information uncertainty
    coordinatePrecision = readr::col_double(), coordinateUncertaintyInMeters = readr::col_double(), 
    spatiallyValid = readr::col_logical(),
    # CHR - Database information
    catalogNumber = readr::col_character(), gbifID = readr::col_character(), datasetID = readr::col_character(),
    institutionCode = readr::col_character(), datasetName = readr::col_character(), 
    otherCatalogNumbers = readr::col_character(), occurrenceID = readr::col_character(), 
    taxonKey = readr::col_character(), coreid = readr::col_character(), 
    recordId = readr::col_character(), collectionID = readr::col_character(),
    associatedSequences = readr::col_character(),
    # CHR - Verbatim information
    verbatimScientificName = readr::col_character(), verbatimEventDate = readr::col_character(),
    # CHR/Factor - Aux info
    associatedTaxa = readr::col_character(), associatedOrganisms = readr::col_character(), 
    fieldNotes = readr::col_character(), sex = readr::col_character(),
    # CHR - Rights info
    rights = readr::col_character(), rightsHolder = readr::col_character(), accessRights = readr::col_character(), 
    dctermsLicense = readr::col_character(), dctermsType = readr::col_character(), 
    dctermsAccessRights = readr::col_character(), associatedReferences = readr::col_character(), 
    bibliographicCitation = readr::col_character(), dctermsBibliographicCitation = readr::col_character(), 
    references = readr::col_character(),
    # Record notes
    # CHR
    flags = readr::col_character(), informationWithheld = readr::col_character(), 
    isDuplicateOf = readr::col_character(),
    # Logical
    hasCoordinate = readr::col_logical(), hasGeospatialIssues = readr::col_logical(), 
    # Factor
    assertions = readr::col_factor(),
    # mix - ALA columns
    occurrenceYear = readr::col_datetime(), id = readr::col_character(), duplicateStatus = readr::col_factor(), 
    associatedOccurrences = readr::col_character(), 
    # CHR - SCAN column
    locationRemarks = readr::col_character(),
    # CHR - dataset origin column
    dataSource = readr::col_character(),
      # bdc columns
     dataBase_scientificName = readr::col_character(), .rou = readr::col_logical(),                         
    .val = readr::col_logical(), .equ = readr::col_logical(), .zer = readr::col_logical(), .cap = readr::col_logical(),                         
    .cen = readr::col_logical(), .sea = readr::col_logical(), .otl = readr::col_logical(), .gbf = readr::col_logical(),                         
    .inst = readr::col_logical(), .dpl = readr::col_logical(), .summary = readr::col_logical(),
    names_clean = readr::col_character(), verbatim_scientificName = readr::col_character(), 
    .uncer_terms = readr::col_logical(), .eventDate_empty = readr::col_logical(), 
    .year_outOfRange = readr::col_logical(),
    .duplicates = readr::col_logical(), .lonFlag = readr::col_logical(), .latFlag = readr::col_logical(), 
    .gridSummary = readr::col_logical(), .basisOfRecords_notStandard = readr::col_logical(),
    .scientificName_empty = readr::col_logical(), .coordinates_empty = readr::col_logical(),
    .coordinates_outOfRange = readr::col_logical(), coordinates_transposed = readr::col_logical(),
    country_suggested = readr::col_character(),  .countryOutlier = readr::col_logical(),
    countryMatch = readr::col_character(), .expertOutlier = readr::col_logical(),
      # jbd flags
    .occurrenceAbsent = readr::col_logical(), .coordinates_country_inconsistent = readr::col_logical(), 
    .unLicensed = readr::col_logical(), .invalidName = readr::col_logical(),
    .sequential = readr::col_logical(), idContinuity = readr::col_logical(), 
    .uncertaintyThreshold = readr::col_logical(),
    .GBIFflags = readr::col_logical(), 
      # Paige columns
    finalLatitude = readr::col_double(), finalLongitude = readr::col_double(), 
    
    Source = readr::col_character(),
    # Dynamic dots for extra columns specified by the user
    ...
  ) # END ColTypes
  return(ColTypes)
} # END ColTypeR

