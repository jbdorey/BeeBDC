  # This function was written by James Dorey on the 20th of May 2022 in ored to define the columns
    # used during occurrence record cleaning
  # For help you may email James at jbdorey[at]me.com

#' Sets up column names and types
#' 
#' This function uses [vroom::cols_only()] to assign a column name and the type of data 
#' (e.g., [vroom::col_character()], 
#' and [vroom::col_integer()]). To see the default columns simply run [BeeDC::ColTypeR()]. 
#' This is intended for use with [readr::read_csv()]. Columns that are not present will NOT be included 
#' in the resulting tibble unless they are specified using [...].
#'
#' @param ... Additional arguments. These can be specified in addition to the ones default to the 
#' function.  For example:
#' * newCharacterColumn = [vroom::col_character()],
#' * newNumericColumn = [vroom::col_integer()],
#' * newLogicalColumn = [vroom::col_logical()]
#' 
#' @importFrom vroom col_character col_double col_factor col_integer col_logical col_datetime
#' @importFrom dplyr %>%
#'
#' @return Returns an object of class col_spec. 
#' See [readr::as.col_spec()] for additional context and explication.
#' @export
#'
#' @examples 
#'   # You can simply return the below for default values
#'   library(dplyr)
#' BeeDC::ColTypeR() 
#' 
#'   # To add new columns you can write
#' ColTypeR(newCharacterColumn = vroom::col_character(), 
#'          newNumericColumn = vroom::col_integer(), 
#'          newLogicalColumn = vroom::col_logical()) 
#' 
#' # Try reading in one of the test datasets as an example:
#' beesFlagged %>% tibble::as_tibble(col_types = BeeDC::ColTypeR())
#'   # OR
#' beesRaw %>% tibble::as_tibble(col_types = BeeDC::ColTypeR())
#' 
#' 
ColTypeR <- function(...){
  requireNamespace("bdc")
  ColTypes <- readr::cols_only(
    # Character Strings
    # CHR — taxonomy
    database_id  = vroom::col_character(), scientificName = vroom::col_character(), 
    family = vroom::col_character(), subfamily = vroom::col_character(), genus = vroom::col_character(), 
    subgenus = vroom::col_character(), subspecies = vroom::col_character(), species = vroom::col_character(), 
    specificEpithet = vroom::col_character(), infraspecificEpithet = vroom::col_character(), 
    acceptedNameUsage = vroom::col_character(), taxonRank = vroom::col_character(),
    scientificNameAuthorship = vroom::col_character(), 
    identificationQualifier = vroom::col_character(), higherClassification = vroom::col_character(), 
    identificationReferences = vroom::col_character(), typeStatus = vroom::col_character(), 
    previousIdentifications = vroom::col_character(), verbatimIdentification = vroom::col_character(), 
    identifiedBy = vroom::col_character(), dateIdentified = vroom::col_character(),
    # DBL — Locality info
    decimalLatitude = vroom::col_double(), decimalLongitude = vroom::col_double(),
    verbatimLatitude = vroom::col_character(), verbatimLongitude = vroom::col_character(),
    verbatimElevation = vroom::col_character(),
    # CHR/Factor — Locality info
    stateProvince = vroom::col_character(), country = vroom::col_character(), continent = vroom::col_factor(), 
    locality = vroom::col_character(), island = vroom::col_character(),
    county = vroom::col_character(), municipality = vroom::col_character(),
    # CHR/Factor — Country codes
    countryCode = vroom::col_factor(), level0Gid = vroom::col_factor(), level0Name = vroom::col_factor(), 
    level1Gid = vroom::col_factor(), level1Name = vroom::col_factor(), license = vroom::col_factor(), 
    issue = vroom::col_character(), 
    # Date/Time — Collection time
    eventDate = vroom::col_character(), 
    eventTime = vroom::col_character(), 
    startDayOfYear = vroom::col_integer(),
    endDayOfYear = vroom::col_integer(),
    # Int — Collection time
    day = vroom::col_integer(), month = vroom::col_integer(), year = vroom::col_integer(),
    # Factor — Collection info
    basisOfRecord = vroom::col_factor(), type = vroom::col_factor(), occurrenceStatus = vroom::col_factor(), 
    # CHR — Collection info
    recordNumber = vroom::col_character(), recordedBy = vroom::col_character(), eventID = vroom::col_character(), 
    Location = vroom::col_character(), samplingProtocol = vroom::col_character(), 
    samplingEffort = vroom::col_character(),
    # Int — Collection info
    individualCount = vroom::col_double(), organismQuantity = vroom::col_double(), 
    # mixed — Information uncertainty
    coordinatePrecision = vroom::col_double(), coordinateUncertaintyInMeters = vroom::col_double(), 
    spatiallyValid = vroom::col_logical(),
    # CHR — Database information
    catalogNumber = vroom::col_character(), gbifID = vroom::col_character(), datasetID = vroom::col_character(),
    institutionCode = vroom::col_character(), datasetName = vroom::col_character(), 
    otherCatalogNumbers = vroom::col_character(), occurrenceID = vroom::col_character(), 
    taxonKey = vroom::col_character(), coreid = vroom::col_character(), 
    recordId = vroom::col_character(), collectionID = vroom::col_character(),
    associatedSequences = vroom::col_character(),
    # CHR — Verbatim information
    verbatimScientificName = vroom::col_character(), verbatimEventDate = vroom::col_character(),
    # CHR/Factor — Aux info
    associatedTaxa = vroom::col_character(), associatedOrganisms = vroom::col_character(), 
    fieldNotes = vroom::col_character(), sex = vroom::col_character(),
    # CHR — Rights info
    rights = vroom::col_character(), rightsHolder = vroom::col_character(), accessRights = vroom::col_character(), 
    dctermsLicense = vroom::col_character(), dctermsType = vroom::col_character(), 
    dctermsAccessRights = vroom::col_character(), associatedReferences = vroom::col_character(), 
    bibliographicCitation = vroom::col_character(), dctermsBibliographicCitation = vroom::col_character(), 
    references = vroom::col_character(),
    # Record notes
    # CHR
    flags = vroom::col_character(), informationWithheld = vroom::col_character(), 
    isDuplicateOf = vroom::col_character(),
    # Logical
    hasCoordinate = vroom::col_logical(), hasGeospatialIssues = vroom::col_logical(), 
    # Factor
    assertions = vroom::col_factor(),
    # mix — ALA columns
    occurrenceYear = vroom::col_datetime(), id = vroom::col_character(), duplicateStatus = vroom::col_factor(), 
    associatedOccurrences = vroom::col_character(), 
    # CHR — SCAN column
    locationRemarks = vroom::col_character(),
    # CHR — dataset origin column
    dataSource = vroom::col_character(),
      # bdc columns
     dataBase_scientificName = vroom::col_character(), .rou = vroom::col_logical(),                         
    .val = vroom::col_logical(), .equ = vroom::col_logical(), .zer = vroom::col_logical(), .cap = vroom::col_logical(),                         
    .cen = vroom::col_logical(), .sea = vroom::col_logical(), .otl = vroom::col_logical(), .gbf = vroom::col_logical(),                         
    .inst = vroom::col_logical(), .dpl = vroom::col_logical(), .summary = vroom::col_logical(),
    names_clean = vroom::col_character(), verbatim_scientificName = vroom::col_character(), 
    .uncer_terms = vroom::col_logical(), .eventDate_empty = vroom::col_logical(), 
    .year_outOfRange = vroom::col_logical(),
    .duplicates = vroom::col_logical(), .lonFlag = vroom::col_logical(), .latFlag = vroom::col_logical(), 
    .gridSummary = vroom::col_logical(), .basisOfRecords_notStandard = vroom::col_logical(),
    .scientificName_empty = vroom::col_logical(), .coordinates_empty = vroom::col_logical(),
    .coordinates_outOfRange = vroom::col_logical(), coordinates_transposed = vroom::col_logical(),
    country_suggested = vroom::col_character(),  .countryOutlier = vroom::col_logical(),
    countryMatch = vroom::col_character(), .expertOutlier = vroom::col_logical(),
      # jbd flags
    .occurrenceAbsent = vroom::col_logical(), .coordinates_country_inconsistent = vroom::col_logical(), 
    .unLicensed = vroom::col_logical(), .invalidName = vroom::col_logical(),
    .sequential = vroom::col_logical(), idContinuity = vroom::col_logical(), 
    .uncertaintyThreshold = vroom::col_logical(),
    .GBIFflags = vroom::col_logical(), 
      # Paige columns
    finalLatitude = vroom::col_double(), finalLongitude = vroom::col_double(), 
    
    Source = vroom::col_character(),
    # Dynamic dots for extra columns specified by the user
    ...
  ) # END ColTypes
  return(ColTypes)
} # END ColTypeR

