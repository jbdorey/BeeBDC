  # This function was written by James Dorey on the 20th of May 2022 in ored to define the columns
    # used during occurrence record cleaning
  # For help you may email James at jbdorey@me.com

#' Sets up column names and types
#' 
#' Uses [readr::cols_only()] to assign a column name and the type of data (e.g., [readr::col_character()], 
#' and [readr::col_integer()]). To see the default columns simply run [BeeDC::ColTypeR()]. Intended
#' to be used with [readr::read_csv()]. Columns that are not present will NOT be included in the resulting 
#' tibble unless they are specified using [...].
#'
#' @param ... Arguments that are additional to those default to the function. For example:
#' * newCharacterColumn = col_character(),
#' * newNumericColumn = col_integer(),
#' * newLogicalColumn = col_logical()
#'
#' @return An object of class col_spec. See [readr::as.col_spec()]
#' @export
#'
#' @examples
#' You can simply return the below for default values
#' ColTypeR() 
#' 
#' To add new columns you can write
#' ColTypeR(newCharacterColumn = col_character(), newNumericColumn = col_integer(), newLogicalColumn = col_logical()) 
#' 
#' Try reading in one of the test datasets as an example:
#' beesFlagged %>% tibble::as_tibble(col_types = ColTypeR())
#' OR
#' beesRaw %>% tibble::as_tibble(col_types = ColTypeR())
#' 
#' 
ColTypeR <- function(...){
  require(readr)
  ColTypes <- readr::cols_only(
    # Character Strings
    # CHR — taxonomy
    database_id  = col_character(), scientificName = col_character(), 
    family = col_character(), subfamily = col_character(), genus = col_character(), 
    subgenus = col_character(), subspecies = col_character(), species = col_character(), 
    specificEpithet = col_character(), infraspecificEpithet = col_character(), 
    acceptedNameUsage = col_character(), taxonRank = col_character(),
    scientificNameAuthorship = col_character(), 
    identificationQualifier = col_character(), higherClassification = col_character(), 
    identificationReferences = col_character(), typeStatus = col_character(), 
    previousIdentifications = col_character(), verbatimIdentification = col_character(), 
    identifiedBy = col_character(), dateIdentified = col_character(),
    # DBL — Locality info
    decimalLatitude = col_double(), decimalLongitude = col_double(),
    verbatimLatitude = col_character(), verbatimLongitude = col_character(),
    # CHR/Factor — Locality info
    stateProvince = col_character(), country = col_character(), continent = col_factor(), 
    locality = col_character(), island = col_character(),
    county = col_character(), municipality = col_character(),
    # CHR/Factor — Country codes
    countryCode = col_factor(), level0Gid = col_factor(), level0Name = col_factor(), 
    level1Gid = col_factor(), level1Name = col_factor(), license = col_factor(), 
    issue = col_character(), 
    # Date/Time — Collection time
    eventDate = col_character(), 
    eventTime = col_character(), 
    # Int — Collection time
    day = col_integer(), month = col_integer(), year = col_integer(),
    # Factor — Collection info
    basisOfRecord = col_factor(), type = col_factor(), occurrenceStatus = col_factor(), 
    # CHR — Collection info
    recordNumber = col_character(), recordedBy = col_character(), eventID = col_character(), 
    Location = col_character(), samplingProtocol = col_character(), samplingEffort = col_character(),
    # Int — Collection info
    individualCount = col_double(), organismQuantity = col_double(), 
    # mixed — Information uncertainty
    coordinatePrecision = col_double(), coordinateUncertaintyInMeters = col_double(), 
    spatiallyValid = col_logical(),
    # CHR — Database information
    catalogNumber = col_character(), gbifID = col_character(), datasetID = col_character(),
    institutionCode = col_character(), datasetName = col_character(), 
    otherCatalogNumbers = col_character(), occurrenceID = col_character(), 
    taxonKey = col_character(), coreid = col_character(), 
    recordId = col_character(), collectionID = col_character(),
    # CHR — Verbatim information
    verbatimScientificName = col_character(), verbatimEventDate = col_character(),
    # CHR/Factor — Aux info
    associatedTaxa = col_character(), associatedOrganisms = col_character(), 
    fieldNotes = col_character(), sex = col_factor(),
    # CHR — Rights info
    rights = col_character(), rightsHolder = col_character(), accessRights = col_character(), 
    dctermsLicense = col_character(), dctermsType = col_character(), 
    dctermsAccessRights = col_character(), associatedReferences = col_character(), 
    bibliographicCitation = col_character(), dctermsBibliographicCitation = col_character(), 
    references = col_character(),
    # Record notes
    # CHR
    flags = col_character(), informationWithheld = col_character(), isDuplicateOf = col_character(),
    # Logical
    hasCoordinate = col_logical(), hasGeospatialIssues = col_logical(), 
    # Factor
    assertions = col_factor(),
    # mix — ALA columns
    occurrenceYear = col_datetime(), id = col_character(), duplicateStatus = col_factor(), 
    associatedOccurrences = col_character(), 
    # CHR — SCAN column
    locationRemarks = col_character(),
    # CHR — dataset origin column
    dataSource = col_character(),
      # bdc columns
     dataBase_scientificName = col_character(), .rou = col_logical(),                         
    .val = col_logical(), .equ = col_logical(), .zer = col_logical(), .cap = col_logical(),                         
    .cen = col_logical(), .sea = col_logical(), .otl = col_logical(), .gbf = col_logical(),                         
    .inst = col_logical(), .dpl = col_logical(), .summary = col_logical(),
    names_clean = col_character(), verbatim_scientificName = col_character(), 
    .uncer_terms = col_logical(), .eventDate_empty = col_logical(), .year_outOfRange = col_logical(),
    .duplicates = col_logical(), .lonFlag = col_logical(), .latFlag = col_logical(), 
    .gridSummary = col_logical(), .basisOfRecords_notStandard = col_logical(),
    .scientificName_empty = col_logical(), .coordinates_empty = col_logical(),
    .coordinates_outOfRange = col_logical(), coordinates_transposed = col_logical(),
    country_suggested = col_character(),  .countryOutlier = col_logical(),
    countryMatch = col_character(), .expertOutlier = col_logical(),
      # jbd flags
    .occurrenceAbsent = col_logical(), .coordinates_country_inconsistent = col_logical(), 
    .unLicensed = col_logical(), .invalidName = col_logical(),
    .sequential = col_logical(), idContinuity = col_logical(), .uncertaintyThreshold = col_logical(),
    .GBIFflags = col_logical(), 
      # Paige columns
    finalLatitude = col_double(), finalLongitude = col_double(), 
    
    Source = col_character(),
    # Dynamic dots for extra columns specified by the user
    ...
  ) # END ColTypes
  return(ColTypes)
} # END ColTypeR

