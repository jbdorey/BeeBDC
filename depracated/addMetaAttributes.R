##### Add Citation: addCitation() #####

addCitation <- function(occurDF,
                        citation){
  if(str_detect(class(flagInfo), 
                "character|data.frame",
                negate = TRUE)){
    stop("Citation must be a character object or a dataframe-like object.")
  }
  
  #add citation as attribute to occurrence dataframe
  attr(occurDF, 'citation') <- citation
  
  return(occurDF)
}


###############################################################################-

##### Add Download Date: addDate #####
### NOT CURRENTLY USED
# addDate <- function(occurDF,
#                     year = lubridate::year(Sys.Date()),
#                     month = lubridate::month(Sys.Date()),
#                     day = lubridate::day(Sys.Date())){
#  date_str <- paste(year, month, day, sep = "-")
#  
#  date_obj <- try(lubridate::ymd(date_str))
#  
#  if(class(date_obj) != "Date") stop("Year, month, and day must be provided as character or numeric values that are coercible to date objects.")
# 
#  attr(occurDF, 'download_date') <- date_obj
#  
#  return(occurDF)
# }

###############################################################################-

##### Add Coordinate Reference System: addDate #####
addCRS <- function(occurDF,
                   crs = "epsg: 4326"){
  attr(occurDF, 'CRS') <- crs
  return(occurDF)
}

###############################################################################-

##### Add Custodian: addCustodian #####
addCustodian <- function(occurDF,
                         custodian){
  if(!is.character(custodian)){
    stop("The data cleaning custodian should be a character string.")
  }
  
  attr(occurDF, 'custodian') <- custodian
  return(occurDF)
}

###############################################################################-

##### Add Custodian Email: addEmail #####
addEmail <- function(occurDF,
                     email){
  if(any(!is.character(email),
         !stringr::str_detect(email, "@"))){
    stop("The data custodian's email must be provided in a valid format.")
  }
  
  attr(occurDF, 'custodianEmail') <- email
  return(occurDF)
}

###############################################################################-

##### Add Geographic Scope: addGeoScope #####
addGeoScope <- function(occurDF,
                        scope = "global"){
  if(!is.character(scope)) stop("The geographic scope of the dataset should be provided as a character string.")
  
  attr(occurDF, 'geoScope') <- scope
  
  return(occurDF)
}

###############################################################################-

##### Add Taxonomic Scope: addTaxoScope #####
addTaxoScope <- function(occurDF,
                         scope){
  if(!is.character(scope)) stop("The taxonomic scope of the dataset should be provided as a character string.")
  
  if(scope == "guess") stop("the 'guess' functionality is not yet implemented.")
  
  attr(occurDF, 'taxoScope') <- scope
  
  return(occurDF)
}