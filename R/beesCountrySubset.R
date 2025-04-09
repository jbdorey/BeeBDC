# Description of the BeeBDC toy dataset beesCountrySubset.csv written by James B. Dorey on the 
# 28th of February 2025

#' A simple scientificName and country_suggested test dataset
#'
#' A small bee occurrence dataset with 1,488 scientificName-country_suggested combinations across 
#' four countries; Fiji, Uganda, Vietnam, and Zambia.
#'
#' @docType data
#'
#' @usage data("beesCountrySubset", package = "BeeBDC")
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#'  \item{scientificName}{Full scientificName as shown on DiscoverLife}
#'  \item{country_suggested}{A country name suggested by the [bdc::bdc_country_standardized()] function.}
#' }
#' @references This data set was created by during the writing of the paper:
#' Dorey, J.B., (upcoming) How many bee species are there? A quantitative global estimate. Journal
#' @keywords datasets
#' @examples
#'
#' beesCountrySubset <- BeeBDC::beesCountrySubset
#' head(beesCountrySubset)
#'
"beesCountrySubset"





