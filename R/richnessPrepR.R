# This function was written by James Dorey to build in the Bee biodiversity process from the [currently] 
# upcoming paper to estimate the number of bee species in the world, entitled 
    # "How many bee species are there? A quantitative global estimate"
# This function comes out of the article titled "How many bee species are there? A quantitative global estimate"
# This function was written from the 13th of February 2025. For questions, please email James
# at jbdorey [at] me.com


#' Prepare occurrence, taxonomy, and checklist data for richness estimation
#' 
#' Takes your occurrence dataset along with a taxonomy and checklist in order to produce 
#' a file that's ready to be passed into the [BeeBDC::richnessEstimateR()] function in order to 
#' estimate species richness using
#' iChao and iNEXT (hill numbers) for countries, continents, or the entire globe. 
#'
#' @param data A data frame or tibble. Occurrence records as input. Needs to include the 
#' `scientificName` column and a "country" column (see `countryColumn`)
#' @param taxonomyFile A data frame or tibble. The taxonomy file to use. 
#' Default = [BeeBDC::beesTaxonomy()] but see [BeeBDC::taxadbToBeeBDC()] for other taxa.
#' @param checklistFile A data frame or tibble. The taxonomy to use. 
#' Default = [BeeBDC::beesChecklist()]; use this as a template to convert other taxa checklists.
#' @param curveFunction A function. The mathematical function that describes the curve used to 
#' randomly sample from in order to fill empty sample sizes for species present in the checklist
#' but not present in the occurrence dataset. Default is `function(x) (228.7531 * x * x^-log(12.1593))`,
#' which is taken from the paper "How many bee species are there? A quantitative global estimate" by 
#' Dorey et al. Models can be fit using [mosaic::fitModel()].
#' @param sampleSize Numeric. The size of the sample randomly drawn from the provided curve.
#' See `curveFunction`. Default = 10000.
#' @param countryColumn Character. The column from which country names should be sought.
#' @param limitGlobal Character vector. A character vector of the countries to filter the data to
#' in order limit the extent of the global-level analysis. Defualt = NULL.
#' @param outPath Character. The output path where the curve plot (curvePlot.pdf) and the output
#' Rdata (richnessInputs.Rda) file will be saved. Default = `tempdir()`.
#' 
#' @return Saves an Rdata file with the data needed to feed into the [BeeBDC::richnessEstimateR()]
#'  function. The [BeeBDC::richnessEstimateR()] function will then 
#' use iChao and/or iNEXT to estimate species richness for countries, continents, and the globe. 
#' Also returns the RData file to the environment.
#' 
#' @importFrom dplyr %>%
#' 
#' @seealso [BeeBDC::countryHarmoniseR()] to harmonise country names based on a short list.
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' data(beesCountrySubset)
#' 
#' estimateDataExample <- richnessPrepR(
#'   data = beesCountrySubset,
#'   # Download the taxonomy
#'   taxonomyFile = BeeBDC::beesTaxonomy(),
#'   # Download the checklist
#'   checklistFile = BeeBDC::beesChecklist(),
#'   curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
#'   sampleSize = 10000,
#'   countryColumn = "country_suggested",
#'   limitGlobal = NULL,
#'   outPath = tempdir()
#' )
#' }

richnessPrepR <- function(
    data = NULL,
    # Download the taxonomy
    taxonomyFile = BeeBDC::beesTaxonomy(),
    # Download the checklist
    checklistFile = BeeBDC::beesChecklist(),
    curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
    sampleSize = 10000,
    countryColumn = "country_suggested",
    limitGlobal = NULL,
    outPath = tempdir()
) {  
  # locally bind variables to the function
  . <- taxonomyAcceptedNames <- taxonomy_synonyms <- data_counts <- taxonomic_status <- NULL
  taxonomic_status<- rNaturalEarth_name<- scientificName<- count_n<- validName<- name<- 
    continent<- name_long<- continent.x<- continent.y <- country_suggested <- NULL
  
  
  # Load required packages 
  requireNamespace("rlang")
  requireNamespace("dplyr")
  
  # Record start time
  startTime <- Sys.time()
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data. I'm a program not a magician.")
  }
  
    ##### 0.2 Prep input data ####
  # Designing data
    # data <- readr::read_csv("/Users/jamesdorey/Desktop/Uni/Packages/BeeBDC_development/beesCountry.csv") %>%
    #   dplyr::select(c("scientificName", "country_suggested"))
  data <- data %>%
    dplyr::rename(countryColumn = tidyselect::any_of(countryColumn)) 
  
  
  #### 1.0 Prep taxonomy and checklist ####
    ##### 1.1 Taxonomy manipulation ####
  # Get this just for valid species 
  taxonomyAcceptedNames <- taxonomyFile %>%
    dplyr::filter(taxonomic_status == "accepted")
  # Get this just for synonyms species 
  taxonomy_synonyms <- taxonomyFile %>%
    dplyr::filter(taxonomic_status == "synonym")
  
  
    ##### 1.2 Checklist manipulation ####
  # Now, for each country pass on the newly-extracted info to the country checklist
  checklistFile <- checklistFile %>% 
    dplyr::left_join(taxonomyFile %>%
                       dplyr::select(tidyselect::any_of(c("validName", "year", 
                                                          "author", "authorCount"))),
                     by = "validName")
  
    # If chosen, limit the countries in the checklist in order to limit the global analysis
  if(!is.null(limitGlobal)){
    checklistFile <- checklistFile %>%
      dplyr::filter(rNaturalEarth_name %in% limitGlobal)
  }
  
  #### 2. Prepare occ data ####
  ##### 2.1 Prepare all counts ####
  ###### a. total counts ####
  # Get counts of species in the dataset
  data_counts <- data %>% 
    dplyr::group_by(scientificName) %>%
    dplyr::count() %>%
    dplyr::mutate(dataFrom = "points")
  
  ##### 2.2 Country counts ####
  # Get the counts of species per country based on point occurrences
  siteSpeciesCounts <- data %>%
    countryHarmoniseR(data = .,
                      countryColumn = "country_suggested") %>%
    dplyr::group_by(scientificName, country_suggested) %>%
    dplyr::count() %>%
    dplyr::mutate(dataFrom = "points") %>%
    dplyr::filter(!is.na(country_suggested)) %>%
    dplyr::group_by(n) %>% 
    dplyr::mutate(count_n = dplyr::n())
  
  
  
  #### 3.0 Sample the curve ####
    ##### 3.1 Plot curve ####
  # Build a plot of the points and the model
  suppressWarnings({
  (curvePlot <- ggplot2::ggplot(data = siteSpeciesCounts, 
                               ggplot2::aes(x = n, y = count_n)) +
     # Literature curve data 
     ggplot2::geom_function(fun = curveFunction, 
                            ggplot2::aes(colour = "#FFC125"), linetype = 1, linewidth = 1) +
     ggplot2::geom_point(data = siteSpeciesCounts,
                         ggplot2::aes(x = n, y = count_n, colour = "#FFC125"),
                         alpha = 0.65) +
     ggplot2::scale_colour_manual(values = c("#FFC125"),
                                  label = c("Literature"),
                                  name = "Data source") +
     ggplot2::labs(x = "Number of occurrences", y = "Count")+ 
     
     # Limit the x-axis to see the curve
     ggplot2::xlim(1,100) + 
     ggplot2::theme(legend.position.inside = c(0.76, 0.86),
                    legend.position = "inside",
                    panel.background = ggplot2::element_rect(fill = NA,
                                                             colour = "black",
                                                             linetype = "solid"),
                    panel.border = ggplot2::element_rect(fill = NA,
                                                         colour = "black",
                                                         linetype = "solid")) )
  
  # Save the plot
  ggplot2::ggsave(paste0(outPath, "/curvePlot.pdf"), 
                  plot = curvePlot, 
                  width = 6, height = 6, units = "in", dpi = 300)
  }) # END suppressWarnings
  
    ##### 3.2 Extract and sample curve ####
  # Get the coordinates from the gam model in ggplot2 and put them in a tibble
  curveExtraction <- dplyr::tibble(
    x_coords = ggplot2::ggplot_build(curvePlot)$data[[1]]$x,
    y_coords = ggplot2::ggplot_build(curvePlot)$data[[1]]$y)
  
  # Set the seed for a consistent result
  set.seed(1234)
  # Get a random sample based on the empirical dataset
  litSample <- sample(x = curveExtraction$x_coords, prob = curveExtraction$y_coords, 
                      replace = TRUE,
                      size = sampleSize) %>%
    round(0)
  
  
  
  
  ##### 3.3 Apply to missing species #####
  # Find the species that are not represented in the occurrence dataset
  noPointSpecies <- taxonomyAcceptedNames %>% 
    dplyr::pull(validName) %>%
    setdiff(data$scientificName)  %>%
    # Add a sample size that is drawn from the same distriubtion of the literature and no-occurrences
    # dataset and generated in litSample
    dplyr::tibble(scientificName = .,
                  n = litSample[1:length(.)],
                  dataFrom = "randomisedSample")
  
  # 
  data_totalCounts <- data_counts %>% 
    dplyr::bind_rows(noPointSpecies)
  
  
  #### 4.0 Manipulate data ####
  
  # Make a function to sample the country sample sizes
  countrySampler <- function(df = NULL){ 
    dataFrom <- NULL
    # Get the maximum value for curveExtraction$x_coords
    max_x <- max(curveExtraction$x_coords)
    # Test that there is sufficient length (i.e., at least one records with n > NA)
    lengthTest <- df$n[complete.cases(df$n)] %>% 
      length()
    # Get the maximum value for the country's sample size or set the minium to n = 1
    if(lengthTest > 0){
      max_n = max(df$n[complete.cases(df$n)])
    }else{
      max_n = 1
    }
    # Find the length of the indput data
    inputLength <- length(df$n) + 100
    # Take the smaller of these two values
    maxSample <- dplyr::if_else(max_x > max_n, max_n, max_x)
    # Now apply the maxxed distribution to the country's checklist data
    df <- df %>%
      dplyr::mutate(n = dplyr::if_else(is.na(n), 
                                       # Draw from the literature distribution
                                       sample(x = curveExtraction$x_coords[1:maxSample],
                                              prob = curveExtraction$y_coords[1:maxSample], 
                                              replace = TRUE,
                                              size = inputLength)[dplyr::row_number()] %>%
                                         round(0),
                                       # Or keep n
                                       n),
                    dataFrom = dplyr::if_else(is.na(dataFrom), "checklist", dataFrom))
    return(df)
  }# END countrySampler
  
  # Now, for those species-country combinations without ANY points, but that should have points
  # from the checklist, make n = a number drawn from the literature records distribution
  # and state that they came from the checklist
  country_speciesChecklistCounts <- checklistFile %>%
    dplyr::select(tidyselect::any_of(c("validName", "rNaturalEarth_name", "year",
                                       "author", "authorCount"))) %>%
    dplyr::full_join(siteSpeciesCounts, by = c("validName" = "scientificName",
                                                   "rNaturalEarth_name" = "country_suggested")) %>%
    # Group by country and then split into a list per group
    dplyr::group_by(rNaturalEarth_name) %>%
    dplyr::group_split() %>%
    # For each country, apply the literature distribution but with an n max of the empirical maximum
    # for that country
    lapply(., countrySampler) %>%
    dplyr::bind_rows() %>%
    # Fix up the year, author, and authorCount columns by removing and re-adding them
    dplyr::select(!tidyselect::any_of(c("year", "author", "authorCount"))) %>% 
    dplyr::left_join(taxonomyFile %>%
                       dplyr::select(tidyselect::any_of(c("validName", "year", 
                                                          "author", "authorCount"))),
                     by = "validName") %>%
    dplyr::distinct(validName, rNaturalEarth_name, .keep_all = TRUE)
  
  # Create the Chao input data including checklist data
  countryChaoData_checklist <- country_speciesChecklistCounts %>%
    dplyr::select(validName, rNaturalEarth_name, n) %>%
    tidyr::pivot_wider(names_from = rNaturalEarth_name,
                       values_from = n,
                       values_fill = 0) %>%
    # Create the rownames
    column_to_rownames_internal("validName") %>%
    dplyr::tibble()
  
  # Create the Chao input data including ONLY occurrence data
  countryChaoData_occs <- siteSpeciesCounts %>%
    dplyr::select(scientificName, country_suggested, n) %>%
    tidyr::pivot_wider(names_from = country_suggested,
                       values_from = n,
                       values_fill = 0) %>%
    # Create the rownames
    column_to_rownames_internal("scientificName") %>%
    dplyr::tibble()
  
  
  ##### 4.1 Continent ####
  # Download a world map to convert countries to continents
  worldMap <- rnaturalearth::ne_countries(returnclass = "sf",
                                          scale = 50, type = "countries") 
  
  # Turn the country occ data into a continent one
  continentOccs <- siteSpeciesCounts %>%
    dplyr::ungroup() %>%
    countryHarmoniseR(countryColumn = "country_suggested") %>%
    # Join first by name
    dplyr::left_join(worldMap %>% dplyr::select(name, continent) %>%
                       sf::st_drop_geometry(), by = c("country_suggested" = "name")) %>%
    # Then join by name_long
    dplyr::left_join(worldMap %>% dplyr::select(name_long, continent) %>%
                       sf::st_drop_geometry(), by = c("country_suggested" = "name_long")) %>%
    # Now merge these continent columns
    dplyr::mutate(continent = dplyr::if_else(is.na(continent.x),
                                             continent.y, continent.x)) %>%
    # drop interim and old count columns
    dplyr::select(!c("continent.x", "continent.y")) %>%
    dplyr::group_by(scientificName, continent) %>%
    dplyr::mutate(sum = sum(n)) %>%
    dplyr::mutate(name_continent = stringr::str_c(scientificName, continent, sep = "__"))
  
  
  # Turn the country checklist data into a continent one
  continentChecklist <- checklistFile %>% 
    dplyr::select(validName, rNaturalEarth_name) %>%
    dplyr::ungroup() %>%
    # Change some country names to better match the continent data
    countryHarmoniseR(countryColumn = "rNaturalEarth_name",
                      continentAnalysis = TRUE) %>%
    # Join first by name
    dplyr::left_join(worldMap %>% dplyr::select(name, continent) %>%
                       sf::st_drop_geometry(), by = c("rNaturalEarth_name" = "name")) %>%
    # Then join by name_long
    dplyr::left_join(worldMap %>% dplyr::select(name_long, continent) %>%
                       sf::st_drop_geometry(), by = c("rNaturalEarth_name" = "name_long")) %>%
    # Now merge these continent columns
    dplyr::mutate(continent = dplyr::if_else(is.na(continent.x),
                                             continent.y, continent.x)) %>%
    # drop interim and old count columns
    dplyr::select(!c("continent.x", "continent.y")) %>%
    dplyr::mutate(name_continent = stringr::str_c(validName, continent, sep = "__"))
  
  # Find the names in the checklist that are missing from the occurrence records
  continentNoPoints <- dplyr::symdiff(continentOccs$name_continent, continentChecklist$name_continent) %>%
    # Extract continent and species names into tibble columns and remove the . column
    dplyr::tibble(scientificName = stringr::str_extract(., ".*__") %>% stringr::str_remove("__"),
                  continent = stringr::str_extract(., "__.*") %>% stringr::str_remove("__"),
                  n = NA,
                  dataFrom = "litEstimate") %>%
    dplyr::select(!.) 
  
  # Combine the occurrence and checklist data 
  continentCounts <- continentOccs %>%
    dplyr::select(!c("country_suggested", "n", "name_continent")) %>%
    dplyr::rename(n = sum) %>%
    dplyr::bind_rows(continentNoPoints) %>%
    # Group by country and then split into a list per group
    dplyr::group_by(continent) %>%
    dplyr::group_split() %>%
    # For each country, apply the literature distribution but with an n max of the empirical maximum
    # for that country
    lapply(., countrySampler) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(scientificName, continent, .keep_all = TRUE) 
  
  # Pivot wider 
  continentWider <- continentCounts %>%
    dplyr::select(scientificName, continent, n) %>%
    tidyr::drop_na() %>% 
    tidyr::pivot_wider(names_from = continent,
                       values_from = n,
                       values_fill = 0) %>%
    # Create the rownames
    column_to_rownames_internal("scientificName") %>%
    dplyr::tibble()
  
  
  #### 5. Save data ####
  
  ##### 5.1 data prep ####
  # Get the difference between the taxonomy and occurrence records
  taxonomyNoPoints <- taxonomyAcceptedNames %>% 
    dplyr::pull(validName) %>%
    setdiff(data$scientificName)
  
  # Re-apply the original column name
  names(continentChecklist)[names(continentChecklist) == "countryColumn"] <- countryColumn
  names(siteSpeciesCounts)[names(siteSpeciesCounts) == "countryColumn"] <- siteSpeciesCounts
  
  
  # Make a list of the data inputs so that this can be run easily on another computer
  richnessInputs <- dplyr::lst(
    # The curve created by implementing mosaic::fitModel over the literature samples  
    curveExtraction,
    checklistFile,
    taxonomyFile,
    taxonomyAcceptedNames,
    taxonomyNoPoints,
    data_counts,
    continentChecklist,
    continentNoPoints,
    worldMap,
    # Get the counts of species per country based on point occurrences
    siteSpeciesCounts) 
  
    #### 5.2 Save data ####
  # Save the file for use on another computer
  richnessInputs %>%
    base::saveRDS(., 
                  file = paste0(outPath, "/richnessInputs.Rda"))
  # Return the data
return(richnessInputs)
  
}
