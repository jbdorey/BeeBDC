# This function was written by James Dorey estimate species richnesses using iChao and iNEXT
# This function was written starting on the 18th of February 2025 
# This function comes out of the article titled "How many bee species are there? A quantitative global estimate"
# at jbdorey[at]me.com

#' Estimate country, continental, and global species richnesses
#' 
#' Takes an output dataset from [BeeBDC::richnessPrepR()] to estimate species richness using
#' iChao and iNEXT (hill numbers) for countries, continents, and/or the entire globe. 
#'
#' @param data an RData file created using the [BeeBDC::richnessPrepR()] function.
#' @param sampleSize Numeric. The size of the sample randomly drawn from the provided curve.
#' See `curveFunction`. Default = 10000.
#' @param countrySamples Numeric. The number of times to sample the country species richness for both
#' iChao and iNEXT. If equal to zero (0), then this will not be analysed. Default = 5.
#' @param continentSamples Numeric. The number of times to sample the continent species richness for both
#' iChao and iNEXT. If equal to zero (0), then this will not be analysed. Default = 5.
#' @param globalSamples Numeric. The number of times to sample the global species richness for both
#' iChao and iNEXT. If equal to zero (0), then this will not be analysed. Default = 5.
#' @param filterToRecordedCountries Logical. If TRUE, the checklist will be filtered to the countries 
#' Where occurrence records were found. Default = TRUE. Change at your own peril.
#' @param countriesToExclude Character vector. You may decide to excluse some countries if they are 
#' being problematic or their sample sizes are too small. Default = NULL.
#' @param k Numeric. For iChao; the cut-off point (default = 10), which separates species into "abundant" and "rare" 
#' groups for abundance data for the estimator ACE; it separates species into "frequent" and 
#' "infrequent" groups for incidence data for the estimator ICE.
#' Default = 10.
#' @param outPath A directory as character. Directory where to save output figure. Default = tempdir().
#' @param fileName A character vector with file name 
#' for the output figure, ending with '.pdf'. Default = "continentSampled.pdf".
#' @param mc.cores Numeric. If > 1, the function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory.
#' Default = 1.
#' 
#' @return Outputs an R file with four tables ("Summary", "SiteOutput", "ContinentOutput", and 
#' "GlobalOutput"; depending on the number required). The summary table shows the Median overall 
#' estimates, while the remaining three shows the outputs from each iteration (useful for plotting, 
#' see relevant vignette). Some figures may also be saved to the selected outPath.
#' 
#' @importFrom dplyr %>%
#' 
#' @seealso [BeeBDC::countryHarmoniseR()] to harmonise country names based on a short list; 
#' [BeeBDC::richnessPrepR()] to produce the input data and for the required column names; as well 
#' as [BeeBDC::ChaoWrapper()] and [BeeBDC::iNEXTwrapper()] for the parallelised implementation of
#' `SpadeR` and `iNEXT` functions.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'   # Use the example data 
#' data(beesCountrySubset)
#' 
#'   # First, 
#' estimateDataExample <- BeeBDC::richnessPrepR(
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
#' 
#'  exampleEstimate <- richnessEstimateR(
#'    data = estimateDataExample,
#'    sampleSize = 10000,
#'    countrySamples = 1,
#'    continentSamples = 1,
#'    globalSamples = 1,
#'    filterToRecordedCountries = TRUE,
#'    mc.cores = 1,
#'    # Directory where to save files
#'    outPath = tempdir(),
#'    fileName = "Sampled.pdf"
#'  )
#'  
#' } # END dontrun
#' 


  # START THE REAL FUNCTION
richnessEstimateR <- function(
    data = NULL,
    sampleSize = 10000,
    countrySamples = 1,
    continentSamples = 1,
    globalSamples = 1,
    countriesToExclude = NULL,
    mc.cores = 1,
    k = 10,
    filterToRecordedCountries = TRUE,
    # Directory where to save files
    outPath = tempdir(),
    fileName = "continentSampled.pdf"
) {  
  # locally bind variables to the function
  . <- taxonomyAcceptedNames <- taxonomy_synonyms <- occData_counts <- NULL
  Name<- statistic<- Estimator<- `95% Lower`<- `95% Upper`<- `95%Upper`<- groupNo<- variable<-
    Observed<- Estimate<- `95%Lower`<- iNEXT_est<- observedRichness<- level<- niNEXT<- 
    iNEXT_lower<- iNEXT_upper<- iNEXT_increasePercent<- iNEXT_increase<- nChao<- iChao_est<- 
    iChao_lower<- iChao_upper<- Assemblage<- continent <- NULL
  rNaturalEarth_name <- combined_site_ChaoiNext <- combined_cont_ChaoiNext <- 
    combined_global_ChaoiNext <- output <- Est_s.e. <- NULL
  
    #### 0.0 data prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data I'm a program not a magician.")
  }
  
  ##### 0.2 SpadeR test ####
  ###### a. test ####
  # Check if SpadeR is installed
  # TRUE if SpadeR is found
  suppressWarnings(
    suggestedTest <- system.file(package='SpadeR') %>% 
      stringr::str_count() > 0 
  )
  
  ###### b. SpadeR ####
  if(suggestedTest == FALSE){
    # Set up instructions for download on fail
    instructions <- paste(" Please try installing the package for yourself", 
                          "using the following command: \n",
                          "install.packages(\"SpadeR\")")
    # Set up fail function for tryCatch
    error_func <- function(e){
      stop(paste("Failed to install the SpadeR package.\n", 
                 instructions))
    }
    # Begin interactive input
    input <- 1
    if (interactive()){
      input <- utils::menu(c("Yes", "No"), 
                           title = paste0("Install the SpadeR package? \n"))
    }
    if(input == 1){
      # Start SpadeR install
      message("Installing the SpadeR package.")
      tryCatch(
        utils::install.packages("SpadeR"), 
        error = error_func, warning = error_func)
    } # END input == 1
    
    else{
      stop(writeLines(paste("The SpadeR package is necessary for BeeBDC::ChaoWrapper\n", 
                            instructions)))
    } # END else
  } # END suggestedTest == FALSE
  
  # Load required packages 
  requireNamespace("stringr")
  requireNamespace("dplyr")
  requireNamespace("iNEXT")
  
  

  
  
  ##### 0.3 iNEXT test ####
  ###### a. test ####
  # Check if iNEXT is installed
  # TRUE if iNEXT is found
  suppressWarnings(
    suggestedTest <- system.file(package='iNEXT') %>% 
      stringr::str_count() > 0 
  )
  
  ###### b. iNEXT ####
  if(suggestedTest == FALSE){
    # Set up instructions for download on fail
    instructions <- paste(" Please try installing the package for yourself", 
                          "using the following command: \n",
                          "install.packages(\"iNEXT\")")
    # Set up fail function for tryCatch
    error_func <- function(e){
      stop(paste("Failed to install the iNEXT package.\n", 
                 instructions))
    }
    # Begin interactive input
    input <- 1
    if (interactive()){
      input <- utils::menu(c("Yes", "No"), 
                           title = paste0("Install the iNEXT package? \n"))
    }
    if(input == 1){
      # Start iNEXT install
      message("Installing the iNEXT package.")
      tryCatch(
        utils::install.packages("iNEXT"), 
        error = error_func, warning = error_func)
    } # END input == 1
    
    else{
      stop(writeLines(paste("The iNEXT package is necessary for BeeBDC::iNEXTwrapper\n", 
                            instructions)))
    } # END else
  } # END suggestedTest == FALSE
  
  
  
  ##### 0.4 data prep ####
  # Building/testing data input
  richnessData <- data
  
  # Extract the datasets from data
  literatureCurve = richnessData$curveExtraction
  taxonomyAcceptedNames = richnessData$taxonomyAcceptedNames
  taxonomyNoPoints = richnessData$taxonomyNoPoints
  taxonomyFile = richnessData$taxonomyFile
  data_counts = richnessData$data_counts
  continentChecklist = richnessData$continentChecklist
  worldMap = richnessData$worldMap
  continentNoPoints = richnessData$continentNoPoints
  siteSpeciesCounts = richnessData$siteSpeciesCounts
  # combine some country names
  checklistFile = richnessData$checklistFile %>% 
    # Change country names
    countryHarmoniseR(countryColumn = "rNaturalEarth_name")
  
  # If no countriesToExclude is provided, make it an empty vector
  if(is.null(countriesToExclude)){
    countriesToExclude = c("")
  }
  
  ##### 0.5 filter to recorded countries ####
  if(filterToRecordedCountries == TRUE){
      # filter continent
    continentChecklist <- continentChecklist %>% 
      dplyr::filter(rNaturalEarth_name %in% siteSpeciesCounts$country_suggested)
      # Filter checklist 
    checklistFile <- checklistFile%>%
      dplyr::filter(rNaturalEarth_name %in% siteSpeciesCounts$country_suggested)

  }
  
  
  #### 1.0 functions ####
    ##### 1.2 richnessSampleR ####
  richnessSampleR <- function(
    # From X:Y iterations
    iterations = NULL,
    data = richnessData,
    # What scale?
    scale = NULL, # Country, Continent, or Global
    # Functions
    ChaoWrapper = ChaoWrapper,
    iNEXTwrapper = iNEXTwrapper
    
  ){
    rNaturalEarth_name <- validName <- country_suggested <- name <- name_long <- continent.x <- 
      continent.y <- scientificName <- dataFrom <- NULL
    #### 1.1.1 Prep ####

    # Extract the datasets from data
    literatureCurve = richnessData$curveExtraction
    taxonomyAcceptedNames = richnessData$taxonomyAcceptedNames
    taxonomyNoPoints = richnessData$taxonomyNoPoints
    taxonomyFile = richnessData$taxonomyFile
    data_counts = richnessData$data_counts
    continentChecklist = richnessData$continentChecklist
    worldMap = richnessData$worldMap
    continentNoPoints = richnessData$continentNoPoints
    siteSpeciesCounts = richnessData$siteSpeciesCounts
    
    # Fix some names in the siteSpeciesCounts
    siteSpeciesCounts <- siteSpeciesCounts %>% 
      countryHarmoniseR(countryColumn = "country_suggested")
    # combine some country names
    checklistFile = richnessData$checklistFile %>% 
      # Change country names
      countryHarmoniseR(countryColumn = "rNaturalEarth_name")
    if(filterToRecordedCountries == TRUE){
      # filter continent
      continentChecklist <- continentChecklist %>% 
        dplyr::filter(rNaturalEarth_name %in% siteSpeciesCounts$country_suggested)
      # Filter checklist 
      checklistFile <- checklistFile%>%
        dplyr::filter(rNaturalEarth_name %in% siteSpeciesCounts$country_suggested)
      
    }
    
    
    #### 1.1.2 Sample the curve ####
    # Sample the curve at the start of each iteration to generate new values
    # Get a random sample based on the empirical dataset
    litSample <- base::sample(x = literatureCurve$x_coords, prob = literatureCurve$y_coords, 
                        replace = TRUE,
                        size = sampleSize) %>%
      round(0) 
    
    

    # Find the species that are not represented in the occurrence dataset
    noPointSpecies <- taxonomyNoPoints  %>%
      # Add a sample size that is drawn from the same distriubtion of the literature and no-occurrences
      # dataset and generated in litSample
      dplyr::tibble(scientificName = .,
                    n = litSample[1:length(.)],
                    dataFrom = "randomisedSample")
    
    # 
    data_totalCounts <- data_counts %>% 
      dplyr::bind_rows(noPointSpecies) %>%
        # Drop NA points
      dplyr::filter(complete.cases(n))
    
    #### 1.1.3 Data prep ####
    ##### a. Country ####
    if(scale == "Country"){
      # Now, for those species-country combinations without ANY points, but that should have points
      # from the checklist, make n = a number drawn from the literature records distribution
      # and state that they came from the checklist
      country_speciesChecklistCounts <- checklistFile %>%
        dplyr::select(
          tidyselect::any_of(c("validName", "rNaturalEarth_name", "year", "author",
                              "authorCount"))) %>%
        dplyr::full_join(siteSpeciesCounts, by = c("validName" = "scientificName",
                                                       "rNaturalEarth_name" = "country_suggested")) %>%
        # Group by country and then split into a list per group
        dplyr::group_by(rNaturalEarth_name) %>%
        dplyr::group_split() %>%
        # For each country, apply the literature distribution but with an n max of the empirical maximum
        # for that country
        base::lapply(., countrySampler) %>%
        dplyr::bind_rows() %>%
        # Fix up the year, author, and authorCount columns by removing and re-adding them
        dplyr::select(!tidyselect::any_of(c("year", "author", "authorCount"))) %>% 
        dplyr::left_join(taxonomyFile %>%
                           dplyr::select(tidyselect::any_of(c("validName", "year", "author",
                                                            "authorCount"))),
                         by = "validName") %>%
        dplyr::distinct(validName, rNaturalEarth_name, .keep_all = TRUE)
      
      
      # Create the Chao input data including checklist data
      countryChaoData_checklist <- country_speciesChecklistCounts %>%
        dplyr::select(validName, rNaturalEarth_name, n) %>%
        tidyr::pivot_wider(names_from = rNaturalEarth_name,
                           values_from = n,
                           values_fill = 0) %>%
        ## Create the rownames
        column_to_rownames_internal("validName") %>%
        dplyr::tibble()
      
    } # END if country
    
    
    ##### b. Continent ####
    if(scale == "Continent"){
      # Turn the country occ data into a continent one
      continentOccs <- siteSpeciesCounts %>%
        dplyr::ungroup() %>%
        # Change some country names to better match the continent data
        countryHarmoniseR(countryColumn = "country_suggested") %>%
        # Join first by name
        dplyr::left_join(worldMap %>% dplyr::select(name, continent) %>%
                           countryHarmoniseR(countryColumn = "name") %>%
                           sf::st_drop_geometry(), by = c("country_suggested" = "name")) %>%
        # Then join by name_long
        dplyr::left_join(worldMap %>% dplyr::select(name_long, continent) %>%
                           countryHarmoniseR(countryColumn = "name_long") %>%
                           sf::st_drop_geometry(), by = c("country_suggested" = "name_long")) %>%
        # Now merge these continent columns
        dplyr::mutate(continent = dplyr::if_else(is.na(continent.x),
                                                 continent.y, continent.x)) %>%
        # drop interim and old count columns
        dplyr::select(!c("continent.x", "continent.y")) %>%
        dplyr::group_by(scientificName, continent) %>%
        dplyr::mutate(sum = sum(n)) %>%
        dplyr::mutate(name_continent = stringr::str_c(scientificName, continent, sep = "__"))
      
      
      # Combine the occurrence and checklist data 
      continentCounts <- continentOccs %>%
        dplyr::select(!base::c("country_suggested", "n", "name_continent")) %>%
        dplyr::rename(n = sum) %>%
        dplyr::bind_rows(continentNoPoints) %>%
        # Group by country and then split into a list per group
        dplyr::group_by(continent) %>%
        dplyr::group_split() %>%
        # For each country, apply the literature distribution but with an n max of the empirical maximum
        # for that country
        base::lapply(., countrySampler) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct(scientificName, continent, .keep_all = TRUE) 
      
      # # Pivot wider 
      continentWider <- continentCounts %>%
        dplyr::select(scientificName, continent, n) %>%
        tidyr::drop_na() %>% 
        tidyr::pivot_wider(names_from = continent,
                           values_from = n,
                           values_fill = 0) %>%
        ## Create the rownames
        column_to_rownames_internal(var = "scientificName") %>%
        dplyr::tibble()
      
    } # END if continent
    
    
    
    #### 1.1.4 Analysis ####
    ##### a. Country ####
    if(tolower(scale) == "country"){
      ###### i. Chao ####
      suppressMessages(
      ChaoOut <- ChaoWrapper(data = countryChaoData_checklist,
                             k = 5,
                             datatype = "abundance",
                             conf = 0.95,
                             mc.cores = 1))
      
    } # END if country
    
    if(tolower(scale) == "country"){
      ###### ii. iNext ####
      # Run iNEXT for each country using the iNEXT wrapper
      suppressMessages(
      iNEXTout <- iNEXTwrapper(data = country_speciesChecklistCounts,
                               variableColumn = "rNaturalEarth_name",
                               valueColumn = "n",
                               datatype = "abundance",
                               mc.cores = 1))
    } # END if continent
    
    
    ##### b. Continent ####
    if(tolower(scale) == "continent"){
      ###### i. Chao ####
      suppressMessages(
      ChaoOut <- ChaoWrapper(data = continentWider,
                             k = 5,
                             datatype = "abundance",
                             conf = 0.95,
                             mc.cores = 1))
      
    } # END if country
    
    if(tolower(scale) == "continent"){
      ###### ii. iNext ####
      # Run iNEXT for each country using the iNEXT wrapper
      suppressMessages(
      iNEXTout <- iNEXTwrapper(data = continentCounts,
                               k = 5,
                               variableColumn = "continent",
                               datatype = "abundance",
                               conf = 0.95,
                               mc.cores = 1))
    } # END if continent
    
    
    ##### c. Global ####
    if(tolower(scale) == "global"){
      
      ###### i. iChao ####
      # Get the Chao values for global bee species diversity
      ChaoOut <- SpadeR::ChaoSpecies(data_totalCounts$n,
                                     datatype = "abundance", 
                                     k = 10, conf = 0.95)
      
      ##### ii. iNEXT ####
      global_iNEXT <- data_totalCounts %>%
        dplyr::select(scientificName, n) %>%   # Create the rownames
        column_to_rownames_internal(var = "scientificName") %>%
        dplyr::tibble()
      
      iNEXTout <- iNEXT::iNEXT(x = global_iNEXT$n, datatype = "abundance",
                               # 0 for species richness Shannon diversity (q = 1, the exponential of Shannon entropy) and Simpson diversity (q = 2, the inverse of Simpson concentration).
                               q = 0)
    } # END if global
    
    # Return a list of the objects produced
    return(dplyr::lst(ChaoOut, iNEXTout) %>%
             stats::setNames(c("ChaoOut", "iNEXTout")) %>%
             dplyr::lst() %>%
             # Set the name as the iteration number 
             stats::setNames(iterations))
  } # END richnessSampleR function
  
  
  ##### 1.3 countrySampler ####
  
  # Make a function to sample the country sample sizes
  countrySampler <- function(df = NULL,
                             iterations = NULL){ 
    dataFrom <- NULL
    # Set a random seed
    #set.seed(iterations*7)
    # Get the maximum value for literatureCurve$x_coords
    max_x <- base::max(literatureCurve$x_coords)
    # Test that there is sufficient length (i.e., at least one records with n > NA)
    lengthTest <- df$n[complete.cases(df$n)] %>% 
      length()
    # Get the maximum value for the country's sample size or set the minium to n = 1
    if(lengthTest > 0){
      max_n = base::max(df$n[complete.cases(df$n)])
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
                                       sample(x = literatureCurve$x_coords[1:maxSample],
                                              prob = literatureCurve$y_coords[1:maxSample], 
                                              replace = TRUE,
                                              size = inputLength)[dplyr::row_number()] %>%
                                         round(0),
                                       # Or keep n
                                       n),
                    dataFrom = dplyr::if_else(is.na(dataFrom), "checklist", dataFrom))
    return(df)
  }# END countrySampler
  
  
  # Use the richnessSampleR function to iteratively generate richness estimates
#### 2.0 Global ####
  if(globalSamples > 0){
  globalSampled <- parallel::mclapply(
    X = 1:globalSamples,
    FUN = richnessSampleR,
    # FUNCTION INPUTS:
    # Input datasets
    data = richnessData,
    # What scale?
    scale = "Global", # Country, Continent, or Global
    # Functions
    ChaoWrapper = ChaoWrapper,
    iNEXTwrapper = iNEXTwrapper,
    mc.cores = mc.cores
  )
  
  counter <- 1
  # Extract all of the Chao data
  ChaoGlobalSampled <- globalSampled %>%
    lapply(X = .,
           function(x){
             rowname <- V1 <- NULL
             counter <<- counter + 1
             extraction <- x[[1]]$ChaoOut$Species_table %>%
               base::as.data.frame() %>%
               rownames_to_column_int(var = "Name") %>% 
               dplyr::mutate(Name = stringr::str_squish(Name))
             # Extract the sample size
             sampleSize <- x[[1]]$ChaoOut$Basic_data_information %>% 
               rownames_to_column_int(var = "Name") %>% 
               dplyr::mutate(Name = stringr::str_squish(Name)) %>%
               base::t() %>% base::as.data.frame() %>% rownames_to_column_int() %>% 
               dplyr::tibble() %>%
               dplyr::select(rowname, V1) %>% dplyr::filter(!V1 %in% c("Sample size", "n")) %>%
               dplyr::rename(n = V1)
             # Add the sample size 
             extraction <- extraction %>% 
               dplyr::mutate(n = sampleSize$n)
           }) 
  # Combine these together
  global_iChaoSamples <- ChaoGlobalSampled %>%
    dplyr::bind_rows() %>%
    # Select the desired statistic estimate
    dplyr::filter(stringr::str_detect(Name, "iChao1 \\(Chiu et al\\. 2014\\)")) %>%
    dplyr::mutate(Name = "iChao") %>% 
    dplyr::mutate(variable = "Global", .before = 1)
  
  # Extract all of the iNEXT data
  counter <- 1
  iNEXTGlobalSampled <- globalSampled %>%
    lapply(X = .,
           function(x){
             counter <<- counter + 1
             extraction <- x[[1]]$iNEXTout$AsyEst %>%
               dplyr::mutate(n = x[[1]]$iNEXTout$DataInfo$n %>%
                               as.character())
           }) 
  
  # Combine these together
  global_iNextSamples <- iNEXTGlobalSampled %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(statistic = rownames(.)) %>% 
    # Select the desired statistic estimate
    dplyr::filter(stringr::str_detect(statistic, "Species Richness")) %>%
    dplyr::mutate(statistic = "iNEXT") %>% 
    # Rename to match with iChao
    dplyr::rename(Name = statistic,
                  Estimate = Estimator,
                  `95%Lower` = `95% Lower`,
                  `95%Upper` = `95% Upper`,
                  `s.e.` = `Est_s.e.`) %>%
    dplyr::mutate(variable = "Global", .before = 1)
  
  combined_global_ChaoiNext <- global_iNextSamples %>%
    dplyr::bind_rows(global_iChaoSamples) %>%
    dplyr::arrange( `95%Upper`) %>%
    # Add a column with group numbers, and then make them into groups of ten
    dplyr::mutate(groupNo = base::ceiling(dplyr::cur_group_id()/10)) %>%
    dplyr::group_by(groupNo)
  
  
  # Combine the data to calculate further statistics
  # Combine the iNEXT data
  globalcombined_iNEXT <- combined_global_ChaoiNext %>%
    dplyr::filter(Name == "iNEXT") %>%
    # Group by country
    dplyr::group_by(variable) %>%
    # Get the stats::medians of the sampled values
    dplyr::mutate(observedRichness = Observed,
                  niNEXT = stats::median(n %>% as.numeric(.)),
                  iNEXT_est = stats::median(Estimate),
                  iNEXT_lower = stats::median(`95%Lower`),
                  iNEXT_upper = stats::median(`95%Upper`),
                  iNEXT_increasePercent = ((iNEXT_est/observedRichness)-1)*100,
                  iNEXT_increase = iNEXT_est - observedRichness,
                  level = "Global") %>%
    dplyr::select(variable, observedRichness, level, niNEXT,
                  iNEXT_est, iNEXT_lower, iNEXT_upper, iNEXT_increasePercent, iNEXT_increase) %>%
    dplyr::distinct()
  
  # Combine the iChao data
  globalcombined_iChao <- combined_global_ChaoiNext %>%
    dplyr::filter(Name == "iChao") %>%
    # Group by country
    dplyr::group_by(variable) %>%
    # Get the stats::medians of the sampled values
    dplyr::mutate(nChao = stats::median(as.numeric(n)),
                  iChao_est = stats::median(Estimate),
                  iChao_lower = stats::median(`95%Lower`),
                  iChao_upper = stats::median(`95%Upper`)) %>%
    dplyr::select(variable, nChao, iChao_est, iChao_lower, iChao_upper) %>%
    dplyr::distinct() 
  
  # Combine them all
  combinedStatistics_sampled_global <- globalcombined_iChao %>%
    dplyr::left_join(globalcombined_iNEXT, by = "variable") %>% 
    # Calculate the iChao increases
    dplyr::mutate(iChao_increasePercent = ((iChao_est/observedRichness)-1)*100,
                  iChao_increase = iChao_est - observedRichness)
  
  } # END 2.0 Global
  
  
  
#### 3.0 Country ####
  if(countrySamples > 0){
  countrySampled <- parallel::mclapply(
    X = 1:countrySamples,
    FUN = richnessSampleR,
    # FUNCTION INPUTS:
    # Input datasets
    data = richnessData,
    # What scale?
    scale = "Country", # Country, Continent, or Global
    # Functions
    ChaoWrapper = ChaoWrapper,
    iNEXTwrapper = iNEXTwrapper,
    mc.cores = mc.cores
  )
  
  counter <- 1
  # Extract all of the Chao data
  ChaoCountrySampled <- countrySampled %>%
    lapply(X = .,
           function(x){
             V1 <- rowname<- NULL
             counter <<- counter + 1
             extraction <- x[[1]]$ChaoOut$richnessTable %>%
               dplyr::mutate(Name = stringr::str_squish(Name))
             # Extract the sample size
             sampleSize <- x[[1]]$ChaoOut$basicTable %>% 
               t() %>% base::as.data.frame() %>% 
               dplyr::mutate(rowname = rownames(.)) %>% 
               dplyr::tibble() %>%
               dplyr::select(rowname, V1) %>% dplyr::filter(!V1 %in% c("Sample size", "n")) %>%
               dplyr::rename(n = V1)
             # Add the sample size 
             extraction <- extraction %>% 
               dplyr::left_join(sampleSize, by = c("variable" = "rowname"))
           }) 
  # Combine these together
  country_iChaoSamples <- ChaoCountrySampled %>%
    dplyr::bind_rows() %>%
    # Select the desired statistic estimate
    dplyr::filter(stringr::str_detect(Name, "iChao1 \\(Chiu et al\\. 2014\\)")) %>%
    dplyr::mutate(Name = "iChao") %>% 
    # Group by continent
    dplyr::group_by(variable)
  
  # Extract all of the iNEXT data
  counter <- 1
  iNEXTCountrySampled <- countrySampled %>%
    lapply(X = .,
           function(x){
             counter <<- counter + 1
             extraction <- x[[1]]$iNEXTout$AsyEst 
             extraction <- extraction %>% 
               dplyr::left_join(x[[1]]$iNEXTout$DataInfo %>% 
                                  dplyr::select(Assemblage, n) %>%
                                  dplyr::mutate(n = as.character(n)),
                                by = c("rNaturalEarth_name" = "Assemblage"))
             
           }) 
  
  # Combine these together
  country_iNextSamples <- iNEXTCountrySampled %>%
    dplyr::bind_rows() %>%
    # Select the desired statistic estimate
    dplyr::filter(stringr::str_detect(statistic, "Species Richness")) %>%
    dplyr::mutate(statistic = "iNEXT") %>% 
    # Group by rNaturalEarth_name
    dplyr::group_by(rNaturalEarth_name) %>%
    # Rename to match with iChao
    dplyr::rename(variable = rNaturalEarth_name,
                  Name = statistic,
                  Estimate = Estimator,
                  `95%Lower` = `95% Lower`,
                  `95%Upper` = `95% Upper`,
                  `s.e.` = `Est_s.e.`)
  
  combined_site_ChaoiNext <- country_iNextSamples %>%
    dplyr::bind_rows(country_iChaoSamples) %>%
    # Remove low sample size or problematic countries
    dplyr::filter(!variable %in% countriesToExclude) %>% 
    dplyr::filter(!stringr::str_detect(variable, "Northern" # for Northern Cyprus
    )) %>%
    dplyr::arrange( `95%Upper`) %>%
    # Add a column with group numbers, and then make them into groups of ten
    dplyr::mutate(groupNo = base::ceiling(dplyr::cur_group_id()/10)) %>%
    dplyr::group_by(groupNo)
  
  suppressWarnings({
  # Make a plot of the iChao values and the confidence intervals
  (count_sampledPlot <- ggplot2::ggplot(data = combined_site_ChaoiNext) + 
      ggplot2::geom_violin(position="dodge", alpha=0.5,
                           ggplot2::aes(fill=Name, 
                                        y=`95%Lower`, x=variable), colour =  NA) +
      ggplot2::geom_violin(position="dodge", alpha=0.5,
                           ggplot2::aes(fill=Name,
                                        y=`95%Upper`, x=variable), colour =  NA) +
      ggplot2::geom_violin(position="dodge", alpha=1,
                           ggplot2::aes(fill=Name,
                                        y=Estimate, x=variable), colour =  "black") +
      ggplot2::scale_fill_manual(values=c("#55AD9B", "#FD9B63")) +
      ggplot2::theme_classic() + ggplot2::xlab("Country") + ggplot2::ylab("iChao") +
      ggplot2::facet_wrap( ggplot2::vars(variable), scales = "free", ncol = 5)+ 
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank()
      )
  )
  # Save the plot
  ggplot2::ggsave(file = paste0("country_", fileName), 
                  path = outPath,
                  plot = count_sampledPlot, device = "pdf",
                  width = 10, height = 50,
                  limitsize = FALSE)
  })
  
  # Combine the data to calculate further statistics
  # Combine the iNEXT data
  countrycombined_iNEXT <- combined_site_ChaoiNext %>%
    dplyr::filter(Name == "iNEXT") %>%
    # Group by country
    dplyr::group_by(variable) %>%
    # Get the stats::medians of the sampled values
    dplyr::mutate(observedRichness = Observed,
                  niNEXT = stats::median(n %>% as.numeric(.)),
                  iNEXT_est = stats::median(Estimate),
                  iNEXT_lower = stats::median(`95%Lower`),
                  iNEXT_upper = stats::median(`95%Upper`),
                  iNEXT_increasePercent = ((iNEXT_est/observedRichness)-1)*100,
                  iNEXT_increase = iNEXT_est - observedRichness,
                  level = "Country") %>%
    dplyr::select(variable, observedRichness, level, niNEXT,
                  iNEXT_est, iNEXT_lower, iNEXT_upper, iNEXT_increasePercent, iNEXT_increase) %>%
    dplyr::distinct()
  
  # Combine the iChao data
  countrycombined_iChao <- combined_site_ChaoiNext %>%
    dplyr::filter(Name == "iChao") %>%
    # Group by country
    dplyr::group_by(variable) %>%
    # Get the stats::medians of the sampled values
    dplyr::mutate(nChao = stats::median(as.numeric(n)),
                  iChao_est = stats::median(Estimate),
                  iChao_lower = stats::median(`95%Lower`),
                  iChao_upper = stats::median(`95%Upper`)) %>%
    dplyr::select(variable, nChao, iChao_est, iChao_lower, iChao_upper) %>%
    dplyr::distinct()
  
  # Combine them all
  combinedStatistics_sampled_country <- countrycombined_iChao %>%
    dplyr::left_join(countrycombined_iNEXT, by = "variable") %>% 
    # Calculate the iChao increases
    dplyr::mutate(iChao_increasePercent = ((iChao_est/observedRichness)-1)*100,
                  iChao_increase = iChao_est - observedRichness)
  
  
  } # END 3.0 Country
  
#### 4.0 Continent ####
  if(continentSamples > 0){
  # Iteratively sample the continent-level richness
  continentSampled <- parallel::mclapply(
    X = 1:continentSamples,
    FUN = richnessSampleR,
    # FUNCTION INPUTS:
    # Input datasets
    data = richnessData,
    # What scale?
    scale = "Continent", # Country, Continent, or Global
    # Functions
    ChaoWrapper = ChaoWrapper,
    iNEXTwrapper = iNEXTwrapper,
    mc.cores = mc.cores
  )
  
  counter <- 1
  # Extract all of the Chao data
  ChaoContinentSampled <- continentSampled %>%
    lapply(X = .,
           function(x){
             V1 <- rowname <- NULL
             counter <<- counter + 1
             extraction <- x[[1]]$ChaoOut$richnessTable
             # Extract the sample size
             sampleSize <- x[[1]]$ChaoOut$basicTable %>% 
               t() %>% base::as.data.frame() %>% 
               dplyr::mutate(rowname = rownames(.)) %>% 
               dplyr::tibble() %>%
               dplyr::select(rowname, V1) %>% dplyr::filter(!V1 %in% c("Sample size", "n")) %>%
               dplyr::rename(n = V1)
             # Add the sample size 
             extraction <- extraction %>% 
               dplyr::left_join(sampleSize, by = c("variable" = "rowname"))
           }) 
  # Combine these together
  continent_iChaoSamples <- ChaoContinentSampled %>%
    dplyr::bind_rows() %>%
    # Select the desired statistic estimate
    dplyr::filter(stringr::str_detect(Name, "iChao1 \\(Chiu et al\\. 2014\\)")) %>%
    dplyr::mutate(Name = "iChao") %>% 
    # Group by continent
    dplyr::group_by(variable)
  
  # Extract all of the iNEXT data
  counter <- 1
  iNEXTContinentSampled <- continentSampled %>%
    lapply(X = .,
           function(x){
             counter <<- counter + 1
             extraction <- x[[1]]$iNEXTout$AsyEst 
             extraction <- extraction %>% 
               dplyr::left_join(x[[1]]$iNEXTout$DataInfo %>% 
                                  dplyr::select(Assemblage, n) %>%
                                  dplyr::mutate(n = as.character(n)),
                                by = c("continent" = "Assemblage"))
             
           }) 
  
  # Combine these together
  continent_iNextSamples <- iNEXTContinentSampled %>%
    dplyr::bind_rows() %>%
    # Select the desired statistic estimate
    dplyr::filter(stringr::str_detect(statistic, "Species Richness")) %>%
    dplyr::mutate(statistic = "iNEXT") %>% 
    # Group by continent
    dplyr::group_by(continent) %>%
    # Rename to match with iChao
    dplyr::rename(variable = continent,
                  Name = statistic,
                  Estimate = Estimator,
                  `95%Lower` = `95% Lower`,
                  `95%Upper` = `95% Upper`,
                  `s.e.` = `Est_s.e.`)
  
  combined_cont_ChaoiNext <- continent_iChaoSamples %>%
    dplyr::bind_rows(continent_iNextSamples)
  
  
  # Combine the data to calculate further statistics
  # Combine the iNEXT data
  contCombined_iNEXT <- combined_cont_ChaoiNext %>%
    dplyr::filter(Name == "iNEXT") %>%
    # Group by country
    dplyr::group_by(variable) %>%
    # Get the stats::medians of the sampled values
    dplyr::mutate(observedRichness = Observed,
                  niNEXT = stats::median(n %>% as.numeric(.)),
                  iNEXT_est = stats::median(Estimate),
                  iNEXT_lower = stats::median(`95%Lower`),
                  iNEXT_upper = stats::median(`95%Upper`),
                  iNEXT_increasePercent = ((iNEXT_est/observedRichness)-1)*100,
                  iNEXT_increase = iNEXT_est - observedRichness,
                  level = "Continent") %>%
    dplyr::select(variable, observedRichness, level, niNEXT,
                  iNEXT_est, iNEXT_lower, iNEXT_upper, iNEXT_increasePercent, iNEXT_increase) %>%
    dplyr::distinct()
  
  # Combine the iChao data
  contCombined_iChao <- combined_cont_ChaoiNext %>%
    dplyr::filter(Name == "iChao") %>%
    # Group by country
    dplyr::group_by(variable) %>%
    # Get the stats::medians of the sampled values
    dplyr::mutate(nChao = stats::median(as.numeric(n)),
                  iChao_est = stats::median(Estimate),
                  iChao_lower = stats::median(`95%Lower`),
                  iChao_upper = stats::median(`95%Upper`)) %>%
    dplyr::select(variable, nChao, iChao_est, iChao_lower, iChao_upper) %>%
    dplyr::distinct()
  
  # Combine them all
  combinedStatistics_sampled_continent <- contCombined_iChao %>%
    dplyr::left_join(contCombined_iNEXT, by = "variable") %>% 
    # Calculate the iChao increases
    dplyr::mutate(iChao_increasePercent = ((iChao_est/observedRichness)-1)*100,
                  iChao_increase = iChao_est - observedRichness)
  
  suppressWarnings({
  # Make a plot of the iChao values and the confidence intervals
  (cont_sampledPlot <- ggplot2::ggplot(data = combined_cont_ChaoiNext) + 
      ggplot2::geom_violin(position="dodge", alpha=0.5,
                           ggplot2::aes(fill=Name, 
                                        y=`95%Lower`, x=variable)#,
                           #fill = "blue"
                           , colour =  NA) +
      ggplot2::geom_violin(position="dodge", alpha=0.5,
                           ggplot2::aes(fill=Name,
                                        y=`95%Upper`, x=variable)#,
                           #fill = "red"
                           , colour =  NA) +
      ggplot2::geom_violin(position="dodge", alpha=1,
                           ggplot2::aes(fill=Name,
                                        y=Estimate, x=variable),
                           #fill = "purple"
                           colour =  "black") +
      ggplot2::scale_fill_manual(values=c("#55AD9B", "#FD9B63")) +
      ggplot2::theme_classic() + ggplot2::xlab("Continent") + ggplot2::ylab("Species estimate") +
      ggplot2::guides(fill= ggplot2::guide_legend(title="Statistic"))) 
  # Save the plot
  ggplot2::ggsave(file = paste0( "continent_", fileName),
                  path = outPath,
                  plot = cont_sampledPlot, device = "pdf",
                  width = 7, height = 5)
})

} # END 4.0 Continent

  
#### 5.0 Combine data ####
    ###### 5.1 missing sheets ####
  if(!exists("combinedStatistics_sampled_country")){
    combinedStatistics_sampled_country <- dplyr::tibble(
      variable = NA_character_, nChao = NA_integer_, iChao_est = NA_integer_,
      iChao_lower = NA_integer_,iChao_upper = NA_integer_,          
      observedRichness = NA_integer_,level = NA_character_,niNEXT = NA_integer_,
      iNEXT_est = NA_integer_,iNEXT_lower = NA_integer_, iNEXT_upper = NA_integer_,
      iNEXT_increasePercent = NA_integer_,iNEXT_increase = NA_integer_,
      iChao_increasePercent = NA_integer_,iChao_increase  = NA_integer_) %>%
      tidyr::drop_na()
  }
  
  if(!exists("combinedStatistics_sampled_continent")){
    combinedStatistics_sampled_continent <- dplyr::tibble(
      variable = NA_character_, nChao = NA_integer_, iChao_est = NA_integer_,
      iChao_lower = NA_integer_,iChao_upper = NA_integer_,          
      observedRichness = NA_integer_,level = NA_character_,niNEXT = NA_integer_,
      iNEXT_est = NA_integer_,iNEXT_lower = NA_integer_, iNEXT_upper = NA_integer_,
      iNEXT_increasePercent = NA_integer_,iNEXT_increase = NA_integer_,
      iChao_increasePercent = NA_integer_,iChao_increase  = NA_integer_) %>%
      tidyr::drop_na()
  }
  if(!exists("combinedStatistics_sampled_global")){
    combinedStatistics_sampled_global <- dplyr::tibble(
      variable = NA_character_, nChao = NA_integer_, iChao_est = NA_integer_,
      iChao_lower = NA_integer_,iChao_upper = NA_integer_,          
      observedRichness = NA_integer_,level = NA_character_,niNEXT = NA_integer_,
      iNEXT_est = NA_integer_,iNEXT_lower = NA_integer_, iNEXT_upper = NA_integer_,
      iNEXT_increasePercent = NA_integer_,iNEXT_increase = NA_integer_,
      iChao_increasePercent = NA_integer_,iChao_increase  = NA_integer_) %>%
      tidyr::drop_na()
  }
  
  
    ##### 5.2 Combine all ####
  # Combine the iterative sampled data above
  combinedStatistics <- dplyr::bind_rows(combinedStatistics_sampled_country %>%
                                           dplyr::mutate(scale = "Country", .after = 1),
                                         combinedStatistics_sampled_continent %>%
                                           dplyr::mutate(scale = "Continent", .after = 1),
                                         combinedStatistics_sampled_global %>%
                                           dplyr::mutate(scale = "Global", .after = 1)
  ) %>%
    dplyr::rename(name = variable)
  
    # Combine all of the outputs into a list for return from the function
  output <- list(combinedStatistics, 
       if(exists("combined_site_ChaoiNext")){combined_site_ChaoiNext},
       if(exists("combined_cont_ChaoiNext")){combined_cont_ChaoiNext},
       if(exists("combined_global_ChaoiNext")){combined_global_ChaoiNext}) %>% 
    stats::setNames(c("Summary", "SiteOutput", "ContinentOutput", "GlobalOutput"))
  
  # Return the output 
  return(output)

} # End function




