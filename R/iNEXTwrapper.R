# This function was written by James B. Dorey starting on the 19th of April 2023 to wrap iNEXT
# and output table for multiple species/countries/variables at once

#' Parallel estimation of species richness in a community using iNEXT
#' 
#' A wrapper for `iNEXT::iNEXT()` to interpolate and extrapolate Hill numbers with order q 
#' (rarify species richness). The wrapper has the ability to estimate species richness for 
#' multiple sites (or countries) at once and to do this using multiple cores.
#'
#' @param data A data frame or tibble. A data frame containing "abundance"-type data per variable
#' (population, country, species...) in columns.
#' @param variableColumn Character. The column to be used to group the data. Probably "country"
#' or "site". Default =  "groupVariable".
#' @param valueColumn Character. The column containing the count data. Defualt =  "n".
#' @param q a number or vector specifying the richness order(s) of Hill numbers.
#' @param datatype data type of input data: individual-based abundance data 
#' (datatype = "abundance"), sampling-unit-based incidence frequencies data 
#' (datatype = "incidence_freq") or species by sampling-units incidence matrix 
#' (datatype = "incidence_raw").
#' @param size an integer vector of sample sizes (number of individuals or sampling units) for 
#' which richness estimates will be computed. If NULL, then richness estimates will be computed 
#' for those sample sizes determined by the specified/default endpoint and knots.
#' @param endpoint an integer specifying the sample size that is the endpoint for 
#' rarefaction/extrapolation. If NULL, then endpoint = double the reference sample size.
#' @param knots an integer specifying the number of equally-spaced knots (say K, default is 40) 
#' between size 1 and the endpoint; each knot represents a particular sample size for which 
#' richness estimate will be calculated. If the endpoint is smaller than the reference sample 
#' size, then iNEXT() computes only the rarefaction esimates for approximately K evenly spaced
#' knots. If the endpoint is larger than the reference sample size, then iNEXT() computes 
#' rarefaction estimates for approximately K/2 evenly spaced knots between sample size 1 and 
#' the reference sample size, and computes extrapolation estimates for approximately K/2 evenly 
#' spaced knots between the reference sample size and the endpoint.
#' @param se a logical variable to calculate the bootstrap standard error and conf confidence 
#' interval.
#' @param conf 	a positive number < 1 specifying the level of confidence interval; default is 0.95.
#' @param nboot an integer specifying the number of replications; default is 50.
#' @param mc.cores Numeric. If > 1, the function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory.
#' Default = 1.
#'
#' @return Returns a list containing two tibbles. The first is a tibble that concatenates the outputs
#' from the basic data and rare species information in columns per input variable (column). The second is a tibble
#' that concatenates the various species richness estimates, with input variables in chunks of rows.
#' Additionally a console output will list the variables (columns) that lacked sufficient data
#' to be analysed.
#' 
#' 
#' @importFrom dplyr %>%
#' 
#' @seealso [iNEXT::iNEXT()] 
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#'   # Read in some example data and use [BeeBDC::richnessPrepR()] to create the example input data
#'  #' data(beesCountrySubset)
#' 
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
#'   # In this function we can directly feed in estimateDataExample$site_speciesCounts 
#'  iNextOut <- iNEXTwrapper(data = estimateDataExample$site_speciesCounts,
#'                           variableColumn = "country_suggested",
#'                           valueColumn = "n",
#'                           q = 0,
#'                           datatype = "abundance",
#'                           conf = 0.95,
#'                           se = TRUE,
#'                           nboot = 50,
#'                           size = NULL,
#'                           endpoint = NULL,
#'                           knots = 40,
#'                           mc.cores = 1)
#' }

iNEXTwrapper <- function(data = NULL,
                         variableColumn = "groupVariable",
                         valueColumn = "n",
                         q = 0,
                         datatype = "abundance",
                         conf = 0.95,
                         se = TRUE,
                         nboot = 50,
                         size = NULL,
                         endpoint = NULL,
                         knots = 40,
                         mc.cores = 1){
  
  groupVariable <- groupCount <- . <- NULL
  
  # Load required packages 
  requireNamespace("stringr")
  requireNamespace("dplyr")
  requireNamespace("iNEXT")
  
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data I'm a program not a magician.")
  }
  
  
  ##### 0.2 iNEXT test ####
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
  
  
  ##### 0.3 Functions ####
  # Duplicate a function within ChaoSpecies to not run data-poor species
  #f <- function(i, data) {
  #  length(data[which(data == i)])
  #}
  
  # wrap the iNEXT function
  wrapper <- function(inputData_i = df_list[[1]]){
      ###### a. data prep ####
      # Format the data coming in
    inputData_i <- inputData_i[[1]] %>%
      dplyr::select(tidyselect::any_of(c("groupVariable", "groupCount"))) %>%  # Create the rownames
      dplyr::tibble()
    
     ###### b. iNEXT ####
    # Don't run species where there are no counts that are less than k
    # i.e. ONLY run species that won't throw an error due to weird/poor cases of data
    # if(any(inputData_i[[1]] > 0 & inputData_i[[1]] < 5) & 
    #    !(f(1, inputData_i[[1]]) == sum(inputData_i[[1]], na.rm = TRUE))
    # ){
      # Run the ChaoSpecies function
      spOutput <- iNEXT::iNEXT(
        x = inputData_i$groupCount,
        datatype = datatype,
        q = q,
        conf = conf,
        se = se,
        nboot = nboot,
        size = size,
        endpoint = endpoint,
        knots = knots)

        ###### c. DataInfo ####
        # Extract the dataInfo and then replace the Assemblage with the groupVariable value
      dataInfo_out <- spOutput$DataInfo %>%
        dplyr::mutate(Assemblage = inputData_i$groupVariable %>% unique())
      
      ###### d. AsyEst ####
        # Save AsyEst and replace the rownames with a column 
      AsyEst_out <- spOutput$AsyEst %>%
        dplyr::mutate(statistic = rownames(.), .before = 1) %>% 
        dplyr::tibble() %>%
          # Add the grouping variable to the start
        dplyr::mutate(groupVariable = inputData_i$groupVariable %>% unique(),
                      .before = 1)
        
      
      ###### e. iNextEst ####
    # Save the iNextEst as a list that's named per groupVariable
      iNextEst <- spOutput #%>%
        #dplyr::lst() %>%
        #stats::setNames(inputData_i$groupVariable %>% unique())
      
      
      # Return the data as a list
      return(list( dataInfo_out, AsyEst_out, iNextEst) %>%
               stats::setNames(c( "DataInfo", "AsyEst", "iNextEst")))
    # }
  }
  
  #### 1.0 Data prep ####
    ##### 1.1 Prep tibble ####
    # Prep the correct column
  # Temporarily rename the variableColumn to "groupVariable" within the function
  data <- data %>%
    dplyr::rename("groupVariable" = tidyselect::any_of(variableColumn))
  # Temporarily rename the variableColumn to "groupVariable" within the function
  data <- data %>%
    dplyr::rename("groupCount" = tidyselect::any_of(valueColumn))
  
    # Turn the data into a list by the country
  data_list <- data %>%
    tidyr::drop_na(groupVariable) %>% 
    dplyr::group_by(groupVariable) %>%
    dplyr::group_split()
    # Create an empty list to add to
  df_list <- dplyr::lst()
  variableNames <- c()
  
    ##### 1.2 Loop prep ####
  # Loop the data to make it into a list (it's a two-level list)
  for(i in 1:length(data_list)){
    # Extract the list and give it the country name
    loopList <- data_list[[i]] %>%
      dplyr::lst() %>%
      stats::setNames(data_list[[i]]$groupVariable[[1]])
    # Add to the variable list to the greater list
    df_list <- df_list %>%
      append( dplyr::lst(loopList))
    # Add the variable name to a running list to ensure name order is maintained
    variableNames <- c(variableNames, data_list[[i]]$groupVariable[[1]])
  }
  # Set the names for the list
  df_list <- df_list %>%
    stats::setNames(variableNames)
  
 
  
  
  #### 2.0 Run functions ####    
  # Run the function per species or level
  iNEXTOutput <- parallel::mclapply(
    X = df_list,
    FUN = wrapper,
    mc.cores = mc.cores
  ) 
  
  
  #### 3.0 Process outputs ####
  ##### 3.1 seperate F + P ####
  # Find the non-null variables and extract those
  non_empty_list_test <- !sapply(iNEXTOutput <- iNEXTOutput, is.null)
  # Save a list of variables that could not be analysed
  failures <- iNEXTOutput[!non_empty_list_test] %>%
    # Get the failure names 
    names()
  # Remove the failed list
  iNEXTOutput <- iNEXTOutput[non_empty_list_test]
  
  
  ##### 3.2 dataInfo outputs ####
  # Now, combine each level of the list across variables
  # Extract the richness table stuff
  dataInfoOut <- lapply(iNEXTOutput, function(x) x[["DataInfo"]]) %>%
    # Bind together with the original two columns
    dplyr::bind_rows()
  
  
  ##### 3.3 AsyEst outputs ####
  # Row bind the richness statistics into a single table
  iNEXTOutputOut <- lapply(iNEXTOutput, function(x) x[["AsyEst"]]) %>%
    dplyr::bind_rows()
  # Return the variableColumn name to its original state
  names(iNEXTOutputOut)[names(iNEXTOutputOut) == "groupVariable"] <- variableColumn
  
  ##### 3.4 iNEXT outputs ####
    # Combine the iNextEsts ino a single list
  iNextEstOut <- iNEXTOutput %>%
    lapply(., function(x) x) %>%
    dplyr::lst() %>%
    stats::setNames("iNextEst")
  
  
  ##### 3.5 Combine ####
  output <- dplyr::lst(dataInfoOut, iNEXTOutputOut, iNextEstOut) %>% 
    stats::setNames(c("DataInfo", "AsyEst", "iNextEst"))
  
  #### 4.0 User output ####
  # provide some user output
  if(length(failures > 0)){
    writeLines(paste0(
      " - We could not examine the following variables (because of insufficent data or sample size): ",
      paste(failures, collapse = ", ")
    ))}
  
  message(paste0(" - Outputs can be found in a list with two tibbles called 'DataInfo' and",
                    " 'AsyEst' and a list of iNext outputs per groupVariable in iNextEst'."))

  
  # Return the output
  return(output)
  
} # END function

  

  
  