# This function was written by James B. Dorey starting on the 14th of March 2023 to wrap ChaoSpecies
# and output table for multiple species/countries/variables at once


#' Parallel estimation of species richness in a community using iChao
#' 
#' ChaoSpecies: Estimation of species richness in a single community based on five types of data: 
#' Type (1) abundance data (datatype="abundance"), Type (1A) abundance-frequency counts 
#' (datatype="abundance_freq_count"), Type (2) incidence-frequency data 
#' (datatype = "incidence_freq"), Type (2A) incidence-frequency counts 
#' (datatype="incidence_freq_count"), and Type (2B) incidence-raw data (datatype="incidence_raw"); 
#' see SpadeR-package details for data input formats.
#'
#' @param data A data frame or tibble. A data frame containing "abundance"-type data per variable
#' (population, country, species...) in columns.
#' @param datatype Character. The type of input data, "abundance", "abundance_freq_count", 
#' "incidence_freq", "incidence_freq_count" or "incidence_raw". So far only tested with "abundance"
#' data. Default = "abundace".
#' @param k Numeric. the cut-off point (default = 10), which separates species into "abundant" and "rare" 
#' groups for abundance data for the estimator ACE; it separates species into "frequent" and 
#' "infrequent" groups for incidence data for the estimator ICE.
#' Default = 10.
#' @param conf Numeric. A positive number equal to or less than 1 specifying the level of 
#' confidence interval. Default = 0.95.
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
#' @seealso [SpadeR::ChaoSpecies()] 
#' 
#' @export
#'
#' @examples
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
#'   # Transform the data for input
#' inputTestData <- estimateDataExample$site_speciesCounts %>%
#'   dplyr::select(scientificName, country_suggested, n) %>%
#'   tidyr::pivot_wider(names_from = country_suggested,
#'                      values_from = n,
#'                      values_fill = 0) %>%
#'   # Create the rownames
#' tibble::column_to_rownames("scientificName") %>%
#'   dplyr::tibble()
#' 
#' iChaoOut <- ChaoWrapper(
#' data = inputTestData,
#' datatype = "abundance",
#' k = 10,
#' conf = 0.95,
#' mc.cores = 1)
#' }


ChaoWrapper <- function(
    data = NULL,
    datatype = "abundance",
    k = 10,
    conf = 0.95,
    mc.cores = 1){
  # locally bind variables to the function
  i <- . <- inputData_i <- spOutput <- richnessOutput <- richnessTable <- basicOutput <-
    df_list <- loopVector <- wrapper <- basicCols <- variable <- rowname <- non_empty_list_test <-
    richnessOut <- basicOut <- failures <- output <- NULL
  
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
  
  
      ##### 0.2 Functions ####
  # Duplicate a function within ChaoSpecies to not run data-poor species
  f <- function(i, data) {
    length(data[which(data == i)])
  }
  
    # wrap the ChaoSpecies function in a wrapper to be passed to mclapply
  wrapper <- function(inputData_i = NULL){
    # Don't run species where there are no counts that are less than k
    # i.e. ONLY run species that won't throw an error due to weird/poor cases of data
    if(any(inputData_i[[1]] > 0 & inputData_i[[1]] < 5) & 
       !(f(1, inputData_i[[1]]) == sum(inputData_i[[1]], na.rm = TRUE))
    ){
      # Run the ChaoSpecies function
      spOutput <- SpadeR::ChaoSpecies(
        data = inputData_i[[1]][complete.cases(inputData_i[[1]])],
        datatype = datatype,
        k = k, 
        conf = conf)
        ###### a. richnessOutput ####
      # Get the richness measures and re-format
      richnessOutput <- spOutput$Species_table %>% as.data.frame() %>% 
        dplyr::mutate(rowname = rownames(.), .before = "Estimate") 
      # Add these data to one table
      richnessTable <- dplyr::tibble(
        variable = names(inputData_i),
        Name = c(richnessOutput$rowname),
        Estimate = c(richnessOutput$Estimate),
        's.e.' = c(richnessOutput$s.e.),
        '95%Lower' = c(richnessOutput$`95%Lower`),
        '95%Upper' = c(richnessOutput$`95%Upper`))
      
      
        ##### b. basicOutput ####
      # Get Basic information into one table
      basicOutput <- spOutput$Basic_data_information %>% as.data.frame() %>% 
          # Remove hanging white spaces at the start of the rowname
        dplyr::mutate(rowname = rownames(.) %>% 
                        gsub(pattern = "^\\s+", replacement = ""),
                      .before = "Variable") %>% 
        dplyr::tibble()
      
      # Set up the data
      basicTable <- basicOutput
      # rename Value to the species name
      names(basicTable) <- c("rowname", "variable", names(inputData_i))
      # Return the tables as a list
      return(list( basicTable, richnessTable) %>%
               setNames(c( "basicTable", "richnessTable")))
    }
  }
  
#### 1.0 Data prep ####
  # Loop the data to make it into a list (it's a two-level list) because mclapply will strip one layer
df_list <- dplyr::lst()
for(i in 1:ncol(data)){
# Extract vector
      loopVector <- dplyr::pull(data, colnames(data)[i]) %>%
        dplyr::lst() %>%
        stats::setNames(., colnames(data)[i])
      # Add to the list
      df_list <- df_list %>%
      append( dplyr::lst(loopVector))
}
  # Set the names for the list
df_list <- df_list %>%
  stats::setNames(colnames(data))

  
#### 2.0 Run functions ####    
# Run the function per species or variable
richnessOutput <- parallel::mclapply(
  X = df_list,
  FUN = wrapper,
  mc.cores = mc.cores
) 


# Save the richness table's first two columns
basicCols <- richnessOutput[[1]]$basicTable %>%
  dplyr::select(rowname, variable)


#### 3.0 Process outputs ####
  ##### 3.1 Separate F + P ####
  # Find the non-null variables and extract those
non_empty_list_test <- !sapply(richnessOutput <- richnessOutput, is.null)
  # Save a list of variables that could not be analysed
failures <- richnessOutput[!non_empty_list_test] %>%
    # Get the failure names 
  names()
  # Remove the failed list
richnessOutput <- richnessOutput[non_empty_list_test]


  ##### 3.2 Basic outputs ####
  # Now, combine each level of the list across variables
  # Extract the richness table stuff
basicOut <- lapply(richnessOutput, function(x) x[["basicTable"]]) %>%
  # Remove the duplicate columns and only keept the output values
  lapply(., function(x) dplyr::select(x, !c("rowname", "variable"))) %>%
    # Bind together with the original two columns
  dplyr::bind_cols(basicCols, .)
  

  ##### 3.3 richness outputs ####
  # Row bind the richness statistics into a single table
richnessOut <- lapply(richnessOutput, function(x) x[["richnessTable"]]) %>%
  dplyr::bind_rows()


  ##### 3.4 Combine ####
output <- dplyr::lst(basicOut, richnessOut) %>% 
  setNames(c("basicTable", "richnessTable"))
  
#### 4.0 User output ####
  # provide some user output on the failures
if(length(failures > 0)){
writeLines(paste0(
  " - We could not examine the following variables (because of insufficent data or sample size): ",
  paste(failures, collapse = ", ")
))}
  # provide user output about the file structure
message(paste0(" - Outputs can be found in a list with two tibbles called 'basicTable' and",
                  " 'richnessTable'."))

  # Return the output
return(output)

} # END ChaoWrapper JBD
