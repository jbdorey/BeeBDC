# This function was written by James B Dorey on the 5th of October 2022
# Its purpose is to create a list of directories for beeData cleaning
# Please contact jbdorey[at]me.com for help


#' Set up global directory paths and create folders
#' 
#' This function sets up a directory for saving outputs (i.e. data, figures) generated through the 
#' use of the BeeBDC package, if the required folders do not already exist.
#'
#' @param RootPath A character String. The `RootPath` is the base path for your project, and all 
#' other paths should ideally be located within the `RootPath`. However, users may specify paths not 
#' contained in the RootPath
#' @param ScriptPath A character String. The `ScriptPath` is the path to any additional functions 
#' that you would like to read in for use with BeeBDC.
#' @param DataPath A character string. The path to the folder containing bee occurrence data 
#' to be flagged and/or cleaned
#' @param DataSubPath A character String. If a `DataPath` is not provided, this will be used as the `DataPath`
#' folder name within the `RootPath.` Default is "/Data_acquisition_workflow"
#' @param DiscLifePath A character String. The path to the folder which contains data from Ascher 
#' and Pcikering's Discover Life website.
#' @param OutPath A character String. The path to the folder where output data will be saved.
#' @param OutPathName A character String. The name of the `OutPath` subfolder located within the 
#' `RootPath.` Default is "Output".
#' @param RDoc A character String. The path to the current script or report, relative to the project 
#' root. Passing an absolute path raises an error. This argument is used by [here::i_am()] and incorrectly
#' setting this may result in `bdc` figures being saved to your computer's root directory
#' @param Report Logical. If TRUE, function creates a "Report" folder within the OutPath-defined 
#' folder. Default = TRUE.
#' @param Check Logical. If TRUE, function creates a "Check" folder within the OutPath-defined 
#' folder. Default = TRUE.
#' @param Figures Logical. If TRUE, function creates a "Figures" folder within the OutPath-defined 
#' folder. Default = TRUE.
#' @param Intermediate Logical. If TRUE, function creates a "Intermediate" folder within the 
#' OutPath-defined folder in which to save intermediate datasets. Default = TRUE.
#' 
#'
#' @return Results in the generation of a list containing the BeeBDC-required directories in your global
#'  environment. This function should be run at the start of each session. Additionally, this 
#'  function will create the BeeBDC-required folders if they do not already exist in the supplied 
#'  directory
#'
#' @importFrom here i_am here
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard/basic usage:
#' RootPath <- tempdir()
#' dirMaker(
  #' RootPath = RootPath,
  #' # Input the location of the workflow script RELATIVE to the RootPath
  #' RDoc = "BDC_repo/BeeCleaning_SciData.R") %>%
  #'   # Add paths created by this function to the .GlobalEnv
  #'   list2env(envir = .GlobalEnv)  
  #' # Set the working directory
  #' setwd(DataPath)
#'
#' # Custom OutPathName provided
#'   dirMaker(
#'  RootPath = RootPath,
#'  # Set some custom OutPath info
#'  OutPath = NULL,
#'  OutPathName = "T2T_Output",
#'  # Input the location of the workflow script RELATIVE to the RootPath
#'  RDoc = "BDC_repo/BeeCleaning_SciData.R") %>%
#'    # Add paths created by this function to the .GlobalEnv
#'    list2env(envir = .GlobalEnv)  
#'  # Set the working directory
#'  setwd(DataPath)
#'
#' # Further customisations are also possible
#' dirMaker(
#'   RootPath = RootPath,
#'   ScriptPath = "...path/Bee_SDM_paper/BDC_repo/BeeBDC/R",
#'   DiscLifePath = "...path/BDC_repo/DiscoverLife_Data",
#'   OutPathName = "AsianPerspective_Output",
#'   # Input the location of the workflow script RELATIVE to the RootPath
#'   RDoc = "AsianPerspecitve_workflow.R") %>%
#'   # Add paths created by this function to the .GlobalEnv
#'   list2env(envir = .GlobalEnv)  
#' # Set the working directory
#' setwd(DataPath)
#' }
#'


dirMaker <- function(
    RootPath = RootPath,
    ScriptPath = NULL,
    DataPath = NULL,
    DataSubPath = "/Data_acquisition_workflow",
    DiscLifePath = NULL,
    OutPath = NULL,
    OutPathName = "Output",
    Report = TRUE,
    Check = TRUE,
    Figures = TRUE,
    Intermediate = TRUE,
    RDoc = NULL
){
  # Package names
  packages <- c("dplyr", "here")
  
  # Install packages not yet installed
  installed_packages <-  rownames(utils::installed.packages(packages))
  if (any(installed_packages == FALSE)) {
    utils::install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(RootPath)){
    stop(paste0(" - No RootPath was given. Please specifcy the root drectory that you want to use ",
                "for your data-cleaning adventures. I'll do the rest."))
  }
  if(is.null(RDoc)){
    stop(paste0(" - Please provide a path for RDoc. This path MUST be relative to the RootPath. ",
                "Hence, if the RootPath is '/user/home/', and the path to the RDoc is ", 
                "'/user/home/beeData/cleaningWorkflow.R', then RDoc == 'beeData/cleaningWorkflow.R"))
  }

    # Set the working directory
  setwd(RootPath)
  
  #### 1.0 Set paths ####
    ##### 1.1 ScriptPath ####
  # Create the ScriptPath if it does not already exist
  if(is.null(ScriptPath)){
  if (!dir.exists(paste0(RootPath, "/BDC_repo/BeeBDC/R"))) {
    dir.create(paste0(RootPath, "/BDC_repo/BeeBDC/R"), recursive = TRUE)
    warning(paste0(" - We created the ", 
                   paste0(RootPath, "/BDC_repo/BeeBDC/R"),
                   "file. This file needs to have the NewFunctions added to it otherise things won't",
                   " work. These can be added from our GitHub"))
  }
  # Choose the location of the script
  ScriptPath <- paste(RootPath,
                      "/BDC_repo/BeeBDC/R", sep = "")
  }else{# If user provides an alternate ScriptRoot path
    if(ScriptPath != FALSE){
    if (!dir.exists(ScriptPath)) {
      dir.create(ScriptPath, recursive = TRUE)
      warning(paste0(" - We created the ", 
                     ScriptPath,
                     "file. This file needs to have the NewFunctions added to it otherise things won't",
                     " work. These can be added from our GitHub"))
    }
  # Choose the location of the script
  ScriptPath <- ScriptPath}
    }
  
    ##### 1.2 DataPath ####
    # Create the DataPath if it does not already exist
  if(is.null(DataPath)){
  if (!dir.exists(paste0(RootPath, DataSubPath))) {
    dir.create(paste0(RootPath, DataSubPath), recursive = TRUE)
    # User warning
    warning(paste0(" - We created the ", 
                   paste0(RootPath, DataSubPath),
                   "file. This file needs to have the occurrence data that you want to use ",
                   "added to it otherise things won't",
                   " work. Please choose this data or download it from the supp. materials of our paper"))
  }
  # Choose the location of your data
  DataPath <- paste(RootPath, DataSubPath, sep = "")
  }else{
    if(DataPath != FALSE){
    if (!dir.exists(DataPath)) {
      dir.create(DataPath, recursive = TRUE)
      # User warning
      warning(paste0(" - We created the ", 
                     DataPath,
                     "file. This file needs to have the occurrence data that you want to use ",
                     "added to it otherise things won't",
                     " work. Please choose this data or download it from the supp. materials of our paper"))
    }
      # Choose the location of the DataPath
      DataPath <- DataPath}
  }
  
    ##### 1.3 DiscLifePath ####
  # Create the DiscLifePath if it does not already exist
  if(is.null(DiscLifePath) ){
  if (!dir.exists(paste0(RootPath, "/BDC_repo/DiscoverLife_Data"))) {
    dir.create(paste0(RootPath, "/BDC_repo/DiscoverLife_Data"), recursive = TRUE)
    {
      dir.create(paste0(RootPath, "/BDC_repo/DiscoverLife_Data"), recursive = TRUE)
        # User warning
      warning(paste0(" - We created the ", 
                     paste0(RootPath, "/BDC_repo/DiscoverLife_Data"),
                     "file. This file needs to have the DiscoverLife_Data added to it otherise things won't",
                     " work. These can be added from our GitHub"))
    }
  }
  # Choose the taxonomy path
  DiscLifePath <- paste(RootPath,
                        "/BDC_repo/DiscoverLife_Data", sep = "")
  }else{
    if(DiscLifePath != FALSE){
    if (!dir.exists(DiscLifePath)) {
      dir.create(DiscLifePath, recursive = TRUE)
      {
        dir.create(DiscLifePath, recursive = TRUE)
        # User warning
        warning(paste0(" - We created the ", 
                       DiscLifePath,
                       "file. This file needs to have the DiscoverLife_Data added to it otherise things won't",
                       " work. These can be added from our GitHub"))
      }
    }
      # Choose the location of the DiscLifePath
      DiscLifePath <- DiscLifePath}
  }
  
  
  ##### 1.4 OutPath ####
  # Create the OutPath if it does not already exist
  if(is.null(OutPath)){
    if (!dir.exists(paste0(DataPath, "/", OutPathName))) {
      dir.create(paste0(DataPath,  "/", OutPathName), recursive = TRUE)
      {
        dir.create(paste0(DataPath, "/", OutPathName), recursive = TRUE)
        # User warning
        warning(paste0(" - We created the ", 
                       paste0(DataPath,  "/", OutPathName),
                       "file."))
      }
    }
    # Choose the taxonomy path
    OutPath <- paste(DataPath,  "/", OutPathName, sep = "")
  }else{
    if(OutPath != FALSE){
    if (!dir.exists(OutPath)) {
      dir.create(OutPath, recursive = TRUE)
      {
        dir.create(OutPath, recursive = TRUE)
        # User warning
        warning(paste0(" - We created the ", 
                       OutPath,
                       "file."))
    }}
      # Choose the location of the OutPath
      OutPath <- OutPath}
  }
  
  #### 2.0 Create paths ####
      # If these bdc folders do not exist in the chosen directory, create them
  if (!dir.exists(paste0(OutPath, "/Check")) & Check != FALSE) {
    dir.create(paste0(OutPath, "/Check"), recursive = TRUE)
  }
  if (!dir.exists(paste0(OutPath, "/Figures")) & Figures != FALSE) {
    dir.create(paste0(OutPath, "/Figures"), recursive = TRUE)
  }
  if (!dir.exists(paste0(OutPath, "/Intermediate")) & Intermediate != FALSE) {
    dir.create(paste0(OutPath, "/Intermediate"), recursive = TRUE)
  }
  if (!dir.exists(paste0(OutPath, "/Report")) & Report != FALSE) {
    dir.create(paste0(OutPath, "/Report"), recursive = TRUE)
  }
  
    ##### 2.1 Make sub-paths ####
  if(Check != FALSE){
  OutPath_Check <- paste(OutPath, "/Check", sep = "")}
  if(Figures != FALSE){
  OutPath_Figures <- paste(OutPath, "/Figures", sep = "")}
  if(Intermediate != FALSE){
  OutPath_Intermediate <- paste(OutPath, "/Intermediate", sep = "")}
  if(Report != FALSE){
  OutPath_Report <- paste(OutPath, "/Report", sep = "")}
  
  #### 3.0 Set here::here ####
    # here::here needs to know where to find all output files. This is set here.
  here::i_am(RDoc)
  
  #### 4.0 Output ####
  return(list(ScriptPath, DataPath, DiscLifePath, OutPath,
              OutPath_Check, OutPath_Figures, OutPath_Intermediate, OutPath_Report) %>%
           stats::setNames(c("ScriptPath", "DataPath", "DiscLifePath", "OutPath",
                      "OutPath_Check", "OutPath_Figures", "OutPath_Intermediate",
                      "OutPath_Report")))
} # END function