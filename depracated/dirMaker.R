# This function was written by James B Dorey on the 5th of October 2022
# Its purpose is to create a list of directories for beeData cleaning
# Please contact jbdorey@me.com for help


#' Set up global directory paths and create folders
#' 
#' If the directory paths do not exist they will be created for you.
#'
#' @param RootPath character string. The `RootPath` is the path at the root of your project and in 
#' which all other paths should ideally
#' be located. However, users may specify paths not contained in the `Rootpath.`
#' @param ScriptPath character string. The `ScriptPath` is the path to any additional functions that 
#' you would like to read in.
#' @param DataPath character string. The path to the folder that contains occurrence data.
#' @param DataSubPath character string. If a `DataPath` is not provided, this will be used as the `DataPath`
#' folder name within the `RootPath.` Default is "/Data_acquisition_workflow"
#' @param DiscLifePath character string. The path to the folder that contains data from Ascher 
#' and Pcikering's Discover Life website.
#' @param OutPath character string. The path to the folder where data will be saved.
#' @param OutPathName character string. The name of the `OutPath` subfolder within the `RootPath.` Default is
#' "Output".
#' @param RDoc character string. The path to the current script or report, relative to the project 
#' root. Passing an absolute path raises an error. This argument is used by [here::i_am()] and incorrectly
#' setting this may result in bdc:: figures being saved to your computer's root directory
#' 
#'
#' @return A list containing the required directories in your global environment. This function should
#' be run at the start of each session. This function will also create the requested folders, 
#' if they do not exist.
#'
#' @importFrom fs dir_exists dir_create
#' @importFrom here i_am here
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Standard/basic usage:
#' RootPath <- "/pathToDirectory"
#' dirMaker(
  #' RootPath = RootPath,
  #' # Input the location of the workflow script RELATIVE to the RootPath
  #' RDoc = "BDC_repo/BeeCleaning_SciData.R") %>%
  #'   # Add paths created by this function to the .GlobalEnv
  #'   list2env(envir = .GlobalEnv)  
  #' # Set the working directory
  #' setwd(DataPath)
#'
#' Custom OutPathName provided
#' RootPath <- "/Users/jamesdorey/Desktop/Uni/My_papers/Bee_SDM_paper"
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
#' Further customisations are also possible
#' RootPath <- "/Users/jamesdorey/Desktop/Uni/My_papers/Bee_SDM_paper"
#' dirMaker(
#'   RootPath = RootPath,
#'   ScriptPath = "...path/Bee_SDM_paper/BDC_repo/NewFunctions",
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
    RDoc = NULL
){
  # Package names
  packages <- c("magrittr", "fs", "here")
  
  # Install packages not yet installed
  installed_packages <-  rownames(installed.packages(packages))
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(RootPath)){
    stop(paste0(" — No RootPath was given. Please specifcy the root drectory that you want to use ",
                "for your data-cleaning adventures. I'll do the rest."))
  }
  if(is.null(RDoc)){
    stop(paste0(" — Please provide a path for RDoc. This path MUST be relative to the RootPath. ",
                "Hence, if the RootPath is '/user/home/', and the path to the RDoc is ", 
                "'/user/home/beeData/cleaningWorkflow.R', then RDoc == 'beeData/cleaningWorkflow.R"))
  }

    # Set the working directory
  setwd(RootPath)
  
  #### 1.0 Set paths ####
    ##### 1.1 ScriptPath ####
  # Create the ScriptPath if it does not already exist
  if(is.null(ScriptPath)){
  if (!fs::dir_exists(paste0(RootPath, "/BDC_repo/NewFunctions"))) {
    fs::dir_create(paste0(RootPath, "/BDC_repo/NewFunctions"), recurse = TRUE)
    warning(paste0(" — We created the ", 
                   paste0(RootPath, "/BDC_repo/NewFunctions"),
                   "file. This file needs to have the NewFunctions added to it otherise things won't",
                   " work. These can be added from our GitHub"))
  }
  # Choose the location of the script
  ScriptPath <- paste(RootPath,
                      "/BDC_repo/NewFunctions", sep = "")
  }else{# If user provides an alternate ScriptRoot path
    if (!fs::dir_exists(ScriptPath)) {
      fs::dir_create(ScriptPath, recurse = TRUE)
      warning(paste0(" — We created the ", 
                     ScriptPath,
                     "file. This file needs to have the NewFunctions added to it otherise things won't",
                     " work. These can be added from our GitHub"))
    }
  # Choose the location of the script
  ScriptPath <- ScriptPath
    }
  
    ##### 1.2 DataPath ####
    # Create the DataPath if it does not already exist
  if(is.null(DataPath)){
  if (!fs::dir_exists(paste0(RootPath, DataSubPath))) {
    fs::dir_create(paste0(RootPath, DataSubPath), recurse = TRUE)
    # User warning
    warning(paste0(" — We created the ", 
                   paste0(RootPath, DataSubPath),
                   "file. This file needs to have the occurrence data that you want to use ",
                   "added to it otherise things won't",
                   " work. Please choose this data or download it from the supp. materials of our paper"))
  }
  # Choose the location of your data
  DataPath <- paste(RootPath, DataSubPath, sep = "")
  }else{
    if (!fs::dir_exists(DataPath)) {
      fs::dir_create(DataPath, recurse = TRUE)
      # User warning
      warning(paste0(" — We created the ", 
                     DataPath,
                     "file. This file needs to have the occurrence data that you want to use ",
                     "added to it otherise things won't",
                     " work. Please choose this data or download it from the supp. materials of our paper"))
    }
    # Choose the location of your data
    DataPath <- DataPath
  }
  
    ##### 1.3 DiscLifePath ####
  # Create the DiscLifePath if it does not already exist
  if(is.null(DiscLifePath)){
  if (!fs::dir_exists(paste0(RootPath, "/BDC_repo/DiscoverLife_Data"))) {
    fs::dir_create(paste0(RootPath, "/BDC_repo/DiscoverLife_Data"), recurse = TRUE)
    {
      fs::dir_create(paste0(RootPath, "/BDC_repo/DiscoverLife_Data"), recurse = TRUE)
        # User warning
      warning(paste0(" — We created the ", 
                     paste0(RootPath, "/BDC_repo/DiscoverLife_Data"),
                     "file. This file needs to have the DiscoverLife_Data added to it otherise things won't",
                     " work. These can be added from our GitHub"))
    }
  }
  # Choose the taxonomy path
  DiscLifePath <- paste(RootPath,
                        "/BDC_repo/DiscoverLife_Data", sep = "")
  }else{
    if (!fs::dir_exists(DiscLifePath)) {
      fs::dir_create(DiscLifePath, recurse = TRUE)
      {
        fs::dir_create(DiscLifePath, recurse = TRUE)
        # User warning
        warning(paste0(" — We created the ", 
                       DiscLifePath,
                       "file. This file needs to have the DiscoverLife_Data added to it otherise things won't",
                       " work. These can be added from our GitHub"))
      }
    }
    # Choose the taxonomy path
    DiscLifePath <- DiscLifePath
  }
  
  
  ##### 1.4 OutPath ####
  # Create the OutPath if it does not already exist
  if(is.null(OutPath)){
    if (!fs::dir_exists(paste0(DataPath, "/", OutPathName))) {
      fs::dir_create(paste0(DataPath,  "/", OutPathName), recurse = TRUE)
      {
        fs::dir_create(paste0(DataPath, "/", OutPathName), recurse = TRUE)
        # User warning
        warning(paste0(" — We created the ", 
                       paste0(DataPath,  "/", OutPathName),
                       "file."))
      }
    }
    # Choose the taxonomy path
    OutPath <- paste(DataPath,  "/", OutPathName, sep = "")
  }else{
    if (!fs::dir_exists(OutPath)) {
      fs::dir_create(OutPath, recurse = TRUE)
      {
        fs::dir_create(OutPath, recurse = TRUE)
        # User warning
        warning(paste0(" — We created the ", 
                       OutPath,
                       "file."))
      }
    }
    # Choose the OutPath path
    OutPath <- OutPath
  }
  
  #### 2.0 Create paths ####
      # If these bdc folders do not exist in the chosen directory, create them
  if (!fs::dir_exists(paste0(OutPath, "/Check"))) {
    fs::dir_create(paste0(OutPath, "/Check"), recurse = TRUE)
  }
  if (!fs::dir_exists(paste0(OutPath, "/Figures"))) {
    fs::dir_create(paste0(OutPath, "/Figures"), recurse = TRUE)
  }
  if (!fs::dir_exists(paste0(OutPath, "/Intermediate"))) {
    fs::dir_create(paste0(OutPath, "/Intermediate"), recurse = TRUE)
  }
  if (!fs::dir_exists(paste0(OutPath, "/Report"))) {
    fs::dir_create(paste0(OutPath, "/Report"), recurse = TRUE)
  }
  
    ##### 2.1 Make sub-paths ####
  OutPath_Check <- paste(OutPath, "/Check", sep = "")
  OutPath_Figures <- paste(OutPath, "/Figures", sep = "")
  OutPath_Intermediate <- paste(OutPath, "/Intermediate", sep = "")
  OutPath_Report <- paste(OutPath, "/Report", sep = "")
  
  #### 3.0 Set here::here ####
    # here::here needs to know where to find all output files. This is set here.
  here::i_am(RDoc)
  
  #### 4.0 Output ####
  return(list(ScriptPath, DataPath, DiscLifePath, OutPath,
              OutPath_Check, OutPath_Figures, OutPath_Intermediate, OutPath_Report) %>%
           setNames(c("ScriptPath", "DataPath", "DiscLifePath", "OutPath",
                      "OutPath_Check", "OutPath_Figures", "OutPath_Intermediate",
                      "OutPath_Report")))
} # END function
