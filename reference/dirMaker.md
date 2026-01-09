# Set up global directory paths and create folders

This function sets up a directory for saving outputs (i.e. data,
figures) generated through the use of the BeeBDC package, if the
required folders do not already exist.

## Usage

``` r
dirMaker(
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
  RDoc = NULL,
  useHere = TRUE
)
```

## Arguments

- RootPath:

  A character String. The `RootPath` is the base path for your project,
  and all other paths should ideally be located within the `RootPath`.
  However, users may specify paths not contained in the RootPath

- ScriptPath:

  A character String. The `ScriptPath` is the path to any additional
  functions that you would like to read in for use with BeeBDC.

- DataPath:

  A character string. The path to the folder containing bee occurrence
  data to be flagged and/or cleaned

- DataSubPath:

  A character String. If a `DataPath` is not provided, this will be used
  as the `DataPath` folder name within the `RootPath.` Default is
  "/Data_acquisition_workflow"

- DiscLifePath:

  A character String. The path to the folder which contains data from
  Ascher and Pcikering's Discover Life website.

- OutPath:

  A character String. The path to the folder where output data will be
  saved.

- OutPathName:

  A character String. The name of the `OutPath` subfolder located within
  the `RootPath.` Default is "Output".

- Report:

  Logical. If TRUE, function creates a "Report" folder within the
  OutPath-defined folder. Default = TRUE.

- Check:

  Logical. If TRUE, function creates a "Check" folder within the
  OutPath-defined folder. Default = TRUE.

- Figures:

  Logical. If TRUE, function creates a "Figures" folder within the
  OutPath-defined folder. Default = TRUE.

- Intermediate:

  Logical. If TRUE, function creates a "Intermediate" folder within the
  OutPath-defined folder in which to save intermediate datasets. Default
  = TRUE.

- RDoc:

  A character String. The path to the current script or report, relative
  to the project root. Passing an absolute path raises an error. This
  argument is used by
  [`here::i_am()`](https://here.r-lib.org/reference/i_am.html) and
  incorrectly setting this may result in `bdc` figures being saved to
  your computer's root directory

- useHere:

  Logical. If TRUE, dirMaker will use
  [`here::i_am()`](https://here.r-lib.org/reference/i_am.html) to
  declare the relative path to 'RDoc'. This is aimed at preserving some
  functionality with where bdc saves summary figures and tables. Default
  = TRUE.

## Value

Results in the generation of a list containing the BeeBDC-required
directories in your global environment. This function should be run at
the start of each session. Additionally, this function will create the
BeeBDC-required folders if they do not already exist in the supplied
directory

## Examples

``` r
  # load dplyr
  library(dplyr)
# Standard/basic usage:
RootPath <- tempdir()
dirMaker(
RootPath = RootPath,
# Input the location of the workflow script RELATIVE to the RootPath
RDoc = NULL,
useHere = FALSE) %>%
  # Add paths created by this function to the environment()
  list2env(envir = environment())  
#>  - We created the /tmp/Rtmpvrz4HI/BDC_repo/BeeBDC/Rfile. This file needs to have the NewFunctions added to it otherise things won't work. These can be added from our GitHub
#>  - We created the /tmp/Rtmpvrz4HI/Data_acquisition_workflowfile. This file needs to have the occurrence data that you want to use added to it otherise things won't work. Please choose this data or download it from the supp. materials of our paper
#> Warning: '/tmp/Rtmpvrz4HI/BDC_repo/DiscoverLife_Data' already exists
#>  - We created the /tmp/Rtmpvrz4HI/BDC_repo/DiscoverLife_Datafile. This file needs to have the DiscoverLife_Data added to it otherise things won't work. These can be added from our GitHub
#> Warning: '/tmp/Rtmpvrz4HI/Data_acquisition_workflow/Output' already exists
#>  - We created the /tmp/Rtmpvrz4HI/Data_acquisition_workflow/Outputfile.
#> <environment: 0x55b27d67ddc0>

# Custom OutPathName provided
  dirMaker(
 RootPath = RootPath,
 # Set some custom OutPath info
 OutPath = NULL,
 OutPathName = "T2T_Output",
 # Input the location of the workflow script RELATIVE to the RootPath
 RDoc = NULL,
 useHere = FALSE) %>%
   # Add paths created by this function to the environment()
   list2env(envir = environment())  
#> Warning: '/tmp/Rtmpvrz4HI/Data_acquisition_workflow/T2T_Output' already exists
#>  - We created the /tmp/Rtmpvrz4HI/Data_acquisition_workflow/T2T_Outputfile.
#> <environment: 0x55b27d3a5d38>
 # Set the working directory

# Further customisations are also possible
dirMaker(
  RootPath = RootPath,
  ScriptPath = "...path/Bee_SDM_paper/BDC_repo/BeeBDC/R",
  DiscLifePath = "...path/BDC_repo/DiscoverLife_Data",
  OutPathName = "AsianPerspective_Output",
  # Input the location of the workflow script RELATIVE to the RootPath
  RDoc = NULL,
  useHere = FALSE) %>%
  # Add paths created by this function to the environment()
  list2env(envir = environment())  
#>  - We created the ...path/Bee_SDM_paper/BDC_repo/BeeBDC/Rfile. This file needs to have the NewFunctions added to it otherise things won't work. These can be added from our GitHub
#> Warning: '...path/BDC_repo/DiscoverLife_Data' already exists
#>  - We created the ...path/BDC_repo/DiscoverLife_Datafile. This file needs to have the DiscoverLife_Data added to it otherise things won't work. These can be added from our GitHub
#> Warning: '/tmp/Rtmpvrz4HI/Data_acquisition_workflow/AsianPerspective_Output' already exists
#>  - We created the /tmp/Rtmpvrz4HI/Data_acquisition_workflow/AsianPerspective_Outputfile.
#> <environment: 0x55b27d174038>


```
