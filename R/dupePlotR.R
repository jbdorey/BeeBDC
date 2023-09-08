# This function was written by James B Dorey on the 29th of September 2022
# Its purpose is to visualise duplicate occurrence data by using a compound bargraph
# Please contact jbdorey[at]me.com for help

#' Create a compound bar graph of duplicate data sources
#' 
#' Creates a plot with two bar graphs. One shows the absolute number of duplicate records for each 
#' data source
#' while the other shows the proportion of records that are duplicated within each data source. 
#' This function requires a dataset that has been run through [BeeBDC::dupeSummary()].
#'
#' @param data A data frame or tibble. Occurrence records as input.
#' @param outPath Character. The path to a directory (folder) in which the output should be saved.
#' @param fileName Character. The name of the output file, ending in '.pdf'.
#' @param legend.position The position of the legend as coordinates. Default = c(0.85, 0.8).
#' @param base_height Numeric. The height of the plot in inches. Default = 7.
#' @param base_width Numeric. The width of the plot in inches. Default = 7.
#' @param ... Other arguments to be used to change factor levels of data sources.
#' @param dupeColours A vector of colours for the levels duplicate, kept duplicate, and unique.
#' Default = c("#F2D2A2","#B9D6BC", "#349B90").
#' @param returnPlot Logical. If TRUE, return the plot to the environment. Default = FALSE.
#'
#' @return Outputs a .pdf figure.
#' @export
#' 
#' @importFrom dplyr %>%
#'
#' @examples
#' 
#' # This example will show a warning for the factor levels taht are not present in the specific 
#' # test dataset
#' dupePlotR(
#'   data = beesFlagged,
#'   # The outPath to save the plot as
#'     # Should be something like: #paste0(OutPath_Figures, "/duplicatePlot_TEST.pdf"),
#'   outPath = tempdir(), 
#'   fileName = "duplicatePlot_TEST.pdf",
#'   # Colours in order: duplicate, kept duplicate, unique
#'   dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
#'   # Plot size and height
#'   base_height = 7, base_width = 7,
#'   legend.position = c(0.85, 0.8),
#'   # Extra variables can be fed into forcats::fct_recode() to change names on plot
#'   GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
#'   ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minckley' = "BMin", Ecd = "Ecd",
#'   Gaiarsa = "Gai", EPEL = "EPEL", Lic = "Lic", Bal = "Bal", Arm = "Arm"
#'   )
dupePlotR <- function(
    data = NULL,
    outPath = NULL,
    fileName = NULL,
    legend.position = c(0.85, 0.8),
    base_height = 7,
    base_width = 7,
    # Factor levels to be changed
    ...,
      # Colours in order: duplicate, kept duplicate, unique
    dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
    returnPlot = FALSE
){
  # locally bind variables to the function
  database_id <- duplicateStatus <- dataSource <- simpleSource <- NULL 
  
    # Load dependencies
  requireNamespace("ggspatial")
  requireNamespace("forcats")
  requireNamespace("dplyr")
  requireNamespace("cowplot")
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(data)){
    stop(" - Please provide an argument for data I'm a program not a magician.")
  }
  if(is.null(outPath)){
    stop(" - Please provide an argument for outPath Seems reckless to let me just guess.")
  }
  if(is.null(fileName)){
    stop(" - Please provide an argument for fileName Seems reckless to let me just guess.")
  }
  if(sum(data$.duplicates) == 0){
    stop(" - No duplicates flagged in the dataset. Stopping process.")
  }
  
  
  #### 1.0 data prep. ####
  # Create the formatted file to create the figure
  # Add duplicates
  dupeTibble <- data %>%
    # Select relevant columns
    dplyr::select(database_id, duplicateStatus, dataSource) %>%
    # Simplify the dataSources
    dplyr::mutate(simpleSource = stringr::str_replace(
      string = dataSource, pattern = "_.*", replacement = "")) 
  
  dupeTibble$duplicateStatus <- dupeTibble$duplicateStatus %>%
    forcats::fct_relevel("Duplicate","Kept duplicate","Unique")
  
  rm(data)
  
  # Recode the simpleSource to be consistent with elsewhere and then order it
  factorised <- dupeTibble$simpleSource %>% 
      # Recode the factors as the user requests
    forcats::fct_recode(...) %>%
      # Turn into a tibble
    dplyr::tibble() %>%
      # Name the single column
    stats::setNames(c("simpleSource")) %>%
      # Group by this column and then count the number of occurrences for each level
    dplyr::group_by(simpleSource) %>%
    dplyr::mutate(count = dplyr::n()) %>%
      # Name these columns
    stats::setNames(c("simpleSource", "count")) 
    # Now re-order the factor by this count and then feed back into dupeTibble
  dupeTibble$simpleSource <-  forcats::fct_reorder(factorised$simpleSource, 
                                                    factorised$count, .desc = TRUE)

    #### 2.0 Total duplicates ####
  dupeBar <- ggplot2::ggplot(dupeTibble, 
                              ggplot2::aes(simpleSource, fill = duplicateStatus)) +
      ggplot2::geom_bar(position = "fill") +
      ggplot2::scale_fill_manual("legend", values = c("Duplicate" = dupeColours[1],
                                                      "Kept duplicate" = dupeColours[2],
                                                      "Unique" = dupeColours[3])) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                     axis.line = ggplot2::element_line(colour = "black"),
                     legend.position="none") +
      ggplot2::ylab("Proportion of records") + ggplot2::xlab("data source")
  
  #### 3.0 Proportion of duplicates ####
  dupHist <- ggplot2::ggplot(dupeTibble, ggplot2::aes(simpleSource, fill = duplicateStatus)) +
      ggplot2::geom_bar() +
      ggplot2::scale_fill_manual("legend", values = c("Duplicate" = dupeColours[1],
                                             "Kept duplicate" = dupeColours[2],
                                             "Unique" = dupeColours[3])) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.title.x= ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", colour = NA),
            axis.line = ggplot2::element_line(colour = "black")) +
      ggplot2::ylab("Number of records") + ggplot2::xlab("data source")
  
  #### 4.0 combine + save ####
  # plot the figures together
  (dupPlot <- cowplot::plot_grid(dupHist + 
                                   ggplot2::theme(legend.position = legend.position,
                                         legend.title = ggplot2::element_blank()),
                                 dupeBar, 
                                 labels = c("(a)","(b)"),
                                 ncol = 1, align = 'v', axis = 'l'))
  # Save the plot
  cowplot::save_plot(filename = paste(outPath, fileName, sep = "/"),
                     plot = dupPlot,
                     base_width = base_width,
                     base_height = base_height)
  
  if(returnPlot == TRUE){
    return(dupPlot)}
  
  
} # END function


