# This function was written by James B Dorey on the 29th of September 2022
# Its purpose is to visualise duplicate occurrence data by using a compound bargraph
# Please contact jbdorey@me.com for help

dupePlotR <- function(
    Data = NULL,
    outpath = NULL,
    legend.position = c(0.85, 0.8),
    base_height = 7,
    base_width = 7,
    # Factor levels to be changed
    ...,
      # Colours in order: duplicate, kept duplicate, unique
    dupeColours = c("#F2D2A2","#B9D6BC", "#349B90")
){
    # Load dependencies
  require(ggplot2)
  require(forcats)
  require(dplyr)
  require(cowplot)
  require(stringr)
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(Data)){
    stop(" — Please provide an argument for Data I'm a program not a magician.")
  }
  if(is.null(outpath)){
    stop(" — Please provide an argument for outpath Seems reckless to let me just guess.")
  }
  
  
  #### 1.0 Data prep. ####
  # Create the formatted file to create the figure
  # Add duplicates
  dupeTibble <- Data %>%
    # Select relevant columns
    dplyr::select(database_id, duplicateStatus, dataSource) %>%
    # Simplify the dataSources
    dplyr::mutate(simpleSource = stringr::str_replace(
      string = dataSource, pattern = "_.*", replacement = "")) 
  
  dupeTibble$duplicateStatus <- dupeTibble$duplicateStatus %>%
    forcats::fct_relevel("Duplicate","Kept duplicate","Unique")
  
  rm(Data)
  
  # Recode the simpleSource to be consistent with elsewhere and then order it
  factorised <- dupeTibble$simpleSource %>% 
      # Recode the factors as the user requests
    forcats::fct_recode(...) %>%
      # Turn into a tibble
    tibble::tibble() %>%
      # Name the single column
    set_names(c("simpleSource")) %>%
      # Group by this column and then count the number of occurrences for each level
    dplyr::group_by(simpleSource) %>%
    dplyr::mutate(count = n()) %>%
      # Name these columns
    set_names(c("simpleSource", "count")) 
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
      ggplot2::ylab("Proportion of records") + ggplot2::xlab("Data source")
  
  #### 3.0 Proportion of duplicates ####
  dupHist <- ggplot2::ggplot(dupeTibble, aes(simpleSource, fill = duplicateStatus)) +
      ggplot2::geom_bar() +
      ggplot2::scale_fill_manual("legend", values = c("Duplicate" = dupeColours[1],
                                             "Kept duplicate" = dupeColours[2],
                                             "Unique" = dupeColours[3])) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.title.x= ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", colour = NA),
            axis.line = ggplot2::element_line(colour = "black")) +
      ggplot2::ylab("Number of records") + ggplot2::xlab("Data source")
  
  #### 4.0 combine + save ####
  # plot the figures together
  (dupPlot <- cowplot::plot_grid(dupHist + 
                                   theme(legend.position = legend.position,
                                         legend.title = element_blank()),
                                 dupeBar, 
                                 labels = c("(a)","(b)"),
                                 ncol = 1, align = 'v', axis = 'l'))
  # Save the plot
  cowplot::save_plot(filename = outpath,
                     plot = dupPlot,
                     base_width = base_width,
                     base_height = base_height)
  
  
  
} # END function


