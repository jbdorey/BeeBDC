# This function was written starting on the 24th of June 2024 by James B Dorey in order
# to wrap iNEXT::ggiNEXT to work with iNEXTwrapper and plot multiple accumulation curves
# at once across pages.

#' ggplot2 extension for a Chao- and iNEXT-wrapper outputs
#' 
#' `BeeBDC::ggRichnessWrapper()` takes data output from `BeeBDC::iNEXTwrapper()` and 
#' `BeeBDC::ChaoWrapper()` to produce plots for multiple groups in one go. The plots can be 
#' multiple per page and across multiple pages.
#' 
#' @param iNEXT_in a list of \code{iNEXT} objects computed by `BeeBDC::iNEXTwrapper()`.
#' @param iChao_in a list of R data created `BeeBDC::ChaoWrapper()`.
#' @param filterOut Character. A list of sites/countries to exclude from plotting; for example,
#' because sample size was inadequate. Default = NULL.
#' @param type three types of plots: sample-size-based rarefaction/extrapolation curve (\code{type = 1}); 
#' sample completeness curve (\code{type = 2}); coverage-based rarefaction/extrapolation curve (\code{type = 3}).
#' From `iNEXT::ggiNEXT()`
#' @param se a logical variable to display confidence interval around the estimated sampling curve.
#' From `iNEXT::ggiNEXT()`
##' @param facet.var create a separate plot for each value of a specified variable: 
#'  no separation \cr (\code{facet.var="None"}); 
#'  a separate plot for each diversity order (\code{facet.var="Order.q"}); 
#'  a separate plot for each assemblage (\code{facet.var="Assemblage"}); 
#'  a separate plot for each combination of order x assemblage (\code{facet.var="Both"}).              
#' From `iNEXT::ggiNEXT()`
#' @param color.var create curves in different colors for values of a specified variable:
#'  all curves are in the same color (\code{color.var="None"}); 
#'  use different colors for diversity orders (\code{color.var="Order.q"}); 
#'  use different colors for sites (\code{color.var="Assemblage"}); 
#'  use different colors for combinations of order x assemblage (\code{color.var="Both"}).  
#' From `iNEXT::ggiNEXT()`
#' @param grey a logical variable to display grey and white ggplot2 theme. 
#' From `iNEXT::ggiNEXT()`
#' @param legendPerPlot Logical. If TRUE, remove the legend from each plot. Default = FALSE
#' @param show_iNEXT Logical. If TRUE, show the estimate and 95% CIs if specified for iNEXT. Default = TRUE.
#' @param showPercent Logical. If TRUE, show the prrcentage increases. Default = TRUE.
#' @param ChaoColour Character. The to be used to graph Chao estimates (95% confidence intervals
#' are shown with reduced opacity). Default = "#55AD9B".
#' @param iNEXTcolour Character. The to be used to graph iNEXT estimates (95% confidence intervals
#' are shown with reduced opacity). Default = "#FD9B63".
#' @param Chao_estimate Character. The name of the Chao estimate to use from those calculated in 
#' `SpadeR::ChaoSpecies()`. The options are "Homogeneous Model","Homogeneous (MLE)",
#' "Chao1 (Chao, 1984)","Chao1-bc","iChao1 (Chiu et al. 2014)","ACE (Chao & Lee, 1992)",
#' "ACE-1 (Chao & Lee, 1992)","1st order jackknife","2nd order jackknife". 
#' Default = "iChao1 (Chiu et al. 2014)".
#' @param nrow Numeric. The number of rows per figure. Figures (that don't fit in the
#' nrow*ncol grid) will be saved into 
#' additional files. Default = 3.
#' @param ncol Numeric. The number of columns per figure. Figures (that don't fit in the
#' nrow*ncol grid) will be saved into 
#' additional files. Default = 4.
#' @param labels Character. The labels for each sub-plot (a, b, c, ...). The default is NULL,
#' which will provide labels a-z as required.
#' @param fileName Character. Prefix to the output files. Default = "richnessPlots".
#' @param outPath Character. The fodler in which to save the plots. Default = `tempdir()`
#' @param base_width Numeric. The width, in inches, to save the plot. Default = 8.3.
#' @param base_height Numeric. The height, in inches, to save the plot. Default = 11.7.
#' @param dpi Numeric. Plot resolution. Also accepts a string input: "retina" (320), 
#' "print" (300), or "screen" (72). Applies only to raster output types. Default = 300.
#' 
#' @param ... other arguments passed on to methods. Not currently used.
#' From `iNEXT::ggiNEXT()`
#' 
#' @return Saves pdf objects and returns a summary table for all levels
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#     # Use the example data 
#' data(beesCountrySubset)
#' 
#'    # Transform data for iNEXT
#'  data_nextWrapper <- beesCountrySubset %>%
#'    dplyr::group_by(scientificName, country_suggested) %>%
#'    dplyr::count() 
#'    
#'  # Calculate iNEXT with the wrapper function
#'  output_iNEXTwrapper <- BeeBDC::iNEXTwrapper(data = data_nextWrapper,
#'                                              variableColumn = "country_suggested",
#'                                              valueColumn = "n",
#'                                              mc.cores = 1)
#'
#'  # Transform data for iChao
#' data_iChao <- beesCountrySubset %>%
#'   dplyr::group_by(scientificName, country_suggested) %>%
#'   dplyr::count() %>%
#'   dplyr::select(scientificName, country_suggested, n) %>%
#'   tidyr::pivot_wider(names_from = country_suggested,
#'                      values_from = n,
#'                      values_fill = 0) %>%
#'   ## Create the rownames
#'   tibble::column_to_rownames("scientificName") %>%
#'   dplyr::tibble()
#'   
#'   # Run the wrapper function
#' output_iChaowrapper <- BeeBDC::ChaoWrapper(data = data_iChao,
#'                                              datatype = "abundance",
#'                                              k = 10,
#'                                              conf = 0.95,
#'                                              mc.cores = 1)
#'                                              
#'    # Make the plots! 
#' plot_summary <- BeeBDC::ggRichnessWrapper(
#' iNEXT_in = output_iNEXTwrapper,
#' iChao_in = output_iChaowrapper,
#' nrow = 2,
#' ncol = 2,
#' labels = NULL,
#' fileName = "speciesRichnessPlots",
#' outPath = tempdir(),
#' base_width = 8.3,
#' base_height = 11.7, 
#' dpi = 300)
#'       
#' }                                       



ggRichnessWrapper <- function(
    iNEXT_in = country_iNEXT,
    iChao_in = NULL,
    filterOut = NULL,
    type = 1,
    se = TRUE,
    facet.var = "None",
    color.var = "Order.q",
    grey = FALSE,
    legendPerPlot = FALSE,
    show_iNEXT = TRUE,
    showPercent = TRUE,
    ChaoColour = "#55AD9B",
    iNEXTcolour = "#FD9B63",
    Chao_estimate = "iChao1 (Chiu et al. 2014)",
    nrow = 3,
    ncol = 4,
    labels = NULL,
    fileName = "richnessPlots",
    outPath = tempdir(),
    base_width = 8.3,
    base_height = 11.7, 
    dpi = 300,
    ...){
  
  decimalLatitude <- decimalLongitude <- database_id <- scientificName <- NULL
  country_iNEXT <- . <- Est_s.e. <- NULL
    
  requireNamespace("magrittr")
  requireNamespace("iNEXT")
  requireNamespace("cowplot")
  requireNamespace("ggplot2")
  
  
  #### 0.0 Prepare function ####
    ##### 0.1 Fatal warnings ####
  if (is.null(iNEXT_in)) {
    stop("No iNEXT_in provided. Please provide some data produced from BeeBDC::ggRichnessWrapper().")
  }
  
    ##### 0.2 Prepare values ####
    # If no labels were provided, take them as the length of nrow and ncol a:z
  if(is.null(labels)){
    labels = LETTERS[1:(nrow*ncol)]
  }
  
  ##### 0.3 Functions ####
  getfun <- function(x) {
    if(length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }
  
  #### 1.0 Prepare data ####
    ##### 1.0 Extract listed countries ####
  dataExtracted <- iNEXT_in$iNextEst$iNextEst
  
    # Filter out levels in filterOut
  if(!is.null(filterOut)){
    dataExtracted <- dataExtracted[!names(dataExtracted) %in% filterOut]
  }
  
    # Create an empty data table to output
  if(!is.null(iChao_in)){
  statisticTable <- dplyr::tibble(
    level = NA_character_,
    n = NA_integer_, observedRichness = NA_integer_, 
    iNEXT_est = NA_integer_, iNEXT_lower = NA_integer_, iNEXT_upper = NA_integer_, 
    iNEXT_increasePercent = NA_integer_,
    iChao_est = NA_integer_, iChao_lower = NA_integer_, iChao_upper = NA_integer_, 
    iChao_increasePercent = NA_integer_
  ) %>%
    tidyr::drop_na()}else{
      statisticTable <- dplyr::tibble(
        level = NA_character_,
        n = NA_integer_, observedRichness = NA_integer_, 
        iNEXT_est = NA_integer_, iNEXT_lower = NA_integer_, iNEXT_upper = NA_integer_, 
        iNEXT_increasePercent = NA_integer_
      ) %>%
        tidyr::drop_na()
    }
  
  #### 1.0 Make plots ####
    ##### 1.1 Build plots ####
  ggiNEXT_fun <- function(dataIn = dataExtracted,
                      type = type,
                      se = se,
                      facet.var = facet.var,
                      color.var = color.var,
                      grey = grey){
    variable <- Name <- scale_x_continuous <- NULL
      # extract the iNEXT data itself
    dataWithin <- dataIn$iNextEst
    
    if(!is.null(iChao_in)){
      # Get the chao data for the current level
      levelChao <- iChao_in$richnessTable %>%
        dplyr::filter(variable == dataIn$DataInfo$Assemblage) %>%
        dplyr::filter(Name %>% stringr::str_squish() == Chao_estimate)
    }
    

        # Don't show choa in title if it's NULL
      if(is.null(iChao_in)){
        iNEXT_plot <- iNEXT::ggiNEXT(x = dataWithin,
                                     type = type,
                                     se = se,
                                     facet.var = facet.var,
                                     color.var = color.var,
                                     grey = grey) +
      ggplot2::ggtitle(paste0(dataIn$AsyEst$groupVariable %>% unique(),"\n",
                              "n = ", format(dataIn$DataInfo$n, big.mark = ","),"\n",
                              "; Obs. = ", format(dataIn$AsyEst$Observed[[1]] %>% round(0),
                                                    big.mark = ","),
                              "; Est. = ", format(dataIn$AsyEst$Estimator[[1]] %>% round(0),
                                                     big.mark = ","),
                                # Calculate percentage
                              if(showPercent == TRUE){
                                paste0(" (+",((1 - (dataIn$AsyEst$Observed[[1]] / dataIn$AsyEst$Estimator[[1]]))*100) %>% 
                                         round(0),"%)")
                              }, # END showPercent
                              "\n",
                              "Lower = ", format(dataIn$AsyEst$`95% Lower`[[1]] %>% round(0),
                                                 big.mark = ","),
                              "; Upper = ", format(dataIn$AsyEst$`95% Upper`[[1]] %>% round(0),
                                                 big.mark = ",")
                              )) + 
          ggplot2::theme_classic() } 
      # DO show choa in title if it's provided
      if(!is.null(iChao_in)){
        iNEXT_plot <- iNEXT::ggiNEXT(x = dataWithin,
                                     type = type,
                                     se = se,
                                     facet.var = facet.var,
                                     color.var = color.var,
                                     grey = grey) +
        ggplot2::ggtitle(paste0(dataIn$AsyEst$groupVariable %>% unique(),"\n",
                                "n = ", format(dataIn$DataInfo$n, big.mark = ","),
                                "; Obs. = ", format(dataIn$AsyEst$Observed[[1]] %>% round(0),
                                                  big.mark = ","),"\n",
                                  # iNEXT
                                "iNEXT = ", format(dataIn$AsyEst$Estimator[[1]] %>% round(0),
                                                    big.mark = ","),
                                # Calculate percentage
                                " (", format(dataIn$AsyEst$`95% Lower`[[1]] %>% round(0),
                                                   big.mark = ","),
                                "-", format(dataIn$AsyEst$`95% Upper`[[1]] %>% round(0),
                                                     big.mark = ","),
                                if(showPercent == TRUE){
                                  paste0("; +",((1 - (dataIn$AsyEst$Observed[[1]] / dataIn$AsyEst$Estimator[[1]]))*100) %>% 
                                           round(0),"%)")
                                }else{paste0(")")},
                                  # iCHAO
                                # Get the chao data for the current level
                                "\niChao = ", format(levelChao$Estimate %>% round(0),
                                                 big.mark = ","),
                                # Calculate percentage
                                " (", format(levelChao$`95%Lower` %>% round(0),
                                             big.mark = ","),
                                "-", format(levelChao$`95%Upper` %>% round(0),
                                            big.mark = ","),
                                if(showPercent == TRUE){
                                  paste0("; +",((1 - (dataIn$AsyEst$Observed[[1]] / levelChao$Estimate))*100) %>% 
                                           round(0),"%)")
                                }else{paste0(")")}
                                
        )) + 
          ggplot2::theme_classic() 
        }
    
    ###### a. legend ####
    
      # Remove the legend from each plot
    if(legendPerPlot == FALSE){
      iNEXT_plot <- iNEXT_plot + 
        ggplot2::theme(legend.position="none")
    } # END legendPerPlot
      
      ###### b. iNext 95% ####
      # Show the estimate and 95% CIs if specified for iNEXT
    if(show_iNEXT == TRUE){
      iNEXT_plot <- iNEXT_plot +
        ggplot2::geom_hline(yintercept = dataIn$AsyEst$Estimator[[1]], linetype="solid", color = iNEXTcolour) +
        ggplot2::geom_hline(yintercept = dataIn$AsyEst$`95% Lower`[[1]], linetype="dashed", color = iNEXTcolour) +
        ggplot2::geom_hline(yintercept = dataIn$AsyEst$`95% Upper`[[1]], linetype="dashed", color = iNEXTcolour) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = dataIn$AsyEst$`95% Upper`[[1]],  
                                          ymax = dataIn$AsyEst$`95% Lower`[[1]]),
                             fill = iNEXTcolour, alpha = 0.1, colour = NA) +
          # Have the plot start at zero on X and Y 
        scale_x_continuous(expand = c(0.001, 0.001)) + scale_y_continuous(expand = c(0.001, 0.001))
    } # END show_iNEXT
      
    ###### c. Chao 95% ####
      # Show the estimate and 95% CIs if specified for iChao
      if(!is.null(iChao_in)){
          # Get the chao data for the current level
        iNEXT_plot <- iNEXT_plot +
          ggplot2::geom_hline(yintercept = levelChao$Estimate, linetype="solid", color = ChaoColour) +
          ggplot2::geom_hline(yintercept = levelChao$`95%Lower`, linetype="dashed", color = ChaoColour) +
          ggplot2::geom_hline(yintercept = levelChao$`95%Upper`, linetype="dashed", color = ChaoColour) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = levelChao$`95%Lower`,  
                                            ymax = levelChao$`95%Upper`),
                               fill = ChaoColour, alpha = 0.1, colour = NA) 
      } # END iChao_in
    
     
    # Return the plot 
    return(iNEXT_plot)
  } # END ggiNEXT_fun 
  
  


  # Make the plots per country using lapply
  countryPlots <- dataExtracted %>%
    lapply(X = .,
           FUN = ggiNEXT_fun,
           type = type,
           se = se,
           facet.var = facet.var,
           color.var = color.var,
           grey = grey)
  

  ##### 1.2 Extract data table ####
  dataExtract_fun <- function(dataIn = dataExtracted,
                          type = type,
                          se = se,
                          facet.var = facet.var,
                          color.var = color.var,
                          grey = grey){
    variable <- Name <- NULL
    # extract the iNEXT data itself
    dataWithin <- dataIn$iNextEst
    
    if(!is.null(iChao_in)){
      # Get the chao data for the current level
      levelChao <- iChao_in$richnessTable %>%
        dplyr::filter(variable == dataIn$DataInfo$Assemblage) %>%
        dplyr::filter(Name %>% stringr::str_squish() == Chao_estimate)
    }
    
    # Simulataneously, create a row of data and add it to the output table
    
    if(!is.null(iChao_in)){
      statisticTable <-  dplyr::tibble(
        level = dataIn$AsyEst$groupVariable %>% unique(),
        n = dataIn$DataInfo$n, observedRichness = dataIn$AsyEst$Observed[[1]], 
        iNEXT_est = dataIn$AsyEst$Estimator[[1]] %>% round(0), 
        iNEXT_lower = dataIn$AsyEst$`95% Lower`[[1]] %>% round(0),
        iNEXT_upper = dataIn$AsyEst$`95% Upper`[[1]] %>% round(0), 
        iNEXT_increasePercent = ((1 - (dataIn$AsyEst$Observed[[1]] / dataIn$AsyEst$Estimator[[1]]))*100) %>%
          round(2),
        iChao_est = levelChao$Estimate %>% round(0), 
        iChao_lower = levelChao$`95%Lower` %>% round(0), iChao_upper = levelChao$`95%Upper` %>% round(0), 
        iChao_increasePercent = ((1 - (dataIn$AsyEst$Observed[[1]] / levelChao$Estimate))*100) %>% 
          round(2)
      ) }else{
        statisticTable <-  dplyr::tibble(
          level = dataIn$AsyEst$groupVariable %>% unique(),
          n = dataIn$DataInfo$n, observedRichness = dataIn$AsyEst$Observed[[1]], 
          iNEXT_est = dataIn$AsyEst$Estimator[[1]] %>% round(0), 
          iNEXT_lower = dataIn$AsyEst$`95% Lower`[[1]],  iNEXT_upper = dataIn$AsyEst$`95% Upper`[[1]], 
          iNEXT_increasePercent = ((1 - (dataIn$AsyEst$Observed[[1]] / dataIn$AsyEst$Estimator[[1]]))*100) %>%
            round(2)
        )
        }
    
    # Return the plot 
    return(statisticTable)
  } # END dataExtract_fun 

  
  # Make the plots per country using lapply
  statisticTable_out <- dataExtracted %>%
    lapply(X = .,
           FUN = dataExtract_fun,
           type = type,
           se = se,
           facet.var = facet.var,
           color.var = color.var,
           grey = grey) %>%
    dplyr::bind_rows()
  
  
  #### 2.0 Combine plots ####
    ##### 2.1 Chunk plot list ####
    # Get the number of chunks to be output
  numberOfChunks <- ceiling(length(countryPlots)/(nrow*ncol) )
  plotsPerPage <- (nrow*ncol)
  
  countryPlot_chunks <- dplyr::lst()
  j = 1
    # Put each chunk of plots into its own list
  for(i in 1:numberOfChunks){
    countryPlot_chunk_i <- countryPlots[j:(plotsPerPage + j)]
    countryPlot_chunks <- append(countryPlot_chunks, list(countryPlot_chunk_i))
    j = j + plotsPerPage 
  }
  
  
  ##### 2.2 Combine plots ####
    # A fucntion to do the cowplots
  multiPlotFun <- function(plotData = countryPlot_chunks,
                           nrow = nrow,
                           ncol = ncol,
                           labels = labels,
                           base_width = base_width,
                           base_height = base_height, 
                           dpi = dpi){
      # Make the plot grid
    plotOut <- do.call(getfun("cowplot::plot_grid"),
                       args = list(plotlist = plotData,
                                   nrow = nrow,
                                   ncol = ncol,
                                   labels = labels[1:length(plotData[lengths(plotData) > 0])]))
    
    # Save the plots
    cowplot::save_plot(
      plot = plotOut,
      filename = paste(outPath, "/", fileName, "_", names(plotData)[[1]], "_to_",
                       names(plotData)[[length(plotData)]], ".pdf", sep = ""),
      base_width = base_height,
      base_height = base_height, 
      dpi = dpi)
    
    # Return the plot grid
    return(plotOut)
  }
  
  
    # Combine the plots with cowplot
 combinedPlots <- lapply(X = countryPlot_chunks,
                         FUN = multiPlotFun,
                         nrow = nrow,
                         ncol = ncol,
                         labels = labels,
                         base_width = base_height,
                         base_height = base_height, 
                         dpi = dpi )
 
 return(statisticTable_out)

} # END ggRichnessWrapper





