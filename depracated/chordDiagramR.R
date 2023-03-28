# This function was written by James B Dorey on the 29th of September 2022
# Its purpose is to visualise duplicate occurrence data by using a chord diagram
# Please contact jbdorey@me.com for help

chordDiagramR <- function(
    # The duplicate data from the dupeSummary function output  
  dupeData = NULL,
  savePath = NULL,
  width = 7,
  height = 6,
  bg = "white",
  # How few distinct dataSources should a group have to be listed as "other"
  smallGrpThreshold = 3,
  title = "Duplicated record sources",
  # The default list of colour palettes to choose from 
  palettes = c("cartography::blue.pal", "cartography::green.pal", 
               "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
               "cartography::purple.pal", "cartography::brown.pal"),
  canvas.ylim = c(-1.0,1.0), 
  canvas.xlim = c(-0.6, 0.25),
  text.col = "black",
  legendX = grid::unit(6, "mm"),
  legendY = grid::unit(18, "mm"),
  legendJustify = c("left", "bottom"),
  niceFacing = TRUE){
  
  require(circlize)
  require(ComplexHeatmap)
  require(stringr)
  require(dplyr)
  require(paletteer)
  require(grid)
  
  #### 0.0 Prep ####
  ##### 0.1 errors ####
  ###### a. FATAL errors ####
  if(is.null(dupeData)){
    stop(" — Please provide an argument for dupeData. I'm a program not a magician.")
  }
  if(is.null(savePath)){
    stop(" — Please provide an argument for savePath. Seems reckless to let me just guess.")
  }
  

# Create a table to go into chord diagram
  suppressMessages(
chordData <- table(dplyr::bind_cols(dupeData$dataSource, dupeData$dataSource_keep)),
classes = "message")


#pdf(file = savePath,
#    width = width, height = height, bg = bg)
  # Create tables of the counts of kept source and duplicate source
keptSource <- table(dupeData$dataSource) %>%
  as.data.frame() %>% tibble::tibble() %>% 
  setNames(c("sourceName", "Frequency")) 
dupeSource <- table(dupeData$dataSource_keep) %>%
  as.data.frame() %>% tibble::tibble() %>% 
  setNames(c("sourceName", "Frequency_dupe")) 
  # Merge the sources and get their sum (for a total frequency count to order by)
colourTable <- dplyr::full_join(keptSource, dupeSource, by = "sourceName") %>%
  dplyr::mutate(Frequency = (Frequency + Frequency_dupe)) %>%
    # Drop the Frequency_dupe column
  dplyr::select(!Frequency_dupe) %>%
  # Get broad source (before first underscore)
  dplyr::mutate( sourceCategories = (sourceName %>%
                                       stringr::str_replace(
                                         string = .,
                                         pattern = "_.*",
                                         replacement = ""
                                       ))) %>%
  dplyr::group_by(sourceCategories) %>%
  dplyr::mutate(  # Count group number
    groupCount = n(),
    # Combine small groups (< smallGrpThreshold)
    sourceCategories = dplyr::if_else(
      groupCount < smallGrpThreshold, "Other", sourceCategories)) %>%
  dplyr::arrange(sourceName, .by_group = TRUE) %>%
  # Re-group
  dplyr::group_by(sourceCategories) %>%
  dplyr::mutate(groupNumber = cur_group_id(),
                # Re-count
                groupCount = n(),
                groupPalette = palettes[groupNumber]) %>%
  # assign colours
  dplyr::mutate(groupColours = 
                  paletteer::paletteer_dynamic(
                    palette = groupPalette[[1]],
                    n = groupCount[[1]]) %>% list(),
                colour = unlist(groupColours)[row_number()])


circlize::circos.clear()

circlize::circos.par(canvas.ylim = canvas.ylim, canvas.xlim = canvas.xlim)
# Create the chord diagrame
circlize::chordDiagram(
  x = chordData,
  order = colourTable$sourceName,
  directional = 1,
  direction.type = c("arrows"),
  link.arr.type = "big.arrow",
  reduce = 0,
    # self links fold directly back onto themselves instead of going to far side
  self.link = 1,
  grid.col = colourTable$colour,
  keep.diagonal = TRUE,
  # name, grid, axis
  annotationTrack = c("grid"),
  preAllocateTracks = list(
    track.height = circlize::mm_h(4),
    track.margin = c(circlize::mm_h(1), 0)),
  scale = FALSE
)
circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = circlize::get.cell.meta.data("xlim")
  ylim = circlize::get.cell.meta.data("ylim")
  sector.name = circlize::get.cell.meta.data("sector.index")
}, bg.border = NA)
# Highlight inputs
for(i in 1:length(unique(colourTable$sourceCategories))){
  loopCat <- colourTable %>%
    dplyr::filter(sourceCategories == unique(colourTable$sourceCategories)[i])
  circlize::highlight.sector(stringr::str_c(loopCat$sourceName), 
                             track.index = 1, col = loopCat$colour[[1]], 
                             text = unique(loopCat$sourceCategories), cex = 0.8, 
                             text.col = text.col, niceFacing = niceFacing)
}
legendList <- c()
# Make legends by creating a list of legends for each sourceCategory
for(i in 1:length(unique(colourTable$sourceCategories))){
  loopCat <- colourTable %>%
    dplyr::filter(sourceCategories == unique(colourTable$sourceCategories)[i])
  legendList[[i]] <- ComplexHeatmap::Legend(labels = stringr::str_c(loopCat$sourceName), 
                                            title = unique(stringr::str_c(loopCat$sourceCategories)), 
                                            legend_gp = grid::gpar(fill = c(loopCat$colour)))
} # END legend loop

lgd_list <- ComplexHeatmap::packLegend(list = legendList)
ComplexHeatmap::draw(lgd_list, x = legendX, 
                     y = legendY, just = legendJustify)

circlize::circos.clear()

title(title)

dev.copy2pdf(file = savePath, height = height, width = width, bg = bg)

#dev.off()
} # END function
