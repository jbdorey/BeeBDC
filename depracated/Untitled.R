# This function was written by James B Dorey on the 29th of September 2022
# Its purpose is to visualise duplciate occurrence data by using a chord diagram
# Please contact jbdorey@me.com for help



# Create a table to go into chord diagram
chordData <- table(dplyr::bind_cols(duplicates$dataSource, duplicates$dataSource_match)) 


pdf(file = paste0(DataPath, "/Output/Figures/", "ChordDiagram.pdf"),
    width = 7, height = 6, bg = "white")

par(mar = c(2, 2, 2, 2)/2, mfrow = c(1,1))

# A list of colour palettes to choose from 
palettes <- c("cartography::blue.pal", "cartography::green.pal", 
              "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
              "cartography::purple.pal", "cartography::brown.pal")

colourTable <- table(duplicates$dataSource) %>%
  as.data.frame() %>% tibble::tibble() %>% 
  setNames(c("sourceName", "Frequency")) %>%
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
    # Combine small groups (< 3)
    sourceCategories = dplyr::if_else(
      groupCount < 3, "Other", sourceCategories)) %>%
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

circlize::circos.par(canvas.ylim = c(-1.0,1.0), canvas.xlim = c(-0.6, 0.25))
# Create the chord diagrame
circlize::chordDiagram(
  x = chordData,
  order = colourTable$sourceName,
  directional = 0,
  reduce = 0,
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
  # circlize::circos.text(mean(xlim), ylim[1] + 1.5, 
  #                       sector.name,
  #                       facing = "clockwise", niceFacing = TRUE, adj = c(0, 0),
  #                       cex = 0.8)
  # circlize::circos.axis(h = "top", labels.cex = 0.5, major.tick.length = 0.2, 
  #                       sector.index = sector.name, track.index = 2,
  #                       labels.facing = "reverse.clockwise", minor.ticks = 0)
}, bg.border = NA)
# Highlight inputs
for(i in 1:length(unique(colourTable$sourceCategories))){
  loopCat <- colourTable %>%
    dplyr::filter(sourceCategories == unique(colourTable$sourceCategories)[i])
  circlize::highlight.sector(stringr::str_c(loopCat$sourceName), 
                             track.index = 1, col = loopCat$colour[[1]], 
                             text = unique(loopCat$sourceCategories), cex = 0.8, 
                             text.col = "black", niceFacing = TRUE)
}
legendList <- c()
# Make legends
for(i in 1:length(unique(colourTable$sourceCategories))){
  loopCat <- colourTable %>%
    dplyr::filter(sourceCategories == unique(colourTable$sourceCategories)[i])
  legendList[[i]] <- ComplexHeatmap::Legend(labels = stringr::str_c(loopCat$sourceName), 
                                            title = unique(stringr::str_c(loopCat$sourceCategories)), 
                                            legend_gp = grid::gpar(fill = c(loopCat$colour)))
} # END legend loop

lgd_list <- ComplexHeatmap::packLegend(list = legendList)
ComplexHeatmap::draw(lgd_list, x = grid::unit(6, "mm"), 
                     y = grid::unit(18, "mm"), just = c("left", "bottom"))

circlize::circos.clear()

title("Duplicated record sources")
dev.off()

