# This is some dead script from the manual chord diagram



# sourceNames <- c("ALA_Apiformes", "iDigBio_apoidea", "USGS_data",
#                  "GBIF_Andrenidae","GBIF_Apidae","GBIF_Colletidae",
#                  "GBIF_Halictidae", "GBIF_Megachilidae","GBIF_Melittidae","GBIF_Stenotritidae",
#                  "SCAN_Andrenidae","SCAN_Apidae", "SCAN_Colletidae" ,  
#                  "SCAN_Halictidae","SCAN_Megachilidae","SCAN_Melittidae","SCAN_Stenotritidae",
#                  # OTHER
#                  "CAES_Anthophila", "Gai_Anthophila",
#                  "ASP_Anthophila", "BMin_Anthophila", "BMont_Anthophila",
#                  "Ecd_Anthophila", "INHS_Anthophila")
# 
# grid.col = c(ALA_Apiformes = "#807dba", iDigBio_apoidea = "#4292c6", USGS_data = "#636363",
#              GBIF_Andrenidae = "#feedde", GBIF_Apidae = "#fdd0a2", GBIF_Colletidae = "#fdae6b",
#              GBIF_Halictidae = "#fd8d3c", GBIF_Megachilidae = "#f16913", GBIF_Melittidae = "#d94801", 
#              GBIF_Stenotritidae = "#8c2d04",
#              
#              SCAN_Andrenidae = "#edf8fb", SCAN_Apidae = "#ccece6", SCAN_Colletidae = "#99d8c9",
#              SCAN_Halictidae = "#66c2a4", SCAN_Megachilidae = "#41ae76", SCAN_Melittidae = "#238b45",
#              SCAN_Stenotritidae = "#005824",
#                # OTHER
#              CAES_Anthophila = "#7a0177", INHS_Anthophila = "#ae017e",
#              Gai_Anthophila = "#dd3497",
#              ASP_Anthophila = "#f768a1", BMin_Anthophila = "#fa9fb5", 
#              BMont_Anthophila = "#fcc5c0",
#              Ecd_Anthophila = "#feebe2")




lgd = ComplexHeatmap::Legend(labels = "Anthophila", title = "ALA", legend_gp = grid::gpar(fill = "#807dba"))
lgd2 = ComplexHeatmap::Legend(labels = "Anthophila", title = "iDigBio", legend_gp = grid::gpar(fill = "#4292c6"))
lgd3 = ComplexHeatmap::Legend(labels = c("Andrenidae","Apidae","Colletidae",
                                         "Halictidae", "Megachilidae","Melittidae","Stenotritidae"),
                              title = "GBIF", legend_gp = grid::gpar(fill = c("#feedde", "#fdd0a2","#fdae6b",
                                                                              "#fd8d3c","#f16913","#d94801",
                                                                              "#8c2d04")))
lgd4 = ComplexHeatmap::Legend(labels = c("Andrenidae","Apidae", "Colletidae" ,  
                                         "Halictidae","Megachilidae","Melittidae","Stenotritidae"),
                              title = "SCAN", legend_gp = grid::gpar(fill = c("#edf8fb","#ccece6","#99d8c9",
                                                                              "#66c2a4","#41ae76","#238b45",
                                                                              "#005824")))
lgd5 = ComplexHeatmap::Legend(labels = c("CAES_Anthophila","INHS_Anthophila", "Gai_Anthophila",
                                         "ASP_Anthophila", "BMin_Anthophila", "BMont_Anthophila", 
                                         "Ecd_Anthophila"),
                              title = "Other", legend_gp = grid::gpar(fill = c("#7a0177","#ae017e",
                                                                               "#dd3497","#f768a1",
                                                                               "#fa9fb5","#fcc5c0", "#feebe2")))
