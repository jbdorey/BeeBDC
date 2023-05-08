require(rnaturalearthdata)
require(rnaturalearth)
require(BeeDC)

data("bees3sp")


testData <- BeeDC::jbd_CfC_chunker(data = bees3sp %>%
                                     tidyr::drop_na(decimalLatitude),
                                        lat = "decimalLatitude",
                                        lon = "decimalLongitude",
                                        country = "country",
                                        # How many rows to process at a time
                                        stepSize = 50,
                                        # Start row
                                        chunkStart = 1,
                                        append = FALSE)



