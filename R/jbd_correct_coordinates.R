  #' Internal function. Detects and corrects transposed geographic coordinates
  #'
  #' This functions detects mismatches between country names informed coordinates.
  #' Once detects, transposed coordinates are corrected by the used of different
  #' coordinates transformations by using the 'jbd_coord_trans' function.
  #'
  #' @param data data.frame. Containing an unique identifier for each records,
  #' geographical coordinates, and country names. Coordinates must be expressed in
  #' decimal degree and in WGS84.
  #' @param x character string. The column name with longitude. Default =
  #' "decimalLongitude".
#' @param y character string. The column name with latitude Default =
#' "decimalLatitude".
#' @param sp character string. The column name with species scientific name.
#' Default = "scientificName".
#' @param idcol idcol character string. The column name with an unique record
#' identifier. #' Default = "idcol".
#' @param cntr_iso2 character string. The column name with the country code
#' assignment of each record. Default = "country_code".
#' @param world_poly polygon. Borders of the world.
#' @param world_poly_iso charterer sting. Iso2 code column of country polygon
#' database
#' @param border_buffer numeric. A distance in decimal degrees used to created a
#' buffer around the country. Records within a given country and at a specified
#' distance from its coast will be not be corrected. Default = 0.2 (~20 km at
#' the equator).
#' @param mc.cores Numeric. If > 1, the jbd_correct_coordinates function will run in parallel
#' using mclapply using the number of cores specified. If = 1 then it will be run using a serial
#' loop. NOTE: Windows machines must use a value of 1 (see ?parallel::mclapply). Additionally,
#' be aware that each thread can use large chunks of memory.
#'  Default = 1.
#'
#' @return Internal function
#'
#' @importFrom CoordinateCleaner cc_val clean_coordinates
#' @importFrom dplyr filter mutate as_tibble select all_of pull bind_rows distinct relocate left_join  %>%
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' }
jbd_correct_coordinates <-
  function(data,
           x,
           y,
           sp,
           idcol,
           cntr_iso2,
           world_poly,
           world_poly_iso,
           border_buffer,
           mc.cores = 1) {
    . <- decimalLatitude <- decimalLongitude <- .summary <- iso2c <- funCoordTrans <- ':=' <- NULL
      #### 1.0 data prep ####
    x_mod <- paste0(x, "_modified")
    y_mod <- paste0(y, "_modified")
    
    occ_country <- data %>% dplyr::filter(!is.na(data[[cntr_iso2]]))
    
      #### 2.0 CoordinateCleaner ####
    # Filter occurrences database to avoid error in clean_coordinates errors
    suppressWarnings({
      suppressMessages({
        occ_country <-
          occ_country %>%
          CoordinateCleaner::cc_val(., lon = x, lat = y) %>%
          dplyr::mutate(
            decimalLatitude = as.numeric(decimalLatitude),
            decimalLongitude = as.numeric(decimalLongitude)
          )
      })
    })
    
    # Detect records outside a country
    suppressWarnings({
      suppressMessages({
        occ_country <- CoordinateCleaner::clean_coordinates(
          x = occ_country,
          lon = x,
          lat = y,
          species = sp,
          countries = cntr_iso2,
          # iso2 code column name
          # testing records in the sea and outside georeferenced countries
          tests = c("seas", "countries"),
          # high-quality countries border database
          country_ref = world_poly %>% sf::as_Spatial(),
          # iso2 code column of country polygon database
          country_refcol = world_poly_iso,
          seas_ref = world_poly %>% sf::as_Spatial(),
          value = "spatialvalid"
        )
      })
    })
    
    
    # Separate those records outside their countries
    occ_country <-
      occ_country %>%
      dplyr::as_tibble() %>%
      dplyr::filter(!.summary, !is.na(occ_country[[cntr_iso2]]))
    
    # now this database have all those records with potential error that be
    # corrected
    message(occ_country %>% nrow(), " occurrences will be tested")
    
    # If occ_country have no data 
    if(nrow(occ_country)==0){
      return(NULL)
    }
    
    # Split database by country code - cntr_iso2
    occ_country <-
      split(occ_country, occ_country[cntr_iso2])
    
    # JBD edit — Remove empty elements from list before testing.
    occ_country <- occ_country[sapply(occ_country, function(x) dim(x)[1]) > 0]
    
    
    # jbd_coord_trans() function will try different coordinate transformations
    # to correct georeferenced occurrences
    coord_test <- list()
    
    #### 3.0 Serial #####
      # If user does not specify for parallel operation
    if(mc.cores < 2){
      for (i in 1:length(occ_country)) {
        message(
          "Processing occurrences from: ",
          occ_country[[i]][cntr_iso2] %>% unique(),
          paste0(" (", nrow(occ_country[[i]]), ")")
        )
        try(coord_test[[i]] <-
              jbd_coord_trans(
                data = occ_country[[i]],
                x = x,
                y = y,
                country_code = cntr_iso2,
                idcol = idcol,
                worldmap = world_poly,
                worldmap_cntr_code = world_poly_iso
              ))
      }
    } # END mc.cores < 2
    
    #### 4.0 Parallel ####
      ##### 4.1 Create funCoordTrans ####
    if(mc.cores > 1){
      # Create the function using the input variables to pass to mcapply
    funCoordTrans <- function(data) {
      . <- ':=' <- NULL
      data <-
        data %>% dplyr::select(
          dplyr::all_of(x),
          dplyr::all_of(y),
          dplyr::all_of(cntr_iso2),
          dplyr::all_of(idcol)
        )
        
        names(data)[names(data) == idcol] <- "idcol"

      d1 <- data.frame(x = data[, x], y = -data[, y], idcol = data[, "idcol"])
      d2 <- data.frame(x = -data[, x], y = data[, y], idcol = data[, "idcol"])
      d3 <- data.frame(x = -data[, x], y = -data[, y], idcol = data[, "idcol"])
      d4 <- data.frame(x = data[, y], y = data[, x], idcol = data[, "idcol"])
      d5 <- data.frame(x = data[, y], y = -data[, x], idcol = data[, "idcol"])
      d6 <- data.frame(x = -data[, y], y = data[, x], idcol = data[, "idcol"])
      d7 <- data.frame(x = -data[, y], y = -data[, x], idcol = data[, "idcol"])
      
      d.list <- list(d1, d2, d3, d4, d5, d6, d7)
      rm(list = paste0("d", 1:7))
      d.list <- lapply(d.list, function(x) {
        colnames(x) <- c("x", "y", "idcol")
        return(x)
      })
      
      over_list <- list()
      
      
      for (d in 1:length(d.list)) {
        caluse <- d.list[[d]] %>% 
          sf::st_as_sf(., coords = c("x", "y"), crs = sf::st_crs("WGS84")) 
        suppressWarnings({
          overresult <- sf::st_intersection(caluse, world_poly) 
        })
        
        if(nrow(overresult) > 0){
          colnames(d.list[[d]]) <-
            c(paste0(x, "_modified"), paste0(y, "_modified"), "idcol")
          over_list[[d]] <- dplyr::left_join(d.list[[d]], data, by = "idcol") %>%
            dplyr::left_join(overresult, by = "idcol")
          rm(caluse)
          filt <-
            which(over_list[[d]][cntr_iso2] == over_list[[d]][world_poly_iso]) 
        }else{
          filt = dplyr::tibble()
        }
        if (length(filt) > 0) {
          over_list[[d]] <- over_list[[d]][filt,]
        } else {
          over_list[[d]] <- NULL
        }
        rm(list = c("overresult", "filt"))
      }
      
      rm(d.list)
      
      non_empty_list_test <- !sapply(over_list <- over_list, is.null)
      
      if (any(non_empty_list_test)) {
        over_list <- over_list[non_empty_list_test]
        over_list <- dplyr::bind_rows(over_list) 
      } else{
        over_list <- dplyr::tibble(
          decimalLongitude = double(),
          decimalLatitude = double(),
          countryCode = character(),
          database_id = character()
        )
      }
      
      # Return the database_id column to its correct name
      over_list <- over_list %>%
        dplyr::rename(!!idcol := idcol)
      
      return(over_list)
    }

    ##### 4.2 Run mclapply ####
      # Run the actual function
coord_test <- parallel::mclapply(occ_country, funCoordTrans,
                     mc.cores = mc.cores
)
} # END mc.cores > 1

    # elimination from the list those countries without correction
    filt <- sapply(coord_test, function(x) nrow(x) > 0)
    
    if(any(filt)){
      coord_test <- coord_test[filt]
      
      # Elimination of those records near to country border (to avoid flip
      # coordinates or sign that fall too close to country border)
      
      for (i in 1:length(coord_test)) {
        n <-
          coord_test[[i]] %>%
          dplyr::select(dplyr::all_of(cntr_iso2)) %>%
          unique() %>%
          dplyr::pull()
        
          # Select only the relevant polygon to buffer
        my_country2 <- world_poly %>%
          dplyr::filter(iso2c %in% n)
        
        # Here filter polygon based on your country iso2c code
        my_country2 <-
          my_country2 %>%
          dplyr::filter(iso2c %in% n) %>%
          # JBD — France was failing to buffer using raster due to TopologyException. Use sf instead.
          sf::st_as_sf() %>% sf::st_buffer(border_buffer) 
        # JBD — turned off for above reason
        # 0.5 degree ~50km near to equator
        # my_country2 <- raster::buffer(my_country, width = border_buffer)
        #  > my_country2
        #  class       : SpatialPolygons 
        #  features    : 1 
        #  extent      : -180, 180, -90, -60.51621  (xmin, xmax, ymin, ymax)
        #  crs         : NA 
        
        coord_sp <- sf::st_as_sf(coord_test[[i]] %>% dplyr::select({{ x }}, {{ y }}),
                                 coords = c(x, y))
        
        sf::st_crs(coord_sp) <- sf::st_crs(my_country2)
        over_occ <- sf::st_join(coord_sp, my_country2) %>%
          dplyr::pull(iso2c) 
        
        # Eliminate as corrected those records too close to country border
        coord_test[[i]] <-
          coord_test[[i]] %>% dplyr::filter(is.na(over_occ))
      }
      
      # Elimination of those records with more than two possible corrections
      coord_test <-
        dplyr::bind_rows(coord_test) %>%
        dplyr::as_tibble() # binding dataframes allocated in the list in a single one
      
      coord_test <-
        coord_test[!duplicated(coord_test[idcol]), ] %>%
        dplyr::relocate(dplyr::all_of(idcol), dplyr::all_of(x), dplyr::all_of(y))
      
      # Merge coord_test with other columns of occurrence database
      coord_test <-
        dplyr::left_join(coord_test,
                         data %>% dplyr::select(-c({{ x }}, {{ y }}, {{ cntr_iso2 }})),
                         by = idcol
        )
      
      return(coord_test)
    }else{
      return(NULL)
    }
  }
