# R/modules/chirps_downloader.R
# CHIRPS data download functionality

library(terra)
library(httr)
library(lubridate)

#' Download CHIRPS data based on coordinates and temporal resolution
#' @param lat Latitude of the point
#' @param lng Longitude of the point
#' @param start_date Start date for data download
#' @param end_date End date for data download
#' @param temporal_resolution Either "Daily", "Pentad", or "Monthly"
#' @param buffer_size Buffer size around point in degrees (default: 0.05)
#' @return SpatRaster stack or NULL if error
download_chirps_data <- function(lat, lng, start_date, end_date, temporal_resolution, buffer_size = 0.05) {
  
  tryCatch({
    # Create bounding box around the point
    bbox <- create_bbox(lat, lng, buffer_size)
    
    # Generate date sequence based on temporal resolution
    date_sequence <- generate_date_sequence(start_date, end_date, temporal_resolution)
    
    # Download data based on temporal resolution
    raster_stack <- switch(
      temporal_resolution,
      "Daily" = download_daily_chirps(bbox, date_sequence),
      "Pentad" = download_pentad_chirps(bbox, date_sequence),
      "Monthly" = download_monthly_chirps(bbox, date_sequence)
    )
    
    return(raster_stack)
    
  }, error = function(e) {
    message("Error downloading CHIRPS data: ", e$message)
    return(NULL)
  })
}

#' Create bounding box around a point
#' @param lat Latitude
#' @param lng Longitude
#' @param buffer Buffer size in degrees
#' @return Named list with bounding box coordinates
create_bbox <- function(lat, lng, buffer) {
  list(
    xmin = lng - buffer,
    xmax = lng + buffer,
    ymin = lat - buffer,
    ymax = lat + buffer
  )
}

#' Generate date sequence based on temporal resolution
#' @param start_date Start date
#' @param end_date End date
#' @param temporal_resolution Temporal resolution
#' @return Vector of dates
generate_date_sequence <- function(start_date, end_date, temporal_resolution) {
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  switch(
    temporal_resolution,
    "Daily" = seq(start_date, end_date, by = "day"),
    "Pentad" = generate_pentad_dates(start_date, end_date),
    "Monthly" = seq(start_date, end_date, by = "month")
  )
}

#' Generate pentad dates (5-day periods)
#' @param start_date Start date
#' @param end_date End date
#' @return Vector of pentad dates
generate_pentad_dates <- function(start_date, end_date) {
  # CHIRPS pentads: 6 pentads per month (days 1-5, 6-10, 11-15, 16-20, 21-25, 26-end)
  dates <- c()
  current_date <- start_date
  
  while (current_date <= end_date) {
    year <- year(current_date)
    month <- month(current_date)
    day <- day(current_date)
    
    # Determine pentad
    pentad <- ceiling(day / 5)
    if (pentad > 6) pentad <- 6  # Last pentad covers remaining days
    
    # Create pentad date (first day of pentad)
    pentad_start_day <- (pentad - 1) * 5 + 1
    pentad_date <- as.Date(paste(year, month, pentad_start_day, sep = "-"))
    
    if (!pentad_date %in% dates && pentad_date <= end_date) {
      dates <- c(dates, pentad_date)
    }
    
    current_date <- current_date + 1
  }
  
  return(as.Date(dates, origin = "1980-01-01"))
}

#' Download daily CHIRPS data
#' @param bbox Bounding box list
#' @param dates Vector of dates
#' @return SpatRaster stack
download_daily_chirps <- function(bbox, dates) {
  
  raster_list <- list()
  base_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/"
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    year <- year(date)
    
    # CHIRPS daily filename format: chirps-v2.0.YYYY.MM.DD.tif.gz
    filename <- sprintf("chirps-v2.0.%04d.%02d.%02d.tif.gz", 
                        year, month(date), day(date))
    
    url <- paste0(base_url, year, "/", filename)
    
    # Download and process
    temp_file <- tempfile(fileext = ".tif.gz")
    
    tryCatch({
      download.file(url, temp_file, mode = "wb", quiet = TRUE)
      
      # Extract and read raster
      temp_tif <- gsub("\\.gz$", "", temp_file)
      R.utils::gunzip(temp_file, temp_tif)
      
      raster <- rast(temp_tif)
      
      # Crop to bounding box
      extent_obj <- ext(bbox$xmin, bbox$xmax, bbox$ymin, bbox$ymax)
      cropped_raster <- crop(raster, extent_obj)
      
      raster_list[[i]] <- cropped_raster
      names(raster_list)[i] <- as.character(date)
      
      # Clean up
      unlink(c(temp_file, temp_tif))
      
    }, error = function(e) {
      message("Failed to download data for ", date, ": ", e$message)
      raster_list[[i]] <- NULL
    })
  }
  
  # Remove NULL elements and create stack
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  
  if (length(raster_list) > 0) {
    return(rast(raster_list))
  } else {
    return(NULL)
  }
}

#' Download pentad CHIRPS data
#' @param bbox Bounding box list
#' @param dates Vector of pentad dates
#' @return SpatRaster stack
download_pentad_chirps <- function(bbox, dates) {
  
  raster_list <- list()
  base_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_pentad/tifs/"
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    year <- year(date)
    month <- month(date)
    day <- day(date)
    
    # Calculate pentad number (1-6 for each month)
    pentad <- ceiling(day / 5)
    if (pentad > 6) pentad <- 6
    
    # CHIRPS pentad filename format: chirps-v2.0.YYYY.MM.pentad.tif.gz
    filename <- sprintf("chirps-v2.0.%04d.%02d.%d.tif.gz", year, month, pentad)
    
    url <- paste0(base_url, filename)
    
    # Download and process (similar to daily)
    temp_file <- tempfile(fileext = ".tif.gz")
    
    tryCatch({
      download.file(url, temp_file, mode = "wb", quiet = TRUE)
      
      temp_tif <- gsub("\\.gz$", "", temp_file)
      R.utils::gunzip(temp_file, temp_tif)
      
      raster <- rast(temp_tif)
      
      extent_obj <- ext(bbox$xmin, bbox$xmax, bbox$ymin, bbox$ymax)
      cropped_raster <- crop(raster, extent_obj)
      
      raster_list[[i]] <- cropped_raster
      names(raster_list)[i] <- paste0(date, "_pentad", pentad)
      
      unlink(c(temp_file, temp_tif))
      
    }, error = function(e) {
      message("Failed to download pentad data for ", date, ": ", e$message)
      raster_list[[i]] <- NULL
    })
  }
  
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  
  if (length(raster_list) > 0) {
    return(rast(raster_list))
  } else {
    return(NULL)
  }
}

#' Download monthly CHIRPS data
#' @param bbox Bounding box list
#' @param dates Vector of monthly dates
#' @return SpatRaster stack
download_monthly_chirps <- function(bbox, dates) {
  
  raster_list <- list()
  base_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/"
  
  # Get unique year-month combinations
  unique_months <- unique(format(dates, "%Y-%m"))
  
  for (i in seq_along(unique_months)) {
    year_month <- unique_months[i]
    year <- as.numeric(substr(year_month, 1, 4))
    month <- as.numeric(substr(year_month, 6, 7))
    
    # CHIRPS monthly filename format: chirps-v2.0.YYYY.MM.tif.gz
    filename <- sprintf("chirps-v2.0.%04d.%02d.tif.gz", year, month)
    
    url <- paste0(base_url, filename)
    
    temp_file <- tempfile(fileext = ".tif.gz")
    
    tryCatch({
      download.file(url, temp_file, mode = "wb", quiet = TRUE)
      
      temp_tif <- gsub("\\.gz$", "", temp_file)
      R.utils::gunzip(temp_file, temp_tif)
      
      raster <- rast(temp_tif)
      
      extent_obj <- ext(bbox$xmin, bbox$xmax, bbox$ymin, bbox$ymax)
      cropped_raster <- crop(raster, extent_obj)
      
      raster_list[[i]] <- cropped_raster
      names(raster_list)[i] <- year_month
      
      unlink(c(temp_file, temp_tif))
      
    }, error = function(e) {
      message("Failed to download monthly data for ", year_month, ": ", e$message)
      raster_list[[i]] <- NULL
    })
  }
  
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  
  if (length(raster_list) > 0) {
    return(rast(raster_list))
  } else {
    return(NULL)
  }
}


######################## extract lat and lon data
#' Extract pixel values at specific coordinates from CHIRPS raster stack
#' @param raster_stack SpatRaster stack from CHIRPS download functions
#' @param lat Latitude of the point
#' @param lng Longitude of the point
#' @param temporal_resolution Temporal resolution used ("Daily", "Pentad", or "Monthly")
#' @return Data frame with dates and precipitation values
extract_chirps_values <- function(raster_stack, lat, lng, temporal_resolution) {
  
  if (is.null(raster_stack)) {
    message("No raster stack provided")
    return(NULL)
  }
  
  tryCatch({
    # Create point from coordinates
    point <- vect(cbind(lng, lat), crs = crs(raster_stack))
    
    # Extract values at the point for all layers
    extracted_values <- extract(raster_stack, point)
    
    # Get layer names (dates)
    layer_names <- names(raster_stack)
    
    # Create data frame
    if (temporal_resolution == "Daily") {
      df <- data.frame(
        date = as.Date(layer_names),
        precipitation_mm = as.numeric(extracted_values[1, -1]), # Remove ID column
        stringsAsFactors = FALSE
      )
    } else if (temporal_resolution == "Pentad") {
      # Parse pentad names (format: "YYYY-MM-DD_pentadX")
      dates <- sapply(layer_names, function(x) {
        date_part <- strsplit(x, "_")[[1]][1]
        as.Date(date_part)
      })
      
      pentads <- sapply(layer_names, function(x) {
        pentad_part <- strsplit(x, "_pentad")[[1]][2]
        as.numeric(pentad_part)
      })
      
      df <- data.frame(
        date = as.Date(dates, origin = "1970-01-01"),
        pentad = pentads,
        precipitation_mm = as.numeric(extracted_values[1, -1]),
        stringsAsFactors = FALSE
      )
    } else if (temporal_resolution == "Monthly") {
      # Parse monthly names (format: "YYYY-MM")
      dates <- as.Date(paste0(layer_names, "-01"))
      
      df <- data.frame(
        date = dates,
        precipitation_mm = as.numeric(extracted_values[1, -1]),
        stringsAsFactors = FALSE
      )
    }
    
    # Remove rows with NA values
    df <- df[!is.na(df$precipitation_mm), ]
    
    # Add coordinate information
    df$latitude <- lat
    df$longitude <- lng
    
    # Sort by date
    df <- df[order(df$date), ]
    
    return(df)
    
  }, error = function(e) {
    message("Error extracting pixel values: ", e$message)
    return(NULL)
  })
}

#' Complete workflow: Download CHIRPS data and extract values at point
#' @param lat Latitude of the point
#' @param lng Longitude of the point
#' @param start_date Start date for data download
#' @param end_date End date for data download
#' @param temporal_resolution Either "Daily", "Pentad", or "Monthly"
#' @param buffer_size Buffer size around point in degrees (default: 0.05)
#' @return Data frame with dates and precipitation values at the specific point
get_chirps_point_data <- function(lat, lng, start_date, end_date, temporal_resolution, buffer_size = 0.05) {
  
  # Download raster stack
  message("Downloading CHIRPS data...")
  raster_stack <- download_chirps_data(lat, lng, start_date, end_date, temporal_resolution, buffer_size)
  
  if (is.null(raster_stack)) {
    message("Failed to download raster data")
    return(NULL)
  }
  
  # Extract values at point
  message("Extracting values at coordinates...")
  point_data <- extract_chirps_values(raster_stack, lat, lng, temporal_resolution)
  
  return(point_data)
}
