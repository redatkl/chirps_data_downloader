source("R/modules/download.R")

source("R/modules/map.R")

# the page downloader of the shiny app
  downloader_ui <- function(id) {
    page_fluid(
      download_ui("download"),
      map_ui("map")
    )
  }
  
  
  
# server side of the page downloader
  downloader_server <- function(id) {
    
    download_server("download")
    map_server("map", map_data = NULL)  # Assuming map_data is defined elsewhere or passed as needed
    
    
  }