# the script R/modules/download.R
source("R/modules/chirps_downloader.R")

# the page download ui
download_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(div(
    style = "
      position: fixed;
      left: 0;
      top: 90px;
      width: 300px;
      height: 100vh;
      overflow-y: auto;
      padding: 10px;
      background-color: #f8f9fa;
      border-right: 1px solid #dee2e6;
      box-sizing: border-box;
    ",
    h2("Download CHIRPS Data", class = "title-config"),
    p("Click on a point on the map and select the parameters below to download CHIRPS data."),
    
    # Input for selecting date range (with ns())
    dateRangeInput(ns("date_range"), "Select Date Range:", 
                   start = Sys.Date() - 30, end = Sys.Date()),
    
    # Input for the temporal resolution (with ns())
    selectInput(ns("temporal_resolution"), "Select Temporal Resolution:", 
                choices = c("Daily", "Pentad", "Monthly"), selected = "Daily"),
    
    # Region input (adding this since it's referenced in server)
    #textInput(ns("region"), "Region:", value = "Default Region"),
    
    # Button to start downloading (already has ns())
    actionButton(ns("fetch_button"), "Prepare Data", class = "btn btn-primary"),
    
    br(),br(),
    
    # Download button (with ns())
    downloadButton(ns("download_data"), "Download Data")
  ))
}

# the page download server
download_server <- function(id, clicked_point) {
  moduleServer(id, function(input, output, session) {
    
    # Create a reactive value to store the terra object
    terra_data <- reactiveVal()
    
    # store the current parameters in a reactive value
    current_params <- reactiveVal()
    
    # Reactive expression to generate data based on inputs
    data_to_download <- reactive({
      # Simulate data generation based on input parameters
      data.frame(
        Date = seq(input$date_range[1], input$date_range[2], by = "day"),
        Region = input$region,
        Temporal_Resolution = input$temporal_resolution,
        Value = runif(length(seq(input$date_range[1], input$date_range[2], by = "day")), 0, 100)
      )
    })
    
    # Observer for button click
    observeEvent(input$fetch_button, {
      # Access the lat/lng from the clicked_point reactive
      current_point <- clicked_point()
      
      if (nrow(current_point) > 0) {
        lat <- current_point$lat[1]  # Get the first (or latest) lat
        lng <- current_point$lng[1]  # Get the first (or latest) lng
        
        lat <- as.numeric(lat)
        lng <- as.numeric(lng)
        
        # Get the selected date range
        start_date <- input$date_range[1]  # Start date
        end_date <- input$date_range[2]
        
        # get the temporal resolution
        temporal_resolution = input$temporal_resolution
        
        # Store the parameters for later use
        current_params(list(
          lat = lat,
          lng = lng,
          temporal_resolution = temporal_resolution
        ))
        
        # Run the custom function
        new_result <- download_chirps_data(start_date = start_date, end_date = end_date,
                                           temporal_resolution = temporal_resolution,
                                           lat = lat, lng = lng, buffer_size = 0.5)
        # Store the terra object in reactive value
        terra_data(new_result)
      }
    })
    
    
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste("chirps_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        
        # Get the terra object and parameters
        terra_obj <- terra_data()  # Call as function to get actual object
        params <- current_params()
        
        data <- extract_chirps_values(
          raster_stack = terra_obj,  # Use the actual terra object
          lat = params$lat,
          lng = params$lng,
          temporal_resolution = params$temporal_resolution
        )
        if (!is.null(data)) {
          write.csv(data, file, row.names = FALSE)
        } else {
          # Create empty file if no data
          write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
        }
      }
    )
  })
}