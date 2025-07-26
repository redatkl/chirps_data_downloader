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
    
    # Button to start downloading (already has ns())
    actionButton(ns("fetch_button"), "Prepare Data", class = "btn btn-primary"),
    
    br(),br(),
    
    # Simple status message
    verbatimTextOutput(ns("status_message")),
    
    br(),
    
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
    
    # Status message reactive
    status_msg <- reactiveVal("")
    
    # Data ready flag
    data_ready <- reactiveVal(FALSE)
    
    # Observer for button click
    observeEvent(input$fetch_button, {
      # Access the lat/lng from the clicked_point reactive
      current_point <- clicked_point()
      
      if (nrow(current_point) > 0) {
        # Update status
        status_msg("Status: Preparing data...")
        data_ready(FALSE)
        
        lat <- current_point$lat[1]  # Get the first (or latest) lat
        lng <- current_point$lng[1]  # Get the first (or latest) lng
        
        lat <- as.numeric(lat)
        lng <- as.numeric(lng)
        
        # Get the selected date range
        start_date <- input$date_range[1]  # Start date
        end_date <- input$date_range[2]
        
        # get the temporal resolution
        temporal_resolution <- input$temporal_resolution
        
        # Store the parameters for later use
        current_params(list(
          lat = lat,
          lng = lng,
          temporal_resolution = temporal_resolution
        ))
        
        # Update status
        status_msg("Status: Downloading CHIRPS data...")
        
        # Run the custom function with error handling
        tryCatch({
          new_result <- download_chirps_data(start_date = start_date, end_date = end_date,
                                             temporal_resolution = temporal_resolution,
                                             lat = lat, lng = lng, buffer_size = 0.5)
          # Store the terra object in reactive value
          terra_data(new_result)
          
          # Update status to ready
          status_msg("Status: Data ready for download!")
          data_ready(TRUE)
          
        }, error = function(e) {
          status_msg(paste("Error:", e$message))
          data_ready(FALSE)
        })
        
      } else {
        status_msg("Please click on a point on the map first.")
      }
    })
    
    # Render status message
    output$status_message <- renderText({
      status_msg()
    })
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste("chirps_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        
        # Check if data is ready
        if (!data_ready()) {
          write.csv(data.frame(Message = "Data not ready. Please prepare data first."), 
                    file, row.names = FALSE)
          return()
        }
        
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
