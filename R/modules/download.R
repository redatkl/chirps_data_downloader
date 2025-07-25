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
download_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Add a reactive value to store results
    result_text <- reactiveVal("")
    
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
      
      # Run the custom function
      new_result <- download_chirps_data(lat = lat, lng = lng)
      result_text(new_result)
      }
    })
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste("chirps_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_to_download(), file, row.names = FALSE)
      }
    )
  })
}