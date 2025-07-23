# the script R/modules/download.R

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
    
    # Input for selecting date range
    dateRangeInput("date_range", "Select Date Range:", 
                   start = Sys.Date() - 30, end = Sys.Date()),
    
    #Input for the temporal resolution
    selectInput("temporal_resolution", "Select Temporal Resolution:", 
                choices = c("Daily", "Pentad", "Monthly"), selected = "Daily"),
    
    # Download button
    downloadButton("download_data", "Download Data")
  )
  )
}


# the page download server
download_server <- function(input, output, session) {
  
  # Reactive expression to generate data based on inputs
  data_to_download <- reactive({
    # Simulate data generation based on input parameters
    data.frame(
      Date = seq(input$date_range[1], input$date_range[2], by = "day"),
      Region = input$region,
      Value = runif(length(seq(input$date_range[1], input$date_range[2], by = "day")), 0, 100)
    )
  })
}