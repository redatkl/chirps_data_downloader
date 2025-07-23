# the script R/modules/download.R

# the page download ui

download_ui <- function(id) {
  
  tagList(
    h2("Download CHIRPS Data"),
    p("Select the parameters below to download CHIRPS data."),
    
    # Input for selecting date range
    dateRangeInput("date_range", "Select Date Range:", 
                   start = Sys.Date() - 30, end = Sys.Date()),
    
    # Input for selecting region
    selectInput("region", "Select Region:", 
                choices = c("Global", "Africa", "Asia", "Europe", "North America", "South America")),
    
    # Download button
    downloadButton("download_data", "Download Data")
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