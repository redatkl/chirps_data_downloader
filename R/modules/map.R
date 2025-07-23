# R/modules/map.R
# Map module UI
map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns("map"), height = "85%", width = "70%"),
    
    # Add a div for the legend
    div(
      id = ns("legend"),
      class = "leaflet-legend"
    )
  )
}

# Map module Server
map_server <- function(id, map_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store clicked points
    clicked_point <- reactiveVal(data.frame(lat = numeric(0), lng = numeric(0)))
    
    output$map <- renderLeaflet({
      leaflet(map_data, 
              options = leafletOptions(
                worldCopyJump = TRUE,  # Prevents multiple worlds
                maxBounds = list(
                  list(-90, -180),  # Southwest corner (lat, lng)
                  list(90, 180)     # Northeast corner (lat, lng)
                ),
                maxBoundsViscosity = 1.0,  # How much to resist dragging outside bounds
                minZoom = 2,  # Minimum zoom level to prevent seeing multiple worlds
                maxZoom = 18,  # Optional: set maximum zoom level
                attributionControl = FALSE
              )) %>%
        addProviderTiles(providers$Esri.WorldImagery)%>%
        addProviderTiles(providers$CartoDB.VoyagerOnlyLabels)%>%
        setView(lng = -6.87, lat = 29 , zoom = 2)  # Adjust initial view as needed
    })
    
    # Handle map clicks - clear previous points and add new one
    observeEvent(input$map_click, {
      click <- input$map_click
      
      # Clear all existing markers first
      leafletProxy("map") %>%
        clearMarkers()
      
      # Store only the new point (replacing any previous point)
      new_point <- data.frame(lat = click$lat, lng = click$lng)
      clicked_point(new_point)
      
      # Add the new marker
      leafletProxy("map") %>%
        addMarkers(
          lng = click$lng,
          lat = click$lat,
          popup = paste("Lat:", round(click$lat, 6), "<br>Lng:", round(click$lng, 6)),
          layerId = "current_marker"  # Single ID since we only have one marker
        )
    })
    
    # Display current clicked coordinates
    output$click_info <- renderText({
      point <- clicked_point()
      if (nrow(point) > 0) {
        paste("Current point:\n",
              "Latitude:", round(point$lat, 6), "\n",
              "Longitude:", round(point$lng, 6))
      } else {
        "Click on the map to add a point"
      }
    })
    
    # Add legend if needed
    output$legend <- renderUI({
      tags$div(
        class = "leaflet-legend-content",
        "Click on the map to place a marker (previous markers will be cleared)"
      )
    })
    
    # Return the current clicked point
    return(clicked_point)
    
  })
}