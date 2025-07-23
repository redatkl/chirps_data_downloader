# Load required libraries
library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(shinyjs)

# global settings
source("global.R")

# ui definition
ui <- navbarPage(
  title = div(
    img(src = "logo/logo.png", height = "60px"),
    span("CHIRPS Data downloader")
  ),
  #selected = 'Geomonitoring',
  #collapsible = TRUE,
  # Move shinyjs and CSS to header
  header = tagList(
    shinyjs::useShinyjs(),
    includeCSS("www/custom.css"),
    # Add favicon
    tags$head(
      tags$link(rel = "icon", type = "image/ico", href = "favicon/favicon.ico"),
      tags$link(rel = "shortcut icon", type = "image/ico", href = "favicon/favicon.ico"),
      # importing bootsrap icons
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css")
    )
  ),
  
  # tabpanels
  tabPanel(
    title = "HOME",
    value = "home",
    icon = icon("home"),
    # Placeholder for home content
    h2("Welcome to chrips data downloader")
  ),
  tabPanel(
    title = "Downloader",
    icon = icon("globe"),
    download_ui("download")
  )
  
  
)


# Server
server <- function(input, output, session) {
  
  
  # We'll build this step by step
  download_server("download")
  
  
}


# Run app
shinyApp(ui = ui, server = server)