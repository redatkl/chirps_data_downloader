# UI modules for each tab
########################## acceuil UI
homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
      .home-container{
        padding: 0;
        height: calc(100vh - 110px);
        overflow: hidden;
      }
        #home-frame {
          border: none;
          width: 100%;
          height: 100%;
          margin: 0;
          padding: 0;
        }
        
      "))
    ),
    
    div(class = "home-container",
        tags$iframe(
          id = "home-frame",
          src = "iframes/home.html", 
          width = "100%",
          height = "100%",
          frameborder = "0"
        ))
  )
}


################################################# Server modules
# Server modules (placeholder for now)
homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
