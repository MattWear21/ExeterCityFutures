library(shiny)
library(leaflet)
library(htmltools)
library(htmlwidgets)



ui <- fluidPage(
  includeCSS("stylesheet.css"),
  titlePanel(
    tags$div(class="header",
             tags$img(src="logo.png")
    )),
  mainPanel(leafletOutput("mymap", height = 500)),
  sidebarPanel(selectInput("polygons", "Select Dataset", 
                           c("None", "Total", "Percentage", "Installed Capacity", 
                             "Domestic Installed Capacity", "Non-Domestic Installed Capacity"),
                           selected = "Total"),
               checkboxInput("panels", "Show Solar Panels", FALSE)
  )
  
)