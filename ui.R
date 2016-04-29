library(shiny)
library(leaflet)

shinyUI(fluidPage(
    titlePanel("Airport Browser"),

    sidebarLayout(
        sidebarPanel(
            helpText("Visualize airports within a country"),

            # Country selector control is generated on the server 
            # and sent to client

            uiOutput("controls"),
            uiOutput("slider") 
        ),
        mainPanel(

            # This is where the generated map lives

            leafletOutput("map", height = "640")
        )
    )
))