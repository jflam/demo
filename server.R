library(shiny)
library(leaflet)
library(dplyr)

# This code runs exactly once. Initialize some global
# variables in our server to hold the airports data.

airports <- read.csv("airports.dat", 
    header = FALSE, 
    stringsAsFactors = FALSE)

colnames(airports) <- 
    c("ID", 
      "name", 
      "city", 
      "country", 
      "IATA_FAA",
      "ICAO", 
      "lat", 
      "lon", 
      "altitude", 
      "timezone",
      "DST",
      "Region")

# Generate a sorted list of countries 

countries <- sort(unique(airports$country))

# Read data from routes.dat, that contains flight 
# routes for all airports. We will use this data to
# identify busy airports.

routes <- read.csv("routes.dat", 
    header = FALSE, 
    stringsAsFactors = FALSE)

colnames(routes) <- 
    c("airline", 
      "airlineID", 
      "sourceAirport",
      "sourceAirportID",
      "destinationAirport",
      "destinationAirportID",
      "codeshare",
      "stops",
      "equipment")

# Extract a count of number of flights departing from 
# an airport and create a new data frame, where "nrow" 
# is the name of the column with the flight count

departures <- routes %>%
    filter(sourceAirportID != "\\N") %>%
    group_by(sourceAirportID) %>%
    summarize(departures = n())

# Merge the departures data set with the 
# original data set that contains that lattitude
# and longitude of airports 

airports_with_departures <- 
    merge(airports, 
        departures, 
        by.x = "ID", 
        by.y = "sourceAirportID")

# Tooltip column contains tooltips for each airport 
# that gives its name (code): departures

airports_with_departures$tooltip <- 
    sprintf("%s (%s): %i", 
        airports_with_departures$name,
        airports_with_departures$IATA_FAA,
        airports_with_departures$departures)

airports_with_departures$radius =
    sqrt(airports_with_departures$departures)

shinyServer(function(input, output) {

    # Reactive that handles filtering data on 
    # post backs from the web browser

    country_data <- reactive({
        req(input$country)
        airports_with_departures %>% 
            filter(country == input$country)
    })

    # Dynamically generate a listbox control for 
    # the client that contains the list of countries 
    # sorted alphabetically

    output$controls <- renderUI({
        selectInput("country",
            label = "Select a country",
            choices = countries, 
            selected = "United States"
        )
    })

    # Dynamically render the slider control based on the
    # minimum and max departures for the selected country

    output$slider <- renderUI({
        max_destinations = max(country_data()$departures)
        sliderInput("departures", 
            "Filter by departures:",
            min = 1, 
            max = max_destinations, 
            value = c(1, max_destinations))
    })

    # Generate the map based on: 
    # a) country selected and 
    # b) min and max 
    # departures in the slider control

    output$map <- renderLeaflet({

        # Subset based on minimum and maximum departure
        # numbers sent from client 

        if (is.null(input$departures)) {
            airport_data <- country_data()
        } else {
            min_departures <- input$departures[1]
            max_departures <- input$departures[2]
            airport_data <- 
                country_data() %>%
                filter(departures >= min_departures & 
                    departures <= max_departures)
        }

        leaflet(data = airport_data) %>%
            addTiles() %>%
            addCircleMarkers( ~ lon, ~ lat, 
                popup = ~tooltip, 
                radius = ~radius,
                stroke = FALSE, 
                fillOpacity = 0.5)
    })
})