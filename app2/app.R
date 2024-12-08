#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

ny_counties <- st_read("data/Counties.shp") |> 
  select(-DOS_LL, -DOSLL_DATE)

voting_covid_data <- read.csv("data/voting_covid_detail.csv") %>%
  mutate(
    result = if_else(democratic_percent > republician_percent, "demo", "rep")
  )

# Merge shapefile with voting data
map_data <- ny_counties %>%
  left_join(voting_covid_data, by = c("NAME" = "County")) |> 
  left_join(poverty_county_level, by = c("NAME" = "county")) |> 
  left_join(voting_age_county_level, by = c("NAME" = "county"))

# UI
ui <- fluidPage(
  titlePanel("Interactive Map from Shapefile"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "color_var",
        "Select Attribute to Color By:",
        choices = colnames(map_data),
        selected = colnames(map_data)[1]
      ),
      sliderInput(
        "opacity",
        "Polygon Opacity:",
        min = 0, max = 1, value = 0.7, step = 0.1
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  server <- function(input, output, session) {
    output$map <- renderLeaflet({
      req(input$color_var)
      
      # Dynamically generate color palette
      palette <- colorNumeric(
        palette = "viridis", 
        domain = map_data[[input$color_var]]
      )
      
      # Create map with shapefile layer
      leaflet(data = map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~palette(map_data[[input$color_var]]),
          fillOpacity = input$opacity,
          color = "black",
          weight = 1,
          label = ~paste0(input$color_var, ": ", map_data[[input$color_var]]),
          group = "Shapefile Layer"
        ) %>%
        addLegend(
          "bottomright",
          pal = palette,
          values = ~map_data[[input$color_var]],
          title = input$color_var,
          opacity = 1
        ) %>%
        addLayersControl(
          overlayGroups = c("Shapefile Layer"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
  }
  
}

# Run the app
shinyApp(ui = ui, server = server)

