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
library(readxl)

ny_counties <- st_read("data/Counties.shp") |> 
  select(-DOS_LL, -DOSLL_DATE)

voting_covid_data <- read.csv("data/voting_covid_detail.csv") %>%
  mutate(
    result = if_else(democratic_percent > republician_percent, "demo", "rep")
  )

poverty_county_level<- read_excel("data/poverty_county_level__clean_v2.0.xlsx")


voting_age_county_level<- read_excel("data/voting_age_county_level_clean_v2.0.xlsx")
  

# Merge shapefile with voting data
map_data <- ny_counties %>%
  left_join(voting_covid_data, by = c("NAME" = "County")) |> 
  left_join(poverty_county_level, by = c("NAME" = "county")) |> 
  left_join(voting_age_county_level, by = c("NAME" = "county"))

map_data <- st_transform(map_data, crs = 4326)

# UI
ui <- fluidPage(
  titlePanel("Voting Results in New York State"),
  sidebarLayout(
    sidebarPanel(
      # Keep the sidebar controls the same
      selectInput("color_var", "Select Data to Display:",
                  choices = c("Voting Result" = "result"),
                  selected = "result"),
      sliderInput("poverty_threshold", "Select Poverty Percentage Threshold:",
                  min = 4,
                  max = 23,
                  value = median(map_data$below_poverty_percentage, na.rm = TRUE),
                  step = 1),
      sliderInput("age_threshold_1", "Select Age Percentage Threshold (65+):",
                  min = 18,
                  max = 36,
                  value = median(map_data$above_65_percent, na.rm = TRUE),
                  step = 1),
      sliderInput("age_threshold_2", "Select Age Percentage Threshold (30-):",
                  min = 12,
                  max = 38,
                  value = median(map_data$below_30_percent, na.rm = TRUE),
                  step = 1)
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression for filtered map data
  filtered_data <- reactive({
    map_data %>%
      filter(below_poverty_percentage >= input$poverty_threshold &
               above_65_percent >= input$age_threshold_1 &
               below_30_percent >= input$age_threshold_2)
  })
  
  # Render Leaflet map focused on New York State
  output$map <- renderLeaflet({
    req(filtered_data())
    
    color_map <- ifelse(map_data$democratic_percent > map_data$republician_percent, "blue", "red")
    
    leaflet(filtered_data()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -77, lat = 43, zoom = 6.3) %>%
      addPolygons(
        fillColor = color_map,
        fillOpacity = 1,
        label = ~paste0("County:", NAME,
                        " Democratic: ", democratic_percent,
                        " Republican: ", republician_percent)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

