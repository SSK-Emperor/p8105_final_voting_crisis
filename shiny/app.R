#install.packages(c("shiny", "leaflet", "sf", "dplyr"))

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)


# Load data (adjust paths as needed)
setwd("C:/Users/geshc/OneDrive - Columbia University Irving Medical Center/桌面/p8105_final_voting_crisis")
ny_counties <- st_read("shiny/data/Counties.shp") 
voting_covid_data <- read.csv("data/voting_covid_detail.csv") %>%
  mutate(
    democratic_percent = as.numeric(democratic_percent),
    republician_percent = as.numeric(republician_percent),
    result = if_else(democratic_percent > republician_percent, "demo", "rep")
  )



# Merge shapefile with voting data
map_data <- ny_counties %>%
  left_join(voting_covid_data, by = c("NAME" = "County")) |> 
  left_join(poverty_county_level, by = c("NAME" = "county")) |> 
  left_join(voting_age_county_level, by = c("NAME" = "county")) 

# Define the color palette for the voting results
voting_palette <- colorFactor(palette = c("blue", "red"), domain = c("demo", "rep"))

# UI
ui <- fluidPage(
  titlePanel("Voting Results in New York by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("color_var", "Select Data to Display:",
                  choices = c("Voting Result" = "result"),
                  selected = "result"),
      sliderInput("poverty_threshold", "Select Poverty Percentage Threshold:",
                  min = min(map_data$below_poverty_percentage, na.rm = TRUE),
                  max = max(map_data$below_poverty_percentage, na.rm = TRUE),
                  value = median(map_data$below_poverty_percentage, na.rm = TRUE),
                  step = 1),
      sliderInput("age_threshold_1", "Select Age Percentage Threshold (65+):",
                  min = min(map_data$above_65_percent, na.rm = TRUE),
                  max = max(map_data$above_65_percent, na.rm = TRUE),
                  value = median(map_data$above_65_percent, na.rm = TRUE),
                  step = 1),
      sliderInput("age_threshold_2", "Select Age Percentage Threshold (30-):",
                  min = min(map_data$below_30_percent, na.rm = TRUE),
                  max = max(map_data$below_30_percent, na.rm = TRUE),
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
  
  # Reactive expression for filtered map data based on input thresholds
  filtered_data <- reactive({
    map_data %>%
      filter(below_poverty_percentage >= input$poverty_threshold &
               above_65_percent >= input$age_threshold)
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(result == "demo", "blue", "red"),
        fillOpacity = input$opacity,
        color = "white",
        weight = 1,
        label = ~paste0("County: ", NAME, "<br>",
                        "Democratic %: ", democratic_percent, "<br>",
                        "Republican %: ", republican_percent, "<br>",
                        "Below Poverty %: ", below_poverty_percentage, "<br>",
                        "Above 65 %: ", above_65_percent)
      )
  })
}

# Run the app
shinyApp(ui, server)