#install.packages(c("shiny", "leaflet", "sf", "dplyr"))

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)


# Load data (adjust paths as needed)
setwd("C:/Users/jklas/OneDrive - Columbia University Irving Medical Center/DS/p8105_final_voting_crisiss")

ny_counties <- st_read("shiny/data/Counties.shp") |> 
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
  titlePanel("Voting Results in New York State"),
  sidebarLayout(
    sidebarPanel(
      # Keep the sidebar controls the same
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
                  step = 1),
      sliderInput("opacity", "Map Opacity:",
                  min = 0, max = 1, value = 0.7, step = 0.1)
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
    
    leaflet(filtered_data()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(map_data$result == "demo", "blue", "red"),
        fillOpacity = input$opacity,
        color = "white",
        weight = 1,
        label = ~paste0("County: ", NAME, "<br>",
                        "Democratic %: ", democratic_percent, "<br>",
                        "Republican %: ", republician_percent, "<br>",
                        "Below Poverty %: ", below_poverty_percentage, "<br>",
                        "Above 65 %: ", above_65_percent, "<br>",
                        "Below 30 %: ", below_30_percent)
      )
  })
}

# Run the app
shinyApp(ui, server)