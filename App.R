
#If you don't have the dependencies is necessary to install
#install.packages(shiny)
#install.packages(shinydashboard)
#install.packages(tidyverse)
#install.packages(lubridate)
#install.packages(leaflet)
#install.packages(plotly)
#install.packages(scales)
#install.packages(sf)
#install.packages(janitor)


library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(scales)
library(sf)
library(janitor)


#Read data and select
bio_occurrence <- read_csv("F:/species-map/data/poland_data.csv") #Important - You must have change the directory.

bio_occurrence <- bio_occurrence %>%
  select(id, scientificName, vernacularName, individualCount, lifeStage, longitudeDecimal, latitudeDecimal,
         locality, modified, taxonRank, family, kingdom,  )

bio_occurrence <- bio_occurrence %>%
    mutate(kingdom = if_else(is.na(kingdom), 'Unknown', kingdom),
         vernacularName = if_else(is.na(vernacularName), 'Unavailable', vernacularName),
         locality = str_remove(locality, ".*-"),
         family =str_replace_all(family, '-',''),
         family = str_to_title(family),
         names = as.factor(paste(scientificName, "|", vernacularName)),
         eventDate = as.Date(modified)
         )

#Dropdown
sv_names <- unique(bio_occurrence$names)


#UI - Dashboard

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = tags$div(
      style = "display: flex; justify-content: center; align-items: center; flex-direction: column;",
      tags$span("Biodiversity Dashboard by Pedro", style = "font-size: 18px;")
    ),
    titleWidth = '100%'
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Species Occurrence in Poland", tabName = "bio_occurrence")
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "bio_occurrence",
        
        #Search Menu
        fluidRow(
          column(
            width = 12,
            style = "display: flex; justify-content: center; align-items: center; padding-top: 10px;",  # Reduzir o espaço entre a barra de pesquisa e o mapa
            selectInput("species_selector", "Please, select a specie:", choices = sv_names)
          )
        ),
        
        # Map/Selected Specie
        fluidRow(
          column(
            width = 12,
            leafletOutput("occurrence_map", height = 400)
          )
        ),
        
        # Graphic Adjustment 
        fluidRow(
          column(
            width = 4,
            box(
              title = "Occurrence of Selected Species by Year", 
              plotlyOutput("occurrence_by_year")
            )
          ),
          column(
            width = 4,
            box(
              title = "Yearly Occurrence of Selected Species by Locality", 
              plotlyOutput("yearly_occurrence_by_locality")
            )
          ),
          column(
            width = 4,
            box(
              title = "Occurrence of Selected Species by Locality", 
              plotlyOutput("occurrence_by_locality_total")
            )
          )
        ),
        
        # Map Total Species
        fluidRow(
          column(
            width = 5,
            h4("Map of Total Species"),
            leafletOutput("total_map", height = 400)
          )
        )
      )
    )
  )
) 

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$species_selector)
    bio_occurrence %>%
      filter(names == input$species_selector)
  })

# Timeline

output$occurrence_timeline <- renderPlotly({
  req(input$Scientific_Vernacular_Name)
  
  bio_occurrence %>%
    filter(names == input$Scientific_Vernacular_Name) %>%
    bio_occurrence <- bio_occurrence %>%
    eventDate = as.Date(modified)
    group_by(names, yr) %>%
    summarise(Count = sum(individualCount)) %>%
    ggplot() +
    geom_col(aes(yr, Count, fill = yr)) +
    theme_minimal()
    
    
})


#Occurrence map by region (Poland)
output$occurrence_map <- renderLeaflet({
  selected_species <- input$species_selector
  filtered_data <- bio_occurrence %>%
    filter(names == selected_species)
  
  leaflet(data = filtered_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~longitudeDecimal, ~latitudeDecimal,
      radius = ~log(individualCount + 1) * 2,
      color = "brown",
      popup = ~paste("<b>Scientific Name:</b>", scientificName, "<br>",
                     "<b>Vernacular Name:</b>", vernacularName, "<br>",
                     "<b>Count:</b>", individualCount, "<br>",
                     "<b>Year:</b>", modified, "<br>",
                       "<b>Locality:</b>", locality)
    )
})

# Map for all Species
output$total_map <- renderLeaflet({
  #Occurrence count by location
  choropleth_data <- bio_occurrence %>%
    group_by(locality) %>%
    summarise(total_species = n()) %>%
    filter(!is.na(locality))
  
  # Coordinates to display on the map
  choropleth_data <- choropleth_data %>%
    mutate(
      longitude = runif(n(), min = 14, max = 24),  # Poland
      latitude = runif(n(), min = 49, max = 55)
    )
  
  leaflet(data = choropleth_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~longitude, ~latitude,
      radius = ~log(total_species + 1) * 2,
      color = "green",
      popup = ~paste("<b>Locality:</b>", locality, "<br>",
                     "<b>Total Species:</b>", total_species)
    )
})

output$occurrence_by_year <- renderPlotly({
  data <- filtered_data()
  
  # Data/year
  yearly_data <- data %>%
    group_by(year = lubridate::year(eventDate)) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(year)) # Remover anos ausentes
  
  # Create Graphic
  p <- ggplot(yearly_data, aes(x = year, y = total_count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Occurrence of", input$species_selector, "by Year"),
         x = "Year", y = "Total Occurrences") +
    theme_minimal()
  
  ggplotly(p)
})


# Graphic Year/Locality
output$occurrence_by_locality <- renderPlotly({
  req(filtered_data())
  
  locality_data <- filtered_data() %>%
    group_by(year = lubridate::year(eventDate), locality) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(year), !is.na(locality))  # Don't show dates without occurences
  
  # Create Graphic
  p <- ggplot(locality_data, aes(x = year, y = total_count, fill = locality)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Yearly Occurrence of", input$species_selector, "by Locality"),
         x = "Year", y = "Total Occurrences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p)
})


# Occurrence by Locality
output$occurrence_by_locality_total <- renderPlotly({
  req(filtered_data())
  
  locality_data <- filtered_data() %>%
    group_by(locality) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(locality))  # Don't show locality without occurences
  
  # Create Graphic
  p <- ggplot(locality_data, aes(x = reorder(locality, total_count), y = total_count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Occurrence of", input$species_selector, "by Locality"),
         x = "Locality", y = "Total Occurrences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p)
})








}




shinyApp(ui=ui, server = server)

