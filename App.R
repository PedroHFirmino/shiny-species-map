
#If you don't have the dependencies is necessary to install
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("leaflet")
#install.packages("plotly")
#install.packages("scales")
#install.packages("sf")
#install.packages("janitor")
#install.packages("rnaturalearth")
#install.packages("devtools")
#install.packages("rsconnect")

if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  install.packages("devtools")
  devtools::install_github("ropensci/rnaturalearthhires")
}

options(repos = c(CRAN = "https://cran.rstudio.com"))

library(rsconnect)
library(rnaturalearth)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(scales)
library(sf)
library(janitor)

poland_border <- ne_states(country = "Poland", returnclass = "sf")


#Read data and select
bio_occurrence <- read_csv("data/poland_data.csv")

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
    title = "Biodiversity Dashboard by Pedro",
    titleWidth = '100%' 
  ),
  
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tabsetPanel(
      tabPanel(
        "Overview",
        
        #Search Menu
        fluidRow(
          column(
            width = 12,
            style = "display: flex; justify-content: center; align-items: center; padding-top: 10px;",  
            selectInput("species_selector", "Please, select a specie:", choices = sv_names)
          )
        ),
        
        # Map/Selected Specie
        fluidRow(
          column(
            width = 12,
            div(
              style = "cursor: default; text-align: center; font-weight: bold; border-bottom: 2px solid #4CAF50; padding-bottom: 10px; margin-bottom: 10px;",
              tags$style(
                HTML("
          #title-hover {
            color: #4CAF50; 
            font-weight: bold;  
            display: inline-block;
          }
          #title-hover:hover {
            color: #ffffff; 
            cursor: pointer;
            transition: color 0.3s ease;
          }
        ")
              ),
              h4(
                id = "title-hover",
                "Map Species/Region"
              )  
            ),
            leafletOutput("occurrence_map", height = 400)
          )
        ),
        #Species Information
        fluidRow(
          column(
            width = 12,
            box(
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              style = "padding: 20px; text-align: center;border: 1px solid #4CAF50;",
              uiOutput("species_details")
            )
          )
        ),
        
        #Graphics
        fluidRow(
          column(
            width = 4,  
            style = "padding: 15px; margin: 0;",  
            plotlyOutput("occurrence_by_year", height = 300)
          ),
          column(
            width = 4,
            style = "padding: 15px; margin: 0;",  
            plotlyOutput("yearly_occurrence_by_locality", height = 300)
          ),
          column(
            width = 4,  
            style = "padding: 15px; margin: 0;",  
            plotlyOutput("occurrence_by_locality_total", height = 300)
          )
        ),
        
        # Map Total Species
        fluidRow(
          column(
            width = 12,
            div(
              style = "cursor: default; text-align: center; font-weight: bold; border-bottom: 2px solid #4CAF50; padding-bottom: 10px; margin-bottom: 10px;",
              tags$style(
                HTML("
          #title-hover {
            color: #4CAF50; 
            font-weight: bold;  
            display: inline-block;
          }
          #title-hover:hover {
            color: #ffffff; 
            cursor: pointer;
            transition: color 0.3s ease;
          }
        ")
              ),
              h4(
                id = "title-hover",
                "Map of Total Species"
              )  
            ),
            leafletOutput("total_map", height = 400),
          )
        ),
        #Graphic Total Species
        fluidRow(
          column(
            width = 12,  
            style = "padding: 15px; margin: 0; ",
            plotlyOutput("total_occurrence_by_year", height = 400)
          )
        )
      )
    )
  )
) 

#Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$species_selector)
    bio_occurrence %>%
      filter(names == input$species_selector)
  })
  
  #Species Information
  output$species_details <- renderUI({
    data <- filtered_data()
    if (nrow(data) > 0) {
      species_info <- data[1, ] 
      tagList(
        tags$h4(tags$strong("Species Information")),
        tags$p(tags$strong("Scientific Name:"), species_info$scientificName),
        tags$p(tags$strong("Vernacular Name:"), species_info$vernacularName),
        tags$p(tags$strong("Count:"), species_info$individualCount),
        tags$p(tags$strong("Year:"), species_info$modified),
        tags$p(tags$strong("Locality:"), species_info$locality)
      )
    } else {
      tags$p("No information available for this species.")
    }
  })


  output$species_map <- renderLeaflet({
    leaflet(data = choropleth_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~log(total_species + 1) * 2,
        color = "brown",
        popup = ~paste("<b>Locality:</b>", locality, "<br>",
                       "<b>Total Species:</b>", total_species, "<br>",
                       "<b>Kingdom:</b>", kingdom, "<br>",
                       "<b>Year Range:</b>", min(year), " - ", max(year))
      )
  })
  
  # Reactive Data for Map Selection
  selected_data <- reactive({
    req(input$species_map_shape_click) # Trigger only when user interacts with map
    clicked_point <- input$species_map_shape_click
    longitude <- clicked_point$lng
    latitude <- clicked_point$lat
    
    # Filter the data based on the clicked location
    choropleth_data %>%
      filter(longitude == !!longitude & latitude == !!latitude)
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
        color = "green",
        popup = ~paste("<b>Scientific Name:</b>", scientificName, "<br>",
                       "<b>Vernacular Name:</b>", vernacularName, "<br>",
                       "<b>Count:</b>", individualCount, "<br>",
                       "<b>Year:</b>", modified, "<br>",
                       "<b>Locality:</b>", locality)
      )
  })
  
  #Map Total Species
  output$total_map <- renderLeaflet({
    
    # Poland Territory
    poland_border <- ne_states(country = "Poland", returnclass = "sf")
    
    #data
    choropleth_data <- bio_occurrence %>%
      group_by(locality) %>%
      summarise(total_species = n(), .groups = 'drop') %>%
      filter(!is.na(locality))
    
    # Add Cordinates
    choropleth_data <- choropleth_data %>%
      mutate(
        longitude = runif(n(), min = 14, max = 24),
        latitude = runif(n(), min = 49, max = 55)
      )
    
    # Convert to sf
    coordinates_sf <- st_as_sf(choropleth_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    
    # Filter in Poland Territory
    coordinates_sf <- st_intersection(coordinates_sf, poland_border)
    
    # 
    if (nrow(coordinates_sf) == 0) {
      print("There is no data within the boundaries of Poland.")
    } else {
      print(head(coordinates_sf))
    }
    

    #Group occurrences by region
    region_data <- coordinates_sf %>%
      group_by(locality) %>%
      summarise(total_species = sum(total_species), .groups = 'drop')
    
    # Create the color palette based on the total number of species
    pal <- colorQuantile("YlOrRd", region_data$total_species, n = 5)
    
    # Render the map with painted regions
    leaflet(data = poland_border) %>%
      addTiles() %>%
      addPolygons(
        data = poland_border,
        fillColor = ~pal(region_data$total_species), 
        weight = 1,
        color = "brown",
        opacity = 0.6,
        fillOpacity = 0.3,
        popup = ~paste("<b>Locality:</b>", region_data$locality, "<br><b>Total Species:</b>", region_data$total_species),
        highlight = highlightOptions(
          weight = 2,                 
          color = "black",            
          fillOpacity = 0.7,          
          bringToFront = TRUE         
        )
      ) %>%
      setView(lng = 19.5, lat = 51, zoom = 6) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = region_data$total_species,
        title = "Total Species",
        opacity = 1
      )
  })
#Graphic for Occurrence by Year
output$occurrence_by_year <- renderPlotly({
  data <- filtered_data()
  
  # Data/year
  yearly_data <- data %>%
    group_by(year = lubridate::year(eventDate)) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(year)) # remove missing years
  
  # Create Graphic 
  p <- ggplot(yearly_data, aes(x = year, y = total_count, fill = as.factor(year))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(yearly_data$year)), "Set3")) +  
    labs(title = paste("Occurrence of", input$species_selector, "by Year"),
         x = "Year", y = "Total Occurrences") +
    theme_minimal()
  
  # Make it responsive
  p_plotly <- ggplotly(p) %>%
    layout(
      title = list(
        text = paste("Occurrence of", input$species_selector, "by Year"),
        font = list(size = 13),
        automargin = TRUE
      )
    )
  
  p_plotly
})

# Graphic Year/Locality
output$yearly_occurrence_by_locality <- renderPlotly({
  req(filtered_data())
  
  # Prepare the data
  locality_data <- filtered_data() %>%
    group_by(year = lubridate::year(eventDate), locality) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(year), !is.na(locality))  # Remove missing years and locales
  
  # Create Graphic
  p <- ggplot(locality_data, aes(x = year, y = total_count, fill = locality)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Yearly Occurrence of", input$species_selector, "by Locality"),
         x = "Year", y = "Total Occurrences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Make it responsive
  p_plotly <- ggplotly(p) %>%
    layout(
      title = list(
        text = paste("Yearly Occurrence of", input$species_selector, "by Locality"),
        font = list(size = 13),
        automargin = TRUE
      )
    )
  
  p_plotly
})

# Occurrence by Locality
output$occurrence_by_locality_total <- renderPlotly({
  req(filtered_data())
  
  locality_data <- filtered_data() %>%
    group_by(locality) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(locality))  # Don't show locality without occurrences
  
  # Create Graphic
  p <- ggplot(locality_data, aes(x = reorder(locality, total_count), y = total_count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Occurrence of", input$species_selector, "by Locality"),
         x = "Locality", y = "Total Occurrences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Make it responsive
  p_plotly <- ggplotly(p) %>%
    layout(
      title = list(
        text = paste("Occurrence of", input$species_selector, "by Locality"),
        font = list(size = 13),
        automargin = TRUE
      )
    )
  
  p_plotly
})

output$total_occurrence_by_year <- renderPlotly({
  yearly_data <- bio_occurrence %>%
    group_by(year = lubridate::year(eventDate)) %>%
    summarise(total_count = sum(individualCount, na.rm = TRUE)) %>%
    filter(!is.na(year))  # Remove missing years
  
  # Create Graphic
  p <- ggplot(yearly_data, aes(x = year, y = total_count, fill = as.factor(year))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(yearly_data$year)), "Set3")) +  
    labs(title = "Total Occurrence of Species by Year",
         x = "Year", y = "Total Occurrences") +
    theme_minimal()
  
  # Make it responsive
  p_plotly <- ggplotly(p) %>%
    layout(
      title = list(
        text = "Total Occurrence of Species by Year",
        font = list(size = 13),
        automargin = TRUE
      )
    )
  
  p_plotly
})






}




shinyApp(ui=ui, server = server)

