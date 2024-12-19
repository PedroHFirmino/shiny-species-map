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
         locality, modified, taxonRank, family, kingdom  )

bio_occurrence <- bio_occurrence %>%
  mutate(kingdom = if_else(is.na(kingdom), 'Unknow', kingdom),
         vernacularName = if_else(is.na(vernacularName), 'Unavailable', vernacularName),
         locality = str_remove(locality, ".*-"),
         family =str_replace_all(family, '-',''),
         family = str_to_title(family),
         names = as.factor(paste(scientificName, "|", vernacularName))
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
      menuItem("Map of Species Occurrence in Poland", tabName = "bio_occurrence")
    )
  ),

dashboardBody(
  tabItems(
    tabItem(
      tabName = "bio_occurrence",
      fluidRow(
        column(
          width = 4,
          selectInput("species_selector", "Choose a specie:", choices = sv_names)
        ),
        column(
          width = 8,
          leafletOutput("occurrence_map",height = 400)
        )
      ),
      fluidRow(
        column(
          width = 12,
          h4("Map of Total Species"),
          leafletOutput("total_map", height = 400)
        )
      )
    )
  )
  )
)




shinyApp(ui=ui, server = server)

