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
  select(id, scientificName, vernacularName, individualCount, lifeStage, sex, longitudeDecimal, latitudeDecimal,
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