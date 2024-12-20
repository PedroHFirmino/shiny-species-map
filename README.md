# Biodiversity Dashboard

This project is an interactive dashboard developed with Shiny and `leaflet`, designed to visualize species occurrences geographically using biodiversity-related data. The dashboard is intended to display an interactive map of species distribution in Poland, allowing users to select specific species to view their occurrences.

Check the Dashboard on [Shinyapps](https://phfirmino.shinyapps.io/species-map/)

## Features

- **Geographical Occurrence Visualization**: Displays a map of Poland with species occurrences. Areas with higher numbers of occurrences are highlighted with varying color intensities.
- **Species Filter**: Users can select a species to filter the occurrences shown on the map.
- **Map Interaction**: The map allows zooming and direct interaction, with information displayed about each region when hovering the mouse over it.

## Technologies Used ⚡

- **Shiny**: A framework for building interactive web applications in R.
- **Leaflet**: A JavaScript library for creating interactive maps, used through the `leaflet` package in R.
- **dplyr**: For data manipulation.
- **sf**: For spatial data manipulation.
- **R**: The programming language used to develop the dashboard.

## Dependencies

Make sure to have the following R packages installed to run the project:

```R
# If you don't have the dependencies, you need to install them
install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("leaflet")
install.packages("plotly")
install.packages("scales")
install.packages("sf")
install.packages("janitor")
install.packages("rnaturalearth")
install.packages("devtools")
install.packages("rsconnect")
```


# How to Run the Project
Follow these steps to run the project:

# Requirements
- R (version 4.0 or higher)
- RStudio

## Steps
Clone the repository or download the project files.

```bash
Copy code
git clone https://github.com/PedroHFirmino/shiny-species-map
```

In RStudio, open the app.R file.

Run the application using the following command in RStudio:

R
Copy code
shiny::runApp()
The dashboard will open in your default web browser.

Project Structure
- App.R: The main file containing the logic of the Shiny app.
- data/: Containing the data used in the project.
- README.md: This file, providing project information.
