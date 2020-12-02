library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rgeos)

#load data import file
load_data_path = file.path('app-scripts','data_import.R')
source(load_data_path)

#load UI file
load_ui_path = file.path('app-scripts','UI.R')
source(load_ui_path)


#load server files
load_server_path = file.path('app-scripts','server.R')
source(load_server_path)

#Run App
shinyApp(ui, server)

