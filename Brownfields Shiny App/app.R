library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rgdal) #reads shapefiles
library(rgeos)

bf <- read.csv("brownfields.csv", stringsAsFactors = FALSE)
pb <- readOGR("Prison_Boundaries","Prison_Boundaries", stringsAsFactors = FALSE)
pb <- spTransform(pb, CRS("+proj=longlat +datum=WGS84"))
pb_sf <- st_as_sf(pb)
pb_crs <- st_crs(pb)

bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

header <- dashboardHeader(title = "EPA Brownfields")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)

input_panel <- wellPanel(
  pickerInput(
    "state", 
    "Select a State", 
    choices = unique(bf$STATE_CODE), 
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE
    ),
    multiple = FALSE),
)

box1 <- box(
  title = "Prison Proximity to Brownfields", 
  solidHeader = TRUE, 
  width = 12,
  status = "warning",
  collapsible = TRUE,
  infoBoxOutput("value1", width = 6),
  infoBoxOutput("value2", width = 6),
  infoBoxOutput("value3", width = 12),
  br(),
  leafletOutput("distPlot")
)

frow1 <- fluidRow(box1)

# combine the fluid rows to make the body
body <- dashboardBody(
  input_panel,
  frow1
)

#completing the ui part with dashboardPage
ui <- dashboardPage(header, sidebar, body, skin='black')

server <- function(input, output, session) { 
  
  output$value1 <- renderInfoBox({
    
    pb_sf_filtered_num <- pb_sf %>%
      filter(STATE == input$state) %>%
      n_distinct()
    
    infoBox(pb_sf_filtered_num,'Number of Prisons', icon = icon("stats", lib='glyphicon'), color = "purple")
  })
  
  output$value2 <- renderInfoBox({
    
    bf_filtered_num <- bf_sf %>%
      filter(STATE_CODE == input$state) %>%
      n_distinct()
    
    infoBox(bf_filtered_num,'Number of Brownfields', icon = icon("stats", lib='glyphicon'), color = "purple")
  })
  
  output$value3 <- renderInfoBox({
    bf_filtered <- bf_sf %>%
      filter(STATE_CODE == input$state)
    pb_sf_filtered <- pb_sf %>%
      filter(STATE == input$state)
    nearest <- st_nearest_feature(pb_sf_filtered$geometry, bf_filtered$geometry)
    dist_nearest <- sapply(1:length(nearest), function(x){st_distance(bf_filtered$geometry[nearest[x]], pb_sf_filtered$geometry[x], by_element = FALSE) %>%
        units::set_units(mi)})
    num <- length(dist_nearest[dist_nearest<1])
    infoBox(num,'Number of Prisons with a Brownfield within 1 mile', icon = icon("stats", lib='glyphicon'), color = "purple")
  })
  
  output$distPlot <- renderLeaflet({
    
    bf_filtered <- bf %>%
      filter(STATE_CODE == input$state)
    pb_sf_filtered <- pb_sf %>%
      filter(STATE == input$state)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons( 
        data = pb_sf_filtered,
        color = "#444444",
        weight = 0.5,
        smoothFactor = 0.8,
        opacity = 1.0,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white",
                                            weight = 1,
                                            bringToFront = TRUE)) %>%
      addCircleMarkers(
        data = bf_filtered, 
        ~LONGITUDE83, 
        ~LATITUDE83,
        label = ~PRIMARY_NAME,
        radius = 1,
        stroke = FALSE,
        fillOpacity = 0.9)
  })
  
}

shinyApp(ui, server)
