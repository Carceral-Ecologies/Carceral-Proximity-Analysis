library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rgeos)

pb <- st_read("Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Thanks Ben!
#convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

pb_crs <- st_crs(pb_sf)

mil <- st_read("installations_ranges/MILITARY_INSTALLATIONS_RANGES_TRAINING_AREAS_PT.shp", stringsAsFactors = FALSE)
mil2 <- st_read("installations_ranges/MILITARY_INSTALLATIONS_RANGES_TRAINING_AREAS_BND.shp", stringsAsFactors = FALSE)

mil <- rbind(mil, mil2)

#convert mil to match (larger) FRS fac data set Coordinate Reference System
mil_sf <- st_transform(mil, crs = 4269)

#reduce mil from polygons to points (centroids) to reduces distance calculation times
mil_sf <- st_transform(mil_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

codes <- read.csv("state_codes.csv", stringsAsFactors = FALSE)
mil_sf <- mil_sf %>% left_join(codes, by = "STATE_TERR")

bf <- read.csv("brownfields.csv", stringsAsFactors = FALSE)
bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

ap <- read.csv("airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

sfs <- read.csv("sf.csv", stringsAsFactors = FALSE) 
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)



pb_sf$FID <- as.character(pb_sf$FID)




ui <- navbarPage("Proximity Analysis", id="nav",
                 
                 tabPanel("Interactive Map",
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("dist_plot", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Prison Explorer"),
                                            
                                            pickerInput(
                                              inputId = "state", 
                                              label = "Select a State", 
                                              choices = sort(unique(pb$STATE)), 
                                              options = list(
                                                `actions-box` = TRUE,
                                                `live-search` = TRUE
                                              ),
                                              multiple = FALSE),
                                            pickerInput(
                                              inputId = "name", 
                                              label = "Search for Prison in State", 
                                              choices = NULL, 
                                              options = list(
                                                `actions-box` = TRUE,
                                                `live-search` = TRUE
                                              ), 
                                              multiple = FALSE),
                                            sliderInput("proximity_val", "Set proximity (in miles):", min = 0, max = 10, value = 1, step = 1)  
                                            
                                            #plotOutput("histCentile", height = 200),
                                            #plotOutput("scatterCollegeIncome", height = 250)
                              )),
                          
                          tags$div(id="cite",
                                   'fill sources'
                          )
                 ), 
                 tabPanel("Data Explorer")
)

server <- function(input, output, session) {
  
  icon_pb <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  icon_pb_selected <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642-red.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  icon_bf <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  icon_sfs <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  icon_ap <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  icon_mil <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  observeEvent(input$state, {
    pb_sf_filtered <- pb_sf %>% 
      filter(STATE == input$state)
    prison_list <- setNames(pb_sf_filtered$FID, pb_sf_filtered$NAME)
    updatePickerInput(session, inputId = "name", choices = prison_list)
  })
  
  
  output$dist_plot <- renderLeaflet({
    
    bf_filtered <- bf %>%
      filter(STATE_CODE == input$state)
    sfs_filtered <- sfs %>%
      filter(STATE_CODE == input$state)
    pb_sf_filtered <- pb_sf %>%
      filter(STATE == input$state)
    ap_sf_filtered <- ap_sf %>%
      filter(state_post_office_code == input$state)
    mil_sf_filtered <- mil_sf %>%
      filter(STATE_CODE == input$state)
    
    html_legend <- "<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true' style='width:20px;height:20px; margin:5px;'>Prisons<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true' style='width:20px;height:20px; margin:5px;'>Brownfields<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true' style='width:20px;height:20px; margin:5px;'>Contaminated Sites<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true' style='width:20px;height:20px; margin:5px;'>Airports<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true' style='width:20px;height:20px; margin:5px;'>Military Sites"
    
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        clusterId = "airports",
        clusterOptions = markerClusterOptions(),
        data = ap_sf_filtered, 
        icon = icon_ap,
        label = ~fac_name, 
        group = "Airports") %>%
      addMarkers(
        clusterId = "military",
        clusterOptions = markerClusterOptions(),
        data = mil_sf_filtered, 
        icon = icon_mil,
        label = ~SITE_NAME, 
        group = "Military Sites") %>%
      addMarkers(
        clusterId = "brownfields",
        clusterOptions = markerClusterOptions(),
        data = bf_filtered, 
        icon = icon_bf,
        ~LONGITUDE83, 
        ~LATITUDE83,
        label = ~PRIMARY_NAME,
        group = "Brownfields") %>%
      addMarkers(
        clusterId = "contaminated",
        clusterOptions = markerClusterOptions(),
        data = sfs_filtered, 
        icon = icon_sfs,
        ~LONGITUDE83, 
        ~LATITUDE83,
        label = ~PRIMARY_NAME,
        group = "Contaminated Sites") %>%
      addMarkers(
        data = pb_sf_filtered, 
        icon = icon_pb,
        label = ~NAME, 
        group = "Prisons", 
        layerId = ~FID) %>%
      addLayersControl(
        overlayGroups=c("Prisons", "Brownfields", "Contaminated Sites", "Airports", "Military Sites"),
        options=layersControlOptions(collapsed=FALSE)) %>%
      addControl(html = html_legend, position = "bottomright") %>% 
      hideGroup("Airports") %>% 
      hideGroup("Brownfields") %>% 
      hideGroup("Contaminated Sites") %>% 
      hideGroup("Military Sites")
  })
  
  calculateNumberInProximity <- function(prison, site) {
    if (site == "bf")
      toxic_site_sf <- bf_sf
    else if (site == "sfs")
      toxic_site_sf <- sfs_sf
    else if (site == "ap")
      toxic_site_sf <- ap_sf
    else 
      toxic_site_sf <- mil_sf
    in_proximity <- st_is_within_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf, dist = (input$proximity_val*1609.34), sparse = FALSE)
    num_in_proximity <- length(in_proximity[in_proximity == TRUE & !is.na(in_proximity)])
    return(num_in_proximity)
  }
  
  showPrisonPopup <- function(prison, lat, lng) {
    selectedPrison <- pb_sf[pb_sf$FID == prison,]
    content <- as.character(tagList(
      tags$h4(selectedPrison$NAME),
      tags$strong(
        sprintf("%s", selectedPrison$ADDRESS),
        tags$br(),
        sprintf("%s, %s %s",selectedPrison$CITY, selectedPrison$STATE, selectedPrison$ZIP)),
      tags$br(),
      sprintf("Type: %s", selectedPrison$TYPE),
      tags$br(),
      sprintf("Status: %s", selectedPrison$STATUS),
      tags$br(),
      sprintf("Population: %s", selectedPrison$POPULATION),
      tags$br(),
      sprintf("Number of brownfields within %s mile(s): %s", input$proximity_val, calculateNumberInProximity(prison, "bf")),
      tags$br(),
      sprintf("Number of contaminated sites within %s mile(s): %s", input$proximity_val, calculateNumberInProximity(prison, "sfs")),
      tags$br(),
      sprintf("Number of airports within %s mile(s): %s", input$proximity_val, calculateNumberInProximity(prison, "ap")),
      tags$br(),
      sprintf("Number of military sites within %s mile(s): %s", input$proximity_val, calculateNumberInProximity(prison, "mil"))
    ))
    leafletProxy("dist_plot") %>% addPopups(lng, lat, content, layerId = prison)
  }
  
  observe({
    leafletProxy("dist_plot") %>% clearPopups()
    event <- input$dist_plot_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPrisonPopup(event$id, event$lat, event$lng)
    })
  })
  
  # to keep track of previously selected prison
  
  prev_prison <- reactiveVal()
  observeEvent(input$name, {
    print(input$name)
    row_selected <- pb_sf[pb_sf$FID == input$name, ]
    print(row_selected)
    proxy <- leafletProxy('dist_plot')
    proxy %>%
      addMarkers(
        data = row_selected,
        icon = icon_pb_selected,
        label = ~row_selected$NAME,
        group = "Prisons",
        layerId = ~row_selected$FID)
    
    # Reset previously selected marker
    if(!is.null(prev_prison()))
    {
      proxy %>%
        addMarkers(
          data = prev_prison(),
          layerId = ~prev_prison()$FID,
          icon = icon_pb)
    }
    # set new value to reactiveVal
    prev_prison(row_selected)
  })
  
  
}

shinyApp(ui, server)
