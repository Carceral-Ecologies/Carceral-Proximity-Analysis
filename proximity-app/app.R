library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rgeos)

#==========================================================================================================
#Data files for points locations
#==========================================================================================================

#Read carceral facility Shapefile
pb <- st_read("data-clean/Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Thanks Ben!
#Convert carceral facilities to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce carceral facilities from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

#Convert the unique ID for carceral facilities to be a string
pb_sf$FID <- as.character(pb_sf$FID)
pb_crs <- st_crs(pb_sf) #get the CRS for carceral facility centroids
pb_sf <- sf::st_transform(pb_sf, "+proj=longlat + datum=WGS84")

#Read military bases data file
mil <- read.csv("data-clean/military_bases.csv", stringsAsFactors = FALSE) 
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)
mil_sf <- sf::st_transform(mil_sf, "+proj=longlat + datum=WGS84")

#Read brownfield data file and convert to sf
bf <- read.csv("data-clean/brownfields.csv", stringsAsFactors = FALSE)
bf <- bf %>% filter(!is.na(LATITUDE83))
bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)
bf_sf <- sf::st_transform(bf_sf, "+proj=longlat + datum=WGS84")

#Read airport data file and convert to sf
ap <- read.csv("data-clean/airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)
ap_sf <- sf::st_transform(ap_sf, "+proj=longlat + datum=WGS84")

#Read superfund sites data file and convert to sf
sfs <- read.csv("data-clean/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LATITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)
sfs_sf <- sf::st_transform(sfs_sf, "+proj=longlat + datum=WGS84")

#Read TRI sites data file and convert to sf
tri <- read.csv("data-clean/tri.csv", stringsAsFactors = FALSE) 
tri <- tri %>% filter(!is.na(X12..LATITUDE))
tri_sf <- st_as_sf(tri, coords = c("X13..LONGITUDE", "X12..LATITUDE"), crs = pb_crs, na.fail = FALSE)
tri_sf <- sf::st_transform(tri_sf, "+proj=longlat + datum=WGS84")

#==========================================================================================================
#Supplemental data files
#==========================================================================================================
#state_codes.csv contains full state names, state abbreviations, and census codes for each state, allowing conversion between the variables in each data file
#Ben: added ' ' to fix Windows column name issue from this .csv, caused by 'Byte Order Mark'
#per https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
codes <- read.csv("data-clean/state_codes.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) 
codes$STATE_NUM <- as.character(codes$STATE_NUM) %>% #convert state census codes to characters because they will eventually be text in file names
  str_pad(2, pad = "0") #add a leading zero to ensure all state numbers are two digits

#There are census tract shapefiles for each state stored in the project files. To reduce reading times, these are not all read in by default but on a state-by-state basis when a user selects a state. Below we add two columns with the path to the zip of each state's shapefile, along with the file itself.
codes <- codes %>%
  mutate(TRACT_ZIP = paste("data-clean/tracts/tl_2019_", STATE_NUM, "_tract.zip", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FOLDER = paste("data-clean/tracts/tl_2019_", STATE_NUM, "_tract/", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FILE = paste("tl_2019_", STATE_NUM, "_tract.shp", sep = "")) #create new column in codes dataframe with location of each census tract shapefile name

#==========================================================================================================
#Data files for calculations
#==========================================================================================================
census_tract_data <- read.csv("data-clean/census_tract_data.csv", stringsAsFactors = FALSE)
pb_with_census_tracts <- read.csv("data-clean/prisons_with_census_tracts.csv", stringsAsFactors = FALSE)
pb_with_facility_distances <- read.csv("data-clean/prisons_with_facility_distances.csv", stringsAsFactors = FALSE)

#Until we verify census tract join, importing brownfield with census tracts separately. Eventually will replace code above to only import this file. 
cls <- c(GEOID="character", STATEFP="character", COUNTYFP="character", TRACTCE="character")
bf_with_census_tracts <- read.csv("data-clean/brownfields_with_census_tracts.csv", colClasses=cls, stringsAsFactors = FALSE)


#==========================================================================================================
#UI
#==========================================================================================================
ui <- navbarPage("Carceral Ecologies Proximity Analysis", id="nav",
                 #-----------------------------------------------------------------------------------------
                 #Front Page
                 #-----------------------------------------------------------------------------------------
                 tabPanel("Interactive Map", #tab panels will appear in the upper navigation
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              leafletOutput("dist_plot", width="100%", height="100%"),
                              #The absolute panel will display user input options
                              absolutePanel(id = "controls", 
                                            class = "panel panel-default", 
                                            fixed = TRUE,
                                            draggable = TRUE, 
                                            top = 100, 
                                            left = 20, 
                                            right = "auto", 
                                            bottom = "auto",
                                            width = 330, 
                                            height = "auto",
                                            
                                            h2("Filter Carceral Facilities"),
                                            
                                            #When the user selects a state, the app will zoom to that portion of the map and display the site data for that state
                                            pickerInput(inputId = "state", 
                                                        label = "Select a State", 
                                                        choices = sort(unique(pb$STATE)), 
                                                        options = list(
                                                          `actions-box` = TRUE,
                                                          `live-search` = TRUE
                                                        ),
                                                        multiple = FALSE),
                                            #A user can search for a carceral facility in the selected state. This will include a dropdown and autocomplete search option. 
                                            pickerInput(inputId = "name", 
                                                        label = "Search for Carceral Facility in State", 
                                                        choices = NULL, 
                                                        options = list(
                                                          `actions-box` = TRUE,
                                                          `live-search` = TRUE
                                                        ), 
                                                        multiple = FALSE),
                                            actionButton("search", "Search"),
                                            hr(),
                                            
                                            splitLayout(p("Filter to"), 
                                                        #A user can filter to carceral facilities with certain status
                                                        pickerInput(inputId = "status", 
                                                                    label = NULL, 
                                                                    choices = NULL,
                                                                    options = list(
                                                                      `actions-box` = TRUE
                                                                    ), 
                                                                    multiple = TRUE),
                                                        p("carceral facilities"),
                                                        cellWidths = c("15%", "50%", "35%")
                                                        ),
                                            splitLayout(p("of type"),
                                                        #A user can filter to certain types of carceral facilities 
                                                        pickerInput(inputId = "type", 
                                                                    label = NULL, 
                                                                    choices = NULL, 
                                                                    options = list(
                                                                      `actions-box` = TRUE,
                                                                      `live-search` = TRUE
                                                                    ), 
                                                                    multiple = TRUE),
                                                        cellWidths = c("25%", "75%")
                                                        ),
                                            splitLayout(p("with a capacity of at least"),
                                                        #A user can filter to a carceral facility capacity
                                                        numericInput("capacity", 
                                                                     label=NULL, 
                                                                     value = NULL, 
                                                                     step = 1),
                                                        cellWidths = c("60%", "40%")
                                                        ),
                                            splitLayout(p("with at least"),
                                                        numericInput("threshold", label = NULL, value = 5, step = 1),
                                                        pickerInput(inputId = "site_type",
                                                                    label = "",
                                                                    choices = unique(pb_sf_with_facility_distances$FACILITY_TYPE), 
                                                                    selected = unique(pb_sf_with_facility_distances$FACILITY_TYPE), 
                                                                    options = list(
                                                                      `actions-box` = TRUE
                                                                    )
                                                                    ), 
                                                        cellWidths = c("30%", "25%", "45%"), 
                                                        cellArgs = list(style = "vertical-align: top")
                                                      ),
                                            splitLayout(p("within"),
                                                        sliderInput("proximity_val", NULL, min = 0, max = 5000, value = 1000, step = 1000),
                                                        p("meters"),
                                                        cellWidths = c("20%", "60%", "20%")
                                                        ),
                                            actionButton("apply_filters", "Apply"),
                                            p("* Note that capacity field is missing for 25% of carceral facilities"),
                                            ),
                              
                              tags$div(id="cite",
                                tags$a(href = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis", "Carceral Ecologies GitHub Repo")
                              )
                          )), 
                 #-----------------------------------------------------------------------------------------
                 #Second Page
                 #-----------------------------------------------------------------------------------------
                 #Data explorer page
                 tabPanel("Data Explorer",
                          div(tags$head(includeCSS("styles.css")),
                              sidebarLayout(
                                            #----------------------------------------------------------------------------
                                            #Inputs
                                            #----------------------------------------------------------------------------
                                            sidebarPanel(id = "controls", 
                                                         class = "panel panel-default", 
                                                         top = 100, 
                                                         left = 20, 
                                                         right = "auto", 
                                                         bottom = "auto",
                                                         width = 2,
                                                   
                                                         h2("Filter Carceral Facilities"),
                                                         
                                                         #When the user selects a state, the app will zoom to that portion of the map and display the site data for that state
                                                         pickerInput(inputId = "state2", 
                                                                     label = "Select a State", 
                                                                     choices = sort(unique(pb_sf$STATE)), 
                                                                     selected = unique(pb_sf$STATE),
                                                                     options = list(
                                                                       `actions-box` = TRUE,
                                                                       `live-search` = TRUE
                                                                     ),
                                                                     multiple = TRUE),
                                                         
                                                         #A user can filter to carceral facilities with certain status
                                                         pickerInput(inputId = "status2", 
                                                                     label = "Carceral Facility Status", 
                                                                     choices = sort(unique(pb_sf$STATUS)),
                                                                     selected = unique(pb_sf$STATUS),
                                                                     options = list(
                                                                       `actions-box` = TRUE
                                                                     ),
                                                                     multiple = TRUE),
                                                   
                                                         #A user can filter to certain types of carceral facilities 
                                                         pickerInput(inputId = "type2", 
                                                                     label = "Carceral Facility Type", 
                                                                     choices = sort(unique(pb_sf$TYPE)),
                                                                     selected = unique(pb_sf$TYPE),
                                                                     options = list(
                                                                       `actions-box` = TRUE,
                                                                       `live-search` = TRUE
                                                                     ), 
                                                                     multiple = TRUE),
                                                         
                                                         #A user can filter to a carceral facilities capacity
                                                         numericInput("capacity2", 
                                                                      "Carceral facilities with capacities greater than or equal to", 
                                                                      max = max(pb_sf$CAPACITY), 
                                                                      value = min(pb_sf$CAPACITY)),
                                                         p("* Note that capacity field is missing for 25% of carceral facilities")
                                                         ),
                                            #----------------------------------------------------------------------------
                                            #Main Panel
                                            #----------------------------------------------------------------------------                                          
                                            mainPanel(width = 10,
                                                      infoBoxOutput("carceral_facilities", width = 3), #display total number of carceral facilities 
                                                      infoBoxOutput("num_pb_bf", width = 3), #display number of carceral facilities with brownfield in census tract
                                                      infoBoxOutput("percent_pb_bf", width = 3), #display percent of carceral facilities with brownfield in census tract
                                                      infoBoxOutput("percent_pb_five_bf", width = 3), #display percent of carceral facilities with five or more brownfields in census tract
                                                      infoBoxOutput("total_capacity", width = 3), #display total capacity across filtered carceral facilities
                                                      infoBoxOutput("total_capacity_pb_bf", width = 3), #display total capacity across carceral facilities with brownfield in census tract
                                                      infoBoxOutput("percent_capacity_pb_bf", width = 3), #display percent of capacity across carceral facilities with brownfield in census tract
                                                      infoBoxOutput("missing_capacity", width = 3), #display number of carceral facilities with missing capacity
                                                      #infoBoxOutput("num_census_pb", width = 3),
                                                      #infoBoxOutput("num_census_bf", width = 3),
                                                      #infoBoxOutput("num_census", width = 3),
                                                      #infoBoxOutput("num_census_pb_bf", width = 3),
                                                      
                                                      tabPanel("Plot", 
                                                               box(plotOutput("pb_bf_frequency"), 
                                                                   width = 12 #plot the distribution of census tract brownfields counts across carceral facilities
                                                               )),
                                                      tabPanel("Table", 
                                                               box(DT::dataTableOutput("pb_most_bf"), 
                                                                   width = 12 #output table of carceral facilities sorted according to most brownfields in census tract
                                                               ))
                                                      )
                                            )
                              )
                        )
            )

#==========================================================================================================
#Server
#==========================================================================================================
server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------------------
  #Icons
  #-----------------------------------------------------------------------------------------
  
  #carceral facility icon
  icon_pb <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true", 
                   iconWidth = 20, 
                   iconHeight = 20)
  
  #selected carceral facility icon (red)
  icon_pb_selected <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642-red.png?raw=true", 
                            iconWidth = 20, 
                            iconHeight = 20)
  
  #brownfields icon
  icon_bf <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true", 
                   iconWidth = 20, 
                   iconHeight = 20)
  
  #superfund sites icon
  icon_sfs <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true", 
                    iconWidth = 20, 
                    iconHeight = 20)
  
  #airports icon
  icon_ap <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true", 
                   iconWidth = 20, 
                   iconHeight = 20)
  
  #military sites icon
  icon_mil <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true", 
                    iconWidth = 20, 
                    iconHeight = 20)
  
  #tri sites icon
  icon_tri <- icons(iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g335.png?raw=true", 
                    iconWidth = 20, 
                    iconHeight = 20)
  
  #-----------------------------------------------------------------------------------------
  #Populate filters when state selected
  #----------------------------------------------------------------------------------------- 
  
  #The app will observe when a user selects a new state and list the names of the carceral facilities associated with that state in the carceral facility search dropdown
  observeEvent(input$state, {
    
    pb_sf_filtered <- 
      pb_sf %>% 
      filter(STATE == input$state) #filter carceral facilities df to selected state
    
    carceral_facility_list <- setNames(pb_sf_filtered$FID, pb_sf_filtered$NAME) #create a named vector with carceral facility names specifying their ids
    updatePickerInput(session, 
                      inputId = "name", 
                      choices = carceral_facility_list, 
                      selected = NULL) #update the update picker input with the carceral facility names for that state
    updatePickerInput(session, 
                      inputId = "status", 
                      choices = sort(unique(pb_sf_filtered$STATUS)), 
                      selected = sort(unique(pb_sf_filtered$STATUS))) #update the update picker input with the carceral facility status for that state
    updatePickerInput(session, 
                      inputId = "type", 
                      choices = sort(unique(pb_sf_filtered$TYPE)), 
                      selected = sort(unique(pb_sf_filtered$TYPE))) #update the update picker input with the carceral facility types for that state
    updateNumericInput(session, 
                       inputId = "capacity", 
                       min = min(pb_sf_filtered$CAPACITY), 
                       max = max(pb_sf_filtered$CAPACITY), 
                       value = min(pb_sf_filtered$CAPACITY)) #update the update picker input with the capcities for that state
  })
  
  #-----------------------------------------------------------------------------------------
  #Create map
  #----------------------------------------------------------------------------------------- 
  
  output$dist_plot <- renderLeaflet({
                                      #Filter all site dfs to the selected state
                                      bf_filtered <- 
                                        bf_sf %>%
                                        filter(STATE_CODE == input$state)
                                      sfs_filtered <- 
                                        sfs_sf %>%
                                        filter(STATE_CODE == input$state)
                                      pb_sf_filtered <- 
                                        pb_sf %>%
                                        filter(STATE == input$state)
                                      ap_sf_filtered <- 
                                        ap_sf %>%
                                        filter(state_post_office_code == input$state)
                                      mil_sf_filtered <- 
                                        mil_sf %>%
                                        filter(STATE_CODE == input$state)
                                      tri_sf_filtered <- 
                                        tri_sf %>%
                                        filter(X8..ST == input$state)
    
                                        #Create a legend displaying icons and labels
                                        html_legend <- 
                                        "<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true' style='width:20px;height:20px; margin:5px;'>Carceral Facilities<br/>
                                        <img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true' style='width:20px;height:20px; margin:5px;'>Brownfields<br/>
                                        <img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true' style='width:20px;height:20px; margin:5px;'>Superfund Sites<br/>
                                        <img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true' style='width:20px;height:20px; margin:5px;'>Airports<br/>
                                        <img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true' style='width:20px;height:20px; margin:5px;'>Military Sites<br/>
                                        <img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g335.png?raw=true' style='width:20px;height:20px; margin:5px;'>TRI Facilities (reporting in 2018)"
                                        
                                        if (nrow(bf_filtered) < 1 |
                                            nrow(sfs_filtered) < 1 |
                                            nrow(pb_sf_filtered) < 1 |
                                            nrow(ap_sf_filtered) < 1 |
                                            nrow(mil_sf_filtered) < 1 |
                                            nrow(tri_sf_filtered) < 1 ) { showModal(modalDialog(title = "Heads up!",
                                                                                              "Please select a different state.."
                                        ))}
                                        #Create map and add markers for each site
                                        else { leaflet() %>%
                                                #setView(lat = 40.7, lng = -100.0, zoom = 3) %>%
                                                addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
                                                addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
                                                addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>% 
                                                addMarkers(clusterId = "tri",
                                                           clusterOptions = markerClusterOptions(),
                                                           data = tri_sf_filtered, 
                                                           icon = icon_tri,
                                                           label = ~X4..FACILITY.NAME, 
                                                           group = "TRI Facilities") %>%
                                                addMarkers(clusterId = "airports",
                                                           clusterOptions = markerClusterOptions(),
                                                           data = ap_sf_filtered, 
                                                           icon = icon_ap,
                                                           label = ~fac_name, 
                                                           group = "Airports") %>%
                                                addMarkers(clusterId = "military",
                                                           clusterOptions = markerClusterOptions(),
                                                           data = mil_sf_filtered, 
                                                           icon = icon_mil,
                                                           label = ~SITE_NAME, 
                                                           group = "Military Sites") %>%
                                                addMarkers(clusterId = "brownfields",
                                                           clusterOptions = markerClusterOptions(),
                                                           data = bf_filtered, 
                                                           icon = icon_bf,
                                                           label = ~PRIMARY_NAME,
                                                           group = "Brownfields") %>%
                                                addMarkers(clusterId = "superfund",
                                                           clusterOptions = markerClusterOptions(),
                                                           data = sfs_filtered, 
                                                           icon = icon_sfs,
                                                           label = ~PRIMARY_NAME,
                                                           group = "Superfund Sites") %>%
                                                addMarkers(data = pb_sf_filtered, 
                                                           icon = icon_pb,
                                                           label = ~NAME, 
                                                           group = "Carceral Facilities", 
                                                           layerId = ~FID) %>%
                                                #Add controls to turn layers on and off
                                                addLayersControl(baseGroups = c("CartoDB.Positron", 
                                                                                 "Esri.WorldImagery", 
                                                                                 "OpenStreetMap"),
                                                                  overlayGroups=c("Carceral Facilities", 
                                                                                  "Brownfields", 
                                                                                  "Superfund Sites", 
                                                                                  "Airports", 
                                                                                  "Military Sites", 
                                                                                  "TRI Facilities"),
                                                                  options=layersControlOptions(collapsed=TRUE)
                                                                  ) %>%
                                                addControl(html = html_legend, position = "bottomright") %>% 
                                                hideGroup("Airports") %>% 
                                                hideGroup("Brownfields") %>% 
                                                hideGroup("Superfund Sites") %>% 
                                                hideGroup("Military Sites") %>%
                                                hideGroup("TRI Facilities")
                                        }
  })
  
  #-----------------------------------------------------------------------------------------
  #Set Pop-up
  #-----------------------------------------------------------------------------------------
  
  #When a user clicks on a carceral facility, this popup will display at the lat lng of the click with information about the carceral facility clicked on.
  show_carceral_facility_popup <- function(carceral_facility, lat, lng){
                                    
                                  selected_carceral_facility <- 
                                    pb_sf %>%
                                    filter(pb_sf$FID == carceral_facility) #Filter the carceral facility df to the clicked on carceral facility
                                  
                                  #Generate text about the carceral facility for the popup, including distance calculations
                                  content <- as.character(tagList(
                                                          tags$h4(selected_carceral_facility$NAME),
                                                          tags$strong(sprintf("%s", selected_carceral_facility$ADDRESS),
                                                                      tags$br(),
                                                                      sprintf("%s, %s %s",
                                                                              selected_carceral_facility$CITY, 
                                                                              selected_carceral_facility$STATE, 
                                                                              selected_carceral_facility$ZIP)),
                                                          tags$br(),
                                                          sprintf("Type: %s", selected_carceral_facility$TYPE),
                                                          tags$br(),
                                                          sprintf("Status: %s", selected_carceral_facility$STATUS),
                                                          tags$br(),
                                                          sprintf("Capacity: %s", selected_carceral_facility$CAPACITY),
                                                          tags$br(),
                                                          sprintf("Population: %s", selected_carceral_facility$POPULATION),
                                                          tags$br(),
                                                          sprintf("Number within %s meters:", input$proximity_val),
                                                          tags$br(),
                                                          sprintf("Superfund sites: %s", 
                                                                  pb_sf_with_facility_distances %>% 
                                                                    filter(FID == carceral_facility & FACILITY_TYPE == "Superfund Site" & DISTANCES <= input$proximity_val) %>%
                                                                    nrow()),
                                                          tags$br(),
                                                          sprintf("Airports: %s", 
                                                                  pb_sf_with_facility_distances %>% 
                                                                    filter(FID == carceral_facility & FACILITY_TYPE == "Airport" & DISTANCES <= input$proximity_val) %>%
                                                                    nrow()),
                                                          tags$br(),
                                                          sprintf("Military sites: %s", 
                                                                  pb_sf_with_facility_distances %>% 
                                                                    filter(FID == carceral_facility & FACILITY_TYPE == "Military Base" & DISTANCES <= input$proximity_val) %>%
                                                                    nrow()),
                                                          tags$br(),
                                                          sprintf("TRI facilities: %s", 
                                                                  pb_sf_with_facility_distances %>% 
                                                                    filter(FID == carceral_facility & FACILITY_TYPE == "TRI Facility" & DISTANCES <= input$proximity_val) %>%
                                                                    nrow())
                                                        ))
                                  
                                  #Add popup to the map
                                  leafletProxy("dist_plot") %>% 
                                    addPopups(lng, lat, content, layerId = carceral_facility)
                                  }
  
  #When a user clicks on the map, the current popups will be cleared and the new popup will be generated. 
  observe({ 
            leafletProxy("dist_plot") %>% 
            clearPopups()
    
            event <- 
              input$dist_plot_marker_click
    
            if (is.null(event)) return()
    
            isolate({
              show_carceral_facility_popup(event$id, 
                                           event$lat, 
                                           event$lng)})
  })
  
  #-----------------------------------------------------------------------------------------
  #Highlight Carceral Facility on Search
  #-----------------------------------------------------------------------------------------
  
  # This variable will store the row in the carceral facility df associated with the *previous* carceral facility selected in the carceral facility name search/dropdown (and marked in red on the map). We keep track of this row because when a new carceral facility is selected from the search/dropdown, we will need to replace the red marker on the map with a blue marker. 
  prev_carceral_facility <- reactiveVal()
  
  #This function will observe when a user selects a new name from the carceral facility name search/dropdown, and recolor that carceral facility's marker red on the map.  
  #Note that at times, with user input, the map reloads, replacing a currently red marker with a blue one, without the list of names changing. 
  #For instance, if a user selects a carceral facility type, but the currently selected name is still within the selected types, the map will reload without the names changing, turning all markers back to blue. 
  #We need to also watch for changes to other user inputs so that this event always gets triggered post-map reload.
  observeEvent(input$search, {
    
    #Filter the carceral facility df to the row with the selected name. Note that because we populated the carceral facility name dropdown as a named vector above (with carceral facility names naming FIDs), we are actuallly filtering to the row with the FID that equals the user input. 
    row_selected <- pb_sf %>%
      filter(pb_sf$FID == input$name)
    
    #Create a map object for controlling a map that's already been rendered.
    proxy <- leafletProxy('dist_plot')
    
    #Add red marker for selected row (carceral facility) to the map
    proxy %>%
      addMarkers(data = row_selected,
                 icon = icon_pb_selected,
                 label = ~row_selected$NAME,
                 group = "Carceral Facilities",
                 layerId = ~row_selected$FID)
    
    #Reset previously selected marker to blue
    #First check to make sure there was a previous carceral facility as there will not be a previous marker to reset on initial run of map
    if(!is.null(prev_carceral_facility())){ 
       if (row_selected != prev_carceral_facility()) {proxy %>%
                                                          addMarkers(data = prev_carceral_facility(),
                                                                    layerId = ~prev_carceral_facility()$FID,
                                                                    icon = icon_pb)

      }
    }
    
    #Set the selected row (carceral facility) to be the next previous carceral facility. 
    prev_carceral_facility(row_selected)
  })
  
  #-----------------------------------------------------------------------------------------
  #Apply Filters
  #-----------------------------------------------------------------------------------------
  
  observeEvent(input$apply_filters, {
    
    apply_filters_fids <- pb_with_facility_distances %>%
                          filter(STATUS %in% input$status & 
                                 STATE == input$state & 
                                 TYPE %in% input$type & 
                                 CAPACITY >= input$capacity) %>%
                          filter(FACILITY_TYPE == input$site_type) %>%
                          filter(DISTANCES <= input$proximity_val) %>%
                          filter(PROXIMITY_INDEX >= input$threshold) %>%
                          distinct(FID) %>%
                          pull()
    
    pb_sf_filtered <- pb_sf %>%
                      filter(FID %in% apply_filters_fids)
    
    if (nrow(pb_sf_filtered) < 1) { showModal(modalDialog(title = "Heads up!",
                                                          "No facilities meet this criteria."
                                                          ))}
    else {
          #Create a map object for controlling a map that's already been rendered.
          proxy <- leafletProxy('dist_plot')
          
          #Add markers to the map
          proxy %>%
            clearMarkers() %>%
            addMarkers(data = pb_sf_filtered,
                       icon = icon_pb,
                       label = ~pb_sf_filtered$NAME,
                       group = "Carceral Facilities",
                       layerId = ~pb_sf_filtered$FID)
          
          carceral_facility_list <- setNames(pb_sf_filtered$FID, pb_sf_filtered$NAME) #create a named vector with carceral facility names specifying their ids
          updatePickerInput(session, 
                            inputId = "name", 
                            choices = carceral_facility_list, 
                            selected = NULL) #update the update picker input with the carceral facility names for that capacity
        }
  })
  
  #-----------------------------------------------------------------------------------------
  #Data Explorer
  #-----------------------------------------------------------------------------------------
  
  #Calculate number of carceral facilities
  output$carceral_facilities <- renderInfoBox({
                                pb_rows <- pb_with_census_tracts %>%
                                           filter(STATE %in% input$state2 & 
                                                  STATUS %in% input$status2 & 
                                                  TYPE %in% input$type2 & 
                                                  CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
                                            nrow()
                    
                                infoBox('Number of carceral facilities', pb_rows, color = "olive")
                                })
  
  #Calculate number of carceral facilities with a brownfield in its census tract
  output$num_pb_bf <- renderInfoBox({
                      num_pb_bf <- pb_with_census_tracts %>% 
                                   filter(!is.na(BF_COUNT) & 
                                          STATE %in% input$state2 & 
                                          STATUS %in% input$status2 &
                                          TYPE %in% input$type2 & 
                                          CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus rows with non-null brownfield count
                                    nrow()
    
                      infoBox('Number of carceral facilities with brownfields in census tract', num_pb_bf, color = "olive")
                      })
  
  #Calculate percent of carceral facilities with a brownfield in census tract
  output$percent_pb_bf <- renderInfoBox({
                          pb_rows <- pb_with_census_tracts %>%
                                     filter(STATE %in% input$state2 & 
                                            STATUS %in% input$status2 & 
                                            TYPE %in% input$type2 & 
                                            CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
                                      nrow()
    
                          per_pb_bf <- pb_with_census_tracts %>% 
                                       filter(!is.na(BF_COUNT) & 
                                              STATE %in% input$state2 & 
                                                STATUS %in% input$status2 & 
                                                TYPE %in% input$type2 & 
                                                CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus rows with non-null brownfield count 
                                        nrow()/pb_rows*100 #calculate rows and then divide by total number of carceral facilities
    
                          per_pb_bf <- paste(as.character(round(per_pb_bf, 2)), '%', sep="") #Round to two decimal places
    
                          infoBox('Percent of carceral facilities with brownfields in census tract', per_pb_bf, color = "olive")
                          })
  
  #Calculate percent of carceral facilities with five or more brownfields in census tract
  output$percent_pb_five_bf <- renderInfoBox({
                               pb_rows <- pb_with_census_tracts %>%
                                          filter(STATE %in% input$state2 & 
                                                 STATUS %in% input$status2 & 
                                                 TYPE %in% input$type2 &
                                                 CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
                                          nrow()
    
                                per_pb_five_bf <- pb_with_census_tracts %>% 
                                                  filter(!is.na(BF_COUNT) & 
                                                         BF_COUNT >= 5 & 
                                                         STATE %in% input$state2 & 
                                                         STATUS %in% input$status2 & 
                                                         TYPE %in% input$type2 & 
                                                         CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus rows with non-null brownfield count plus rows with 5 or more brownfields 
                                                  nrow()/pb_rows*100 #calculate rows and then divide by total number of carceral facilities
                                
                                per_pb_five_bf <- paste(as.character(round(per_pb_five_bf, 2)), '%', sep="") #Round to two decimal places
                                
                                infoBox('Percent of carceral facilities with five or more brownfields in census tract', per_pb_five_bf, color = "olive")
                                })
  
  #Calculate capacity across carceral facilities
  output$total_capacity <- renderInfoBox({
                           pb_capacity <- pb_with_census_tracts %>%
                                          filter(CAPACITY != -999 & 
                                                 STATE %in% input$state2 &
                                                 STATUS %in% input$status2 & 
                                                 TYPE %in% input$type2 & 
                                                 CAPACITY >= input$capacity2) %>%  #Filter to selected user inputs plus non-null capacities
                                          summarize(total_capacity = sum(CAPACITY)) #sum capacity
                            
                            infoBox('Total Capacity of Carceral Facilities', pb_capacity$total_capacity, color = "olive")
                            })
  
  #Calculate capacity of carceral facilities with a brownfield in its census tract
  output$total_capacity_pb_bf <- renderInfoBox({
                                 capacity_pb_bf <- pb_with_census_tracts %>% 
                                                   filter(CAPACITY != -999 & 
                                                          !is.na(BF_COUNT) & 
                                                          STATE %in% input$state2 & 
                                                          STATUS %in% input$status2 & 
                                                          TYPE %in% input$type2 & 
                                                          CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus non-null capacities plus non-null brownfield count
                                                    summarize(total_capacity = sum(CAPACITY)) #sum capacity
                                  
                                  infoBox('Total capacity of carceral facilities with brownfields in census tract', capacity_pb_bf$total_capacity, color = "olive")
                                  })
  
  #Calculate percent of carceral facilities with a brownfield in census tract
  output$percent_capacity_pb_bf <- renderInfoBox({
                                   pb_capacity <- pb_with_census_tracts %>%
                                                  filter(CAPACITY != -999 & 
                                                         STATE %in% input$state2 & 
                                                         STATUS %in% input$status2 & 
                                                         TYPE %in% input$type2 & 
                                                         CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus non-null capacities
                                                  summarize(total_capacity = sum(CAPACITY)) #sum capacity
                                  
                                   per_capacity_pb_bf <- pb_with_census_tracts %>% 
                                                        filter(CAPACITY != -999 & 
                                                               !is.na(BF_COUNT) & 
                                                               STATE %in% input$state2 & 
                                                               STATUS %in% input$status2 & 
                                                               TYPE %in% input$type2 & 
                                                               CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus non-null capacities plus non-null brownfield count
                                                        summarize(percent_capacity = sum(CAPACITY)/pb_capacity$total_capacity*100) #calculate total capacity of carceral facilities in census tract with brownfield and then divide by total capacity across all carceral facilities
                                  
                                   per_capacity_pb_bf <- paste(as.character(round(per_capacity_pb_bf$percent_capacity, 2)), '%', sep="") #Round to two decimal places
                                  
                                   infoBox('Percent of total capacity in carceral facilities with brownfields in census tract', per_capacity_pb_bf, color = "olive")
                                   })
  
  #Calculate percent of carceral facilities with five or more brownfields in census tract
  output$missing_capacity <- renderInfoBox({
                             pb_rows <- pb_with_census_tracts %>%
                                        filter(STATE %in% input$state2 & 
                                               STATUS %in% input$status2 & 
                                               TYPE %in% input$type2 & 
                                               CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
                                        nrow()
    
                             missing_capacity_rows <- pb_with_census_tracts %>%
                                                      filter(CAPACITY == -999 & 
                                                             STATE %in% input$state2 & 
                                                             STATUS %in% input$status2 & 
                                                             TYPE %in% input$type2) %>% #Filter to selected user inputs plus rows with capacity missing
                                                      nrow()/pb_rows*100
                              
                             missing_capacity_rows <- paste(as.character(round(missing_capacity_rows, 2)), '%', sep="") #Round to two decimal places
                              
                             infoBox('Percent of carceral facilities with missing capacity', missing_capacity_rows, color = "yellow")
                             })
  
  output$num_census_bf <- renderInfoBox({
                          census_bf_rows <- bf_with_census_tracts %>%
                                            count(GEOID) %>%
                                            filter(n > 1) %>%
                                            nrow()
    
                          infoBox('Number of census tracts with 2 brownfields', census_bf_rows, color = "yellow")
                          })
  
  output$num_census_pb <- renderInfoBox({
                          census_pb_rows <- pb_with_census_tracts %>%
                                            count(GEOID) %>%
                                            filter(n > 0) %>%
                                            nrow()
    
                          infoBox('Number of census tracts with a carceral facility', census_pb_rows, color = "yellow")
                          })
  
  output$num_census_pb_bf <- renderInfoBox({
                             census_pb_bf <- census_tract_data %>%
                                             filter(!is.na(BF_COUNT) & 
                                                    BF_COUNT > 0 & 
                                                    !is.na(PB_COUNT)) %>%
                                             nrow()
    
                             infoBox('Number of census tracts with a carceral facility and 2 brownfields', census_pb_bf, color = "yellow")
                             })
  
  output$num_census <- renderInfoBox({
                       census_bf_rows <- bf_with_census_tracts %>%
                                         count(GEOID) %>%
                                         filter(n > 0) %>%
                                         nrow()
    
                       census_pb_bf <- census_tract_data %>%
                                       filter(!is.na(BF_COUNT) & 
                                              BF_COUNT > 0 & 
                                              !is.na(PB_COUNT)) %>%
                                       nrow()
    
                       census_rows <-  census_pb_bf / census_bf_rows * 100
    
                       infoBox('Percentage of census tracts with two brownfields that also have a carceral facility', census_rows, color = "yellow")
                       })
  
  #Data table of carceral facilities with brownfields in census tract, sorted according to number of brownfields
  output$pb_most_bf <- DT::renderDataTable(
    
                       pb_with_census_tracts %>%
                         filter(!is.na(BF_COUNT) & 
                                STATE %in% input$state2 & 
                                STATUS %in% input$status2 & 
                                TYPE %in% input$type2 &
                                CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
                          select(NAME, CITY, STATE, GEOID, TYPE, POPULATION, CAPACITY, BF_COUNT) %>%
                          arrange(desc(BF_COUNT))
                       
                        )
  
  #Frequency plot displaying distribution of brownfield census tract counts across carceral facilities
  output$pb_bf_frequency <- renderPlot(
    
                            pb_with_census_tracts %>%
                              filter(!is.na(BF_COUNT) & 
                                     STATE %in% input$state2 & 
                                     STATUS %in% input$status2 & 
                                     TYPE %in% input$type2 & 
                                     CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
                              ggplot(aes(x = BF_COUNT, col = TYPE)) +
                              geom_freqpoly(binwidth = 1) +
                              theme_bw() +
                              labs(x = "Number of Brownfields in Census Tract", y = "Number of Carceral Facilities")
                            
                              )
  
}

#==========================================================================================================
#Run App
#==========================================================================================================
shinyApp(ui, server)

