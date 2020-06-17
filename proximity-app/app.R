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

#Read prison Shapefile
pb <- st_read("data-clean/Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Thanks Ben!
#Convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

#Convert the unique ID for prisons to be a string
pb_sf$FID <- as.character(pb_sf$FID)
pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids

#Read military bases data file
mil <- st_read("data-clean/military_bases.csv", stringsAsFactors = FALSE) 
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read brownfield data file and convert to sf
bf <- read.csv("data-clean/brownfields.csv", stringsAsFactors = FALSE)
bf <- bf %>% filter(!is.na(LATITUDE83))
bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Until we verify census tract join, importing brownfield with census tracts separately. Eventually will replace code above to only import this file. 
cls <- c(GEOID="character", STATEFP="character", COUNTYFP="character", TRACTCE="character")
bf_with_census_tracts <- read.csv("data-clean/brownfields_with_census_tracts.csv", colClasses=cls, stringsAsFactors = FALSE)

#Read airport data file and convert to sf
ap <- read.csv("data-clean/airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read superfund sites data file and convert to sf
sfs <- read.csv("data-clean/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LATITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Read TRI sites data file and convert to sf
tri <- read.csv("data-clean/tri.csv", stringsAsFactors = FALSE) 
tri <- tri %>% filter(!is.na(X12..LATITUDE))
tri_sf <- st_as_sf(tri, coords = c("X13..LONGITUDE", "X12..LATITUDE"), crs = pb_crs, na.fail = FALSE)

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


#==========================================================================================================
#UI
#==========================================================================================================
ui <- navbarPage("Proximity Analysis", id="nav",
                 
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
                                            
                                            h2("Prison Explorer"),
                                            
                                            #When the user selects a state, the app will zoom to that portion of the map and display the site data for that state
                                            pickerInput(
                                              inputId = "state", 
                                              label = "Select a State", 
                                              choices = sort(unique(pb$STATE)), 
                                              options = list(
                                                `actions-box` = TRUE,
                                                `live-search` = TRUE
                                              ),
                                              multiple = FALSE),
                                            #A user can search for a prison in the selected state. This will include a dropdown and autocomplete search option. 
                                            pickerInput(
                                              inputId = "name", 
                                              label = "Search for Prison in State", 
                                              choices = NULL, 
                                              options = list(
                                                `actions-box` = TRUE,
                                                `live-search` = TRUE
                                              ), 
                                              multiple = FALSE),
                                            #A user can filter to prisons with certain status
                                            pickerInput(
                                              inputId = "status", 
                                              label = "Filter to Prison Status", 
                                              choices = NULL,
                                              options = list(
                                                `actions-box` = TRUE
                                              ), 
                                              multiple = TRUE),
                                            #A user can filter to certain types of prisons 
                                            pickerInput(
                                              inputId = "type", 
                                              label = "Filter to Prison Types", 
                                              choices = NULL, 
                                              options = list(
                                                `actions-box` = TRUE,
                                                `live-search` = TRUE
                                              ), 
                                              multiple = TRUE),
                                            #A user can filter to a prison capacity
                                            numericInput("capacity", "Filter to prisons with capacities greater than or equal to", value = NULL, step = 1),
                                            p("* Note that capacity field is missing for 25% of prisons"),
                                            #A user can select the distance at which proximity calculations will be performed.
                                            sliderInput("proximity_val", "Set proximity (in miles):", min = 0, max = 10, value = 1, step = 1),
                                            selectInput("bmap", "Base map tile provider", choices =
                                                          c("CartoDB.Positron",
                                                            "Esri.WorldImagery",
                                                            "OpenStreetMap")    #A user can select the basemap.
                                                        
                                            )),
                              
                              tags$div(id="cite",
                                       'fill sources'
                              )
                          )), 
                 #Data explorer page
                 tabPanel("Data Explorer",
                          div(
                            tags$head(
                              # Include our custom CSS
                              includeCSS("styles.css")
                            ),
                            
                            sidebarLayout(
                              sidebarPanel(id = "controls", 
                                           class = "panel panel-default", 
                                           top = 100, 
                                           left = 20, 
                                           right = "auto", 
                                           bottom = "auto",
                                           width = 2,
                                           
                                           h2("Filters"),
                                           
                                           #When the user selects a state, the app will zoom to that portion of the map and display the site data for that state
                                           pickerInput(
                                             inputId = "state2", 
                                             label = "Select a State", 
                                             choices = sort(unique(pb_with_census_tracts$STATE)), 
                                             selected = unique(pb_with_census_tracts$STATE),
                                             options = list(
                                               `actions-box` = TRUE,
                                               `live-search` = TRUE
                                             ),
                                             multiple = TRUE),
                                           #A user can filter to prisons with certain status
                                           pickerInput(
                                             inputId = "status2", 
                                             label = "Filter to Prison Status", 
                                             choices = sort(unique(pb_with_census_tracts$STATUS)),
                                             selected = unique(pb_with_census_tracts$STATUS),
                                             options = list(
                                               `actions-box` = TRUE
                                             ),
                                             multiple = TRUE),
                                           #A user can filter to certain types of prisons 
                                           pickerInput(
                                             inputId = "type2", 
                                             label = "Filter to Prison Types", 
                                             choices = sort(unique(pb_with_census_tracts$TYPE)),
                                             selected = unique(pb_with_census_tracts$TYPE),
                                             options = list(
                                               `actions-box` = TRUE,
                                               `live-search` = TRUE
                                             ), 
                                             multiple = TRUE),
                                           #A user can filter to a prison capacity
                                           numericInput("capacity2", "Filter to prisons with capacities greater than or equal to", max = max(pb_with_census_tracts$CAPACITY), value = min(pb_with_census_tracts$CAPACITY)),
                                           p("* Note that capacity field is missing for 25% of prisons")
                              ),
                              mainPanel(
                                width = 10,
                                infoBoxOutput("prisons", width = 3), #display total number of prisons 
                                infoBoxOutput("num_pb_bf", width = 3), #display number of prisons with brownfield in census tract
                                infoBoxOutput("percent_pb_bf", width = 3), #display percent of prisons with brownfield in census tract
                                infoBoxOutput("percent_pb_five_bf", width = 3), #display percent of prisons with five or more brownfields in census tract
                                infoBoxOutput("total_capacity", width = 3), #display total capacity across filtered prisons
                                infoBoxOutput("total_capacity_pb_bf", width = 3), #display total capacity across prisons with brownfield in census tract
                                infoBoxOutput("percent_capacity_pb_bf", width = 3), #display percent of capacity across prisons with brownfield in census tract
                                infoBoxOutput("missing_capacity", width = 3), #display number of prisons with missing capacity
                                #infoBoxOutput("num_census_pb", width = 3),
                                #infoBoxOutput("num_census_bf", width = 3),
                                #infoBoxOutput("num_census", width = 3),
                                #infoBoxOutput("num_census_pb_bf", width = 3),
                                
                                tabPanel("Plot", 
                                         box(
                                           plotOutput("pb_bf_frequency"), width = 12 #plot the distribution of census tract brownfields counts across prisons
                                         )),
                                tabPanel("Table", 
                                         box(
                                           DT::dataTableOutput("pb_most_bf"), width = 12 #output table of prisons sorted according to most brownfields in census tract
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
  
  #prison icon
  icon_pb <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  #selected prison icon (red)
  icon_pb_selected <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642-red.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  #brownfields icon
  icon_bf <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  #superfund sites icon
  icon_sfs <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  #airports icon
  icon_ap <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  #military sites icon
  icon_mil <- icons(
    iconUrl = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true", 
    iconWidth = 20, iconHeight = 20
  )
  
  #create a reacticve ___ for census tracts
  tracts <- reactiveValues(df_data = NULL)
  
  
  #The app will observe when a user selects a new state and do two things - 1) populate the tracts variable with the census tracts shapefile for the state, and 2) list the names of the prisons associated with that state in the prison search dropdown
  observeEvent(input$state, {
    
    #To keep size down, census tract shapefiles will remain zipped and will only be unzipped when the state is selected. Temporary unzipped files will be stored in a directory called unzipped, which will be deleted and then repopulated each time a new state is selected
    #if (dir.exists("tracts/unzipped")) #check if unzipped directory exists 
    #unlink("tracts/unzipped", recursive = TRUE) #delete directory if it exists
    
    codes_filtered <- codes %>%
      filter(STATE_CODE == input$state) #Filter codes df to selected state
    
    #unzip(codes_filtered$TRACT_ZIP, exdir = "tracts/unzipped") #unzip the census tract shapefile zip for the state into "tracts/unzipped". Note that we created the TRACT_ZIP variable with a mutate function above.  
    #tracts$df_data <- st_read(paste("tracts/unzipped/",codes_filtered$TRACT_FILE, sep = ""), stringsAsFactors = FALSE) #read shapefile into the reactive value we created above
    #tracts$df_data <- st_read(paste(codes_filtered$TRACT_FOLDER, codes_filtered$TRACT_FILE, sep = ""), stringsAsFactors = FALSE) #read shapefile into the reactive value we created above
    #tracts$df_data <- st_transform(tracts$df_data, crs = 4269)
    
    pb_sf_filtered <- pb_sf %>% 
      filter(STATE == input$state) #filter prisons df to selected state
    prison_list <- setNames(pb_sf_filtered$FID, pb_sf_filtered$NAME) #create a named vector with prison names specifying their ids
    updatePickerInput(session, inputId = "name", choices = prison_list, selected = NULL) #update the update picker input with the prison names for that state
    updatePickerInput(session, inputId = "status", choices = sort(unique(pb_sf_filtered$STATUS)), selected = sort(unique(pb_sf_filtered$STATUS))) #update the update picker input with the prison status for that state
    updatePickerInput(session, inputId = "type", choices = sort(unique(pb_sf_filtered$TYPE)), selected = sort(unique(pb_sf_filtered$TYPE))) #update the update picker input with the prison types for that state
    updateNumericInput(session, inputId = "capacity", min = min(pb_sf_filtered$CAPACITY), max = max(pb_sf_filtered$CAPACITY), value = min(pb_sf_filtered$CAPACITY)) #update the update picker input with the capcities for that state
  }, priority = 3)
  
  #The app will observe when a user selects a new type and updates the search select options
  observeEvent(c(input$status, input$type, input$capacity), {
    req(input$status)
    req(input$type)
    req(input$capacity)
    pb_sf_filtered <- pb_sf %>%
      filter(STATE == input$state & STATUS %in% input$status & TYPE %in% input$type & CAPACITY >= input$capacity) #filter prisons df to selected state, type, and capacity
    prison_list <- setNames(pb_sf_filtered$FID, pb_sf_filtered$NAME) #create a named vector with prison names specifying their ids
    updatePickerInput(session, inputId = "name", choices = prison_list, selected = NULL) #update the update picker input with the prison names for that capacity
  }, priority = 0)
  
  output$dist_plot <- renderLeaflet({
    req(input$name) #wait of name input to populate
    #Filter all site dfs to the selected state
    bf_filtered <- bf %>%
      filter(STATE_CODE == input$state)
    sfs_filtered <- sfs %>%
      filter(STATE_CODE == input$state)
    pb_sf_filtered <- pb_sf %>%
      filter(STATUS %in% input$status & STATE == input$state & TYPE %in% input$type & CAPACITY >= input$capacity)
    ap_sf_filtered <- ap_sf %>%
      filter(state_post_office_code == input$state)
    mil_sf_filtered <- mil_sf %>%
      filter(STATE_CODE == input$state)
    
    #Create a legend displaying icons and labels
    html_legend <- "<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true' style='width:20px;height:20px; margin:5px;'>Prisons<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true' style='width:20px;height:20px; margin:5px;'>Brownfields<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true' style='width:20px;height:20px; margin:5px;'>Superfund Sites<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true' style='width:20px;height:20px; margin:5px;'>Airports<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true' style='width:20px;height:20px; margin:5px;'>Military Sites"
    
    #Create map and add markers for each site
    leaflet() %>%
      addProviderTiles(input$bmap) %>%
      # addPolygons(
      #   data = tracts$df_data,
      #   color = "#444444", 
      #   weight = 1, 
      #   smoothFactor = 0.5,
      #   opacity = 1.0, 
      #   fillOpacity = 0.1,
      #   highlightOptions = highlightOptions(color = "white", weight = 2),
      #   group = "Census Tracts"
      # ) %>%
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
        clusterId = "superfund",
        clusterOptions = markerClusterOptions(),
        data = sfs_filtered, 
        icon = icon_sfs,
        ~LONGITUDE83, 
        ~LATITUDE83,
        label = ~PRIMARY_NAME,
        group = "Superfund Sites") %>%
      addMarkers(
        data = pb_sf_filtered, 
        icon = icon_pb,
        label = ~NAME, 
        group = "Prisons", 
        layerId = ~FID) %>%
      addLayersControl(   #Add controls to turn layers on and off
        overlayGroups=c("Prisons", "Brownfields", "Superfund Sites", "Airports", "Military Sites"),
        options=layersControlOptions(collapsed=FALSE)) %>%
      addControl(html = html_legend, position = "bottomright") %>% 
      #hideGroup("Census Tracts") %>%
      hideGroup("Airports") %>% 
      hideGroup("Brownfields") %>% 
      hideGroup("Superfund Sites") %>% 
      hideGroup("Military Sites")
  })
  
  #This function will be used to calculate the number of a given type of site within proximity of a prison. It is called as the showPrisonPopup function (called when a user clicks on a prison on the map) generates text for a popup balloon. It takes as inputs the prison FID and the type of site for which the calculation will be run. The distance at which proximity will be calculated will have been set by the user in the user controls.  
  calculateNumberInProximity <- function(prison, site) {
    
    #Check which type of site the calculation will be run for in order to select the df we will be calculating based on. 
    if (site == "bf")
      toxic_site_sf <- bf_sf
    else if (site == "sfs")
      toxic_site_sf <- sfs_sf
    else if (site == "ap")
      toxic_site_sf <- ap_sf
    else 
      toxic_site_sf <- mil_sf
    
    #Checks whether objects from the selected site type df (stored in toxic_site_sf) are within the user-specified distance to the selected prison. Distance calculations are by default in meters so we multiply the user input (which is in miles) by 1609.34 to convert to meters. 
    in_proximity <- st_is_within_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf, dist = (input$proximity_val*1609.34), sparse = FALSE) 
    
    #Count the number of sites that are in proximity or not NA
    num_in_proximity <- length(in_proximity[in_proximity == TRUE & !is.na(in_proximity)]) 
    
    return(num_in_proximity) #Return the number in proximity
  }
  
  #When a user clicks on a prison, this popup will display at the lat lng of the click with information about the prison clicked on.
  showPrisonPopup <- function(prison, lat, lng) {
    
    selectedPrison <- pb_sf %>%
      filter(pb_sf$FID == prison) #Filter the prison df to the clicked on prison
    
    #Generate text about the prison for the popup, including distance calculations
    content <- as.character(tagList(
      tags$h4(selectedPrison$NAME),
      tags$strong(
        sprintf("%s", selectedPrison$ADDRESS),
        tags$br(),
        sprintf("%s, %s %s",selectedPrison$CITY, selectedPrison$STATE, selectedPrison$ZIP)
      ),
      tags$br(),
      sprintf("Type: %s", selectedPrison$TYPE),
      tags$br(),
      sprintf("Status: %s", selectedPrison$STATUS),
      tags$br(),
      sprintf("Population: %s", selectedPrison$POPULATION),
      tags$br(),
      sprintf("Number within %s miles:", input$proximity_val),
      tags$br(),
      sprintf("Brownfields: %s", calculateNumberInProximity(prison, "bf")),
      tags$br(),
      sprintf("Superfund sites: %s", calculateNumberInProximity(prison, "sfs")),
      tags$br(),
      sprintf("Airports: %s", calculateNumberInProximity(prison, "ap")),
      tags$br(),
      sprintf("Military sites: %s", calculateNumberInProximity(prison, "mil"))
    ))
    
    #Add popup to the map
    leafletProxy("dist_plot") %>% addPopups(lng, lat, content, layerId = prison)
  }
  
  #When a user clicks on the map, the current popups will be cleared and the new popup will be generated. 
  observe({
    leafletProxy("dist_plot") %>% clearPopups()
    event <- input$dist_plot_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPrisonPopup(event$id, event$lat, event$lng)
    })
  })
  
  # This variable will store the row in the prison df associated with the *previous* prison selected in the prison name search/dropdown (and marked in red on the map). We keep track of this row because when a new prison is selected from the search/dropdown, we will need to replace the red marker on the map with a blue marker. 
  prev_prison <- reactiveVal()
  
  #This function will observe when a user selects a new name from the prison name search/dropdown, and recolor that prison's marker red on the map.  
  #Note that at times, with user input, the map reloads, replacing a currently red marker with a blue one, without the list of names changing. 
  #For instance, if a user selects a prison type, but the currently selected name is still within the selected types, the map will reload without the names changing, turning all markers back to blue. 
  #We need to also watch for changes to other user inputs so that this event always gets triggered post-map reload.
  observeEvent(c(input$name, input$status, input$type, input$capacity), {
    req(input$name) #wait for name input to fill
    req(input$status) #wait for status input to fill
    req(input$type) #wait for type input to fill
    req(input$capacity) #wait for capacity input to fill
    
    #Filter the prison df to the row with the selected name. Note that because we populated the prison name dropdown as a named vector above (with prison names naming FIDs), we are actuallly filtering to the row with the FID that equals the user input. 
    row_selected <- pb_sf %>%
      filter(pb_sf$FID == input$name)
    
    #Create a map object for controlling a map that's already been rendered.
    proxy <- leafletProxy('dist_plot')
    
    #Add red marker for selected row (prison) to the map
    proxy %>%
      addMarkers(
        data = row_selected,
        icon = icon_pb_selected,
        label = ~row_selected$NAME,
        group = "Prisons",
        layerId = ~row_selected$FID)
    
    #Reset previously selected marker to blue
    if(!is.null(prev_prison())) #check to make sure there was a previous prison as there will not be a previous marker to reset on initial run of map
    {
      #Check whether prev_prison() has been filtered out since last filter input. If so, we will remove the marker from the map rather than reseting it to blue. 
      if (prev_prison()$STATUS %in% input$status & prev_prison()$TYPE %in% input$type & prev_prison()$CAPACITY >= input$capacity)
        filtered = 0
      else
        filtered = 1 
      
      if (row_selected != prev_prison()) {
        if (filtered == 0) {
          proxy %>%
            addMarkers(
              data = prev_prison(),
              layerId = ~prev_prison()$FID,
              icon = icon_pb)
        }
        else if (filtered == 1) {
          proxy %>%
            removeMarker(
              layerId = prev_prison()$FID)
        }
      }
    }
    #Set the selected row (prison) to be the next previous prison. 
    prev_prison(row_selected)
    
  }, priority = 2)
  
  #Calculate number of prisons
  output$prisons <- renderInfoBox({
    pb_rows <- pb_with_census_tracts %>%
      filter(STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
      nrow()
    
    infoBox('Number of prisons', pb_rows, color = "olive")
  })
  
  #Calculate number of prisons with a brownfield in its census tract
  output$num_pb_bf <- renderInfoBox({
    num_pb_bf <- pb_with_census_tracts %>% 
      filter(!is.na(BF_COUNT) & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus rows with non-null brownfield count
      nrow()
    
    infoBox('Number of prisons with brownfields in census tract', num_pb_bf, color = "olive")
  })
  
  #Calculate percent of prisons with a brownfield in census tract
  output$percent_pb_bf <- renderInfoBox({
    pb_rows <- pb_with_census_tracts %>%
      filter(STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
      nrow()
    
    per_pb_bf <- pb_with_census_tracts %>% 
      filter(!is.na(BF_COUNT) & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus rows with non-null brownfield count 
      nrow()/pb_rows*100 #calculate rows and then divide by total number of prisons
    
    per_pb_bf <- paste(as.character(round(per_pb_bf, 2)), '%', sep="") #Round to two decimal places
    
    infoBox('Percent of prisons with brownfields in census tract', per_pb_bf, color = "olive")
  })
  
  #Calculate percent of prisons with five or more brownfields in census tract
  output$percent_pb_five_bf <- renderInfoBox({
    pb_rows <- pb_with_census_tracts %>%
      filter(STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
      nrow()
    
    per_pb_five_bf <- pb_with_census_tracts %>% 
      filter(!is.na(BF_COUNT) & BF_COUNT >= 5 & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus rows with non-null brownfield count plus rows with 5 or more brownfields 
      nrow()/pb_rows*100 #calculate rows and then divide by total number of prisons
    
    per_pb_five_bf <- paste(as.character(round(per_pb_five_bf, 2)), '%', sep="") #Round to two decimal places
    
    infoBox('Percent of prisons with five or more brownfields in census tract', per_pb_five_bf, color = "olive")
  })
  
  #Calculate capacity across prisons
  output$total_capacity <- renderInfoBox({
    pb_capacity <- pb_with_census_tracts %>%
      filter(CAPACITY != -999 & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>%  #Filter to selected user inputs plus non-null capacities
      summarize(total_capacity = sum(CAPACITY)) #sum capacity
    
    infoBox('Total Capacity of Prisons', pb_capacity$total_capacity, color = "olive")
  })
  
  #Calculate capacity of prisons with a brownfield in its census tract
  output$total_capacity_pb_bf <- renderInfoBox({
    capacity_pb_bf <- pb_with_census_tracts %>% 
      filter(CAPACITY != -999 & !is.na(BF_COUNT) & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus non-null capacities plus non-null brownfield count
      summarize(total_capacity = sum(CAPACITY)) #sum capacity
    
    infoBox('Total capacity of prisons with brownfields in census tract', capacity_pb_bf$total_capacity, color = "olive")
  })
  
  #Calculate percent of prisons with a brownfield in census tract
  output$percent_capacity_pb_bf <- renderInfoBox({
    pb_capacity <- pb_with_census_tracts %>%
      filter(CAPACITY != -999 & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus non-null capacities
      summarize(total_capacity = sum(CAPACITY)) #sum capacity
    
    per_capacity_pb_bf <- pb_with_census_tracts %>% 
      filter(CAPACITY != -999 & !is.na(BF_COUNT) & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs plus non-null capacities plus non-null brownfield count
      summarize(percent_capacity = sum(CAPACITY)/pb_capacity$total_capacity*100) #calculate total capacity of prisons in census tract with brownfield and then divide by total capacity across all prisons
    
    per_capacity_pb_bf <- paste(as.character(round(per_capacity_pb_bf$percent_capacity, 2)), '%', sep="") #Round to two decimal places
    
    infoBox('Percent of total capacity in prisons with brownfields in census tract', per_capacity_pb_bf, color = "olive")
  })
  
  #Calculate percent of prisons with five or more brownfields in census tract
  output$missing_capacity <- renderInfoBox({
    pb_rows <- pb_with_census_tracts %>%
      filter(STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
      nrow()
    
    missing_capacity_rows <- pb_with_census_tracts %>%
      filter(CAPACITY == -999 & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2) %>% #Filter to selected user inputs plus rows with capacity missing
      nrow()/pb_rows*100
    
    missing_capacity_rows <- paste(as.character(round(missing_capacity_rows, 2)), '%', sep="") #Round to two decimal places
    
    infoBox('Percent of prisons with missing capacity', missing_capacity_rows, color = "yellow")
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
    
    infoBox('Number of census tracts with a prison', census_pb_rows, color = "yellow")
  })
  
  output$num_census_pb_bf <- renderInfoBox({
    census_pb_bf <- census_tract_data %>%
      filter(!is.na(BF_COUNT) & BF_COUNT > 0 & !is.na(PB_COUNT)) %>%
      nrow()
    
    infoBox('Number of census tracts with a prison and 2 brownfields', census_pb_bf, color = "yellow")
  })
  
  output$num_census <- renderInfoBox({
    census_bf_rows <- bf_with_census_tracts %>%
      count(GEOID) %>%
      filter(n > 0) %>%
      nrow()
    
    census_pb_bf <- census_tract_data %>%
      filter(!is.na(BF_COUNT) & BF_COUNT > 0 & !is.na(PB_COUNT)) %>%
      nrow()
    
    census_rows <-  census_pb_bf / census_bf_rows * 100
    
    infoBox('Percentage of census tracts with two brownfields that also have a prison', census_rows, color = "yellow")
  })
  
  #Data table of prisons with brownfields in census tract, sorted according to number of brownfields
  output$pb_most_bf <- DT::renderDataTable(
    pb_with_census_tracts %>%
      filter(!is.na(BF_COUNT) & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
      select(NAME, CITY, STATE, GEOID, TYPE, POPULATION, CAPACITY, BF_COUNT) %>%
      arrange(desc(BF_COUNT))
  )
  
  #Frequency plot displaying distribution of brownfield census tract counts across prisons
  output$pb_bf_frequency <- renderPlot(
    pb_with_census_tracts %>%
      filter(!is.na(BF_COUNT) & STATE %in% input$state2 & STATUS %in% input$status2 & TYPE %in% input$type2 & CAPACITY >= input$capacity2) %>% #Filter to selected user inputs
      ggplot(aes(x = BF_COUNT, col = TYPE)) +
      geom_freqpoly(binwidth = 1) +
      theme_bw() +
      labs(x = "Number of Brownfields in Census Tract", y = "Number of Prisons")
  )
  
}

#==========================================================================================================
#Run App
#==========================================================================================================
shinyApp(ui, server)

