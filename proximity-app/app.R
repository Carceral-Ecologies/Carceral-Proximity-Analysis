library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rgeos)

#Read prison Shapefile
pb <- st_read("Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Thanks Ben!
#Convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids

#Read two military site shapefiles
mil <- st_read("installations_ranges/MILITARY_INSTALLATIONS_RANGES_TRAINING_AREAS_PT.shp", stringsAsFactors = FALSE)
mil2 <- st_read("installations_ranges/MILITARY_INSTALLATIONS_RANGES_TRAINING_AREAS_BND.shp", stringsAsFactors = FALSE)

#Bind the two military site shapefiles
mil <- rbind(mil, mil2)

#Convert military sites to match (larger) FRS fac data set Coordinate Reference System
mil_sf <- st_transform(mil, crs = 4269)

#Reduce military sites from polygons to points (centroids) to reduces distance calculation times
mil_sf <- st_transform(mil_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

#state_codes.csv contains full state names, state abbreviations, and census codes for each state, allowing conversion between the variables in each data file
codes <- read.csv("state_codes.csv", stringsAsFactors = FALSE) 
codes$STATE_NUM <- as.character(codes$STATE_NUM) %>% #convert state census codes to characters because they will eventually be text in file names
  str_pad(2, pad = "0") #add a leading zero to ensure all state numbers are two digits

#There are census tract shapefiles for each state stored in the project files. To reduce reading times, these are not all read in by default but on a state-by-state basis when a user selects a state. Below we add two columns with the path to the zip of each state's shapefile, along with the file itself.
codes <- codes %>%
  mutate(TRACT_ZIP = paste("tracts/tl_2019_", STATE_NUM, "_tract.zip", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FILE = paste("tl_2019_", STATE_NUM, "_tract.shp", sep = "")) #create new column in codes dataframe with location of each census tract shapefile name

mil_sf <- mil_sf %>% left_join(codes, by = "STATE_TERR") #Add the state abbreviation to military site data frame, which only contains the fill state name.

#Read brownfield data file and convert to sf
bf <- read.csv("brownfields.csv", stringsAsFactors = FALSE)
bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Read airport data file and convert to sf
ap <- read.csv("airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read contaminated sites data file and convert to sf
sfs <- read.csv("sf.csv", stringsAsFactors = FALSE) 
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)


#Convert the unique ID for prisons to be a string
pb_sf$FID <- as.character(pb_sf$FID)

ui <- navbarPage("Proximity Analysis", id="nav",
                 
                 tabPanel("Interactive Map", #tab panels will appear in the upper navigation
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              leafletOutput("dist_plot", width="100%", height="100%"),
                              
                              #The absolute panel will display user input options
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                                            width = 330, height = "auto",
                                            
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
                          #Eventually this will be a second page of data outputs
                          tabPanel("Data Explorer")
)

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
  
  #contaminated sites icon
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
    if (dir.exists("tracts/unzipped")) #check if unzipped directory exists 
      unlink("tracts/unzipped", recursive = TRUE) #delete directory if it exists
    
    codes_filtered <- codes %>%
      filter(STATE_CODE == input$state) #Filter codes df to selected state
    
    unzip(codes_filtered$TRACT_ZIP, exdir = "tracts/unzipped") #unzip the census tract shapefile zip for the state into "tracts/unzipped". Note that we created the TRACT_ZIP variable with a mutate function above.  
    tracts$df_data <- st_read(paste("tracts/unzipped/",codes_filtered$TRACT_FILE, sep = ""), stringsAsFactors = FALSE) #read shapefile into the reactive value we created above
    tracts$df_data <- st_transform(tracts$df_data, crs = 4269)
    
    pb_sf_filtered <- pb_sf %>% 
      filter(STATE == input$state) #filter prisons df to selected state
    prison_list <- setNames(pb_sf_filtered$FID, pb_sf_filtered$NAME) #create a named vector with prison names specifying their ids
    updatePickerInput(session, inputId = "name", choices = prison_list) #update the update picker input with the prison names for that state
  })
  
  
  
  output$dist_plot <- renderLeaflet({
    
    #Filter all site dfs to the selected state
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
    
    #Create a legend displaying icons and labels
    html_legend <- "<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g4642.png?raw=true' style='width:20px;height:20px; margin:5px;'>Prisons<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g5269.png?raw=true' style='width:20px;height:20px; margin:5px;'>Brownfields<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/g945.png?raw=true' style='width:20px;height:20px; margin:5px;'>Contaminated Sites<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path863.png?raw=true' style='width:20px;height:20px; margin:5px;'>Airports<br/>
<img src='https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/icons/path865.png?raw=true' style='width:20px;height:20px; margin:5px;'>Military Sites"
    
    #Create map and add markers for each site
    leaflet() %>%
      addProviderTiles(input$bmap) %>%
      addPolygons(
        data = tracts$df_data,
        color = "#444444", 
        weight = 1, 
        smoothFactor = 0.5,
        opacity = 1.0, 
        fillOpacity = 0.1,
        highlightOptions = highlightOptions(color = "white", weight = 2),
        group = "Census Tracts"
      ) %>%
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
      addLayersControl(   #Add controls to turn layers on and off
        overlayGroups=c("Prisons", "Brownfields", "Contaminated Sites", "Airports", "Military Sites", "Census Tracts"),
        options=layersControlOptions(collapsed=FALSE)) %>%
      addControl(html = html_legend, position = "bottomright") %>% 
      hideGroup("Census Tracts") %>%
      hideGroup("Airports") %>% 
      hideGroup("Brownfields") %>% 
      hideGroup("Contaminated Sites") %>% 
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
      sprintf("Contaminated sites: %s", calculateNumberInProximity(prison, "sfs")),
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
  observeEvent(input$name, {
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
    if(!is.null(prev_prison()))
    {
      proxy %>%
        addMarkers(
          data = prev_prison(),
          layerId = ~prev_prison()$FID,
          icon = icon_pb)
    }
    #Set the selected row (prison) to be the next previous prison. 
    prev_prison(row_selected)
  })
  
  
}

shinyApp(ui, server)

