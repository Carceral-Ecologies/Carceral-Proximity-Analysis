#==========================================================================================================
#UI
#==========================================================================================================
ui <- navbarPage("Carceral EJ Mapper", id="nav",
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
                                            
                                            splitLayout(p("Filter to:"), 
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
                                            #When users switch the input below on, these new inputs will display
                                            uiOutput("sfs_new_inputs"),
                                            uiOutput("mil_new_inputs"),
                                            uiOutput("ap_new_inputs"),
                                            uiOutput("tri_new_inputs"),
                                            actionButton("apply_filters", "Apply"),
                                            splitLayout(p("Include filters for proximity to"),
                                                        pickerInput(inputId = "any_all",
                                                                    label = NULL,
                                                                    choices = c("Any","All")),
                                                        p(" of:"),
                                                        cellWidths = c("57%", "28%", "15%")            
                                            ),
                                            splitLayout(
                                              switchInput(inputId = "sfs_include",
                                                          label = "Superfund Sites", 
                                                          labelWidth = "50%"),
                                              switchInput(inputId = "mil_include",
                                                          label = "Military Bases",
                                                          labelWidth = "50%"),
                                              cellWidths = c("50%", "50%")
                                            ),
                                            splitLayout(
                                              switchInput(inputId = "ap_include",
                                                          label = "Airports",
                                                          labelWidth = "50%"),
                                              #switchInput(inputId = "bf_include",
                                              #            label = "Brownfields")),
                                              switchInput(inputId = "tri_include",
                                                          label = "TRI Facilities",
                                                          labelWidth = "50%"),
                                              cellWidths = c("50%", "50%")
                                            ),
                                            p("* Note that carceral facility data was last updated in 2018. Capacity field is missing for 25% of carceral facilities.
                                              Superfund sites include both NPL and non-NPL sites. TRI facilities are those that reported to the EPA in 2018. See 
                                              About page for more information on data sources."),
                              ),
                              
                              tags$div(id="cite",
                                       tags$a(href = "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis", "Carceral Ecologies GitHub Repo")
                              )
                          )),
                 #-----------------------------------------------------------------------------------------
                 #Second Page to go here
                 #-----------------------------------------------------------------------------------------
                 #About page
                 tabPanel("About",
                          div(tags$head(includeCSS("styles.css")),
                              mainPanel(width = 12,
                                        h1("About the EJ Mapper"),
                                        p("The Carceral EJ Mapper is a tool for examining the proximity of carceral facilities in the United States 
                                        to various environmental hazards. Within the context of a US state, users may apply filter conditions to determine
                                        which carceral facilities have at least a certain number of superfund sites, airports, military bases, and TRI facilities
                                        within a given proximity to the facility. The tool is useful for identifying carceral facilities
                                        and incarcerated people on the frontlines of environemntal injustice in the US."),
                                        h1("Instructions"),
                                        p("Due to the number of objects represented on the map, the Carceral EJ Mapper can currently only display and filter
                                          facilities on state-by-state basis. First, select the state under consideration. The map will relocate to that state,
                                          and the search box will repopulate with the names of carceral facilities in that state.
                                          In the upper right hand corner of screen, you can adjust the basemap and toggle layers for brownfields, 
                                          superfund sites, airports, military bases, and TRI facilities on/off. Using the Search dropdown in the Filter controls, you
                                          can also highlight the location of a particular carceral facility on the map."),
                                        p("Below the search functions, you will find controls to filter carceral facilities displayed on the map by their
                                          status, type, capacity, and the number of environmental hazards proximate to the facility. First, use the dropdowns to
                                          indicate the status and type of carceral facilities you intend to appear on the map, along with the numeric input box to
                                          indicate the minimum capacity of carceral facilities you intend to appear on the map. Note that the capacity field is 
                                          missing for 25% of carceral facilities on the map. Next, select which types of environmental hazards you intend to consider
                                          in your query by toggling the switchbox next to that facility type 'ON'. This will add controls for the facility type to the
                                          filters. Using these new filters, enter the distance (in meters) in which you would consider that facility type proximate to the 
                                          carceral facility. Also, enter the minimum number of such facilities that should be within this distance to the carceral facility
                                          in order for it to remain on the map once the filters have been applied."),
                                        p("If you turned multiple switchboxes 'ON' in order to run your query for multiple types of facilities, you can indicate whether the map
                                          should be filtered based on 'Any' or 'All' of the criteria using the dropdown box above the switchboxes. Once you have completed your
                                          query, click 'Apply' to filter the map."),
                                        h1("Data Sources"),
                                        tags$ul(
                                          tags$li("Airports: Airports were sourced from the Department of Homeland Security's HIFLD database in a dataset called Aircraft Landing Facilities.
                                                  This data was filtered to those in which the fac_type was equal to \"AIRPORT\". It is up to date as of July 13, 2018."),
                                          tags$li("Brownfields: Brownfields were sourced from the EPA's Facility Registry Service (FRS). We downloaded this
                                                  dataset as a CSV file and then filtered it to those in which the SITE_TYPE_NAME was equal to \"BROWNFIELDS SITE\"."),
                                          tags$li("Military Bases: Military bases were sourced from the Office of the Assistant Secretary of Defense's Defense Installations Spatial Data Infrastructure.
                                                  This data represents point locations and boundaries of US Military Installations, Ranges, and Training Areas as of May 2019."),
                                          tags$li("Carceral Facilities: Carceral facility boundary data was sourced from the Department of Homeland Security's HIFLD database. 
                                                  While this data was collected in 2020, it is more than a decade more up-to-date than the most recent DOJ prison survey. 
                                                  Points are placed at the centroid of each facility boundary."),
                                          tags$li("Superfund sites: Superfund sites are any EPA facilities in the Facility Registry System (FRS) that are associated with SEMS, 
                                                  Superfund's IT system. To gather these facilities, we downloaded the EPA's FRS dataset as a CSV file and then filtered it 
                                                  to those in which \"SEMS\" was detected in the PGM_SYS_ACRNMS variable. This currently includes both National Priorities List (NPL) 
                                                  and non-NPL sites."),
                                          tags$li("TRI Facilities: TRI facilities include any facilities that reported emissions in the 2018 EPA Toxic Release Inventory. TRI facilities
                                                  are US industrial facilities with 10 or more employees that emit above a certain threshold of TRI-regulated chemicals in a given year.
                                                  For more specific information on how the EPA defines a TRI Facility, see: https://www.epa.gov/toxics-release-inventory-tri-program/basics-tri-reporting")
                                          
                                        ),
                                        h1("Documentation"),
                                        p("See full documentation for the map at:"),
                                        a(href="https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis", "https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis"), 
                                        h1("Credits"),
                                        p("The Carceral EJ Mapper was developed as a part of a collaboration between the UCLA Carceral Ecologies Lab,
                                          led by Nicholas Shapiro, and the UC Davis Hack for California Research Cluster, led by Lindsay Poirier."),
                                        p("Recommended Citation: Deckard Barnes, Brittany Bates, Ben Millam, Priyanshi Nigam, Lindsay Poirier, Savannah Ramirez, Michelle Servin, & Nicholas Shapiro. (2021, March 31). Carceral EJ Mapper (Version 1.1.2). Zenodo. http://doi.org/10.5281/zenodo.4667080.")
                              )
                          )
                          
                 )
)
