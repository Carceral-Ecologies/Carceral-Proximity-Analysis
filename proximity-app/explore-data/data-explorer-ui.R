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