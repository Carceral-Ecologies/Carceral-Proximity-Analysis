#-----------------------------------------------------------------------------------------
#Update Inputs
#-----------------------------------------------------------------------------------------

#Create new filters if switchInput is turned on. 
sfs_inputs <- reactive({
  if (input$sfs_include == TRUE) {div(hr(),
                                      splitLayout(p("with at least"),
                                                  numericInput(inputId = "sfs_num", 
                                                               label = NULL, 
                                                               min = 1,
                                                               value = 1, 
                                                               step = 1),
                                                  p("superfund sites"),
                                                  cellWidths = c("35%", "30%", "35%"), 
                                                  cellArgs = list(style = "vertical-align: top")),
                                      splitLayout(p("within"),
                                                  sliderInput(inputId = "sfs_proximity_val", 
                                                              label = NULL, 
                                                              min = 0, 
                                                              max = 5000, 
                                                              value = 1000, 
                                                              step = 100),
                                                  p("meters"),
                                                  cellWidths = c("20%", "60%", "20%")
                                      ))}
})
mil_inputs <- reactive({
  if (input$mil_include == TRUE) {div(hr(),
                                      splitLayout(p("with at least"),
                                                  numericInput(inputId = "mil_num", 
                                                               label = NULL, 
                                                               min = 1,
                                                               value = 1, 
                                                               step = 1),
                                                  p("military bases"),
                                                  cellWidths = c("35%", "30%", "35%"), 
                                                  cellArgs = list(style = "vertical-align: top")),
                                      splitLayout(p("within"),
                                                  sliderInput(inputId = "mil_proximity_val", 
                                                              label = NULL, 
                                                              min = 0, 
                                                              max = 5000, 
                                                              value = 1000, 
                                                              step = 100),
                                                  p("meters"),
                                                  cellWidths = c("20%", "60%", "20%")
                                      ))} 
})
ap_inputs <- reactive({
  if (input$ap_include == TRUE) {div(hr(),
                                     splitLayout(p("with at least"),
                                                 numericInput(inputId = "ap_num", 
                                                              label = NULL, 
                                                              min = 1,
                                                              value = 1, 
                                                              step = 1),
                                                 p("airports"),
                                                 cellWidths = c("35%", "30%", "35%"), 
                                                 cellArgs = list(style = "vertical-align: top")),
                                     splitLayout(p("within"),
                                                 sliderInput(inputId = "ap_proximity_val", 
                                                             label = NULL, 
                                                             min = 0, 
                                                             max = 5000, 
                                                             value = 1000, 
                                                             step = 100),
                                                 p("meters"),
                                                 cellWidths = c("20%", "60%", "20%")
                                     ))}
})
tri_inputs <- reactive({
  if (input$tri_include == TRUE) {div(hr(),
                                      splitLayout(p("with at least"),
                                                  numericInput(inputId = "tri_num", 
                                                               label = NULL, 
                                                               min = 1,
                                                               value = 1, 
                                                               step = 1),
                                                  p("TRI Facilities"),
                                                  cellWidths = c("35%", "30%", "35%"), 
                                                  cellArgs = list(style = "vertical-align: top")),
                                      splitLayout(p("within"),
                                                  sliderInput(inputId = "tri_proximity_val", 
                                                              label = NULL, 
                                                              min = 0, 
                                                              max = 5000, 
                                                              value = 1000, 
                                                              step = 100),
                                                  p("meters"),
                                                  cellWidths = c("20%", "60%", "20%")
                                      ))} 
  
})


#Add new inputs to sidebar
output$sfs_new_inputs <- renderUI({
  sfs_inputs()
}) 

output$mil_new_inputs <- renderUI({
  mil_inputs()
}) 

output$ap_new_inputs <- renderUI({
  ap_inputs()
}) 

output$tri_new_inputs <- renderUI({
  tri_inputs()
}) 
