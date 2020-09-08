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