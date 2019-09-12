source('global.R')


shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # reactive station choice
  stationData <- reactive({
    filter(kerr, FDT_STA_ID %in% input$stationSelection)  })
  
  # Year UI output
  output$Year <- renderUI({
    req(stationData())
    years <- unique(stationData()$Year)
    selectInput('YearSelection', "Choose as year to plot", choices = years) })
  
  # Map 
  output$lakeMap <- renderLeaflet({
    mapview(kerr_sf, label=kerr_sf$FDT_STA_ID, zcol = 'Profile',
            layer.name = 'DEQ Kerr Stations')@map
  })
  
  # set up some things
  #parameterSwitch <- reactive(
  #  switch(input$parameterSelection,
  #         'Temperature' = `Temp Celsius`, 'pH' = `Field Ph`, 'Dissolved Oxygen' = `DO` ))
  

  # Heatmap
  output$heatmap <- renderPlot({
    req(stationData(), input$YearSelection)
    
    parameterSwitch <- switch(input$parameterSelection,
                              'Temperature' = 'Temp Celsius', 'pH' = 'Field Ph', 'Dissolved Oxygen' = 'DO' )
    criteriaSwitch <- switch(input$parameterSelection,
                             'Temperature' = 'Max Temperature (C)', 'pH' = 'pH Min', 'Dissolved Oxygen' = 'Dissolved Oxygen Min (mg/L)')
    colorPalSwitch <- switch(input$parameterSelection,
                             'Temperature' = FALSE, 'pH' = TRUE, 'Dissolved Oxygen' = TRUE)
    
    int_heatmap(stationData(), 
                stationFilter = unique(stationData()$FDT_STA_ID), 
                yearFilter = input$YearSelection, 
                stationCol = FDT_STA_ID,
                dateCol = Date, 
                depthCol = Depth, 
                parameterCol = parameterSwitch, 
                criteriaCol = criteriaSwitch,
                dateFormat= '%Y-%m-%d',
                reverseColorPalette = TRUE)
    
    
  })
})