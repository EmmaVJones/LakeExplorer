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
  
  # Max Temp UI output
  output$UItempMax <- renderUI({
    req(stationData())
    numericInput('tempMax', 'Maximum Temperature Threshold',
                 value = unique(stationData()$`Max Temperature (C)`))  })
  
  # DO Min UI output
  output$UIDOMin <- renderUI({
    req(stationData())
    numericInput('DOMin', 'Minimum DO Threshold',
                 value = unique(stationData()$`Dissolved Oxygen Min (mg/L)`))  })
  
  # Max pH UI output
  output$UIpHMax <- renderUI({
    req(stationData())
    numericInput('pHMax', 'Maximum pH Threshold',
                 value = unique(stationData()$`pH Max`))  })
  
  # Min pH UI output
  output$UIpHMin <- renderUI({
    req(stationData())
    numericInput('pHMin', 'Minimum pH Threshold',
                 value = unique(stationData()$`pH Min`))  })
  
  
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
  
  output$selectedData <- DT::renderDataTable({
    req(stationData())
    DT::datatable(stationData(), rownames = F, escape = FALSE, extensions = 'Buttons',
                  options= list(scrollX = TRUE, pageLength = nrow(stationData()), 
                                scrollY = "250px", dom='Btf',
                                buttons = list('copy')))
  })
  
  # Habitable Width
  output$habWidth <- renderPlotly({
    req(stationData(), input$tempMax, input$DOMin, input$pHMax, input$pHMin)
    habitableWidthPlot(dat= stationData(), 
                       Tmax= input$tempMax, 
                       DOmin= input$DOMin, 
                       pHmax= input$pHMax, 
                       pHmin= input$pHMin)
    #habitableWidthPlot(dat= stationData(), 
    #                   Tmax= as.numeric(input$tempMax), 
    #                   DOmin= as.numeric(input$DOMin), 
    #                   pHmax= as.numeric(input$pHMax), 
    #                   pHmin= as.numeric(input$pHMin))
  })
})