# Run in R 3.6.1

shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage("VDEQ Lake Profile Explorer Tool",
                                 tabPanel('About',
                                          h5("This tool is meant to expedite the data analysis process for each Integrated Reporting cycle.
                                             This tool accepts data output directly from the 'HowToWrite2018IRProbMonChapter.Rmd' script, the 
                                             same script that feeds into the automated reporting script, to streamline the analysis process prior
                                             to autogenerating a final report."),
                                          h3(strong('Pro Tip: You can turn layers on and off in the interactive plotly plots by clicking on them in the legend.')),
                                          br(), hr(), br(),
                                          h4('Please contact Emma Jones (emma.jones@deq.virginia.gov) with any questions or concerns about the app.') ),
                                 tabPanel('Dashboard',
                                          sidebarPanel(width = 4,
                                                       leafletOutput('lakeMap'),
                                                       selectInput('stationSelection', 'Choose a station to analyze', choices = unique(kerr$FDT_STA_ID))),
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel('Heatmap',
                                                       wellPanel(fluidRow(
                                                         column(6,selectInput('parameterSelection', 'Choose a parameter to plot', 
                                                                              choices = c('Temperature', 'pH', 'Dissolved Oxygen'))),
                                                         column(6,uiOutput('Year')))),
                                                       plotOutput('heatmap'),
                                                       br(), hr(),
                                                       h3('Raw Data (filtered by year selected)'),
                                                       DT::dataTableOutput('selectedData')) ,
                                            tabPanel('Habitable Width',
                                                     wellPanel(
                                                       helpText('Values preset to station specific WQS. Manually adjust to recalculate based on taxa specific values.'),
                                                       fluidRow(column(6,uiOutput('UItempMax')),
                                                                column(6,uiOutput('UIDOMin'))),
                                                       fluidRow(column(6,uiOutput('UIpHMin')),
                                                                column(6,uiOutput('UIpHMax')))),
                                                     br(), hr(),
                                                     #tableOutput('habWidth'))
                                                     plotlyOutput('habWidth'))
                                                       
                                          ))))
                    ))))
                                          