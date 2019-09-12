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
                                                       leafletOutput('lakeMap'),br(),
                                                       selectInput('stationSelection', 'Choose a station to analyze', choices = unique(kerr$FDT_STA_ID)),
                                                       selectInput('parameterSelection', 'Choose a parameter to plot', 
                                                                   choices = c('Temperature', 'pH', 'Dissolved Oxygen')),
                                                       uiOutput('Year')),
                                          mainPanel(plotOutput('heatmap'))
                                                       
                                          )
                                 )
                    ))))
                                          