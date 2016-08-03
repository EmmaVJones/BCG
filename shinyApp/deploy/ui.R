shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
  navbarPage('Ecoregion 69 BCG Model: Fish',
             tabPanel('About',fluidRow(column(10,
                                              h5("This app was created to run the Biological Condition 
                                                 Gradient (BCG) model developed for EPA Level III
                                                 Ecoregion 69- Central Appalachians by Jen Stamp and
                                                 Erik Leppo (Tetra Tech)."),
                                              h5("To run this application, follow the on screen prompts
                                                 on each tab. Proceed from left to right across the
                                                 navigation bar to run the BCG model, moving from 
                                                 'Data Upload' to 'Subbasin Connection' and finally to
                                                 'BCG Model Results.'"),
                                              h5("Questions regarding model applicability and usage should
                                                 be directed to Jason Hill (jason.hill@deq.virginia.gov) and
                                                 Lou Reynolds (reynolds.louis@epa.gov). Please contact Emma 
                                                 Jones (emma.jones@deq.virginia.gov) for all questions 
                                                 regarding app troubleshooting.")))),
             tabPanel('Data Upload',
                      sidebarPanel(
                        h4(strong('Instructions:')),
                        p("Please upload site taxa list(s) in either a flat file (.csv) or by connecting to
                          an appropriate database table. All data fed into the model must be formatted correctly.
                          If you are unsure whether your data is in the correct format, please download the 
                          'template.csv' file first to check your data structure."),
                        hr(),
                        downloadButton('downloadTemplate',"Download template.csv"),
                        fileInput('sites','Upload Sites (flat file)',accept='.csv',width='100%'),
                        h5('Database connection option here.')),
                      mainPanel(tableOutput('inputTable'))
                      ),
             tabPanel('Subbasin Connection',
                      sidebarPanel(
                        h4(strong('Instructions:')),
                        p("Please upload subbasin shapefile. If this is your first time running the app, the
                          subbasin shapefile may be downloaded from: ____DEQWebsite__________. Make sure you 
                          unzip the file prior to uploading it to the BCG application. Remember to select all
                          6 files that make up a shapefile when uploading the GIS data (.shp, .dbf, .sbn, 
                          .sbx, .shx, and .prj)."),
                        hr(),
                        fileInput('GISbasins','Upload Subbasins shapefile',accept=c('.shp','.dbf','.sbn','.sbx'
                                                                                    ,'.shx',".prj"), multiple=TRUE)),
                      mainPanel(# conditional panel? make them upload data first?
                        p('Click on sites and basins for further information.'),
                        p('Review the table below to ensure all sites are associated with the correct subbasin.'),
                        leafletOutput('Map'),
                        tableOutput('stationsWithSubbasins'))
                      ),
             tabPanel('BGC Model Results',
                      sidebarPanel(
                        h4(strong('Instructions:')),
                        p("Once you have ensured the sites are plotting in the correct Subbasin, click 'Run BCG Model'
                          to run the model. Once the calculations are complege, results will be displayed in a table
                          in the main panel. A progress bar on the top of the screen will indicate computational 
                          progress. You may download a flat file of results by clicking the 'Download Results' button or you 
                          can send results back to your database (if connected) by clicking 'Send to Database.'"),
                        actionButton("runModel","Run BCG Model"),
                        br(),
                        hr(),
                        downloadButton("downloadResults","Download Results"),
                        actionButton("sendtoDB","Send to Database")),
                      mainPanel(
                        h4(strong('BCG Model Results'),
                           dataTableOutput('BCGresults')))
             ))))
                                            

                                              