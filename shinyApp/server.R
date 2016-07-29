source('global.R')
template <- read.csv('C:/HardDriveBackup/R/BCG/BCGgit/shinyApp/data/template.csv')
options(digits=3)

shinyServer(function(input, output, session) {
  ## Tab 1
  # Download taxa list template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                                 content=function(file){write.csv(template,file)})
  
  # Upload taxa list
  #inputFile <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/sampleList_GIS.csv',row.names=NULL)
  inputFile <- reactive({inFile <- input$sites
  if(is.null(inFile))
    return(NULL)
  read.csv(inFile$datapath)
  })
  output$inputTable <- renderTable({inputFile()})
  
  
 
  ## Tab 2
  # Upload SubBasin Shapefile
    userFile <- reactive({
      validate(need(input$GISbasins, message=FALSE))
      input$GISbasins
    })
    # Read in shapefile
    shp <- reactive({
      req(input$GISbasins)
      if(!is.data.frame(userFile())) return()
      infiles <- userFile()$datapath
      dir <- unique(dirname(infiles))
      outfiles <- file.path(dir, userFile()$name)
      purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
      x <- try(readOGR(dir, strsplit(userFile()$name[1], "\\.")[[1]][1]), TRUE)
      if(class(x)=="try-error") NULL else x
    })
    # check projection (must be WGS84 to work with leaflet)
    valid_proj <- reactive({ req(shp()); if(is.na(proj4string(shp()))) FALSE else TRUE })
    # project if not in correct projection originally
    shp_wgs84 <- reactive({
      req(shp(), valid_proj())
      if(valid_proj()) spTransform(shp(), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) else NULL
    })
    # lat/long values
    lon <- reactive({ if(valid_proj()) (xmin(shp_wgs84()) + xmax(shp_wgs84()))/2 else 0 })
    lat <- reactive({ if(valid_proj()) (ymin(shp_wgs84()) + ymax(shp_wgs84()))/2 else 0 })
    
    # Empty leaflet map
    output$Map <- renderLeaflet({ leaflet() %>% setView(0, 0, zoom=2) %>% addTiles() })
    observe({
      if(!is.null(shp())){
        x <- leafletProxy("Map") %>% clearShapes() %>% clearMarkers() %>% setView(lon(), lat(), zoom=2)
        x %>% addPolygons(data=shp_wgs84(), weight=2)}})
  
  
  #shpfileName <- reactive({if(is.null(input$GISbasins))
   #                           return(NULL)
    #                          strsplit(input$GISbasins$name[1],"\\.")})
  
  #shpfilePath <- reactive({if(is.null(input$GISbasins))
   #                           return(NULL)
    #                          unique(dirname(input$GISbasins$datapath))})
  #shp <- reactive({if(is.null(input$GISbasins))
   #                           return(NULL)
    #                          readOGR(shpfilePath(),shpfileName()[[1]][1])})
  
  #output$GISbasins2<- renderPrint({shp()})#shpfileName()[[1]][1]})#input$GISbasins$name})
  #output$datapath2 <- renderPrint({class(shp())})#input$GISbasins$datapath[1]})
  
  
  
  
  
  # Connect Subbasins
  #subB <- reactive({if(!is.null(inputFile())&!is.null(GISbasins())){SubbasinConnection(inputFile())}})
  #output$stationsWithSubbasins <- renderTable({
    #subB <- subset(subB(), !duplicated(SampleName))
    #select(subB,-c(CommonName,ScientificName,Count,Date,Subbasin_short))})
  #leafletOutput()
  
  
  
})
  