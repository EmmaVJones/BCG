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
  #shp <- reactive({
   # shp_file <- readOGR(input$GISbasins)
    #if(!is.data.frame(userFile())) return()
    #infiles <- userFile()$datapath
    #dir <- unique(dirname(infiles))
    #outfiles <- file.path(dir, userFile()$name)
    #purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
    #x <- try(readOGR(dir, strsplit(userFile()$name[1], "\\.")[[1]][1]), TRUE)
    #if(class(x)=="try-error") NULL else x
  #})
  
  output$GISbasins2<- renderPrint({input$GISbasins})
  output$datapath2 <- renderPrint({input$GISbasins$datapath})
  # Connect Subbasins
  #subB <- reactive({if(!is.null(inputFile())&!is.null(GISbasins())){SubbasinConnection(inputFile())}})
  #output$stationsWithSubbasins <- renderTable({
    #subB <- subset(subB(), !duplicated(SampleName))
    #select(subB,-c(CommonName,ScientificName,Count,Date,Subbasin_short))})
  #leafletOutput()
  
  
  
})
  