
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(postGIStools)
library(RPostgreSQL)
library(maptools)
library(rgdal)

shinyServer(function(input, output, session) {
  
 
    activeLayers <- c()
    
    values <- reactiveValues(
      source = "",
      importedDF = NULL,
      customDF = NULL
    )
    
    
    repoLocation <- "C:/Users/rmiller/Desktop/repo"
    
    
 
  
    con <- dbConnect(PostgreSQL(), dbname = "gis", 
                     user = "gis_user",
                     host = "localhost",
                     password = "motech")
    
    
    query <- "select * from region.mapping"
    res <- dbGetQuery(con, query)

    
    polyTable <- res[1,3]
    
    

   
  

   

   


  output$ui <- renderUI({
    if (is.null(res))
      return()
    
    
    selectInput("regionType", "Region Type", 
                 choices =  c(Choose='1', res$region_type))
  })
  

  attrib <- eventReactive(input$goButton, {
    query <- paste("select * from region.",input$regionType, sep="")
    attr <- dbGetQuery(con, query)
    attr$geoid <- as.character(attr$geoid)
    attr
  })
  
  getPoly <-  eventReactive(input$goButton, {
    query <- paste("select geoid,name, geom from public.", res$polygon_table[res$region_type == input$regionType], sep="")
    poly <- get_postgis_query(con, query,  geom_name = "geom")
    
    # Placeholder to map just ME, NH and VT
   # poly <- poly[startsWith(poly$geoid, "23") | startsWith(poly$geoid, "33") | startsWith(poly$geoid, "50"),]
    
    poly
  })
  
  
  
  
  
  
  output$poly <- renderText({
    names(getPoly())
  })
    
  
  obs <- observe({
    updateSelectInput(
      session, 'layers', 
      choices = names(attrib()[-1]),
      selected = NULL
    )
  })
 
  nChoice <- eventReactive(input$addLayer,{
    activeLayers <<- unique(c(activeLayers, input$layers))
    activeLayers
  })
  
  output$layerChoice <- renderText({
    nChoice()
  })
  
  obs2 <- observe({
    updateSelectInput(
      session, 'currLayers', 
      choices = nChoice(),
      selected = NULL
    )
  })  
  
  remChoice <- eventReactive(input$removeLayer,{
      activeLayers <<- setdiff(activeLayers, input$currLayers)
      activeLayers
  })
  
  obs3 <- observe({
    updateSelectInput(
      session, 'currLayers', 
      choices = remChoice(),
      selected = NULL
    )
  })    
  

  output$addedChoices <- renderDataTable({
    
    j <- currentDF()
    data.frame(j)
  })
  
 
 
  
  
  observeEvent(input$buildTable, {
    p <- getPoly()
    a <- attrib()
    
    
    
    for( i in 1:length(activeLayers)){
      word <- activeLayers[i]
      p[[word]] <- with(a, a[[word]][match(p$geoid, a$geoid)])
    }
    
    output$fileStatus <- renderMenu({
      menuItem("Working Table",
               badgeLabel = "FOUND", badgeColor = "green")
    })
    
    values$customDF <- p
    values$source <- "custom"
    
    
  })
  

  
  observe({
    j <- currentDF()
    j <- j[, -which(names(j) %in% c("name","geoid"))]
    
    
    updateRadioButtons(
      session, "layerRadio",
      choices = c("None" = "none", names(j))
    )
  })
  
  output$fileStatus <- renderMenu({
    menuItem("Working Table",
             badgeLabel = "MISSING", badgeColor = "red")
  })
 
 
 
  
  output$currentMap <- renderLeaflet({
    mData <- currentDF()
    if(is.null(mData)){
      return()
    }
    
    leaflet(mData, width="100%", height="800px") %>%
      addTiles() %>% 
      addPolygons()
  })
  

  
  observe({
    mData <- currentDF()
    if(is.null(mData)){
      return()
    }
    


    if(input$layerRadio == "none"){
      leafletProxy("currentMap", data = mData) %>%
        clearTiles() %>% 
        addProviderTiles(input$tiles) %>%
        clearShapes() %>%
        addPolygons() %>%
        clearControls()
    }
    
    else{
      
    layer <- mData[[input$layerRadio]]
    
    pal <- colorBin(
      palette = input$colors,
      domain = layer
    )
    
   
    
    
    leafletProxy("currentMap", data = mData) %>%
      clearTiles() %>% 
      addProviderTiles(input$tiles) %>%
      clearShapes() %>%
      addPolygons(
        stroke=FALSE, fillOpacity=0.5, smoothFactor = 0.5,
        color = ~pal(layer),
        popup = paste(mData$name, "<br>",
                      layer)
      ) %>%
      clearControls() %>%
      addLegend("bottomright", pal=pal, values= layer, title=input$layerRadio)

    }

  })
  

  
  
  
  exportResult <- eventReactive(input$export, {
    #writeOGR(obj= poly, dsn= "C:/Users/rmiller/Desktop/repo", layer="itWorked", driver="ESRI Shapefile")
    
    j <- currentDF()
    if(is.null(j)){
      return("Error: Export NOT Successful")
    }
    writeSpatialShape(j, paste(repoLocation,input$layerName, sep=""))
    
    
    "Export successful"
  })
  
  
  output$exportMessage <- renderText({
    exportResult()
  })
  
  
  
  
  observeEvent(input$selectImport, {
    values$importedDF <- readOGR(repoLocation, layer=input$importLayer, stringsAsFactors = FALSE)
    values$source <- "imported"
  })
  
 output$importMessage <- renderText({
   importResult()
 })
 
 
 
 
 
 
 
 
 
 observeEvent(input$selectFirst, {
   values$source <- "imported"
 })
 
 observeEvent(input$selectSecond, {
   values$source <- "custom"
 })
 
 output$val_out <- renderText({ 
   names( currentDF() )
   })
 

 
 
 currentDF <- reactive({
   imp <- values$importedDF
   cus <- values$customDF
   
   
   switch(values$source,
          "imported" = imp,
          "custom" = cus
          )
 })

  
})