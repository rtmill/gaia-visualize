


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)

shinyUI(dashboardPage(
  dashboardHeader(title = "Gaia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      hr(),
      menuItemOutput("fileStatus"),
      menuItem(
        "Layers",
        icon = icon("object-ungroup"),
        collapsible =
          radioButtons(
            "layerRadio",
            label =  NULL,
            choices = list("None" = "none"),
            selected = NULL
          ),
        
        hr()
      ),
      
      menuItem(
        "Tiles",
        icon = icon("image"),
        collapsible =
          radioButtons(
            "tiles",
            label = NULL,
            choices = list("CartoDB" = "CartoDB.Positron",
                           "Stamen Toner" = "Stamen.Toner"),
            selected = "CartoDB.Positron"
          ),
        hr()
      ),
      menuItem(
        "Colors",
        icon = icon("paint-brush"),
        collapsible =
          radioButtons(
            "colors",
            label =  NULL,
            choices = list("Red" = "Reds",
                           "Blue" = "Blues",
                           "Green" = "Greens"),
            selected = "Reds"
          ),
        
        hr()
      )
    )
  ),
  
  
  dashboardBody(tabItems(
    tabItem(tabName = "home",
            h2("Home"),
            textOutput("val_out"),
            actionButton("selectFirst","first"),
            actionButton("selectSecond","second")
            
            
            ),
    
    tabItem(tabName = "data",
            tabsetPanel(
              tabPanel(
                "Create New",
                
                
                fluidRow(column(4, wellPanel(
                  uiOutput("ui"),
                  actionButton("goButton", "Go!")
                ),
                hr()
                )),
                
                
                
                fluidRow(
                  
                  column(
                    5, wellPanel(
                      selectInput(
                        'layers',
                        label = "Available Layers",
                        choices = NULL,
                        multiple = TRUE,
                        selectize = FALSE
                      ),
                      actionButton("addLayer", "Add")
                    )
                  ),
                  column(
                    5, wellPanel(
                      selectInput(
                        'currLayers',
                        label = "Current Layers",
                        choices = NULL,
                        multiple = TRUE,
                        selectize = FALSE
                      ),
                      actionButton("removeLayer", "Remove")
                    )
                  )),
                fluidRow(
                  column(7,
                         actionButton("buildTable", "Build Working Table", 
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  )
                )
              ),
              tabPanel("Import",
                       selectInput(
                         'importLayer',
                         label = "Saved Layers",
                         choices = ogrListLayers("C:/Users/rmiller/Desktop/repo"),
                         multiple = FALSE,
                         selectize = TRUE
                       ),
                       actionButton("selectImport", "Import")
              
              )
              ,
              tabPanel("Export",
                       textInput("layerName", "Layer Name:"),
                       actionButton("export", "Export"),
                       textOutput("exportMessage")
              ),
              
              
              tabPanel("View Current",
                       dataTableOutput("addedChoices")
              )
            )),
    
    tabItem(
      tabName = "map",
      box(
        title = "Map",
        width = "100%",
        height = "800px",
        leafletOutput("currentMap", width = "100%", height = "800px")
      )
    )
  ))
))
