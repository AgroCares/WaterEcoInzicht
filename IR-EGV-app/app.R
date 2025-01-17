# IR EGV app maken

# Settings -----------------------------
# packages en functies laden
rm(list=ls())                               #maakt geheugen leeg voor aanvang
options(repos = c(CRAN = "https://cran.rstudio.com"))
options(rsconnect.max.bundle.size=1000000000)

source("./scripts/helpers_iregv.R")

library(sf)
library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(ggplot2)
library(rgdal)
library(data.table)

# other settings
proj4.rd <- 28992
proj4.google <-4326

# data importeren ----------------------------

## opslaan Rdataset voor app --------------------------------
theData <- base::readRDS('./data/theData.rds') %>% as.data.table()
theData <- theData[theData$afronding == 'Nee',]

###Import LAT framework
# LATframework <- utils::read.csv("./data/coordinates_LAT_framework.csv", header = TRUE, sep = ";")
# saveRDS(LATframework, file= "./data/LATframework.rds")
LATframework <- base::readRDS("./data/LATframework.rds")  

#Import reference points
#referencepoints <- utils::read.csv("./data/reference.points.csv", header = TRUE, sep = ";")
# saveRDS(referencepoints, file= "./data/referencepoints.rds")
referencepoints <- base::readRDS('./data/referencepoints.rds') 


# app------------------------------

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  headerPanel('IR-EGV diagram'),
  sidebarPanel(
    selectInput(inputId = 'eag', 'Ecologisch analyse gebied', choices = sort(unique(theData$locatie.EAG)), selected = "2550-EAG-1", multiple = TRUE),
    selectInput(inputId = 'locatiecode', 'Locatiecode', choices = sort(unique(theData$locatiecode)), selected = "BOT001", multiple = TRUE),
    selectInput(inputId = 'MeetpuntenSeizoen', 'Metingen uit zomer of winter weergeven', unique(theData$seizoen), 
                selected = "zomer", multiple = TRUE),
    sliderInput(inputId = 'period',label = 'Periode',
                min = round(min(theData$jaar),0), max= round(max(theData$jaar),0),
                value = c(min(theData$jaar), max(theData$jaar)),
                round = TRUE, sep = "")
  ),
  mainPanel(
    fluidRow(
           plotly::plotlyOutput("plot1"),
           leaflet::leafletOutput("map1")
    )
    
  )
)

server <- function(input, output, session) {
          
          observeEvent(input$eag, {
            updateSelectInput(session = session, inputId='locatiecode',
                              choices = sort(unique(theData$locatiecode[theData$locatie.EAG == input$eag])),
                              selected = "BOT001")
            }, ignoreInit = FALSE, ignoreNULL = TRUE
)

          selectedData <- reactive({
              gebiedData <- theData[theData$locatie.EAG %in% unique(input$eag),]
              gebiedData <- gebiedData[gebiedData$locatiecode %in% unique(input$locatiecode),]
              gebiedData <- gebiedData[gebiedData$seizoen %in% unique(input$MeetpuntenSeizoen),]

              gebiedData <- gebiedData[(gebiedData$jaar > input$period[1]
                               & gebiedData$jaar < input$period[2]),]
              
              if (dim(gebiedData)[1] * dim(gebiedData)[2] == 0){return(NULL)}
              else{return(gebiedData)}
  
            })

            output$plot1 <- plotly::renderPlotly({
              p = createIR_EGV_Graph(selectedData(),LATframework, referencepoints)
              if (is.null(p)){return(NULL)}
              else{
              plotly::ggplotly(p, tooltip = "all")}
              }
            )
            
            output$map1 <- renderLeaflet({
              gebiedData <- selectedData()
              
              if (length(gebiedData) == 0){return(NULL)}
              else{
                
              gebiedData <- st_transform(st_as_sf(gebiedData, coords = c("locatie.x","locatie.y"),crs = proj4.rd),proj4.google) 
              pal <- leaflet::colorFactor(palette = rainbow(length(unique(gebiedData$locatiecode))),  domain = gebiedData$locatiecode)

              leaflet::leaflet() %>%
                addCircles(data = gebiedData, label = as.character(gebiedData$locatiecode),
                           labelOptions = c(permanent =TRUE),
                           weight = 3, radius=40, color= ~pal(gebiedData$locatiecode), fillOpacity = 0.8) %>%
                
              addTiles()
              }
            })
           }

shiny::shinyApp(ui = ui, server = server)
