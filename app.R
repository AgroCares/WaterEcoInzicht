# Settings-------------------------------------------------------
rm(list=ls())                               #maakt geheugen leeg voor aanvang
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load packages and functions-------------------------------------
require(data.table)
require(sf)
source('scripts/ppr_funs_waterecoinzicht.R')

# other settings
col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red")
labels <- c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2")

# load input------------------------------------------------------
# load aggregated toetsdata generated with ..
EKRset <- readRDS('data/krw/EKRset.rds')
ekr_scores2 <- readRDS('data/krw/ekr_scores2.rds')
ekr_scores1 <- readRDS('data/krw/ekr_scores1.rds')
ekr_scores1 <- ekr_scores1[level == 1,]
ekr_scores2 <- ekr_scores2[level == 1,]
gEAG <- st_read("input/gebiedsinfo/EAG20210709.gpkg") %>% st_transform(4326)
eag_wl <- data.table::fread('./input/gebiedsinfo/EAG_Opp_kenmerken_20220811.csv')
gEAG <- merge(gEAG, eag_wl, all.x = TRUE)

# Shiny server ui --------------------------------------------------
require(shiny)
require(shinythemes)
require(plotly)
# require(leaflet)

ui <- fluidPage(
  # theme = shinytheme("sandstone"),
  
  headerPanel("Ecologische waterkwaliteit in het beheergebied van Amstel, Gooi en Vecht"
              ),
  sidebarLayout(
    sidebarPanel(
      style = "overflow-y:scroll; position:relative;", # om sidebar mee naar beneden te laten bewegen
      p("In onderstaande keuzelijst kan een gebied (KRW waterlichaam of overig water) worden gekozen. In de kaart hieronder is de ligging van gebieden terug te vinden."),
      selectInput(inputId = 'toetsgebied', 'Waterlichaam',
                  sort(unique(EKRset$waterlichaam)), # hier waterlichaam of iets wat NL niet onderscheid, voor ovwat is dit eag
                  selected = "Gaasperplas"),
      # leafletOutput("mymap"),
      p(""),
      p("In onderstaande keuzelijst kunnen meerdere biologisch kwaliteitselementen worden gekozen."),
      selectInput(inputId = 'maatlat', 'KRW maatlat',
                  sort(unique(EKRset$Waardebepalingsmethode.code)), 
                  selected = "Maatlatten2018 Ov. waterflora", multiple = T),
      p("Voor de beoordeling van de biologische kwaliteit van de waterlichamen wordt gekeken naar het voorkomen van vier kwaliteitselementen: 
        fytoplankton (algen), waterplanten (overige waterflora), macrofauna (met het blote oog zichtbare ongewervelde dieren, zoals slakken en libellen) en vissen. 
        De biologische kwaliteit is het belangrijkste onderdeel van de KRW-beoordeling van de ecologische toestand. Monitoring van de biologische kwaliteit en de ondersteunende fysisch-chemische kwaliteit wordt uitgevoerd volgens landelijke richtlijnen."
        ),
      sliderInput(inputId = 'period',label = 'Periode',  
                  min = min(EKRset$jaar), max=max(EKRset$jaar), 
                  value = c(min(EKRset$jaar), max=max(EKRset$jaar)), 
                  sep = ""),
      shiny::p("De resultaten van de gekozen waterlichaam en kwaliteitselementen worden in verschillende
                  figuren weergegeven. In deze figuren wordt de ecologische kwaliteit uitgedrukt met behulp van een Ecologische KwaliteitsRatio (EKR). 
                  Dat is de eenheid waarin de biologische toestand van het oppervlaktewater wordt uitgedrukt. De EKR heeft altijd een waarde tussen 0 en 1. Voor elk watertype (vorm van het water, bodemtype, chemische typering en stromend of stilstaand) zijn nationaal andere kwaliteitseisen opgesteld, de betekenis van de EKR verschilt daardoor per type. Voor alle watertypen geldt dat de waarde 1 overeenkomt met de natuurlijke referentie en een 0 overeenkomt met hele slechte toestand."
      )
      ), 
    mainPanel(
      tabsetPanel(
        # tabPanel("Overzicht", 
        #          h5("Toelichting"),
        #          p("Hieronder staat een figuur waarin fracties meetlocaties per kwaliteitsklasse (van ekr scores)
        #             staan weergegeven per EAG."),
        #          plotOutput("plot11", width = "100%", height = "800px")
        #          ),
        
        tabPanel("Grafieken per KRW-waterlichaam",  
                 
                 shiny::p("In dit tabblad wordt de ecologische kwaliteit per KRW-waterlichaam of overig water weergegeven."),
                 p("De ecologische kwaliteit wordt voor de KRW beoordeeld per KRW-waterlichaam, de ecologische kwaliteit van overig water wordt beoordeeld per ecologisch analysegebied (EAG).
                    Een EAG is een opdeling van een afvoergebied of KRW waterlichaam in eenheden met een min
                    of meer gelijke ecohydrologie.
                    Voor het overig water is het gekozen waterlichaam dus één EAG, KRW waterlichamen kunnen uit meerdere EAG's bestaan."
                   ),
                shiny::p("In onderstaand figuur wordt de huidige toestand
                  vergeleken met doelen. De achtergrondkleuren in het figuur staan
                  voor de klasseindeling van het huidige doel. Wanneer de zwarte streep over
                  de groene achtergrondkleur (GEP of korte-termijndoel overig water) valt is het doel gehaald."
                          ),
                 shiny::plotOutput("plot1b"),
                 shiny::p("In onderstaand figuur wordt per kwaliteitselement de verandering van ekr scores van maatlatten,
                  deelmaatlatten en indicatoren in de tijd weergegeven."), 
                 plotly::plotlyOutput("plot6"),
                 shiny::p("In onderstaand figuur staat de fractieverdeling van ekr scores ofwel het percenatage locaties in een gebied die een bepaalde kwaliteit heeft."),
                 plotly::plotlyOutput("plot5")
        ),
        tabPanel("Grafiek per ecologisch analysegebied",  
                 
                 shiny::p("In dit tabblad wordt de ecologische kwaliteit van KRW- en overig water per ecologisch analysegebied (EAG)
                 weergegeven. Een EAG is een opdeling van een afvoergebied of KRW waterlichaam in eenheden met een min
                    of meer gelijke ecohydrologie."),
                 shiny::p("In onderstaand figuur wordt de huidige toestand
                  vergeleken met doelen. De achtergrondkleuren in het figuur staan
                  voor de klasseindeling van het huidige doel. Wanneer de zwarte streep over
                  de groene achtergrondkleur (GEP korte-termijndoel overig water) valt is het doel gehaald."),
                 shiny::plotOutput("plot1a"),
                 shiny::p("In onderstaand figuur wordt per kwaliteitselement de verandering van ekr scores van maatlatten,
                  deelmaatlatten en indicatoren in de tijd weergegeven."), 
                 plotly::plotlyOutput("plot1"),
                 shiny::p("In onderstaand figuur staat de fractieverdeling van ekr scores ofwel het percenatage locaties in een gebied die een bepaalde kwaliteit heeft."),
                 plotly::plotlyOutput("plot2")
        )
      )
      
    )))

# Shiny server -----------------------------------------------------------------
server <- function(input, output) {
  # # overzichtsgrafieken 1e tab
  # selectedData4 <- reactive({
  #   gebiedData <- EKRset[EKRset$Waardebepalingsmethode.code == input$maatlat,]
  #   gebiedData <- gebiedData[gebiedData$jaar >= input$period[1] &
  #                              gebiedData$jaar<= input$period[2],]
  #   # selectedData4 <- EKRset[EKRset$Waardebepalingsmethode.code == "Maatlatten2018 Fytoplankton",]
  # })
  # 
  # output$plot11 <- renderPlot({
  #   p<- polarHistogram(selectedData4())
  #   print(p)
  # })
  
  # output$mymap <- renderLeaflet({
  #   
  #   pal <- colorFactor(
  #     palette = "viridis",
  #     domain = gEAG$GAFIDENT)
  #   
  #  
  #   leaflet() %>%
  #     addPolygons(data= gEAG, layerId = gEAG$GAFIDENT, popup= paste("Naam:", gEAG$GAFNAAM, "<br>",
  #                                                          "Ident:", gEAG$GAFIDENT,"<br>",
  #                                                          "KRW naam", gEAG$SGBP3_NAAM,"<br>",
  #                                                          "KRW ident", gEAG$KRW_SGBP3),
  #                 stroke = T, color= ~pal(GAFIDENT), opacity=0.8, weight = 1, smoothFactor = 0.8,
  #                 fill=T, fillOpacity = 0.6) %>%
  #     addProviderTiles("Esri.WorldGrayCanvas")
  # })
  
  # selectie voor tab per EAG
  selectedDataeag <- reactive({
    gebiedData <- ekr_scores2[ekr_scores2$wbmethode %in% input$maatlat,]
    gebiedData <- gebiedData[gebiedData$waterlichaam == input$toetsgebied ,]
    # gebiedData <- ekr_scores2[ekr_scores2$wbmethode == "Maatlatten2018 Fytoplankton" & ekr_scores2$waterlichaam == "Botshol" ,]
  })
  
  output$plot1a <- renderPlot({
    ppr_ekrplot_EAG(selectedDataeag())
  })
  
  # selectie voor tab per EAG
  selectedData <- reactive({
    gebiedData <- EKRset[EKRset$Waardebepalingsmethode.code == input$maatlat,]
    # select only unaggregated data
    gebiedData <- gebiedData[grepl("^NL11_*",HoortBijGeoobject.identificatie), ]
    gebiedData <- gebiedData[gebiedData$waterlichaam == input$toetsgebied ,]
    gebiedData <- gebiedData[gebiedData$jaar >= input$period[1] &
                               gebiedData$jaar<= input$period[2],]
    })
  
  output$plot1 <- renderPlotly({
    ggplotly(p = plotEKRlijnfs(selectedData(), gebied = 'eag'), tooltip = "all")
  })
  
  output$plot2 <- renderPlotly({
    ggplotly(p = plotFractiePerMaatlatFacetEAG(selectedData()), height = 1000, width = 1000, tooltip = "all") 
  })
  
  # selectie voor tab KRW
  selectedDataKRW <- reactive({
    
    gebiedData <- ekr_scores1[ekr_scores1$wbmethode %in% input$maatlat,]
    gebiedData <- gebiedData[gebiedData$waterlichaam == input$toetsgebied ,]
    gebiedData1 <- if(nrow(gebiedData)< 1){
                         ekr_scores2[ekr_scores2$wbmethode %in% input$maatlat & ekr_scores2$waterlichaam %in% input$toetsgebied,]
                    }else(gebiedData1 <- gebiedData)
    })
  
  output$plot1b <- renderPlot({
    ppr_ekrplot_gebied(selectedDataKRW()) 
  })
  
  selectedData6 <- reactive({
    gebiedData <- EKRset[EKRset$Waardebepalingsmethode.code %in% input$maatlat,]
    gebiedData <- gebiedData[gebiedData$waterlichaam == input$toetsgebied ,]
    # select only weighed scores
    gebiedData1 <- gebiedData[!grepl("^NL11_*", HoortBijGeoobject.identificatie), ] 
    # scores of indicators (level 3') are not present so here sel unweighed data
    gebiedData2 <- gebiedData[grepl("^NL11_*", HoortBijGeoobject.identificatie) & level == 3, ]
    gebiedData <- rbind(gebiedData1,gebiedData2)
    gebiedData <- gebiedData[gebiedData$jaar >= input$period[1] &
                               gebiedData$jaar<= input$period[2],]
   
  })
  
  output$plot6 <- renderPlotly({
    ggplotly(p = plotEKRlijnfs(selectedData6()), tooltip = "all")
  })
  
  output$plot5 <- renderPlotly({
    ggplotly(p = plotFractiePerMaatlat(selectedData()), height = 1000, width = 1000, tooltip = "all")
  })
  
}


# ShinyApp -------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

