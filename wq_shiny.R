library(shiny)
library(rgdal)
library(leaflet)

ui <- fluidPage(
  titlePanel("Water Quality Explorer"),
  leaflet::leafletOutput(
     outputId = "mymap",
     height = 600
   ),
  textOutput("temp"),
  tags$head(tags$style("#temp{color: black;
                                 font-size: 18px;
                                 font-style: bold;
                                 }")
            ),
  # format sidebar with inputs and output table 
  fluidRow(
    column(4,
      h4("Select a segment, date range, use class, category, and parameter to begin.  
       Select a segment by clicking on the map above and select other parameters using the drop down menus.  
       Calculations will be made for the desired parameters.")
           ),
    column(4,
      dateRangeInput(inputId = "daterange",
                 label = "Choose a date range",
                 start = "2008-01-01",
                 end = "2018-12-31",
                 min = "2008-01-01",
                 max = "2018-12-31",
                 format = "mm-dd-yyyy"),
  
    # define use class
    selectInput(inputId = "useclass",
              label = "Use Class",
              useclasses)
           ),
    column(4,
              uiOutput("category"),
   uiOutput("indicator"))
  ),
  
  fluidRow(
    column(4,
    formattableOutput("table"),
    h6("* Hardness-based standards are dynamically calculated using temperature and hardness data collected during the specified time period.  
       Calculated standard on this web app may differ from the regulatory standard.\n**Blue trendline in scatterplot is fitted with a LOESS smooth function.")
       ),
  # format columns
  column(4, 
   plotOutput("boxplot")
  ),
  column(4,
   plotOutput("scatterplot")      
         )
  )
)

server <- function(input, output) {
  # render leaflet map with shapefile and options for basemaps
  factpal <- colorFactor(rainbow(36), seg_shp$AUID)
  newpal <- colorFactor(palette = c("orange","yellow","yellow","green"), levels = c("5","3a","3b","1"))
  bounds <- bbox(seg_shp)
  
  output$mymap <- renderLeaflet(
      leaflet(data = seg_shp, options = leafletOptions(minZoom = 9)) %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>% 
        addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
        addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
        # add polyline layer with individual segments
        addPolylines(layerId = ~AUID,label = ~PortionDes, weight = 3, group = "Segments (Selectable)", color = ~factpal(AUID),
                     highlightOptions = highlightOptions(color = "mediumseagreen",opacity = 1.0,weight = 3,bringToFront = TRUE)) %>%
        # add polyline layer with regulatory status codes
        addPolylines(label = ~PortionDes, weight = 3, group = "Impairment (View only)", fillOpacity = 1, color = ~newpal(Cat),
                     highlightOptions = highlightOptions(color = "mediumseagreen",opacity = 1.0,weight = 3,bringToFront = TRUE)) %>%
        # set boundaries for viewing
        setMaxBounds( lng1 = bounds[1,1] - 1
                      , lat1 = bounds[2,1] - 1
                      , lng2 = bounds[1,2] + 1
                      , lat2 = bounds[2,2] + 1) %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("Topo", "Imagery", "Terrain"),
        overlayGroups = c("Segments (Selectable)", "Impairment (View only)"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addLegend(title = "Regulatory status",
                colors = c("orange","yellow","green"),
                labels = c("5: (303(d) Impaired)", "3a/b: (Not enough information/M & E)","1: All attaining standards"),
                group = "Impairment (View only)") %>% 
      hideGroup(group = "Impairment (View only)")  
  )
  
  # selec segment by map shape click, return text
  observe({
    click = input$mymap_shape_click
    seg = input$mymap_shape_click$id
    if(is.null(click))
      output$temp <- renderText({
        "No segment selected. Click a segment to initiate program."})
    else
      output$temp <- renderText({
        paste("Segment:", seg)
      })
  })
  
  # render interactive useclass, category, and water quality parameter selection
  df0 <- eventReactive(input$useclass,{
    wqStandards %>% filter(UseClass %in% input$useclass)
  })
  output$category <- renderUI({
    selectInput("category","Choose category",sort(unique(df0()$Category)))
  })
  
  df1 <- eventReactive(input$category,{
    df0() %>% filter(Category %in% input$category)
  })
  
  output$indicator <- renderUI({
    selectInput("indicator","Desired water quality parameter",sort(unique(df1()$Indicator)))
  })
  # render boxplot with interactive data
  output$boxplot <- renderPlot({
    buildBoxplot(watershedData, input$useclass, input$indicator, input$category, input$mymap_shape_click$id, input$daterange[1], input$daterange[2])
  })
  # render scatterplot with interactive data
  output$scatterplot <- renderPlot({
    buildScatterplot(watershedData, input$useclass, input$indicator, input$category, input$mymap_shape_click$id, input$daterange[1], input$daterange[2])
  })
  # render table with interactive data
  output$table <- renderFormattable({
    getSummaryStats(watershedData, input$useclass, input$indicator, input$category, input$mymap_shape_click$id, input$daterange[1], input$daterange[2])
  })

}

shinyApp(ui = ui, server = server)
