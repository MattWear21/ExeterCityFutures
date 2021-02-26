labelsTotal <- sprintf(
  "<strong>%s</strong><br/>Solar Installations: %g",
  inner.exeter@data[["name"]], exe.installation$freq
) %>% lapply(htmltools::HTML)

labelsPercentage <- sprintf(
  "<strong>%s</strong><br/>Solar Installations Per Population: %g Percent",
  inner.exeter@data[["name"]], solarperpop
) %>% lapply(htmltools::HTML)

labelsInstalledCapacity <- sprintf(
  "<strong>%s</strong><br/>Mean Installed Capacity: %g kW",
  inner.exeter@data[["name"]], installed.capacity$average
) %>% lapply(htmltools::HTML)

labelsDomesticInstalledCapacity <- sprintf(
  "<strong>%s</strong><br/>Total Domestic Installed Capacity: %g kW",
  inner.exeter@data[["name"]], domestic.installations$totcap
) %>% lapply(htmltools::HTML)

labelsNonDomesticInstalledCapacity <- sprintf(
  "<strong>%s</strong><br/>Total Non-Domestic Installed Capacity: %g kW",
  inner.exeter@data[["name"]], nondomestic.installations$totcap
) %>% lapply(htmltools::HTML)

labelOption <- labelOptions(
  style = list("font-weight" = "normal", "padding" = "3px 8px"),
  textsize = "15px",
  direction = "auto")

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      fitBounds(-3.86, 50.6, -3.3, 50.87) %>%
      setMaxBounds(-3.86, 50.6, -3.3, 50.87) %>%
      addMeasure(
        position = "bottomright",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      
      addEasyButtonBar(
        easyButton(
          icon='fa-globe', title='Zoom to Level 1',
          onClick=JS("function(btn, map){ map.setZoom(10);}")),
        easyButton(
          icon='fa-crosshairs', title='Locate Me',
          onClick=JS("function(btn, map){ map.locate({setView: true});}"))
      )
  })
  
  observe({
    proxy <- leafletProxy("mymap")
    proxy %>% clearControls() %>% clearShapes()
    if (input$polygons == "None") {
      proxy %>%
        clearControls() %>%
        clearShapes()
    }
    if (input$polygons == "Total") {
      proxy %>%
        addPolygons(data=inner.exeter, 
                    color = ~pal2(exe.installation$freq), 
                    stroke = FALSE, 
                    smoothFactor = 0.3,
                    weight=2, 
                    fillOpacity = 0.9,
                    group="Total",
                    highlight = highlightOptions(
                      color = "#666", 
                      fillOpacity = 1.0),
                    label=labelsTotal,
                    labelOptions = labelOption) %>%
        
        addLegend(data=inner.exeter, 
                  "bottomleft", 
                  pal = pal2, 
                  values = ~exe.installation$freq, 
                  opacity = 1.0, 
                  title = "Solar Installations", 
                  group="Total")
    }
    if (input$polygons == "Percentage") {
      proxy %>%
        addPolygons(data=inner.exeter,
                    stroke = FALSE,
                    weight=100,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    color = ~pal2(solarperpop),
                    group="Percentage",
                    highlight = highlightOptions(
                      color = "#666",
                      fillOpacity = 1.0),
                    label=labelsPercentage,
                    labelOptions = labelOption) %>%
        
        addLegend(data=inner.exeter,
                  "bottomleft",
                  pal = pal2,
                  values = ~solarperpop,
                  opacity = 1.0,
                  title = "Solar Installations %",
                  group="Percentage")
    }
    if (input$polygons == "Installed Capacity") {
      proxy %>%
        addPolygons(data=inner.exeter,
                    stroke = FALSE,
                    weight=100,
                    smoothFactor = 0.3,
                    fillOpacity = 0.8,
                    color = ~pal2(installed.capacity$average),
                    #group = "Installed Capacity",
                    highlight = highlightOptions(
                      color = "#666",
                      fillOpacity = 1.0),
                    label=labelsInstalledCapacity,
                    labelOptions = labelOption) %>%
        
        addLegend(data=inner.exeter,
                  "bottomleft",
                  pal = pal2,
                  values = ~installed.capacity$average,
                  opacity = 1.0,
                  title = "Installed Capacity",
                  group="Installed Capacity")
    }
    if (input$polygons == "Domestic Installed Capacity") {
      proxy %>%
        addPolygons(data=inner.exeter,
                    stroke = FALSE,
                    weight=100,
                    smoothFactor = 0.3,
                    fillOpacity = 0.8,
                    color = ~pal2(domestic.installations$totcap),
                    #group = "Installed Capacity",
                    highlight = highlightOptions(
                      color = "#666",
                      fillOpacity = 1.0),
                    label=labelsDomesticInstalledCapacity,
                    labelOptions = labelOption) %>%
        
        addLegend(data=inner.exeter,
                  "bottomleft",
                  pal = pal2,
                  values = ~domestic.installations$totcap,
                  opacity = 1.0,
                  title = "Domestic Installed Capacity",
                  group="Domestic Installed Capacity")
    }
    if (input$polygons == "Non-Domestic Installed Capacity") {
      proxy %>%
        addPolygons(data=inner.exeter,
                    stroke = FALSE,
                    weight=100,
                    smoothFactor = 0.3,
                    fillOpacity = 0.8,
                    color = ~pal2(nondomestic.installations$totcap),
                    #group = "Installed Capacity",
                    highlight = highlightOptions(
                      color = "#666",
                      fillOpacity = 1.0),
                    label=labelsNonDomesticInstalledCapacity,
                    labelOptions = labelOption) %>%
        
        addLegend(data=inner.exeter,
                  "bottomleft",
                  pal = pal2,
                  values = ~nondomestic.installations$totcap,
                  opacity = 1.0,
                  title = "Non-Domestic Installed Capacity",
                  group="NonDomestic Installed Capacity")
    }
    
  })
  
  observe({
    proxy2 <- leafletProxy("mymap")
    proxy2 %>% clearMarkers() %>% clearMarkerClusters()
    if (input$panels) {
      proxy2 %>%
        addMarkers(lng = longitude, lat = latitude)
    }
  })
  
  
}