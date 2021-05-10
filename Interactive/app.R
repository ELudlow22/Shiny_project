library(shiny)
library(shinipsum)
library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(raster)
library(rgbif)
library(ggplot2)
library(dplyr)
library(rgbif)
library(BIRDS)








ui <- fluidPage(plotOutput(outputId = "hawfinch_plot"), leafletOutput(outputId = "map"))



server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet() %>% 
    addTiles(group= "OSM") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group= "Satellite") %>%
    setView(lng = -3.0886, lat = 54.4609, zoom = 9) %>%
    addFeatures(lakes_ll, group = "Lakes Map") %>%
    addFeatures(rivers_ll, group = "Rivers Map")%>%
    addFeatures(settle_ll, group = "Settlements Map")%>%
    addRasterImage(elevation_ll ,col=terrain.colors(30), group = "Elevation Map") %>% 
    addCircleMarkers(squirrel2$decimalLongitude.processed, squirrel2$decimalLatitude.processed,
                     radius=2, fillOpacity = 0.5, opacity = 0.5, col="red", label = "Red Squirrel", group = "Red Squirrel")%>%
    addCircleMarkers(otter$decimalLongitude.processed, otter$decimalLatitude.processed,
                     radius=2, fillOpacity = 0.5, opacity = 0.5, col="blue", label = "Otter", group = "Otter")%>%
    addCircleMarkers(hawfinch$decimalLongitude.processed, hawfinch$decimalLatitude.processed,
                     radius=2, fillOpacity = 0.5, opacity = 0.5, col="yellow", label = "Hawfinch", group = "Hawfinch")%>%
    addLayersControl(
        baseGroups = c("OSM", "Satellite"),
        overlayGroups = c("Elevation Map", "Lakes Map", "Rivers Map", "Settlements Map", "Red Squirrel", "Otter", "Hawfinch"),
        options = layersControlOptions(collapsed = TRUE)
    )

    })
    observeEvent(input$map, {
        click<-input$map
        text<-paste("Latitude ", click$lat, "Longititude ", click$lng)
        print(text)
    })
    
    output$hawfinch_plot <- renderPlot(
        ggplot(hfrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
            geom_line() + xlab("Years") + ylab("Birds observed"))
    
}

shinyApp(ui = ui, server = server)
    





















labelOptions = labelOptions(interactive = "TRUE")