# Install and load the relevant packages ####

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
library(shiny)
library(sf)
library(raster)
library(leaflet)
library(leafem)

library(rgdal)
source("LOS.R")

# Load up the non-interactive data in preparation for use in the shiny app ####

# Elevation data ####

# Import raster elevation data Ordnance Survey projection
elevation <- raster("spatial/spatial/elevation.tif")

# Setting the colours so that low elevation is represented by green and high elevation to brown
plot(elevation, col=terrain.colors(30))


# Creating a dynamic map that can overlay different baselayers ####

# Converting to latitude-longitude to enable the dynamic projection
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude

elevation_ll <- projectRaster(elevation, crs=ll_crs)

# elevation500m is a coarser defined version of the elevation map that allows the script to run faster
elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together

elevation500m_ll <- projectRaster(elevation500m, crs = ll_crs)

mapview(elevation500m_ll)

# This will be displayed in leaflet 

elevation_view <- leaflet() %>% 
    addTiles(group = "OSM (default)") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation") %>% 
    addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"), 
        overlayGroups = c("Elevation"),
        options = layersControlOptions(collapsed = TRUE)
    )

# To view it
elevation_view  


# Loading in other variables in the same format as the elevation data, converting it to latitude and longitude ####

# Settlement data 
settlement    <- st_read("spatial/spatial/cumbria_settlements.shp")
settlement_ll <- st_transform(settlement, crs = ll_crs)

# Lakes data
lakes    <- st_read("spatial/spatial/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes, crs = ll_crs, color="red")


# Roads data
roads    <- st_read("spatial/spatial/cumbria_roads.shp")
roads_ll <- st_transform(roads, crs = ll_crs)

# Rivers data
rivers    <- st_read("spatial/spatial/cumbria_rivers.shp")
rivers_ll <- st_transform(rivers, crs = ll_crs)

# Cable data
cable    <- st_read("all_datasets5_0/Cable.shp")
cable_ll <- st_transform(cable, crs = ll_crs)

# Substation data
substation    <- st_read("all_datasets5_0/Substations.shp")
substation_ll <- st_transform(substation, crs = ll_crs)

# Overhead Lines data
ohl    <- st_read("all_datasets5_0/OHL.shp")
ohl_ll <- st_transform(ohl, crs = ll_crs)

# Tower data
tower    <- st_read("all_datasets5_0/Towers.shp")
tower_ll <- st_transform(tower, crs = ll_crs)


# Downloading data from the NBN Atlas ####

# Species were based on those that may be of particular interest to see and record the wildlife of Cumbria

# Reading in Ruddy Duck records that were restricted to Cumbria using the polygon tool in NBN Atlas
# Any results that were not 'accepted', thus, a high level of certainty were filtered out
ruddy_duck <- read.csv("ruddy_duck/ruddy_duck.csv")
#ruddy_duck <- ruddy_duck[ruddy_duck$identificationVerificationStatus.processed == "Accepted",]

# Identifying and displaying how records have changed over time
ggplot(ruddy_duck, aes(x=year.processed)) +
    geom_histogram()

rdrecords_per_yr <- ruddy_duck %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

ggplot(rdrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line()

# The same process is performed but for Egyptian Geese
egyptian_goose <- read.csv("egyptian_goose/egyptian_goose.csv")
#egyptian_goose <- egyptian_goose[egyptian_goose$identificationVerificationStatus.processed == "Accepted",]

ggplot(egyptian_goose, aes(x=year.processed)) +
    geom_histogram()

egrecords_per_yr <- egyptian_goose %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

ggplot(egrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line()

# Finally observe records for the Natterer's Bat
bat <- read.csv("records-2021-05-09/records-2021-05-09.csv")
bat <- bat[bat$identificationVerificationStatus.processed == "Accepted",]

ggplot(bat, aes(x=year.processed)) +
    geom_histogram()

batrecords_per_yr <- bat %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

ggplot(batrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line()

# Creating an intermediate step of showing the species first in their own interactive map ####
# Using addCircleMarkers the data can be plotted with identifying features
# The overlayGroups and grouping feature within addCircleMarkers means these can be toggled on and off

species_plot <- leaflet() %>%
    addTiles(group = "OSM (default)") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
    addCircleMarkers(ruddy_duck$decimalLongitude.processed, ruddy_duck$decimalLatitude.processed, label = ruddy_duck$scientificName.processed, 
                     labelOptions = labelOptions(interactive = "TRUE"),
                     radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", group = "Ruddy Duck", popup = ruddy_duck$scientificNameprocessed) %>%
    addCircleMarkers(egyptian_goose$decimalLongitude.processed, egyptian_goose$decimalLatitude.processed, label = egyptian_goose$scientificName.processed, 
                     labelOptions = labelOptions(interactive = "TRUE"),
                     radius = 2, fillOpacity = 0.5, opacity = 0.5, col="yellow", group = "Egyptian Goose", popup = egyptian_goose$scientificName.processed) %>%
    addCircleMarkers(bat$decimalLongitude.processed, bat$decimalLatitude.processed, label = bat$scientificName.processed,
                     labelOptions = labelOptions(interactive = "TRUE"),
                     radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group = "Natterer's Bat", popup = bat$scientificNameprocessed) %>%  
    addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"), 
        overlayGroups = c("Ruddy Duck", "Egyptian Goose", "Natterer's Bat"),
        options = layersControlOptions(collapsed = TRUE)
    )  

species_plot


# Joining these two intermediary steps together to be displayed on one map shwoing the environment and species of interest ####

map_view <- leaflet() %>% 
    addTiles(group= "OSM") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group= "Satellite") %>%
    setView(lng = -3.0886, lat = 54.4609, zoom = 9) %>%
    addFeatures(lakes_ll, group = "Lakes Map") %>%
    addFeatures(rivers_ll, group = "Rivers Map")%>%
    addFeatures(cable_ll, color = "pink", group = "Cables Map")%>%
    addFeatures(substation_ll, color = "pink", group = "Substations Map")%>%
    addFeatures(ohl_ll, color = "pink", group = "Overhead Lines Map")%>%
    addFeatures(tower_ll, color = "pink", group = "Towers Map")%>%
    addFeatures(settlement_ll, group = "Settlements Map")%>%
    addRasterImage(elevation500m_ll ,col=terrain.colors(30), group = "Elevation Map") %>% 
    addCircleMarkers(ruddy_duck$decimalLongitude.processed, ruddy_duck$decimalLatitude.processed, label = ruddy_duck$scientificName.processed,
                     radius=2, fillOpacity = 0.5, opacity = 0.5, col="red", group = "Ruddy Duck", popup = ruddy_duck$scientificName.processed,)%>%
    addCircleMarkers(egyptian_goose$decimalLongitude.processed, egyptian_goose$decimalLatitude.processed, label = egyptian_goose$scientificName.processed,
                     radius=2, fillOpacity = 0.5, opacity = 0.5, col="yellow", group = "Egyptian Goose", popup = egyptian_goose$scientificNameprocessed)%>%
    addCircleMarkers(bat$decimalLongitude.processed, bat$decimalLatitude.processed, label = bat$scientificName.processed,
                     labelOptions = labelOptions(interactive = "TRUE"),
                     radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group= "Natterer's Bat", popup = bat$scientificNameprocessed) %>% 
    addLayersControl(
        baseGroups = c("OSM", "Satellite"),
        overlayGroups = c("Elevation Map", "Lakes Map", "Rivers Map", "Settlements Map", "Ruddy Duck", "Natterer's Bat", "Egyptian Goose", "Cables Map", "Towers Map", "Substations Map", "Overhead Lines Map"),
        options = layersControlOptions(collapsed = TRUE)
    )

map_view


# Load in images of the species of interest to be used later in the decision support system ####

egyptian_goose_image <- base64enc::dataURI(file="www/egyptian_goose.JPG", mime="image/jpg")
ruddy_duck_image     <- base64enc::dataURI(file="www/ruddy_duck.JPG", mime="image/jpg")
natterers_bat_image  <- base64enc::dataURI(file="www/natterers_bat_image.JPG", mime="image/jpg")


# Similarly name the relevant record plots so they can be fed into the shiny app easier ####

bat_plot <- ggplot(batrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line()
ruddy_duck_plot <- ggplot(rdrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line()
egyptian_goose_plot <- ggplot(egrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line()


# Using the above information, process it into the shiny app to display an interactive map
# Add pieces of text to provide a more rounded description to aid the users experience
# Use radioButtons to provide a checklist where the user can select an option

ui <- fluidPage(
    
    # Application title
    titlePanel("The Rural Environment in Cumbria"),
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel( p(strong('Species of Interest:')),
                      
                      p(strong('The Ruddy Duck:'),
                        "The Ruddy Duck (Oxyura jamaicensis) is a bird from North America that is known for its prominent stiff tail. They also have slightly peaked heads and fairly short, thick necks. 
                      In the summer, male ruddy ducks' bills turn a distinctive bright blue colour, as seen in the image. 
                      In the winter they tend to be a much more grey or brown colour similar to females. 
                      Ruddy Ducks were imported to the UK in 1948 where they bred in the wild, and by 2000 the UK population numbered 6,000 birds.
                      Eradication programmes have reduced the UK’s population to 400-500 birds, evident by the sheer drop off in records seen by the corresponding graph. 
                        Its records can be seen as red markers in the interactive map at the bottom of the page."),
                      
                      img(src=ruddy_duck_image, height="100%", width="100%", align = "centre"),
                      
                      p(strong('The Egyptian Goose:'),
                        "The Egyptian goose (Alopochen aegyptiaca) is native to Africa. It was first brought to Britain in the 17th Century as an ornamental bird for country gentlemen
                      Used to warmer weather, the Egyptian Goose struggled to establish itself in the harsh conditions of the UK. Recently it has witnessed a population explosion across the nation. 
                      As the graph shows, Cumbria experienced a population explosion in 2008 but has not seemed to stabilise, causing it to still be rare. 
                      They can be easily identified with a distinctive 'eyepatch' marking around the eye and long thin legs comparable to a chickens.
                      These can be seen on the interactive map as yellow markers."), 
                      
                      img(src=egyptian_goose_image, height="100%", width="100%", align = "centre"),
                      
                      p(strong('The Natterer`s Bat:'),
                        "The Natterer's Bat (Myotis nattereri) was revealed to be the only species of bat that hibernates in Cumbria. Thus, recordings and observations of its whereabouts are vital for conservation efforts.
                          This medium-sized bat is a nocturnal creature that mostly feeds off moths and other flying insects. It will often be recorded roosting in old buildings such as churches.
                          The graph of its records show it is a much more consistent sighting in Cumbria but does succumb to yearly fluctuations
                        Its data points can be seen in the interactive map, coloured blue."),
                      
                      img(src=natterers_bat_image, height="100%", width="100%", align = "centre"), 
                      
                      
                      
                      
                      p("Evaluating the information provided, please give some feedback offering an opinion on the feasibility and consequences or lack of that implementing a wind farm may have for the rural environment."),
                      
                      
                      radioButtons(inputId = "my_checkgroup", 
                                   h2("Do you think Cumbria could facilitate more onshore wind farms"), 
                                   choices = list("Yes" = 1, 
                                                  "No" = 2, 
                                                  "Depends how they were instated" = 3,
                                                  "Unsure, require more information" = 4),
                                   selected = 1), 
                      actionButton(inputId="my_submitstatus", label="Submit")),
        
        mainPanel( p("Please feel free to read the side bar, which details species of interest present in Cumbria. 
        Below are graphs for records of the corresponding species. 
        Underneath these, an interactive map can be used to investigate different environmental variables, toggling them on and off as you wish.
        This decision support system enables a more complete judgement to be made when considering different factors that may affect large scale projects such as the installation of a wind farm."),
                   
                   plotOutput(outputId = "ruddy_duck_plot"),
                   
                   plotOutput(outputId = "egyptian_goose_plot"),
                   
                   plotOutput(outputId = "bat_plot"),
                   
                   leafletOutput(outputId = "map_view"),
                   
                   sliderInput("radius", "radius",
                               min = 1000, max = 10000,
                               value = 5000),
                   
                   leafletOutput(outputId = "viewshed"))))


server <- function(input, output, session) {
    output$map_view <- renderLeaflet({
        leaflet() %>% 
            addTiles(group= "OSM") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group= "Satellite") %>%
            setView(lng = -3.0886, lat = 54.4609, zoom = 9) %>%
            addFeatures(lakes_ll, group = "Lakes Map") %>%
            addFeatures(rivers_ll, group = "Rivers Map")%>%
            addFeatures(cable_ll, color = "pink", group = "Cables Map")%>%
            addFeatures(substation_ll, group = "Substations Map")%>%
            addFeatures(ohl_ll, color = "pink", group = "Overhead Lines Map")%>%
            addFeatures(tower_ll, color = "pink", group = "Towers Map")%>%
            addFeatures(settlement_ll, group = "Settlements Map")%>%
            addRasterImage(elevation500m_ll ,col=terrain.colors(30), group = "Elevation Map") %>% 
            addCircleMarkers(ruddy_duck$decimalLongitude.processed, ruddy_duck$decimalLatitude.processed, label = ruddy_duck$scientificName.processed,
                             radius=2, fillOpacity = 0.5, opacity = 0.5, col="red", group = "Ruddy Duck", popup = ruddy_duck$scientificName.processed,)%>%
            addCircleMarkers(egyptian_goose$decimalLongitude.processed, egyptian_goose$decimalLatitude.processed, label = egyptian_goose$scientificName.processed,
                             radius=2, fillOpacity = 0.5, opacity = 0.5, col="yellow", group = "Egyptian Goose", popup = egyptian_goose$scientificName.processed,)%>%
            addCircleMarkers(bat$decimalLongitude.processed, bat$decimalLatitude.processed, label = bat$scientificName.processed,
                             labelOptions = labelOptions(interactive = "TRUE"),
                             radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group= "Natterer's Bat", popup = bat$scientificNameprocessed) %>% 
            addLayersControl(
                baseGroups = c("OSM", "Satellite"),
                overlayGroups = c("Elevation Map", "Lakes Map", "Rivers Map", "Settlements Map", "Ruddy Duck", "Egyptian Goose", "Natterer's Bat", "Cables Map", "Towers Map", "Substations Map", "Overhead Lines Map"),
                options = layersControlOptions(collapsed = TRUE)
            )
        
    })
    
    sliderValues <- reactive({
        
        data.frame(
            Name = c("radius"),
            Value = as.character(c(input$radius)),
            stringsAsFactors = FALSE)
        
    })
    
    output$viewshed <- renderLeaflet({
        leaflet() %>%
            setView(lng = -3.0886, lat=54.4609, zoom=11) %>%
            addRasterImage(elevation_ll, colors=terrain.colors(25))
    })
    
    # Detect a click event and grab coordinates for viewshed
    observeEvent(input$viewshed_click, {
        coord <- input$viewshed_click
        lng <- coord$lng 
        lat <- coord$lat
        
        # Create a sf points feature in latitude longitude 4326, and project back to OS 2770
        # The if(length(c(lng,lat))) needed as when Shiny starts there is no data here
        # so would otherwise give an error. On clicking the map, the lng and lat recorded
        if(length(c(lng,lat))==2){
            turbine_pt_ll <- data.frame(lat = lat, lng = lng) %>% 
                st_as_sf(coords = c("lng", "lat")) %>% 
                st_set_crs(4326)
            turbine_pt_os <- st_transform(turbine_pt_ll, crs=27700)
            turbine_pt_os <- st_geometry(turbine_pt_os)[[1]] # Only want geometry
            
            # Calculate 5km viewshed and reproject back to lat-lon
            viewshed_5km_os <- viewshed(dem=elevation500m, windfarm=turbine_pt_os,
                                        h1=1.5, h2=70, radius=5000)
            viewshed_5km_ll <- projectRaster(viewshed_5km_os, crs=ll_crs)
            
            # Add to existing map            
            leafletProxy("viewshed") %>% 
                addRasterImage(viewshed_5km_ll, color="red")
    
        observeEvent(input$map_view, {
        click<-input$map_view
        text<-paste("Latitude ", click$lat, "Longitude ", click$lng)
        print(text)
    })
        }
    })
    
    
    output$ruddy_duck_plot <- renderPlot(
        ggplot(rdrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
            geom_line()+ ggtitle("Change over time of Ruddy Duck records in Cumbria") + xlab("Years of observation") + ylab("No. Ruddy Ducks recorded") +
            theme_classic())
    
    output$egyptian_goose_plot <- renderPlot(
        ggplot(egrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
            geom_line()+ ggtitle("Change over time of Egyptian Goose records in Cumbria") + xlab("Years of observation") + ylab("No. Egyptian Geese recorded") +
            theme_classic())
    
    output$bat_plot <- renderPlot(
        ggplot(batrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
            geom_line() + ggtitle("Change over time of Natterer's Bat records in Cumbria") + xlab("Years of observation") + ylab("No. Natterer's Bat recorded") + 
            theme_classic())
    
}

# Run the shiny----

shinyApp(ui = ui, server = server)




