#Install and load the relevant packages ####

library(ggplot2)
library(dplyr)
library(leaflet)
library(leafem)
library(mapview)
library(BIRDS)
library(sf)
library(raster)
library(rgbif)
options("rgdal_show_exportToProj4_warnings"="none")


# Elevation data ####

# Import raster elevation data Ordnance Survey projection
elevation <- raster("spatial/spatial/elevation.tif")

# Setting the colours so that low elevation is represented by green and high elevation to brown
plot(elevation, col=terrain.colors(30))


#Creating a dynamic map that can overlay different baselayers ####

# Converting to latitude-longitude to enable the dynamic projection
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude

elevation_ll <- projectRaster(elevation, crs=ll_crs)

# elevation500m is a coarser defined version of the elevation map that allows the script to run faster
elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together

elevation500m_ll <- projectRaster(elevation500m, crs=ll_crs)

mapview(elevation500m_ll)

#This will be displayed in leaflet 

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

#Settlement data 
settlement    <- st_read("spatial/spatial/cumbria_settlements.shp")
settlement_ll <- st_transform(settlement,crs=ll_crs)

# Lakes data
lakes    <- st_read("spatial/spatial/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes,crs=ll_crs)

# Roads data
roads    <- st_read("spatial/spatial/cumbria_roads.shp")
roads_ll <- st_transform(roads,crs=ll_crs)

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

# Reading in Red Squirrel records that were restricted to Cumbria using the polygon tool in NBN Atlas
# Any results that were not 'accepted', thus, a high level of certainty were filtered out
squirrel2 <- read.csv("squirrel/squirrel.csv")
squirrel2 <- squirrel2[squirrel2$identificationVerificationStatus.processed == "Accepted",]

# Identifying and displaying how records have changed over time
ggplot(squirrel2, aes(x=year.processed)) +
  geom_histogram()

sqrecords_per_yr <- squirrel2 %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(sqrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line()

# The same process is performed but for Hawfinch birds
hawfinch <- read.csv("records-2021-05-06/records-2021-05-06.csv")
hawfinch <- hawfinch[hawfinch$identificationVerificationStatus.processed == "Accepted",]

ggplot(hawfinch, aes(x=year.processed)) +
  geom_histogram()

hfrecords_per_yr <- hawfinch %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(hfrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line()

# Similarly also for Otters
otter <- read.csv("otter/otter.csv")
otter <- otter[otter$identificationVerificationStatus.processed == "Accepted",]

ggplot(otter, aes(x=year.processed)) +
  geom_histogram()

otrecords_per_yr <- otter %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(otrecords_per_yr, aes(x = year.processed, y=count_per_year)) +
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
#The overlayGroups and grouping feature within addCircleMarkers means these can be toggled on and off

species_plot <- leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(squirrel2$decimalLongitude.processed, squirrel2$decimalLatitude.processed, label = squirrel2$scientificName.processed, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", group = "Red Squirrel", popup = squirrel2$scientificNameprocessed) %>%
  addCircleMarkers(hawfinch$decimalLongitude.processed, hawfinch$decimalLatitude.processed, label = hawfinch$scientificName.processed, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group = "Hawfinch", popup = hawfinch$scientificName.processed) %>%
  addCircleMarkers(otter$decimalLongitude.processed, otter$decimalLatitude.processed, label = otter$scientificName.processed,
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="yellow", group = "Otter", popup = otter$scientificNameprocessed) %>% 
  addCircleMarkers(bat$decimalLongitude.processed, bat$decimalLatitude.processed, label = bat$scientificName.processed,
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="pink", group = "Natterer's Bat", popup = bat$scientificNameprocessed) %>%  
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Red Squirrel", "Hawfinch", "Otter", "Natterer's Bat"),
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
  addFeatures(cable_ll, group = "Cables Map")%>%
  addFeatures(substation_ll, group = "Substations Map")%>%
  addFeatures(ohl_ll, group = "Overhead Lines Map")%>%
  addFeatures(tower_ll, group = "Towers Map")%>%
  addFeatures(settlement_ll, group = "Settlements Map")%>%
  addRasterImage(elevation500m_ll ,col=terrain.colors(30), group = "Elevation Map") %>% 
  addCircleMarkers(squirrel2$decimalLongitude.processed, squirrel2$decimalLatitude.processed, label = squirrel2$scientificName.processed,
                   radius=2, fillOpacity = 0.5, opacity = 0.5, col="red", group = "Red Squirrel", popup = squirrel2$scientificName.processed,)%>%
  addCircleMarkers(otter$decimalLongitude.processed, otter$decimalLatitude.processed, label = otter$scientificName.processed,
                   radius=2, fillOpacity = 0.5, opacity = 0.5, col="blue", group = "Otter", label = otter$scientificName.processed,)%>%
  addCircleMarkers(hawfinch$decimalLongitude.processed, hawfinch$decimalLatitude.processed, popup = hawfinch$scientificName.processed,
                   radius=2, fillOpacity = 0.5, opacity = 0.5, col="yellow", group = "Hawfinch", popup = hawfinch$scientificNameprocessed)%>%
  addCircleMarkers(bat$decimalLongitude.processed, bat$decimalLatitude.processed, label = bat$scientificName.processed,
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="pink", group= "Natterer's Bat", popup = bat$scientificNameprocessed) %>% 
  addLayersControl(
    baseGroups = c("OSM", "Satellite"),
    overlayGroups = c("Elevation Map", "Lakes Map", "Rivers Map", "Settlements Map", "Red Squirrel", "Otter", "Natterer's Bat", "Hawfinch", "Cables Map", "Towers Map", "Substations Map", "Overhead Lines Map"),
    options = layersControlOptions(collapsed = TRUE)
  )

map_view










