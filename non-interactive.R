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

