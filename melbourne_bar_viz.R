###########################################################################
#
# Title: # Melbourne bar vizualisation with leaflet and R
#
# Author: Julien-Yacine Chaqra
# Date: 07/01/2018
#
###########################################################################

# Packages ----------------------------------------------------------------
# install.packages("rgdal")
# devtools::install_github('rstudio/leaflet') 

library(htmlwidgets)
library(magrittr)
library(deldir)
library(rgdal)
library(mapproj)
library(dplyr)
library(leaflet)
library(sp)
library(ggplot2)
library(KernSmooth)
library(geosphere)
library(tableHTML)
library(plotly)


# Data --------------------------------------------------------------------
# https://www.data.gouv.fr/fr/datasets/bars-pubs-et-brasseries-artisanales-dopen-beer-map-ile-de-france-mai-2015/
# https://github.com/Guts/Paris-Beer-Week/blob/master/data/raw_data/getOpenBeerMap.py


## Update the data from the overpass api -----------------------------------
# execute_py_script <- paste0("python ", file.path(getwd(),"scrape_data.py"))
# system(execute_py_script)


# Import and vizualise ----------------------------------------------------

# Melbourne
biere <- rgdal::readOGR("./melbourne_bar_viz.geojson") %>% 
  as.data.frame() %>% 
  mutate(name = as.character(NAME), OSM_ID = as.character(OSM_ID)) %>% 
  filter(!is.na(name)) %>% 
  select(.,OSM_ID, name, coords.x1, coords.x2) %>%
  rename(longitude = coords.x1, latitude = coords.x2) %>% 
  distinct(longitude, latitude, .keep_all = TRUE)

# Simple vizualisation
melbourne_bar_basic_map <- leaflet() %>% 
  addTiles() %>% 
  setView(lat = -37.814, lng = 144.96332 , zoom = 12) %>%
  addCircles(lng = biere$longitude, lat = biere$latitude, radius = 1) %>%
  addCircleMarkers(lng = biere$longitude, lat = biere$latitude, popup = biere$name, radius = 2) 

melbourne_bar_basic_map
# saveWidget(widget = melbourne_bar_basic_map, file = "./melbourne_bar_basic_map.html")

# Voronoi diagram ---------------------------------------------------------
# Source
# http://flowingdata.com/2016/04/12/voronoi-diagram-and-delaunay-triangulation-in-r/
# https://rud.is/b/2015/07/26/making-staticinteractive-voronoi-map-layers-in-ggplotleaflet/

vor_pts <- SpatialPointsDataFrame(cbind(biere$longitude,
                                        biere$latitude),
                                  biere, match.ID=TRUE)

# bar with duplicated coordinates ?
# biere[duplicated(biere[,c("longitude", "latitude")]),]

SPointsDF_to_voronoi_SPolysDF <- function(sp) {
  # tile.list extracts the polygon data from the deldir computation
  vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2]))
  
  lapply(1:(length(vor_desc)), function(i) {
    
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    
    # now we can make the Polygon(s)
    Polygons(list(Polygon(tmp)), ID=i)
    
  }) -> vor_polygons
  
  # hopefully the caller passed in good metadata!
  sp_dat <- sp@data
  
  # this way the IDs _should_ match up w/the data & voronoi polys
  rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons),
                                  'polygons'),
                             slot, 'ID')
  
  SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                           data=sp_dat)
  
}

vor <- SPointsDF_to_voronoi_SPolysDF(sp = vor_pts)


# Voronoï
melbourne_bar_voronoi_map <- leaflet() %>% 
  addTiles() %>% 
  setView(lat = -37.814, lng = 144.96332 , zoom = 12) %>%
  addPolygons(data = vor, color = "#FF1E90", fillColor = "transparent", weight = 2) %>% 
  addCircles(lng = biere$longitude, lat = biere$latitude, radius = 1) %>%
  addCircleMarkers(lng = biere$longitude, lat = biere$latitude, popup = biere$name, radius = 2) 

melbourne_bar_voronoi_map
# saveWidget(widget = melbourne_bar_voronoi_map, file = "./melbourne_bar_voronoi_map.html")



# density heatmap  -------------------------------------------------------------
# Source:
# https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package

kde <- biere %>% select(., longitude, latitude) %>% 
  bkde2D(., bandwidth=c(.0045, .0068), gridsize = c(75,75))

CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)


melbourne_bar_density_map <- leaflet() %>% 
  addTiles() %>% 
  setView(lat = -37.814, lng = 144.96332 , zoom = 12) %>%
  addPolygons(data = spgons, color = heat.colors(n = NLEV, alpha = NULL)[LEVS]) %>% 
  addCircles(lng = biere$longitude, lat = biere$latitude, radius = 1) %>%
  addCircleMarkers(lng = biere$longitude, lat = biere$latitude, popup = biere$name, radius = 2) 

melbourne_bar_density_map
# saveWidget(widget = melbourne_bar_density_map, file = "./melbourne_bar_density_map.html")





# Counting the number of bar in a radius of 20, 100 and 500 meters --------

# Sometimes you wanna go from one bar to another
# google: "R number of point in a radius"

biere_arround <- cbind(biere, sapply(c(20, 100, 500), function(x){
  rowSums(geosphere::distm(biere[, c("longitude", "latitude")], fun = distHaversine) < x)
}) %>%  set_colnames(c("m_20", "m_100", "m_500"))) %>% arrange(desc(m_20), desc(m_100), desc(m_500))


ggplot(data = biere_arround, aes(x = m_20)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:3) +
  ggtitle("Number of bars in a radius of 20 meters") +
  xlab("Number of bars") +
  ylab("Count") 

ggplot(data = biere_arround, aes(x = m_100)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:10) +
  ggtitle("Number of bars in a radius of 100 meters") +
  xlab("Number of bars") +
  ylab("Count")

ggplot(data = biere_arround, aes(x = m_500)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  ggtitle("Number of bars in a radius of 500 meters") +
  xlab("Number of bars") +
  ylab("Count")
