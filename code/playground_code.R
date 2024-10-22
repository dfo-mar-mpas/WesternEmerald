#load libraries -----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(robis)
library(ggspatial)

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load mapping data -- 
#Load the Scotian Shelf-Bay of Fundy Planning Region
bioregion <- data_planning_areas()%>%
  st_transform(CanProj)

#load the network polygons 
network <- data_draft_areas()%>%
  st_transform(CanProj)

webca <- network%>%filter(SiteName_E == "Western/Emerald Banks Marine Refuge")

plot_lims <- webca%>%
            st_buffer(350*1000)%>%
            st_bbox()

bioclassification <- read_sf("data/shapefiles/bioclassification_clusters.shp")%>%
  st_transform(CanProj)

bioclassification_webca <- bioclassification%>%
  st_make_valid()%>%
  st_intersection(webca)

webca_buffer <- webca%>%
              st_buffer(200*1000)


ggplot()+
  geom_sf(data=bioclassification,aes(fill=factor(cl)))+
  geom_sf(data=webca,fill=NA)+
  geom_sf(data=webca_buffer,fill="white",alpha=0.5)+
  theme_bw()+
  coord_sf(xlim=plot_lims[c(1,3)],
           ylim=plot_lims[c(2,4)],
           expand=0)+
  annotation_scale()
