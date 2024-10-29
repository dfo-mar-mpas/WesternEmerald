#load libraries -----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(robis)
library(ggspatial)
library(viridis)

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load polygons
webca <- read_sf("data/shapefiles/webca.shp")%>%st_transform(CanProj)
clusters <- read_sf("data/shapefiles/bioclassification_clusters.shp")%>%
            filter(bioregion == 'MAR')%>%
            st_transform(CanProj)%>%
            mutate(cluster = case_when(cl == 6 ~ 'WSS/Outer BoF',
                                       cl == 5 ~ 'WSS: Banks/Inner BoF',
                                       cl == 4 ~ 'ESS: Banks',
                                       cl == 3 ~ 'ESS',
                                       cl == 2 ~ 'Laurentian Channel/Shelf Break',
                                       cl == 1 ~ 'Slope'))

uncertainty <- read_sf("data/shapefiles/bioclassification_uncertain_clusters.shp")%>%st_transform(CanProj)

#load the network polygons 
network <- data_draft_areas()%>%
  st_transform(CanProj)

#basemap
basemap <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                   dplyr::select(name_en,geometry)%>%
                   st_as_sf()%>%
                   st_union()%>%
                   st_transform(CanProj)%>%
                   st_as_sf()%>%
                   mutate(country="Canada"),
                 ne_states(country = "United States of America",returnclass = "sf")%>%
                   dplyr::select(name_en,geometry)%>%
                   st_as_sf()%>%
                   st_union()%>%
                   st_transform(CanProj)%>%
                   st_as_sf()%>%
                   mutate(country="USA"))

#set plotting limits
webca_lims <- webca%>%
              st_buffer(50*1000)%>%
              st_bbox()

uncertain_webca <- uncertainty%>%st_intersection(webca)
clusters_webca <- clusters%>%st_intersection(webca)
clusters_uncertain <- clusters_webca%>%st_intersection(uncertain_webca)

webca_classes <- ggplot()+
                  geom_sf(data=clusters_uncertain,aes(fill=cluster))+
                  geom_sf(data=network,fill=NA)+
                  geom_sf(data=basemap)+
                  geom_sf(data=webca,fill=NA,lwd=1.05)+
                  theme_bw()+
                  coord_sf(xlim=webca_lims[c(1,3)],ylim=webca_lims[c(2,4)])+
                  labs(fill="")+
                  theme(legend.position = "inside",
                        legend.position.inside = c(0.8,0.15),
                        legend.background = element_blank())+
                  annotation_scale()+
                  scale_fill_viridis(discrete = TRUE,direction = -1)


ggsave("output/webca_bioclassification_uncertainty.png",webca_classes,
       height=5,width=5,units="in",dpi=300)
