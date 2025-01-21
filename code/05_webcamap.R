#Create a pretty map code

#load libraries ----
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(MarConsNetData)
library(patchwork)
library(ggspatial)

s2_as_sf = FALSE

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Load the Scotian Shelf-Bay of Fundy Planning Region
bioregion <- data_planning_areas()%>%
  st_transform(CanProj)

#load the network polygons 
network <- data_draft_areas()%>%
  st_transform(CanProj)

webca <- network%>%
         filter(SiteName_E == "Western/Emerald Banks Marine Refuge")%>%
         st_make_valid()

#load the bioclassification layers
bioclass <- read_sf("data/shapefiles/bioclassification_clusters.shp")%>%
            filter(bioregion=="MAR")%>%
            mutate(classification =  case_when(
              bioregion == 'MAR' & cl == 6 ~ 'WSS/Outer BoF',
              bioregion == 'MAR' & cl == 5 ~ 'WSS: Banks/Inner BoF',
              bioregion == 'MAR' & cl == 4 ~ 'ESS: Banks',
              bioregion == 'MAR' & cl == 3 ~ 'ESS',
              bioregion == 'MAR' & cl == 2 ~ 'Laurentian Channel/Shelf Break',
              bioregion == 'MAR' & cl == 1 ~ 'Slope'))%>%
            st_transform(CanProj)

webca_class <- bioclass%>%
               filter(classification %in% c('WSS/Outer BoF','WSS: Banks/Inner BoF'))%>%
               st_intersection(webca)

#download a basemap
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
                   mutate(country="USA"),
                 ne_states(country = "Greenland",returnclass = "sf")%>%
                   dplyr::select(name_en,geometry)%>%
                   st_as_sf()%>%
                   st_union()%>%
                   st_transform(CanProj)%>%
                   st_as_sf()%>%
                   mutate(country="USA"))

#create plot limits
webca_lim <- webca%>%
             st_transform(utm)%>%
             st_buffer(40)%>%
             st_transform(CanProj)%>%
             st_bbox()

region_lim <- bioregion%>%
              st_bbox()

#Make the zoomed in plot on WEBCA

p1 <- ggplot()+
      geom_sf(data=basemap)+
      geom_sf(data=webca_class,aes(fill=classification))+
      geom_sf(data=network,fill=NA)+
      theme_bw()+
      coord_sf(expand=0,xlim=webca_lim[c(1,3)],ylim=webca_lim[c(2,4)])+
      theme(legend.position = "inside",
            legend.position.inside = c(0.8,0.1),
            legend.title=element_blank(),
            legend.background = element_blank())+
      annotation_scale()

#plot of the larger bioregion
p2 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=webca_class,aes(fill=classification))+
  geom_sf(data=network,fill=NA)+
  geom_sf(data=webca_lim%>%st_as_sfc(),fill=NA,linewidth=0.8,lty=2)+
  theme_bw()+
  coord_sf(expand=0,xlim=region_lim[c(1,3)],ylim=region_lim[c(2,4)])+
  theme(legend.position = "none")+
  annotation_scale(location="br")

combo <- p1+p2+plot_layout(ncol=2)

ggsave("output/combo_webca_plot.png",combo,width=10,height=6,units="in",dpi=300)  
      
  
  