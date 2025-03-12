### Initial code to make a map and show how to use sf. 

#load libraries ----
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(MarConsNetData)

s2_as_sf = FALSE

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Load the Scotian Shelf-Bay of Fundy Planning Region
bioregion <- data_planning_areas()%>%
              st_transform(CanProj)

#load the network polygons 
network <- data_draft_areas()%>%
           st_transform(CanProj)

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

#set up plot limits
plot_lims <- bioregion%>%
             st_bbox()%>% #get the bounding box
             st_as_sfc()%>%
             st_transform(utm)%>% #convert to a planar (km) projection
             st_buffer(25)%>% #create a buffer on that bounding box of 25 km - this is faster than doing a buffer on the polygon
             st_transform(CanProj)%>%
             st_bbox()

#make a plot for the readme
p1 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=network)+
  geom_sf(data=network%>%filter(SiteName_E == "Western/Emerald Banks Marine Refuge"),fill="coral2")+
  coord_sf(expand=0,xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)])+
  theme_bw()

ggsave("output/readmeplot.png",p1,width=3.5,height=4.5,units="in",dpi=300) #there is some guess work with the height and width ratio. I am not sure of the best way to do it. 
knitr::plot_crop("output/readmeplot.png")  

#map for the poster


current_width <- plot_lims["xmax"] - plot_lims["xmin"]
current_height <- plot_lims["ymax"] - plot_lims["ymin"]
current_ratio <- current_width / current_height

# Target ratio is 4:3
target_ratio <- 4/3

if (current_ratio < target_ratio) {
  # Need to increase width
  width_to_add <- (target_ratio * current_height) - current_width
  plot_lims["xmin"] <- plot_lims["xmin"] - (width_to_add / 2)
  plot_lims["xmax"] <- plot_lims["xmax"] + (width_to_add / 2)
} else if (current_ratio > target_ratio) {
  # Need to increase height
  height_to_add <- (current_width / target_ratio) - current_height
  plot_lims["ymin"] <- plot_lims["ymin"] - (height_to_add / 2)
  plot_lims["ymax"] <- plot_lims["ymax"] + (height_to_add / 2)
}



p2 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=network,fill="grey80",alpha=0.7)+
  geom_sf(data=network%>%filter(SiteName_E == "Western/Emerald Banks Marine Refuge"),fill="coral2",linewidth=1.3,alpha=0.7)+
  coord_sf(expand=0,xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)])+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"))

ggsave("output/posterplot.png",p2,width=48,height=36,units = "in",dpi=600)
knitr::plot_crop("output/posterplot.png")  #gets rid of any white space. 

#more of a NS focus


plot_lims <- network%>%
  filter(SiteName_E == "Western/Emerald Banks Marine Refuge")%>%
  st_bbox()%>% #get the bounding box
  st_as_sfc()%>%
  st_transform(utm)%>% #convert to a planar (km) projection
  st_buffer(200)%>% 
  st_transform(CanProj)%>%
  st_bbox()

current_width <- plot_lims["xmax"] - plot_lims["xmin"]
current_height <- plot_lims["ymax"] - plot_lims["ymin"]
current_ratio <- current_width / current_height

# Target ratio is 4:3
target_ratio <- 4/3

if (current_ratio < target_ratio) {
  # Need to increase width
  width_to_add <- (target_ratio * current_height) - current_width
  plot_lims["xmin"] <- plot_lims["xmin"] - (width_to_add / 2)
  plot_lims["xmax"] <- plot_lims["xmax"] + (width_to_add / 2)
} else if (current_ratio > target_ratio) {
  # Need to increase height
  height_to_add <- (current_width / target_ratio) - current_height
  plot_lims["ymin"] <- plot_lims["ymin"] - (height_to_add / 2)
  plot_lims["ymax"] <- plot_lims["ymax"] + (height_to_add / 2)
}

p3 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=network,fill="grey80",alpha=0.7)+
  geom_sf(data=network%>%filter(SiteName_E == "Western/Emerald Banks Marine Refuge"),fill="coral2",linewidth=1.3,alpha=0.7)+
  coord_sf(expand=0,xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)])+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"))

ggsave("output/posterplot2.png",p3,width=48,height=36,units = "in",dpi=300)
knitr::plot_crop("output/posterplot2.png")  #gets rid of any white space. 
