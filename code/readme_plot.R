### Initial code to make a map and show how to use sf. 

#load libraries ----
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(MarConsNetData)
library(terra)

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

#Process bathymetry data 
load("data/rvdata.RData")

deep <- rvdata%>%data.frame()%>%filter(!is.na(DEPTH))%>%pull(DEPTH)%>%quantile(.,0.9)%>%round(.,1)%>%as.numeric()*-1 #90th percentile
shallow <- rvdata%>%data.frame()%>%filter(!is.na(DEPTH))%>%pull(DEPTH)%>%quantile(.,0.1)%>%round(.,1)%>%as.numeric()*-1 #10th percentile

mar_bathy <- rast("data/gebco_cropped.tif") #this is a cropped bathymetric layer to the Maritimes Bioregion from here - https://www.gebco.net/

contour_deep <- as.contour(mar_bathy, levels = -250)%>% # could also use 237 from 'deep' but just round out to 250
  st_as_sf()%>%
  st_transform(CanProj)%>%
  st_intersection(bioregion)

contour_shallow <- as.contour(mar_bathy, levels = shallow)%>%
  st_as_sf()%>%
  st_transform(CanProj)%>%
  st_intersection(bioregion)

##load in RV stations

webca <- network %>%
  filter(site == "Western/Emerald Banks Marine Refuge") %>%
  st_transform(CanProj) %>%
  st_make_valid()%>%
  st_union() %>%
  st_buffer(dist = 1) %>%  # Small positive buffer to connect nearby polygons
  st_buffer(dist = -1) %>% # Negative buffer to shrink back
  st_cast("POLYGON")%>%
  st_make_valid()%>%
  st_union()

#polygons for the buffers
buffer_shapes <- st_read("data/shapefiles/webca_buffers.shp")%>%st_transform(CanProj)

#snip out WEBCA and the 50 buffer from the 100 so the transparency looks right when plotting
buffer_poly <- rbind(
               buffer_shapes%>%
               filter(buffer==50)%>%
               st_difference(webca),
               buffer_shapes%>%
               filter(buffer==100)%>%
               st_difference(buffer_shapes%>%filter(buffer==50))%>%
               dplyr::select(buffer)
               )%>%
               arrange(-buffer)%>%
               mutate(buffer=as.character(buffer))

#load RV sets
load("data/rvstations_filtered.RData")

rv_formatted <- rvstations_filtered%>%
  rename(buffer=distance_category)%>%
  mutate(buffer=as.character(buffer))

#make a plot for the readme
p1 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=network)+
  geom_sf(data=network%>%filter(SiteName_E == "Western/Emerald Banks Marine Refuge"),fill="orange")+
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
  geom_sf(data=contour_deep,linetype=2,linewidth=0.5,col="grey30")+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=buffer_poly,aes(fill=buffer),alpha=0.8)+
  geom_sf(data=rv_formatted%>%filter(YEAR>2015),aes(fill=buffer),shape=21,col="black")+
  geom_sf(data=network%>%filter(site != "Western/Emerald Banks Marine Refuge"),fill="grey80",alpha=0.7)+
  geom_sf(data=network%>%filter(site == "Western/Emerald Banks Marine Refuge"),fill=NA,linewidth=1.3,alpha=0.7)+
  scale_fill_manual(values = c("50" = "orange", "100" = "cornflowerblue"))+
  coord_sf(expand=0,xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)])+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"),
        legend.position = "none")

ggsave("output/posterplot.png",p2,width=48,height=36,units = "in",dpi=600)

#more of a NS focus


plot_lims2 <- network%>%
  filter(site == "Western/Emerald Banks Marine Refuge")%>%
  st_bbox()%>% #get the bounding box
  st_as_sfc()%>%
  st_transform(utm)%>% #convert to a planar (km) projection
  st_buffer(200)%>% 
  st_transform(CanProj)%>%
  st_bbox()

current_width <- plot_lims2["xmax"] - plot_lims2["xmin"]
current_height <- plot_lims2["ymax"] - plot_lims2["ymin"]
current_ratio <- current_width / current_height

# Target ratio is 4:3
target_ratio <- 4/3

if (current_ratio < target_ratio) {
  # Need to increase width
  width_to_add <- (target_ratio * current_height) - current_width
  plot_lims2["xmin"] <- plot_lims2["xmin"] - (width_to_add / 2)
  plot_lims2["xmax"] <- plot_lims2["xmax"] + (width_to_add / 2)
} else if (current_ratio > target_ratio) {
  # Need to increase height
  height_to_add <- (current_width / target_ratio) - current_height
  plot_lims2["ymin"] <- plot_lims2["ymin"] - (height_to_add / 2)
  plot_lims2["ymax"] <- plot_lims2["ymax"] + (height_to_add / 2)
}

p3 <-ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=buffer_poly,aes(fill=buffer),alpha=0.8)+
  geom_sf(data=contour_deep,linetype=2,linewidth=1,col="grey10")+
  geom_sf(data=rv_formatted%>%filter(YEAR>2015),aes(fill=buffer),shape=21,col="black",size=10)+
  geom_sf(data=network%>%filter(site != "Western/Emerald Banks Marine Refuge"),fill="grey80",alpha=0.7)+
  geom_sf(data=network%>%filter(site == "Western/Emerald Banks Marine Refuge"),fill=NA,linewidth=4,alpha=0.7)+
  scale_fill_manual(values = c("50" = "orange", "100" = "cornflowerblue"))+
  coord_sf(expand=0,xlim=plot_lims2[c(1,3)],ylim=plot_lims2[c(2,4)])+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"),
        legend.position="none")

ggsave("output/posterplot3.png",p3,width=48,height=36,units = "in",dpi=300)


#### code for rv formattingg

# load("data/rvdata.RData")
# webca <- read_sf("data/shapefiles/webca.shp")%>% #note that this is different than pulling from 'network' as this shape file has two parts, which are needed to do the buffer properly
#   st_transform(CanProj)
# 
# bioclass <- read_sf("data/shapefiles/bioclassification_clusters.shp") %>%
#   filter(bioregion == "MAR", cl %in% c(5, 6)) %>%  # Filter for bioclasses 5 and 6
#   mutate(classification = case_when(
#     bioregion == 'MAR' & cl == 6 ~ 'WSS/Outer BoF',
#     bioregion == 'MAR' & cl == 5 ~ 'WSS: Banks/Inner BoF'
#   )) %>%
#   st_transform(CanProj)
# 
# 
# #create different buffered distances -----
# webca_50 <- webca%>%
#   st_transform(utm)%>% #planar 'km' coordinates
#   st_buffer(dist = 50)%>%
#   st_union()%>%
#   st_as_sf()%>%
#   st_transform(CanProj)
# 
# webca_100 <- webca%>%
#   st_transform(utm)%>% #planar 'km' coordinates
#   st_buffer(dist = 100)%>%
#   st_union()%>%
#   st_as_sf()%>%
#   st_transform(CanProj)
# 
# network <- data_draft_areas() %>% 
#   st_transform(CanProj)%>%
#   rename(site=SiteName_E,type=Classification_E,geometry=geoms)
# 
# rvstations_filtered <- rvdata%>%
#   st_transform(CanProj)%>%
#   st_join(.,network%>%dplyr::select(site,type,geometry))%>%
#   st_join(.,bioclass%>%dplyr::select(classification,geometry))%>%
#   mutate(
#     distance_category = case_when(
#       st_intersects(.,webca%>%st_union(), sparse = FALSE) ~ 0,
#       st_intersects(., webca_50, sparse = FALSE) ~ 50,
#       st_intersects(., webca_100, sparse = FALSE) ~ 100,
#       TRUE ~ NA
#     ),
#     in_network = ifelse(!is.na(site), TRUE, FALSE),
#     in_western_emerald_bank = ifelse(site == "Western/Emerald Banks Marine Refuge", 
#                                      TRUE, 
#                                      FALSE)
#   )%>%
#   filter(!is.na(distance_category))%>%
#   mutate(id = paste(SETNO, YEAR, sep = "-")) %>%  # Use unique id
#   distinct(id, .keep_all = TRUE)

#save(rvstations_filtered,file="data/rvstations_filtered.RData")
