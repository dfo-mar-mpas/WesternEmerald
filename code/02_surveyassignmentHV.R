

## Rv survey analysis

# Load libraries ----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(ggspatial)
library(viridis)
library(patchwork)

# Projections ----
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Load mapping data ----
# Load the Scotian Shelf-Bay of Fundy Planning Region
bioregion <- data_planning_areas() %>% 
  st_transform(CanProj)

# Load the network polygons 
network <- data_draft_areas() %>% 
  st_transform(CanProj)%>%
  rename(site=SiteName_E,type=Classification_E,geometry=geoms)

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

#250m contour
contour_250 <- read_sf("data/shapefiles/contour_250.shp")%>%
               st_transform(CanProj)%>%
               st_intersection(bioregion)

#load basemap
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

#load RV data
load("data/rvdata.RData")

#pull out Western Emerald Bank Polygon
webca <- read_sf("data/shapefiles/webca.shp")%>% #note that this is different than pulling from 'network' as this shape file has two parts, which are needed to do the buffer properly
  st_transform(CanProj)%>%
  st_union()


#create different buffered distances -----

webca_50 <- webca%>%
  st_transform(utm)%>% #planar 'km' coordinates
  st_buffer(dist = 50)%>%
  st_union()%>%
  st_as_sf()%>%
  st_transform(CanProj)

webca_100 <- webca%>%
  st_transform(utm)%>% #planar 'km' coordinates
  st_buffer(dist = 100)%>%
  st_union()%>%
  st_as_sf()%>%
  st_transform(CanProj)

#now do the cut outs * this was missing before
webca_100 <- webca_100%>%st_difference(webca_50)

webca_50 <- webca_50%>%st_difference(webca)


buffer_poly <- rbind(webca%>%
                       st_union()%>%
                       st_as_sf(),
                     webca_50,webca_100)%>%
  mutate(buffer=c(0,50,100))%>%
  arrange(-buffer)

st_write(buffer_poly,"data/shapefiles/webca_buffers.shp")


##assign rvsets to the network and classifications

rv_df <- rvdata%>%
  st_transform(CanProj)%>%
  st_join(.,network%>%dplyr::select(site,type,geometry))%>%
  st_join(.,bioclass%>%dplyr::select(classification,geometry))%>%
  mutate(
    distance_category = case_when(
      st_intersects(.,webca%>%st_union(), sparse = FALSE) ~ 0,
      st_intersects(., webca_50, sparse = FALSE) ~ 50,
      st_intersects(., webca_100, sparse = FALSE) ~ 100,
      TRUE ~ NA
    ),
    in_network = ifelse(!is.na(site), TRUE, FALSE),
    in_western_emerald_bank = ifelse(site == "Western/Emerald Banks Marine Refuge", 
                                     TRUE, 
                                     FALSE)
  )

rvstations <- rv_df%>%
  mutate(id=paste(SETNO,YEAR,sep="-"))%>% #stations are repeated in this dataset for each species, this will prune it down
  distinct(id,.keep_all=TRUE)

#plot the buffers
ggplot()+
  geom_sf(data = buffer_poly, aes(fill = factor(buffer)), color = "black", alpha = 0.5) +
  geom_sf(data=rvstations%>%filter(YEAR>2010),size=0.25)+
  geom_sf(data=basemap)+
  coord_sf(xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)],expand=0)+
  theme_bw()+
  annotation_scale()+
  scale_fill_viridis_d(option = "viridis", direction = -1)+
  labs(fill="Buffer distance (km)")

#show how the plot works
buffer_plot <- ggplot()+
  geom_sf(data = contour_250,lwd=0.4,col="grey40")+
  geom_sf(data = buffer_poly , aes(fill = factor(buffer)), color = "black", alpha = 0.5) +
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=rvstations%>%
            filter(!is.na(distance_category),
                   YEAR>2015), #just to thin the data out a bit
          aes(fill=factor(distance_category)),shape=21)+
  geom_sf(data=rvstations%>%
            filter(is.na(distance_category),
                   YEAR>2015),pch=3,size=0.15,col="grey80")+
  geom_sf(data=webca,fill=NA,col="black",lwd=1.05)+
  coord_sf(xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)],expand=0)+
  theme_bw()+
  annotation_scale()+
  labs(fill="Buffer (km)")+
  scale_fill_brewer(palette = "Set1")

ggsave("output/buffer_plot2.0.png",buffer_plot,height=6,width=6.5,units="in",dpi=300)

rv_count <- rvstations%>%
  data.frame()%>%
  mutate(buffer = ifelse(is.na(distance_category),"Outside",distance_category))%>%
  group_by(YEAR,buffer)%>%
  summarise(count=n())%>%
  ungroup()%>%
  data.frame()%>%
  mutate(buffer=factor(buffer,levels=c("0","50","100","Outside")),
         decade = paste0(substr(YEAR, 1, 3), "0s"))

count_plot <- ggplot(data=rv_count%>%filter(buffer!="Outside"),aes(x=YEAR,y=count,group=buffer,fill=buffer))+
  geom_line(aes(col=buffer))+
  geom_point(shape=21,col="black")+
  theme_bw()+
  labs(fill="Buffer (km)",col="Buffer (km)",y="Number of stations",x="")

ggsave("output/count_plot2.0.png",count_plot,height=6,width=6.5,units="in",dpi=300)

count_box <- ggplot(rv_count %>% filter(buffer != "Outside"), 
                   aes(x = decade, y = count, group = interaction(decade, buffer), fill = buffer)) +
              geom_boxplot() +
              geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5,shape=21,size=0.5) +
              theme_bw() +
              labs(x = "", y = "# RV sets per year", fill = "Buffer (km)") +
              scale_fill_brewer(palette = "Set1")

#now make a fancy plot

plot_lims2 <- buffer_poly%>%
              st_transform(utm)%>%
              st_buffer(30)%>%
              st_transform(CanProj)%>%
              st_bbox()

buffer_plot2 <- ggplot()+
  geom_sf(data=network%>%filter(site!='Western/Emerald Banks Marine Refuge'),fill=NA)+
  geom_sf(data = contour_250,lwd=0.4,col="grey40")+
  geom_sf(data = buffer_poly , aes(fill = factor(buffer)), color = "black", alpha = 0.5) +
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=rvstations%>%
            filter(!is.na(distance_category),
                   YEAR>2015), #just to thin the data out a bit
          aes(fill=factor(distance_category)),shape=21,)+
  geom_sf(data=rvstations%>%
            filter(is.na(distance_category),
                   YEAR>2015),pch=3,size=0.15,col="grey80")+
  geom_sf(data=webca,fill=NA,col="black",lwd=1.05)+
  coord_sf(xlim=plot_lims2[c(1,3)],ylim=plot_lims2[c(2,4)],expand=0)+
  theme_bw()+
  labs(fill="Buffer (km)")+
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0))


combo <- buffer_plot2 + count_box + plot_layout(ncol=2)


combo <- count_box + 
  inset_element(buffer_plot2, 
                left = 0, 
                bottom = 0.6, 
                right = 0.4, 
                top = 0.95)

ggsave("output/combo_count.png",combo,height=6,width=7,units="in",dpi=300)

#save a datafile so you can use it in your analyses. 
save(rv_df,file="data/rv_data_processed2.0.RData")
