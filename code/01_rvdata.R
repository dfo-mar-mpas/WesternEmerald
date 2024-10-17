## This code is the initial code to get RV Survey Data for the Maritimes Region

#load libraries -----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(robis)

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


#Aquire the RV survey data
rvopen <- data_MarRV_survey_open()

#assign a common naming system (this will help to get around year conventions)
gs_names <- c("GSCAT","GSINF","GSMISSIONS","GSSPECIES")

for(i in gs_names){assign(i,rvopen[[which(grepl(i,names(rvopen)))]])}

#Now identify a coordinate for each station. For some stations there is only the 'start' others have the 'end'
#for those with a start and an end we can calculate the mid-point

GSINF <- GSINF%>%
         rowwise()%>%
          mutate(
            # Check if both start and end points are available
            MLAT = if (!is.na(SLAT) & !is.na(SLONG) & !is.na(ELAT) & !is.na(ELONG)) {
              geosphere::midPoint(c(SLONG, SLAT), c(ELONG, ELAT))[2] # Extract latitude
            } else if (!is.na(SLAT) & !is.na(SLONG)) {
              SLAT # Only start points available
            } else if (!is.na(ELAT) & !is.na(ELONG)) {
              ELAT # Only end points available
            } else {
              NA_real_ # Neither points are available
            },
            MLONG = if (!is.na(SLAT) & !is.na(SLONG) & !is.na(ELAT) & !is.na(ELONG)) {
              geosphere::midPoint(c(SLONG, SLAT), c(ELONG, ELAT))[1] # Extract longitude
            } else if (!is.na(SLAT) & !is.na(SLONG)) {
              SLONG # Only start points available
            } else if (!is.na(ELAT) & !is.na(ELONG)) {
              ELONG # Only end points available
            } else {
              NA_real_ # Neither points are available
            },
            ID = case_when(
              !is.na(SLAT) & !is.na(ELAT) ~ "Middle", # Both start and end points available
              !is.na(SLAT) ~ "Start", # Only start points are available
              !is.na(ELAT) ~ "End", # Only end points are available
              TRUE ~ NA_character_ # If neither are available
            )
          ) %>%
          ungroup()
        

# get obis data to merge taxonomy info with RV data (https://obis.org/dataset/7b6fa45f-e4fd-4e40-a537-97eb2f63c690)
# this takes hours to download!

# This bit has been run -----


# obis <- occurrence(datasetid = "7b6fa45f-e4fd-4e40-a537-97eb2f63c690")

#save(obis,file = "output/obis.RData")


# 
# #Now merge the files into a flattened dataframe -- this is long
# rvdata <- GSCAT%>% #catch data
#   left_join(.,GSMISSIONS)%>% #general info on the cruises
#   left_join(.,GSSPECIES%>%rename(SPEC = CODE))%>% #species IDs
#   left_join(.,GSINF)%>% #set specific data
#   mutate(std_count = TOTNO*1.75/DIST, #standardized to a set tow distance of 1.75 nm
#          std_wgt = TOTWGT*1.75/DIST)%>%
#   st_as_sf(coords=c("MLONG","MLAT"),crs=latlong) |> #convert to sf object
#   left_join(obiskey,
#             by = "SCI_NAME")

load("data/rvdata.RData")

# load ("output/obis.RData")
# obiskey <- obis |>
#   as.data.frame() |> 
#   mutate(SCI_NAME = toupper(scientificName)) |> 
#   select(SCI_NAME,family, order, class, phylum, kingdom) |> 
#   unique() |> 
#   group_by(SCI_NAME) |> 
#   mutate(n=n()) |> 
#   rowwise() |>
#   mutate(na_count = sum(is.na(c_across(family:kingdom)))) |>  # Count the number of NAs in each row (excluding SCI_NAME)
#   group_by(SCI_NAME) |>
#   slice_min(na_count, with_ties = FALSE) |>  # Keep the row with the fewest NAs
#   ungroup() |>
#   select(-na_count)  # Remove the na_count column


#save(rvdata,file="data/rvdata.RData") 
fish <- rvdata |> 
  filter(class %in% c("Teleostei","Elasmobranchii"))

notfish <- rvdata |> 
  filter(!class %in% c("Teleostei","Elasmobranchii"))


##load the OBIS data for the RV records and merge the full taxonomic information to then filter out fish and not fish. 

#data frame of just the stations - note that rvdata is a lengthened dataframe so each species for each station, on each survey is repeated. there is no need for the duplicate entries 
rv_stations <- GSINF%>%
               mutate(date=as.POSIXct(SDATE),
                      year=year(date),
                      decade = paste0(floor(year/10)*10,"'s"))%>%
               st_as_sf(coords=c("MLONG","MLAT"),crs=latlong)%>%
               st_transform(CanProj)

p1 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=rv_stations,size=0.12)+
  geom_sf(data=network,alpha=0.5)+
  geom_sf(data=network%>%filter(SiteName_E == "Western/Emerald Banks Marine Refuge"),
          fill="coral2",alpha=0.5)+
  facet_wrap(.~decade,nrow=2)+
  coord_sf(expand=0,xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)])+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggsave("output/rvsets_all.png",p1,width=8,height=5,units="in",dpi=300)
knitr::plot_crop("output/rvsets_all.png")    




          
