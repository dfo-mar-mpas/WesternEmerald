#load libraries -----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(robis)
library(ggspatial)

#source data scripting funciton
source("code/webca_fish_trend2.0.R")

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load filtered RV data from 03_filteredRVStationsHV.R
load("data/rv_data_processed2.0.RData")

rv_df <- rv_df%>%
  filter(YEAR>1984,!is.na(distance_category))%>% # in 1984 the gear was changed. There is a significant amount of work required to 'adjust' for the catability differnces so to avoid those, we just remove the pre-1985 data
  mutate(period = case_when(YEAR < 1993 ~ "pre-collapse", #periods taken from Nancy's paper
                            YEAR > 1992 & YEAR < 2006 ~ "post-collapse",
                            YEAR > 2005 ~ "recent"),
         #aggregate the distance categories
         dist_100 = case_when(distance_category!=0 & distance_category>100 ~ NA,
                              distance_category!=0 & distance_category<200 ~ "outside",
                              TRUE ~ "inside"),
         dist_50 = case_when(distance_category!=0 & distance_category>50 ~ NA,
                             distance_category!=0 & distance_category<100 ~ "outside",
                             TRUE ~ "inside"))


test_df <- rvstations_filtered%>%data.frame()%>%group_by(YEAR,classification,distance_category)%>%
  summarise(mean_temp=mean(BOTT_TEMP,na.rm=T),sd_temp=sd(BOTT_TEMP,na.rm=T))%>%ungroup(
    )%>%mutate(distance_category=factor(distance_category))%>%filter(!is.na(classification))

ggplot(data=test_df%>%filter(distance_category %in% c("0","50","100"), YEAR !=2004),
       aes(x=YEAR, y=mean_temp, col=distance_category, group = distance_category)
       )+geom_point()+ facet_wrap(~classification, ncol = 2)+ theme_bw()+ stat_smooth(method = "Im", se = F)


